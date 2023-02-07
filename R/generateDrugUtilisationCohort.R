# Copyright 2022 DARWIN EU (C)
#
# This file is part of DrugUtilisation
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

#' It instantiates the cohorts and their supplementary information
#' (cohorts_info) for the DUS study
#'
#' @param cdm 'cdm' object created with CDMConnector::cdm_from_con(). It must
#' must contain at least 'drug_exposure', 'drug_strength' and
#' observation_period' tables. The 'cdm' object must contain the
#' 'write_schema' as attribute and  the user should have permission to write on
#' it. It is a compulsory input, no default value is provided.
#' @param ingredientConceptId Ingredient OMOP concept that we are interested for
#' the study. It is a compulsory input, no default value is provided.
#' @param conceptSetPath Path to a folder with the concept sets of interest.
#' Concept sets must be stored in OMOP .json files. If NULL all the descendants
#' of ingredient concept id will be used. By default: NULL.
#' @param studyStartDate Minimum date where the incident exposed eras should
#' start to be considered. Only incident exposed eras larger than StudyStartDate
#' are allowed. If it is NULL no restriction is applied. By default: NULL.
#' @param studyEndDate Maximum date where the incident exposed eras should
#' start to be considered. Only incident exposed eras before StudyEndDate
#' are allowed. If it is NULL no restriction is applied. By default: NULL.
#' @param summariseMode Choice on how to summarize the exposures. There are
#' three options:
#' "FixedTime" each individual is followed the exact same number of days
#' specified in 'fixedTime' argument.
#' "AllEras" we summarize the output will be a summary of the exposed eras of
#' each individual. Each individual can contribute multiple times.
#' "FirstEra" we only consider the first observable era of each individual. In
#' this case each individual can not contribute with multiple rows.
#' By default: "AllEras".
#' @param fixedTime Time period after first exposure where we summarize the
#' ingredient of interest. Argument only considered if 'summariseMode' =
#' "FixedTime". No default value is provided.
#' @param daysPriorHistory Minimum number of days of prior history
#' (observation time) required for the incident eras to be considered. By
#' default: 0, meaning it has to be in observation_period table.
#' When Null, we do not check if in observation_period table.
#' @param gapEra Number of days between two continuous exposures to be
#' considered in the same era. By default: 180.
#' @param priorUseWashout Prior days without exposure. By default: NULL.
#' @param imputeDuration Whether/how the duration should be imputed
#' "eliminate", "median", "mean", "quantile25", "quantile75".
#' . By default: eliminate
#' @param durationRange Range between the duration must be comprised. It should
#' be a numeric vector of length two, with no NAs and the first value should be
#' equal or smaller than the second one. It is only required if imputeDuration
#' = TRUE. If NULL no restrictions are applied. By default: NULL.
#'
#' @return The function returns the 'cdm' object with the created tables as
#' references of the object.
#' @export
#'
#' @examples
generateDrugUtilisationCohort <- function(cdm,
                                          ingredientConceptId = NULL,
                                          conceptSetPath = NULL,
                                          studyStartDate = NULL,
                                          studyEndDate = NULL,
                                          summariseMode = "AllEras",
                                          fixedTime = 365,
                                          daysPriorHistory = 0,
                                          gapEra = 30,
                                          priorUseWashout = NULL,
                                          imputeDuration = "eliminate",
                                          durationRange = c(1, NA)) {
  errorMessage <- checkmate::makeAssertCollection()
  # first round of initial checks, assert Type
  checkmate::assertClass(
    cdm,
    classes = "cdm_reference",
    add = errorMessage
  )
  checkmate::assertCharacter(
    conceptSetPath,
    len = 1,
    null.ok = TRUE,
    add = errorMessage
  )
  checkmate::assertCount(
    ingredientConceptId,
    null.ok = TRUE,
    add = errorMessage
  )
  if (is.null(conceptSetPath) && is.null(ingredientConceptId)) {
    errorMessage$push(
      "'conceptSetPath' or 'ingredientConceptId' should be provided"
    )
  }
  checkmate::assertDate(
    studyStartDate,
    any.missing = FALSE,
    len = 1,
    null.ok = TRUE,
    add = errorMessage
  )
  checkmate::assertDate(
    studyEndDate,
    any.missing = FALSE,
    len = 1,
    null.ok = TRUE,
    add = errorMessage
  )
  checkmate::assertChoice(
    summariseMode,
    choices = c("AllEras", "FirstEra", "FixedTime"),
    add = errorMessage
  )
  if (summariseMode == "FixedTime") {
    checkmate::assertCount(
      fixedTime,
      positive = TRUE,
      add = errorMessage
    )
  }
  checkmate::assertCount(
    daysPriorHistory,
    null.ok = TRUE,
    add = errorMessage
  )
  checkmate::assertCount(
    gapEra,
    add = errorMessage
  )
  if (is.character(imputeDuration)) {
    checkmate::assertChoice(
      imputeDuration,
      choices = c("eliminate", "median", "mean", "quantile25", "quantile75"),
      add = errorMessage
    )
  } else {
    checkmate::assertCount(
      imputeDuration,
      positive = TRUE,
      add = errorMessage
    )
  }
  checkmate::assertNumeric(
    durationRange,
    len = 2,
    null.ok = TRUE,
    add = errorMessage
  )
  checkmate::reportAssertions(collection = errorMessage)

  if (is.null(durationRange)) {
    durationRange <- c(NA, NA)
  }

  # second round of initial checks
  checkmate::assertTRUE(
    all(
      c("drug_strength", "drug_exposure", "observation_period") %in% names(cdm)
    ),
    add = errorMessage
  )
  if (!is.null(conceptSetPath)) {
    if (!file.exists(conceptSetPath)) {
      stop(glue::glue("Invalid concept set path {conceptSetPath}"))
    } else {
      if (dir.exists(conceptSetPath)) {
        conceptSets <- dplyr::tibble(concept_set_path = list.files(
          path = conceptSetPath,
          full.names = TRUE
        ))
      } else {
        conceptSets <- dplyr::tibble(concept_set_path = .env$conceptSetPath)
      }
      conceptSets <- conceptSets %>%
        dplyr::filter(tools::file_ext(.data$concept_set_path) == "json") %>%
        dplyr::mutate(
          concept_set_name =
            tools::file_path_sans_ext(basename(.data$concept_set_path))
        ) %>%
        dplyr::mutate(cohort_definition_id = dplyr::row_number())
      if (conceptSets %>% nrow() == 0) {
        stop(glue::glue("No 'json' file found in {conceptSetPath}"))
      }
    }
  }
  if (sum(is.na(durationRange)) == 0) {
    checkmate::assertTRUE(
      durationRange[1] <= durationRange[2],
      add = errorMessage
    )
  }
  checkmate::reportAssertions(collection = errorMessage)

  if (!is.null(conceptSetPath)) {
    tryCatch(
      expr = conceptList <- readConceptSets(conceptSets),
      error = function(e) {
        stop("The json file is not a properly formated OMOP concept set.")
      }
    )

    conceptList <- conceptList %>%
      dplyr::filter(.data$include_descendants == FALSE) %>%
      dplyr::union(
        cdm[["concept_ancestor"]] %>%
          dplyr::select(
            "concept_id" = "ancestor_concept_id",
            "descendant_concept_id"
          ) %>%
          dplyr::inner_join(
            conceptList %>%
              dplyr::filter(.data$include_descendants == TRUE),
            copy = TRUE,
            by = "concept_id"
          ) %>%
          dplyr::select(-"concept_id") %>%
          dplyr::rename("concept_id" = "descendant_concept_id") %>%
          dplyr::collect()
      ) %>%
      dplyr::select(-"include_descendants") %>%
      dplyr::rename("drug_concept_id" = "concept_id")
    # eliminate the ones that is_excluded = TRUE
    conceptList <- conceptList %>%
      dplyr::filter(.data$is_excluded == FALSE) %>%
      dplyr::select("cohort_definition_id", "drug_concept_id") %>%
      dplyr::anti_join(
        conceptList %>%
          dplyr::filter(.data$is_excluded == TRUE),
        by = "drug_concept_id"
      )
    if (!is.null(ingredientConceptId)) {
      conceptList <- cdm[["drug_strength"]] %>%
        dplyr::filter(.data$ingredient_concept_id == .env$ingredientConceptId) %>%
        dplyr::select("drug_concept_id") %>%
        dplyr::collect() %>%
        dplyr::inner_join(conceptList, by = "drug_concept_id")
    }
    conceptSets <- conceptSets %>%
      dplyr::select(
        "cohortId" = "cohort_definition_id",
        "cohortName" = "concept_set_name",
        "concepSetPath" = "concept_set_path"
      )
  } else {
    conceptSets <- dplyr::tibble(
      cohortId = 1,
      cohortName = paste0("ingredient: ", ingredientConceptId),
      conceptSetPath = as.character(NA)
    )
    conceptList <- cdm[["drug_strength"]] %>%
      dplyr::filter(.data$ingredient_concept_id == .env$ingredientConceptId) %>%
      dplyr::select("drug_concept_id") %>%
      dplyr::collect() %>%
      dplyr::mutate(cohort_definition_id = 1)
  }

  if (nrow(conceptList) == 0) {
    stop("No concepts were not found in the vocabulary using this settings")
  }

  # get sql dialect of the database
  dialect <- CDMConnector::dbms(attr(cdm, "dbcon"))
  # get the name of the info table

  # split conceptList in small bits smaller than 500k to avoid problems with
  # redshift
  numberMaxCodes <- 500000
  numberCodes <- nrow(conceptList)
  if (numberCodes <= numberMaxCodes) {
    idStart <- 1
    idEnd <- numberCodes
  } else {
    idStart <- seq(1, numberCodes, by = numberMaxCodes)
    idEnd <- idStart + numberMaxCodes - 1
    idEnd[idEnd > numberCodes] <- numberCodes
  }

  # subset drug_exposure and only get the drug concept ids that we are
  # interested in.
  for (k in 1:length(idStart)) {
    cohort.k <- cdm[["drug_exposure"]] %>%
      dplyr::select(
        "subject_id" = "person_id",
        "drug_concept_id",
        "drug_exposure_start_date",
        "drug_exposure_end_date"
      ) %>%
      dplyr::inner_join(
        conceptList[idStart[k]:idEnd[k],],
        by = "drug_concept_id",
        copy = TRUE
      ) %>%
      dplyr::compute()
    if (k == 1) {
      cohort <- cohort.k
    } else {
      cohort <- cohort %>%
        dplyr::union_all(cohort.k) %>%
        dplyr::compute()
    }
  }


  if (cohort %>% dplyr::tally() %>% dplyr::pull("n") == 0) {
    stop(
      "No record found with the current specifications in drug_exposure table"
    )
  }

  attrition <- addAttitionLine(cohort, "Initial Exposures")

  # compute the number of days exposed according to:
  # days_exposed = end - start + 1
  cohort <- cohort %>%
    dplyr::mutate(days_exposed = dbplyr::sql(
      CDMConnector::datediff(
        start = "drug_exposure_start_date",
        end = "drug_exposure_end_date"
      )
    ) + 1)

  # impute or eliminate the exposures that duration does not fulfill the
  # conditions ( <=0; <durationRange[1]; >durationRange[2])
  cohort <- imputeVariable(
    x = cohort,
    variableName = "days_exposed",
    impute = imputeDuration,
    lowerBound = durationRange[1],
    upperBound = durationRange[2],
    imputeValueName = "imputeDuration"
  ) %>%
    dplyr::mutate(days_to_add = as.integer(.data$days_exposed - 1)) %>%
    dplyr::compute() %>%
    dplyr::mutate(drug_exposure_end_date = as.Date(dbplyr::sql(
      dateadd(
        date = "drug_exposure_start_date",
        number = "days_to_add"
      )
    ))) %>%
    dplyr::compute()

  attrition <- attrition %>%
    dplyr::union_all(addAttitionLine(cohort, "Imputation"))


  cohort <- cohort %>%
    dplyr::select(
      "cohort_definition_id",
      "subject_id",
      "date_event" = "drug_exposure_start_date"
    ) %>%
    dplyr::mutate(date_id = -1) %>%
    dplyr::union_all(
      cohort %>%
        dplyr::mutate(
          date_event = as.Date(!!CDMConnector::dateadd(
            date = "drug_exposure_end_date",
            number = gapEra
          )),
          date_id = 1
        ) %>%
        dplyr::select(
          "cohort_definition_id", "subject_id", "date_event", "date_id"
        )
    ) %>%
    dplyr::group_by(.data$cohort_definition_id, .data$subject_id) %>%
    dbplyr::window_order(.data$date_event, .data$date_id) %>%
    dplyr::mutate(cum_id = cumsum(.data$date_id)) %>%
    dplyr::filter(
      .data$cum_id == 0 || (.data$cum_id == -1 && .data$date_id == -1)
    ) %>%
    dplyr::mutate(
      name = dplyr::if_else(
        .data$date_id == -1,
        "cohort_start_date",
        "cohort_end_date"
      ),
      era_id = dplyr::if_else(
        .data$date_id == -1,
        1,
        0
      )
    ) %>%
    dplyr::mutate(era_id = cumsum(as.numeric(.data$era_id))) %>%
    dplyr::ungroup() %>%
    dbplyr::window_order() %>%
    dplyr::select(
      "cohort_definition_id", "subject_id", "era_id", "name", "date_event"
    ) %>%
    tidyr::pivot_wider(names_from = "name", values_from = "date_event") %>%
    dplyr::mutate(cohort_end_date = as.Date(!!CDMConnector::dateadd(
      date = "cohort_end_date",
      number = -gapEra
    ))) %>%
    dplyr::compute()

  attrition <- attrition %>%
    dplyr::union_all(addAttitionLine(cohort, "Eras"))


  if (!is.null(priorUseWashout)) {
    cohort <- cohort %>%
      dplyr::left_join(
        cohort %>%
          dplyr::select(
            "cohort_definition_id", "subject_id", "era_id",
            "prior_era" = "cohort_end_date"
          ) %>%
          dplyr::mutate(era_id = .data$era_id + 1),
        by = c("cohort_definition_id", "subject_id", "era_id")
      ) %>%
      dplyr::mutate(prior_era = as.numeric(!!CDMConnector::datediff(
        "prior_era", "cohort_start_date"
      ))) %>%
      dplyr::filter(
        is.na(.data$prior_era) | .data$prior_era > .env$priorUseWashout
      ) %>%
      dplyr::select(-"prior_era", -"era_id") %>%
      dplyr::compute()
    attrition <- attrition %>%
      dplyr::union_all(addAttitionLine(
        cohort,
        paste0("Prior washout of ", priorUseWashout, " days")
      ))

  } else {
    cohort <- cohort %>% dplyr::select(-"era_id")
  }

  if (!is.null(studyStartDate)) {
    cohort <- cohort %>%
      dplyr::filter(.data$cohort_start_date >= .env$studyStartDate)

    attrition <- attrition %>%
      dplyr::union_all(addAttitionLine(
        cohort,
        paste0("Start after or at ", studyStartDate)
      ))

  }

  if (!is.null(studyEndDate)) {
    cohort <- cohort %>%
      dplyr::filter(.data$cohort_start_date <= .env$studyEndDate)

    attrition <- attrition %>%
      dplyr::union_all(addAttitionLine(
        cohort,
        paste0("Start before or at ", studyEndDate)
      ))

  }

  if (!is.null(daysPriorHistory)) {
    cohort <- inObservation(cohort, cdm = cdm) %>%
      dplyr::filter(.data$in_observation == TRUE) %>%
      dplyr::compute()

    attrition <- attrition %>%
      dplyr::union_all(addAttitionLine(
        cohort,
        "In observation on cohort_start_date"
      ))

    cohort <- addPriorHistory(cohort, cdm = cdm) %>%
      dplyr::filter(.data$prior_history >= .env$daysPriorHistory) %>%
      dplyr::select(-"prior_history")

    attrition <- attrition %>%
      dplyr::union_all(addAttitionLine(
        cohort,
        paste0("At least ", daysPriorHistory, " days of prior history")
      ))

  }

  if (summariseMode == "FirstEra") {
    cohort <- cohort %>%
      dplyr::group_by(.data$cohort_definition_id, .data$subject_id) %>%
      dplyr::filter(
        .data$cohort_start_date == min(.data$cohort_start_date, na.rm = TRUE)
      ) %>%
      dplyr::ungroup()
    attrition <- attrition %>%
      dplyr::union_all(addAttitionLine(cohort, "Only first era"))

  } else if (summariseMode == "FixedTime") {
    cohort <- cohort %>%
      dplyr::group_by(.data$cohort_definition_id, .data$subject_id) %>%
      dplyr::summarise(
        cohort_start_date = min(.data$cohort_start_date, na.rm = TRUE)
      ) %>%
      dplyr::mutate(cohort_end_date = !!CDMConnector::dateadd(
        "cohort_start_date",
        fixedTime - 1
      )) %>%
      dplyr::ungroup()
    attrition <- attrition %>%
      dplyr::union_all(addAttitionLine(
        cohort,
        paste0("Only first era; fixedTime = ", fixedTime, " days")
      ))
  }

  cohort <- cohort %>%
    dplyr::select(
      "cohort_definition_id", "subject_id", "cohort_start_date",
      "cohort_end_date"
    ) %>%
    dplyr::compute()

  attr(cohort, "cohortSet") <- conceptSets

  attr(cohort, "attrition") <- attrition %>%
    dplyr::left_join(
      dplyr::tibble(
        order_id = c(1:10),
        reason = c(
          "Initial Exposures",
          "Imputation",
          "Eras",
          paste0("Prior washout of ", priorUseWashout, " days"),
          paste0("Start after or at ", studyStartDate),
          paste0("Start before or at ", studyEndDate),
          "In observation on cohort_start_date",
          paste0("At least ", daysPriorHistory, " days of prior history"),
          "Only first era",
          paste0("Only first era; fixedTime = ", fixedTime, " days")
        )
      ),
      by = "reason"
    ) %>%
    dplyr::arrange(.data$cohort_definition_id, .data$order_id) %>%
    dplyr::select(-"order_id")

  return(cohort)
}

#' Impute or eliminate values under a certain conditions
#'
#' @param x x
#' @param variableName variableName
#' @param impute impute
#' @param lowerBound lowerBound
#' @param upperBound upperBound
#' @param imputeValueName imputeValueName
#'
#' @noRd
imputeVariable <- function(x,
                           variableName,
                           impute,
                           lowerBound,
                           upperBound,
                           imputeValueName) {
  # rename the variable of interest to variable
  x <- x %>%
    dplyr::rename("variable" = .env$variableName)
  # identify (as impute = 1)
  x <- x %>%
    dplyr::mutate(impute = dplyr::if_else(is.na(.data$variable),
      1,
      0
    ))

  # identify (as impute = 1) the values smaller than lower bound
  if (!is.na(lowerBound)) {
    x <- x %>%
      dplyr::mutate(impute = dplyr::if_else(
        is.na(.data$variable),
        1,
        dplyr::if_else(.data$variable < .env$lowerBound,
          1,
          .data$impute
        )
      ))
  }
  # identify (as impute = 1) the values greater than upper bound
  if (!is.na(upperBound)) {
    x <- x %>%
      dplyr::mutate(impute = dplyr::if_else(
        is.na(.data$variable),
        1,
        dplyr::if_else(.data$variable > .env$upperBound,
          1,
          .data$impute
        )
      ))
  }
  # if impute is false then all values with impute = 1 are not considered
  if (impute == "eliminate") {
    x <- x %>%
      dplyr::filter(.data$impute == 0)
  }

  if (impute == "median") {
    x <- x %>% dplyr::mutate(variable = dplyr::if_else(
      .data$impute == 1,
      stats::median(
        x %>% dplyr::filter(.data$impute == 0) %>%
          dplyr::pull("variable")
      ),
      .data$variable
    ))
  }


  if (impute == "mean") {
    x <- x %>% dplyr::mutate(variable = dplyr::if_else(
      .data$impute == 1,
      base::mean(
        x %>% dplyr::filter(.data$impute == 0) %>%
          dplyr::pull("variable")
      ),
      .data$variable
    ))
  }
  # %>%
  # dplyr::rename(!!imputeValueName := "imputeValue")

  if (impute == "quantile25") {
    x <- x %>% dplyr::mutate(variable = dplyr::if_else(
      .data$impute == 1,
      as.numeric(
        stats::quantile(
          x %>% dplyr::filter(.data$impute == 0) %>%
            dplyr::pull("variable"),
          0.25,
          na.rm = TRUE
        )
      ),
      .data$variable
    ))
  }


  if (impute == "quantile75") {
    x <- x %>% dplyr::mutate(variable = dplyr::if_else(
      .data$impute == 1,
      as.numeric(
        stats::quantile(
          x %>% dplyr::filter(.data$impute == 0) %>%
            dplyr::pull("variable"),
          0.75,
          na.rm = TRUE
        )
      ),
      .data$variable
    ))
  }


  if (is.numeric(impute)) {
    x <- x %>%
      dplyr::rename("imputeValue" = .env$imputeValueName) %>%
      dplyr::mutate(variable = dplyr::if_else(.data$impute == 1,
        .data$imputeValue,
        .data$variable
      ))
  }

  if (imputeValueName == "imputeDuration") {
    x <- x %>% dplyr::mutate(variable = floor(.data$variable))
  }

  x <- x %>%
    dplyr::select(-"impute") %>%
    dplyr::rename(!!variableName := "variable")

  return(x)
}

#' Add line to the attrition tibble
#'
#' @noRd
addAttitionLine <- function(cohort, reason) {
  if ("cohort_definition_id" %in% colnames(cohort)) {
    cohort <- cohort %>% dplyr::group_by(.data$cohort_definition_id)
  }
  return(
    cohort %>%
      dplyr::summarise(
        number_subjects = dplyr::n_distinct(.data$subject_id),
        number_records = dplyr::n()
      ) %>%
      dplyr::collect() %>%
      dplyr::mutate(reason = .env$reason)
  )
}

#' Function to read the concept sets and export a tibble with
#' cohort_definition_id and drug_concept_id with the list of drug_concept_id
#' included in each concept set
#' @noRd
readConceptSets <- function(conceptSets) {
  for (k in 1:nrow(conceptSets)) {
    conceptSetName <- conceptSets$concept_set_name[k]
    conceptSet <- RJSONIO::fromJSON(conceptSets$concept_set_path[k])
    conceptSet <- lapply(conceptSet$items, function(x) {
      x <- append(x, x[["concept"]])
      x[["concept"]] <- NULL
      return(x)
    }) %>%
      dplyr::bind_rows() %>%
      dplyr::mutate(
        cohort_definition_id = .env$conceptSets$cohort_definition_id[k]
      )
    if (k == 1) {
      conceptList <- conceptSet
    } else {
      conceptList <- rbind(conceptList, conceptSet)
    }
  }
  conceptList <- conceptList %>%
    dplyr::select(
      "cohort_definition_id",
      "concept_id" = "CONCEPT_ID",
      "is_excluded" = "isExcluded",
      "include_descendants" = "includeDescendants"
    )
  return(conceptList)
}

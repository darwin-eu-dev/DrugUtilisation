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

#' Generates a cohort of the drug use of a certain list of concepts.
#'
#' @param cdm 'cdm' object created with CDMConnector::cdm_from_con(). It must
#' must contain at least 'drug_exposure', 'drug_strength' and
#' observation_period' tables. The 'cdm' object must contain the
#' 'write_schema' as attribute and  the user should have permission to write on
#' it. It is a compulsory input, no default value is provided.
#' @param conceptSetList Names list of concept sets.
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
#' @param tablePrefix The stem for the permanent tables that will
#' be created. If NULL, temporary tables will be used throughout.
#'
#' @return The function returns the 'cdm' object with the created tables as
#' references of the object.
#' @export
#'
#' @examples
generateDrugUtilisationCohort <- function(cdm,
                                          conceptSetList,
                                          name,
                                          temporary = TRUE,
                                          studyPeriod = NULL,
                                          summariseMode = "AllEras",
                                          fixedTime = 365,
                                          daysPriorHistory = 0,
                                          gapEra = 30,
                                          priorUseWashout = 0,
                                          imputeDuration = "eliminate",
                                          daysExposedRange = c(1, Inf)) {
  checkCdm(
    cdm, c("drug_exposure", "observation_period", "person", "drug_strength")
  )
  checkConceptSetList(conceptSetList)
  checkCohortName(name, names(cdm))
  checkmate::assertLogical(temporary, any.missing = FALSE, len = 1)
  checkStudyPeriod(studyPeriod)
  checkSummariseMode(summariseMode, fixedTime)
  checkmate::assertIntegerish(daysPriorHistory, lower = 0, len = 1)
  checkmate::assertIntegerish(gapEra, lower = 0, len = 1)
  checkmate::assertIntegerish(priorUseWashout, lower = 0, len = 1)
  checkImputeDuration(imputeDuration)
  checkRange(daysExposedRange)

  # attr(cdm, "temporary") <- temporary
  # compute <- function(x, cdm) {
  #   CDMConnector::computeQuery(
  #     x,
  #     name = paste0(attr(cdm, "write_prefix"), CDMConnector:::uniqueTableName()),
  #     temporary = attr(cdm, "temporary"),
  #     schema = attr(cdm, "write_schema"),
  #     overwrite = TRUE
  #   )
  # }

  # subset drug_exposure and only get the drug concept ids that we are
  # interested in.
  cohort <- subsetTable(cdm, conceptList, "drug_exposure")

  if (cohort %>% dplyr::tally() %>% dplyr::pull("n") == 0) {
    cli::cli_abort(
      "No record found with the current specifications in drug_exposure table"
    )
  }

  # get cohort set
  cohortSet <- attr(cohort, "cohortSet") %>%
    dplyr::mutate(
      study_period_start = .env$studyPeriod[1],
      study_period_end = .env$studyPeriod[2],
      summarise_mode = .env$summariseMode,
      fixed_time = .env$fixedTime,
      days_prior_history = .env$daysPriorHistory,
      gap_era = .env$gapEra,
      prior_use_washout = .env$priorUseWashout,
      impute_duration = .env$imputeDuration,
      days_exposed_range_min = .env$daysExposedRange[1],
      days_exposed_range_max = .env$daysExposedRange[2]
    )

  attrition <- attritionLine(NULL, cohort, "Initial Exposures")

  # correct days exposed
  cohort <- correctDaysExposed(cohort, cdm)
  attrition <- attritionLine(attrition, cohort, "Days exposed imputation")


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
    dplyr::union_all(addattritionLine(cohort, "Eras"))


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
      dplyr::union_all(addattritionLine(
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
      dplyr::union_all(addattritionLine(
        cohort,
        paste0("Start after or at ", studyStartDate)
      ))

  }

  if (!is.null(studyEndDate)) {
    cohort <- cohort %>%
      dplyr::filter(.data$cohort_start_date <= .env$studyEndDate)

    attrition <- attrition %>%
      dplyr::union_all(addattritionLine(
        cohort,
        paste0("Start before or at ", studyEndDate)
      ))

  }

  if (!is.null(daysPriorHistory)) {
    cohort <- inObservation(cohort, cdm = cdm) %>%
      dplyr::filter(.data$in_observation == TRUE) %>%
      dplyr::compute()

    attrition <- attrition %>%
      dplyr::union_all(addattritionLine(
        cohort,
        "In observation on cohort_start_date"
      ))

    cohort <- addPriorHistory(cohort, cdm = cdm) %>%
      dplyr::filter(.data$prior_history >= .env$daysPriorHistory) %>%
      dplyr::select(-"prior_history")

    attrition <- attrition %>%
      dplyr::union_all(addattritionLine(
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
      dplyr::union_all(addattritionLine(cohort, "Only first era"))

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
      dplyr::union_all(addattritionLine(
        cohort,
        paste0("Only first era; fixedTime = ", fixedTime, " days")
      ))
  }

  cohort <- cohort %>%
    dplyr::select(
      "cohort_definition_id", "subject_id", "cohort_start_date",
      "cohort_end_date"
    )

  attrition <- attrition %>%
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

  cohortCount <- cohort %>%
    dplyr::group_by(.data$cohort_definition_id) %>%
    dplyr::summarise(
      number_records = dplyr::n(),
      number_subjects = dplyr::n_distinct(.data$subject_id),
      .groups = "drop"
    ) %>%
    dplyr::compute()

  con <- attr(cdm, "dbcon")
  if(is.null(tablePrefix)){
    cohortRef <- cohort %>% CDMConnector::computeQuery()
    cohortSetTableName <- CDMConnector::uniqueTableName()
    DBI::dbWriteTable(con, cohortSetTableName, conceptSets, temporary = TRUE)
    cohortSetRef <- dplyr::tbl(con, cohortSetTableName)
    cohortAttritionTableName <- CDMConnector::uniqueTableName()
    DBI::dbWriteTable(con, cohortAttritionTableName, attrition, temporary = TRUE)
    cohortAttritionRef <- dplyr::tbl(con, cohortAttritionTableName)
    cohortCountRef <- cohortCount
  } else {
    writeSchema <- attr(cdm, "write_schema")
    cohortRef <- CDMConnector::computeQuery(
      x = cohort, name = tablePrefix, temporary = FALSE,
      schema = writeSchema, overwrite = TRUE
    )
    DBI::dbWriteTable(
      conn = con, name = CDMConnector::inSchema(writeSchema, paste0(tablePrefix, "_set"), CDMConnector::dbms(con)),
      value = as.data.frame(conceptSets),
      overwrite = TRUE
    )
    cohortSetRef <- dplyr::tbl(con, CDMConnector::inSchema(writeSchema, paste0(tablePrefix, "_set"), CDMConnector::dbms(con)))
    DBI::dbWriteTable(
      conn = con, name = CDMConnector::inSchema(writeSchema, paste0(tablePrefix, "_attrition"), CDMConnector::dbms(con)),
      value = as.data.frame(attrition),
      overwrite = TRUE
    )
    cohortAttritionRef <- dplyr::tbl(con, CDMConnector::inSchema(writeSchema, paste0(tablePrefix, "_attrition"), CDMConnector::dbms(con)))
    cohortCountRef <- CDMConnector::computeQuery(
      x = cohortCount, name = paste0(tablePrefix, "_count"), temporary = FALSE,
      schema = writeSchema, overwrite = TRUE
    )
  }

  cohort <- CDMConnector::newGeneratedCohortSet(
    cohortRef = cohortRef,
    cohortSetRef = cohortSetRef,
    cohortAttritionRef = cohortAttritionRef,
    cohortCountRef = cohortCountRef
  )

  return(cohort)
}

#' Impute or eliminate values under a certain conditions
#' @noRd
imputeVariable <- function(x,
                           column,
                           impute,
                           range,
                           imputeRound = FALSE) {
  # identify (as impute = 1)
  x <- x %>%
    dplyr::mutate(impute = dplyr::if_else(is.na(.data[[column]]), 1, 0))

  # identify (as impute = 1) the values smaller than lower bound
  if (!is.na(range[1])) {
    x <- x %>%
      dplyr::mutate(impute = dplyr::if_else(
        .data$impute == 0,
        dplyr::if_else(.data[column] < !!range[1], 1, 0),
        1
      ))
  }
  # identify (as impute = 1) the values greater than upper bound
  if (!is.na(range[2])) {
    x <- x %>%
      dplyr::mutate(impute = dplyr::if_else(
        .data$impute == 0,
        dplyr::if_else(.data[column] > !!range[2], 1, 0),
        1
      ))
  }
  # if impute is false then all values with impute = 1 are not considered
  if (impute == "eliminate") {
    x <- x %>%
      dplyr::filter(.data$impute == 0)
  }

  if (impute == "median") {
    x <- x %>% dplyr::mutate(
      !!column := dplyr::if_else(
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

#' Add a line in the attrition table. If the table does not exist it is created
#'
#' @param attrition An attrition table. If NULL a new attrition table is created.
#' @param x A table in the cdm. It must contain 'cohort_definition_id'
#' @param reason A character with the name of the reason.
#'
#' @return The function returns the 'cdm' object with the created tables as
#' references of the object.
#' @export
#'
#' @examples
addAttritionLine <- function(attrition = NULL, x, reason) {
  if (!is.null(attrition)) {
    checkmate::assertTibble(attrition)
    checkmate::assertTRUE(all(
      c(
        "cohort_definition_id", "number_records", "number_subjects", "reason_id",
        "reason", "excluded_records", "excluded_subjects"
      ) %in% colnames(attrition)
    ))
  }
  checkmate::assertClass(x, "tbl")
  checkmate::assertTRUE(all(c("cohort_definition_id", "subject_id") %in% colnames(x)))
  checkmate::assertCharacter(reason, len = 1)
  attrition <- attritionLine(atrition, x, reason)
  return(attrition)
}

#' @noRd
attritionLine <- function(atrition, x, reason) {
  if (is.null(attrition)) {
    attrition <- countAttrition(x, reason, 1)
  } else {
    id <- max(attrition$reason_id)
    attrition <- attrition %>%
      dplyr::bind_rows(countAttrition(x, reason, id)) %>%
      addExcludedCounts(id)
  }
  return(attrition)
}

#' @noRd
countAttrition <- function(x, reason, id) {
  if (id == 1) {
    num <- 0
  } else {
    num <- as.numeric(NA)
  }
  attrition <- x %>%
    dplyr::group_by(.data$cohort_definition_id) %>%
    dplyr::summarise(
      number_records = dplyr::n(),
      number_subjects = dplyr::n_distinct(.data$subject_id),
      .groups = "drop"
    ) %>%
    dplyr::collect() %>%
    dplyr::mutate(
      reason_id = id, reason = .env$reason, excluded_records = num, excluded_subjects = num
    )
  return(attrition)
}

#' @noRd
addExcludedCounts <- function(x, id) {
  attrition %>%
    dplyr::group_by(.data$cohort_definition_id) %>%
    dplyr::mutate(
      excluded_records = dplyr::if_else(
        is.na(.data$excluded_records),
        .data$number_records[.env$id] - .data$number_records[.env$id - 1],
        .data$excluded_records
      ),
      excluded_subjects = dplyr::if_else(
        is.na(.data$excluded_subjects),
        .data$number_subjects[.env$id] - .data$number_subjects[.env$id - 1],
        .data$excluded_subjects
      )
    )
}

#' @noRd
subsetTable <- function(cdm, conceptSetList, table) {
  # create cohortSet
  cohortSet <- dplyr::tibble(cohort_name = names(conceptSetList)) %>%
    dplyr::mutate(cohort_definition_id = dplyr::row_number())
  # get names
  conceptId <- "drug_concept_id"
  startDate <- "drug_exposure_start_date"
  endDate <- "drug_exposure_end_date"
  # create concept list
  conceptList <- purrr::map(conceptSetList, dplyr::as_tibble) %>%
    dplyr::bind_rows(.id = "cohort_name") %>%
    dplyr::rename("concept_id" =  "value") %>%
    dplyr::inner_join(
      cohortSet %>%
        dplyr::select("cohort_name", "cohort_definition_id"),
      by = "cohort_name"
    ) %>%
    dplyr::select(-"cohort_name")
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
  # subset the table
  for (k in 1:length(idStart)) {
    cohort.k <- cdm[[table]] %>%
      dplyr::select(
        "subject_id" = "person_id",
        "concept_id" = !!conceptId,
        "cohort_start_date" = !!startDate,
        "cohort_end_date" = !!endDate
      ) %>%
      dplyr::inner_join(
        conceptList[idStart[k]:idEnd[k],],
        by = "concept_id",
        copy = TRUE
      ) %>%
      compute(cdm)
    if (k == 1) {
      cohort <- cohort.k
    } else {
      cohort <- cohort %>%
        dplyr::union_all(cohort.k)
    }
  }
  cohort <- compute(cohort, cdm)
  attr(cohort, "cohortSet") <- cohortSet
  return(cohort)
}

#' @noRd
correctDaysExposed <- function(x, daysExposedRange, cdm) {
  # compute the number of days exposed according to:
  # days_exposed = end - start + 1
  x <- x %>%
    dplyr::mutate(days_exposed = !!CDMConnector::datediff(
      start = "cohort_start_date",
      end = "cohort_end_date"
    ) + 1)

  # impute or eliminate the exposures that duration does not fulfill the
  # conditions (<daysExposedRange[1]; >daysExposedRange[2])
  x <- imputeVariable(
    x = x,
    variableName = "days_exposed",
    impute = imputeDuration,
    lowerBound = daysExposedRange[1],
    upperBound = daysExposedRange[2],
    imputeValueName = "imputeDuration"
  ) %>%
    dplyr::mutate(days_to_add = as.integer(.data$days_exposed - 1)) %>%
    dplyr::compute() %>%
    dplyr::mutate(drug_exposure_end_date = as.Date(dbplyr::sql(
      CDMConnector::dateadd(
        date = "drug_exposure_start_date",
        number = "days_to_add"
      )
    ))) %>%
    compute(cdm)
  return(x)
}




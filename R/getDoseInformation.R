# Copyright 2022 DARWIN EU (C)
#
# This file is part of DrugUtilizationCharacteristics
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

#' Explain function
#'
#' @param cdm 'cdm' object created with CDMConnector::cdm_from_con(). It must
#' must contain at least 'drug_exposure', 'drug_strength' and
#' observation_period' tables.
#' @param dusCohortName Name of the cohort that we want to obtain the dose
#' information.
#' @param conceptSetPath Path to a folder with the concept sets of interest.
#' Concept sets must be stored in OMOP .json files. If NULL all the descendants
#' of ingredient concept id will be used. By default: NULL.
#' @param ingredientConceptId Ingredient OMOP concept that we are interested for
#' the study. It is a compulsory input, no default value is provided.
#' @param gapEra Number of days between two continuous exposures to be
#' considered in the same era. By default: 180.
#' @param eraJoinMode How two different continuous exposures are joined in an
#' era. There are four options:
#' "Zero" the exposures are joined considering that the period between both
#' continuous exposures the subject is treated with a daily dose of zero. The
#' time between both exposures contributes to the total exposed time.
#' "Join" the exposures are joined considering that the period between both
#' continuous exposures the subject is treated with a daily dose of zero. The
#' time between both exposures does not contribute to the total exposed time.
#' "Previous" the exposures are joined considering that the period between both
#' continuous exposures the subject is treated with the daily dose of the
#' previous subexposure. The time between both exposures contributes to the
#' total exposed time.
#' "Subsequent" the exposures are joined considering that the period between
#' both continuous exposures the subject is treated with the daily dose of the
#' subsequent subexposure. The time between both exposures contributes to the
#' total exposed time.
#' By default: "Previous".
#' @param overlapMode How the overlapping between two exposures that do not
#' start on the same day is solved inside a subexposure. There are five possible
#'  options:
#' "Previous" the considered daily_dose is the one of the earliest exposure.
#' "Subsequent" the considered daily_dose is the one of the new exposure that
#' starts in that subexposure.
#' "Minimum" the considered daily_dose is the minimum of all of the exposures in
#' the subexposure.
#' "Maximum" the considered daily_dose is the maximum of all of the exposures in
#' the subexposure.
#' "Sum" the considered daily_dose is the sum of all the exposures present in
#' the subexposure.
#' By default: "Previous".
#' @param sameIndexMode How the overlapping between two exposures that start on
#' the same day is solved inside a subexposure. There are five possible options:
#' "Minimum" the considered daily_dose is the minimum of all of the exposures in
#' the subexposure.
#' "Maximum" the considered daily_dose is the maximum of all of the exposures in
#' the subexposure.
#' "Sum" the considered daily_dose is the sum of all the exposures present in
#' the subexposure.
#' By default: "Sum".
#' @param imputeDuration Whether/how the duration should be imputed
#' "eliminate", "median", "mean", "quantile25", "quantile75".
#' . By default: eliminate
#' @param imputeDailyDose Whether/how the daily_dose should be imputed
#' "eliminate", "median", "mean", "quantile25", "quantile75". By default:
#' "eliminate"
#' @param durationRange Range between the duration must be comprised. It should
#' be a numeric vector of length two, with no NAs and the first value should be
#' equal or smaller than the second one. It is only required if imputeDuration
#' = TRUE. If NULL no restrictions are applied. By default: NULL.
#' @param dailyDoseRange Range between the daily_dose must be comprised. It
#' should be a numeric vector of length two, with no NAs and the first value
#' should be equal or smaller than the second one. It is only required if
#' imputeDailyDose = TRUE. If NULL no restrictions are applied. By default:
#' NULL.
#'
#' TERMINOLOGY
#' - exposure: we refer to exposure to a row in the drug_exposure table of the
#' cdm object.
#' - subexposure: period of time were the number of exposures that a person is
#' exposed to does not change.
#' - continuous exposure: period of time where the individual is exposed with no
#' interruption. This period can be comprised by multiple subexposures and
#' exposure.
#' - exposed gap: period of time that the individual is not exposed (there are
#' no exposures in the time period), but its length is smaller or equal than
#' gapEra parameter value so the individual is considered to be exposed.
#' - non exposed gap: period of time that the individual is not exposed (there are
#' no exposures in the time period) and its length is larger than gapEra
#' parameter value
#' - era: period of time the individual is considered exposed. It is formed by
#' continuous exposures and exposed gaps.
#'
#' @return The function returns the dose information for all the individuals of
#' dusCohortName.
#' @export
#'
#' @examples
getDoseInformation <- function(cdm,
                               dusCohortName,
                               conceptSetPath = NULL,
                               ingredientConceptId,
                               gapEra = 30,
                               eraJoinMode = "Previous", # proposal "Zero"
                               overlapMode = "Previous", # proposal "Sum"
                               sameIndexMode = "Sum",
                               imputeDuration = "eliminate",
                               imputeDailyDose = "eliminate",
                               durationRange = c(1, NA),
                               dailyDoseRange = c(0, NA)) {
  # first round of initial checks, assert Type
  errorMessage <- checkmate::makeAssertCollection()
  checkmate::assertClass(
    cdm,
    classes = "cdm_reference",
    add = errorMessage
  )
  checkmate::assertCharacter(
    dusCohortName,
    len = 1,
    any.missing = FALSE,
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
    add = errorMessage
  )
  checkmate::assertCount(
    gapEra,
    add = errorMessage
  )
  checkmate::assertChoice(
    eraJoinMode,
    choices = c("Previous", "Subsequent", "Zero"),
    add = errorMessage
  )
  checkmate::assertChoice(
    overlapMode,
    choices = c("Previous", "Subsequent", "Minimum", "Maximum", "Sum"),
    add = errorMessage
  )
  checkmate::assertChoice(
    sameIndexMode,
    choices = c("Minimum", "Maximum", "Sum"),
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
  if (is.character(imputeDailyDose)) {
    checkmate::assertChoice(
      imputeDailyDose,
      choices = c("eliminate", "median", "mean", "quantile25", "quantile75"),
      add = errorMessage
    )
  } else {
    checkmate::assertNumeric(
      imputeDailyDose,
      any.missing = FALSE,
      len = 1
    )
  }
  checkmate::assertNumeric(
    durationRange,
    len = 2,
    null.ok = TRUE,
    add = errorMessage
  )
  checkmate::assertNumeric(
    dailyDoseRange,
    len = 2,
    null.ok = TRUE,
    add = errorMessage
  )
  checkmate::reportAssertions(collection = errorMessage)

  if (is.null(durationRange)) {
    durationRange <- c(NA, NA)
  }
  if (is.null(dailyDoseRange)) {
    dailyDoseRange <- c(NA, NA)
  }

  # second round of initial checks
  checkmate::assertTRUE(
    all(c("drug_strength", "drug_exposure", dusCohortName) %in% names(cdm)),
    add = errorMessage
  )
  checkmate::assertTRUE(
    length(colnames(cdm[[dusCohortName]])) == 4,
    add = errorMessage
  )
  checkmate::assertTRUE(
    all(colnames(cdm[[dusCohortName]]) %in% c(
      "cohort_definition_id", "subject_id", "cohort_start_date",
      "cohort_end_date"
    )),
    add = errorMessage
  )
  if (!is.null(conceptSetPath)) {
    if (!file.exists(conceptSetPath)) {
      stop(glue::glue("Invalid concept set path {conceptSetPath}"))
    } else {
      if (dir.exists(conceptSetPath)) {
        conceptSetPathFiles <- list.files(
          path = conceptSetPath,
          full.names = TRUE
        )
        conceptSetPathFiles <- conceptSetPathFiles[
          tools::file_ext(conceptSetPathFiles) == "json"
        ]
        if (length(conceptSetPathFiles) == 0) {
          stop(glue::glue("No 'json' file found in {conceptSetPath}"))
        } else if (length(conceptSetPathFiles) > 1) {
          stop(glue::glue(
            "More than one 'json' file found in {conceptSetPath}",
            ". Please provide the path to one of them."
          ))
        } else {
          conceptSetPath <- conceptSetPathFiles
        }
      }
    }
  }
  if (!(cdm$drug_strength %>%
    dplyr::filter(.data$ingredient_concept_id == .env$ingredientConceptId) %>%
    dplyr::tally() %>%
    dplyr::pull("n") > 0)) {
    errorMessage$push(glue::glue(
      "Ingredient concept id ({ingredientConceptId}) has not counts in ",
      "drug_stregth table."
    ))
  }
  if (sum(is.na(durationRange)) == 0) {
    checkmate::assertTRUE(
      durationRange[1] <= durationRange[2],
      add = errorMessage
    )
  }
  if (sum(is.na(dailyDoseRange)) == 0) {
    checkmate::assertTRUE(
      dailyDoseRange[1] <= dailyDoseRange[2],
      add = errorMessage
    )
  }
  # THIS CHECKS SHOULD BE SIMPLIFIED WHEN CHECKS IN CDMConnector:: are allowed
  # check drug exposure table exist
  cdm_drug_exp_exists <- inherits(cdm$drug_exposure, "tbl_dbi")
  checkmate::assertTRUE(cdm_drug_exp_exists, add = errorMessage)
  if (!isTRUE(cdm_drug_exp_exists)) {
    errorMessage$push("- table `drug exposure` is not found")
  }
  # check drug strength table exist
  cdm_drug_str_exists <- inherits(cdm$drug_strength, "tbl_dbi")
  checkmate::assertTRUE(cdm_drug_str_exists, add = errorMessage)
  if (!isTRUE(cdm_drug_str_exists)) {
    errorMessage$push("- table `drug strength` is not found")
  }
  checkmate::reportAssertions(collection = errorMessage)

  # Get the list of drug concept id to us
  conceptList <- getConceptList(
    conceptSetPath = conceptSetPath,
    ingredientConceptId = ingredientConceptId,
    cdm = cdm
  )

  if (nrow(conceptList) == 0) {
    stop("No concepts were not found in the vocabulary using this settings")
  }

  # get sql dialect of the database
  dialect <- CDMConnector::dbms(attr(cdm, "dbcon"))

  # subset drug_exposure and only get the drug concept ids that we are
  # interested in.
  cohort <- cdm[["drug_exposure"]] %>%
    dplyr::select(
      "subject_id" = "person_id",
      "drug_concept_id",
      "drug_exposure_id",
      "drug_exposure_start_date",
      "drug_exposure_end_date",
      "quantity"
    ) %>%
    dplyr::inner_join(
      cdm[[dusCohortName]] %>%
        dplyr::select("subject_id", "cohort_start_date", "cohort_end_date") %>%
        dplyr::distinct(),
      by = "subject_id"
    ) %>%
    dplyr::inner_join(
      conceptList,
      by = "drug_concept_id",
      copy = TRUE
    ) %>%
    dplyr::compute()

  attrition <- addAttitionLine(cohort, "Initial counts getDoseInformation")

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
  )

  attrition <- attrition %>%
    dplyr::union_all(addAttitionLine(cohort, "Impute Duration"))

  # correct drug exposure end date according to the new duration
  cohort <- cohort %>%
    dplyr::mutate(days_to_add = as.integer(.data$days_exposed - 1)) %>%
    dplyr::compute() %>%
    dplyr::mutate(drug_exposure_end_date = as.Date(dbplyr::sql(
      dateadd(
        date = "drug_exposure_start_date",
        number = "days_to_add"
      )
    ))) %>%
    dplyr::select(-"days_to_add", -"days_exposed")

  # We compute the daily dose using drugExposureDiagnostics function (to be
  # updated in the next interation as drugExposureDiagnostics does not work as
  # we want)
  cohort <- cohort %>%
    addDailyDose(cdm = cdm, ingredientConceptId = ingredientConceptId) %>%
    dplyr::filter(.data$drug_exposure_start_date <= .data$cohort_end_date) %>%
    dplyr::filter(.data$drug_exposure_end_date >= .data$cohort_start_date) %>%
    dplyr::select(-"quantity", -"drug_dose_type")

  # impute or eliminate the exposures that daily_dose does not fulfill the
  # conditions ( <0; <dailyDoseRange[1]; >dailyDoseRange[2])
  cohort <- imputeVariable(
    x = cohort,
    variableName = "daily_dose",
    impute = imputeDuration,
    lowerBound = dailyDoseRange[1],
    upperBound = dailyDoseRange[2],
    imputeValueName = "imputeDailyDose"
  ) %>%
    dplyr::compute()

  attrition <- attrition %>%
    dplyr::union_all(addAttitionLine(cohort, "Impute DailyDose"))

  if (cohort %>% dplyr::tally() %>% dplyr::pull("n") == 0) {
    stop("No exposure found inside the studied periods.")
  }
  # split the exposures in subexposures inside each cohort
  cohort <- splitSubexposures(cohort)

  # add the overlapping flag
  cohort <- addOverlappingFlag(cohort)

  # add the type of subexposure
  cohort <- addTypeSubexposure(cohort, gapEra)

  # add era_id
  cohort <- addEraId(cohort)

  # add continuous_exposure_id
  cohort <- addContinuousExposureId(cohort)

  # solve same index day overlapping
  cohort <- solveSameIndexOverlap(cohort, sameIndexMode)

  # solve not same index overlapping
  cohort <- solveOverlap(cohort, overlapMode)

  # add daily dose to gaps
  cohort <- addGapDailyDose(cohort, eraJoinMode)

  # summarise cohort to obtain the dose table
  doseTable <- summariseCohort(cohort)

  return(doseTable)
}

#' @noRd
splitSubexposures <- function(x) {
  x_intervals <- x %>%
    dplyr::select(
      "subject_id", "cohort_start_date", "cohort_end_date",
      "date_event" = "drug_exposure_start_date"
    ) %>%
    dplyr::distinct() %>%
    dplyr::union_all(
      x %>%
        dplyr::select(
          "subject_id", "cohort_start_date", "cohort_end_date",
          "date_event" = "drug_exposure_start_date"
        ) %>%
        dplyr::distinct() %>%
        dplyr::mutate(date_event = as.Date(!!CDMConnector::dateadd(
          "date_event", -1
        )))
    ) %>%
    dplyr::union_all(
      x %>%
        dplyr::select(
          "subject_id", "cohort_start_date", "cohort_end_date",
          "date_event" = "drug_exposure_end_date"
        ) %>%
        dplyr::distinct()
    ) %>%
    dplyr::union_all(
      x %>%
        dplyr::select(
          "subject_id", "cohort_start_date", "cohort_end_date",
          "date_event" = "drug_exposure_end_date"
        ) %>%
        dplyr::distinct() %>%
        dplyr::mutate(date_event = as.Date(!!CDMConnector::dateadd(
          "date_event", 1
        )))
    ) %>%
    dplyr::group_by(
      .data$subject_id, .data$cohort_start_date, .data$cohort_end_date
    ) %>%
    dbplyr::window_order(.data$date_event) %>%
    dplyr::mutate(id1 = dplyr::row_number()) %>%
    dplyr::mutate(
      id2 = floor(1.25 + 0.5 * .data$id1),
      date_type = dplyr::if_else(
        .data$id1 %% 2 == 0,
        "subexposure_start_date",
        "subexposure_end_date"
      )
    ) %>%
    dplyr::select(-"id1") %>%
    dplyr::filter(.data$date_event >= .data$cohort_start_date) %>%
    dplyr::filter(.data$date_event <= .data$cohort_end_date) %>%
    dbplyr::window_order() %>%
    tidyr::pivot_wider(names_from = "date_type", values_from = "date_event") %>%
    dplyr::select(-"id2") %>%
    dplyr::ungroup() %>%
    dplyr::compute()

  x_intervals <- x_intervals %>%
    dplyr::filter(is.na(.data$subexposure_start_date)) %>%
    dplyr::mutate(subexposure_start_date = .data$cohort_start_date) %>%
    dplyr::union_all(
      x_intervals %>%
        dplyr::filter(is.na(.data$subexposure_end_date)) %>%
        dplyr::mutate(subexposure_end_date = .data$cohort_end_date)
    ) %>%
    dplyr::union_all(
      x_intervals %>%
        dplyr::filter(
          !is.na(.data$subexposure_start_date) &&
            !is.na(.data$subexposure_end_date)
        ) %>%
        dplyr::mutate(
          subexposure_start_date = dplyr::if_else(
            .data$subexposure_start_date < .data$cohort_start_date,
            .data$cohort_start_date,
            .data$subexposure_start_date
          ),
          subexposure_end_date = dplyr::if_else(
            .data$subexposure_end_date > .data$cohort_end_date,
            .data$cohort_end_date,
            .data$subexposure_end_date
          )
        )
    ) %>%
    dplyr::group_by(
      .data$subject_id, .data$cohort_start_date, .data$cohort_end_date
    ) %>%
    dbplyr::window_order(.data$subexposure_start_date) %>%
    dplyr::mutate(subexposure_id = dplyr::row_number()) %>%
    dplyr::ungroup() %>%
    dbplyr::window_order() %>%
    dplyr::mutate(subexposed_days = !!CDMConnector::datediff(
      "subexposure_start_date", "subexposure_end_date"
    ) + 1) %>%
    dplyr::compute()

  # we join the exposures with the overlapping periods and we only consider the
  # exposures that contribute to each overlapping period
  x_intervals <- x_intervals %>%
    dplyr::left_join(
      x_intervals %>%
        dplyr::inner_join(
          x,
          by = c("subject_id", "cohort_start_date", "cohort_end_date")
        ) %>%
        dplyr::filter(
          .data$drug_exposure_start_date <= .data$subexposure_start_date
        ) %>%
        dplyr::filter(
          .data$drug_exposure_end_date >= .data$subexposure_end_date
        ),
      by = c(
        "subject_id", "cohort_start_date", "cohort_end_date",
        "subexposure_start_date", "subexposure_end_date", "subexposure_id",
        "subexposed_days"
      )
    ) %>%
    dplyr::compute()


  return(x_intervals)
}

#' @noRd
addOverlappingFlag <- function(x) {
  x <- x %>%
    dplyr::group_by(
      .data$subject_id, .data$cohort_start_date, .data$cohort_end_date,
      .data$subexposure_id
    ) %>%
    dplyr::mutate(overlapping = dplyr::n()) %>%
    dplyr::ungroup()
  return(x)
}

#' @noRd
addTypeSubexposure <- function(x, gapEra) {
  x <- x %>%
    dplyr::group_by(
      .data$subject_id, .data$cohort_start_date, .data$cohort_end_date
    ) %>%
    dplyr::mutate(
      type_subexposure = dplyr::case_when(
        !is.na(.data$drug_concept_id) ~ "exposed",
        is.na(.data$drug_concept_id) &
          .data$subexposed_days <= .env$gapEra &
          .data$subexposure_id > 1 &
          .data$subexposure_id < max(.data$subexposure_id, na.rm = TRUE) ~
          "gap",
        TRUE ~ "unexposed"
      )
    )
  return(x)
}

#' @noRd
addEraId <- function(x) {
  x <- x %>%
    dplyr::mutate(era_id = dplyr::if_else(
      .data$type_subexposure == "unexposed" & .data$subexposure_id > 1,
      1,
      0
    )) %>%
    dbplyr::window_order(.data$subexposure_start_date) %>%
    dplyr::mutate(era_id = cumsum(as.numeric(.data$era_id))) %>%
    dplyr::mutate(era_id = dplyr::if_else(
      .data$type_subexposure == "unexposed",
      as.numeric(NA),
      .data$era_id + 1
    )) %>%
    dbplyr::window_order()
  return(x)
}

#' @noRd
addContinuousExposureId <- function(x) {
  x <- x %>%
    dplyr::mutate(continuous_exposure_id = dplyr::if_else(
      .data$type_subexposure != "exposed" & .data$subexposure_id > 1,
      1,
      0
    )) %>%
    dbplyr::window_order(.data$subexposure_start_date) %>%
    dplyr::mutate(continuous_exposure_id = cumsum(as.numeric(
      .data$continuous_exposure_id
    ))) %>%
    dplyr::mutate(continuous_exposure_id = dplyr::if_else(
      .data$type_subexposure != "exposed",
      as.numeric(NA),
      .data$continuous_exposure_id + 1
    )) %>%
    dplyr::ungroup() %>%
    dbplyr::window_order()
  return(x)
}

#' @noRd
solveSameIndexOverlap <- function(x, sameIndexMode) {
  x_same_index <- x %>%
    dplyr::group_by(
      .data$subject_id, .data$cohort_start_date, .data$subexposure_id,
      .data$drug_exposure_start_date
    ) %>%
    dplyr::filter(dplyr::n() > 1)
  if (sameIndexMode == "Minimum") {
    x_same_index <- x_same_index %>%
      dplyr::left_join(
        x_same_index %>%
          dplyr::filter(
            .data$daily_dose == min(.data$daily_dose, na.rm = TRUE)
          ) %>%
          dplyr::filter(
            .data$drug_exposure_id == min(.data$drug_exposure_id, na.rm = TRUE)
          ) %>%
          dplyr::mutate(considered_subexposure = "yes"),
        by = colnames(x_same_index)
      ) %>%
      dplyr::mutate(considered_subexposure = dplyr::if_else(
        is.na(.data$considered_subexposure),
        "no",
        "yes"
      ))
  } else if (sameIndexMode == "Maximum") {
    x_same_index <- x_same_index %>%
      dplyr::left_join(
        x_same_index %>%
          dplyr::filter(
            .data$daily_dose == max(.data$daily_dose, na.rm = TRUE)
          ) %>%
          dplyr::filter(
            .data$drug_exposure_id == min(.data$drug_exposure_id, na.rm = TRUE)
          ) %>%
          dplyr::mutate(considered_subexposure = "yes"),
        by = colnames(x_same_index)
      ) %>%
      dplyr::mutate(considered_subexposure = dplyr::if_else(
        is.na(.data$considered_subexposure),
        "no",
        "yes"
      ))
  } else if (sameIndexMode == "Sum") {
    x_same_index <- x_same_index %>%
      dplyr::mutate(daily_dose = sum(.data$daily_dose, na.rm = TRUE)) %>%
      dplyr::filter(
        .data$drug_exposure_id == min(.data$drug_exposure_id, na.rm = TRUE)
      ) %>%
      dplyr::union_all(
        x_same_index %>%
          dplyr::filter(
            .data$drug_exposure_id > min(.data$drug_exposure_id, na.rm = TRUE)
          ) %>%
          dplyr::mutate(daily_dose = 0)
      ) %>%
      dplyr::mutate(considered_subexposure = "yes")
  }
  x <- x %>%
    dplyr::anti_join(
      x_same_index,
      by = c(
        "subject_id", "cohort_start_date", "cohort_end_date", "subexposure_id",
        "drug_exposure_start_date"
      )
    ) %>%
    dplyr::union_all(x_same_index %>% dplyr::ungroup()) %>%
    dplyr::compute()

  return(x)
}

#' @noRd
solveOverlap <- function(x, overlapMode) {
  x_overlap <- x %>%
    dplyr::group_by(
      .data$subject_id, .data$cohort_start_date, .data$subexposure_id,
    ) %>%
    dplyr::filter(
      is.na(.data$considered_subexposure) |
        .data$considered_subexposure == "yes"
    ) %>%
    dplyr::filter(dplyr::n() > 1)
  if (
    x_overlap %>% dplyr::ungroup() %>% dplyr::tally() %>% dplyr::pull("n") > 0
  ){
    if (overlapMode == "Minimum") {
      x_overlap <- x_overlap %>%
        dplyr::select(-"considered_subexposure")
      x_overlap <- x_overlap %>%
        dplyr::left_join(
          x_overlap %>%
            dplyr::filter(.data$daily_dose > 0) %>%
            dplyr::filter(
              .data$daily_dose == min(.data$daily_dose, na.rm = TRUE)
            ) %>%
            dplyr::filter(
              .data$drug_exposure_id == min(.data$drug_exposure_id, na.rm = TRUE)
            ) %>%
            dplyr::mutate(considered_subexposure = "yes"),
          by = colnames(x_overlap)
        ) %>%
        dplyr::mutate(considered_subexposure = dplyr::if_else(
          is.na(.data$considered_subexposure),
          "no",
          "yes"
        ))
    } else if (overlapMode == "Maximum") {
      x_overlap <- x_overlap %>%
        dplyr::select(-"considered_subexposure")
      x_overlap <- x_overlap %>%
        dplyr::left_join(
          x_overlap %>%
            dplyr::filter(
              .data$daily_dose == max(.data$daily_dose, na.rm = TRUE)
            ) %>%
            dplyr::filter(
              .data$drug_exposure_id == min(.data$drug_exposure_id, na.rm = TRUE)
            ) %>%
            dplyr::mutate(considered_subexposure = "yes"),
          by = colnames(x_overlap)
        ) %>%
        dplyr::mutate(considered_subexposure = dplyr::if_else(
          is.na(.data$considered_subexposure),
          "no",
          "yes"
        ))
    } else if (overlapMode == "Sum") {
      x_overlap <- x_overlap %>%
        dplyr::mutate(considered_subexposure = "yes")
    } else if (overlapMode == "Previous") {
      x_overlap <- x_overlap %>%
        dplyr::mutate(
          first_drug_exposure_start_date =
            min(.data$drug_exposure_start_date, na.rm = TRUE)
        ) %>%
        dplyr::mutate(
          considered_subexposure = dplyr::if_else(
            .data$drug_exposure_start_date ==
              .data$first_drug_exposure_start_date,
            "yes",
            "no"
          )
        ) %>%
        dplyr::select(-"first_drug_exposure_start_date")
    } else if (overlapMode == "Subsequent") {
      x_overlap <- x_overlap %>%
        dplyr::mutate(
          last_drug_exposure_start_date =
            max(.data$drug_exposure_start_date, na.rm = TRUE)
        ) %>%
        dplyr::mutate(
          considered_subexposure = dplyr::if_else(
            .data$drug_exposure_start_date ==
              .data$last_drug_exposure_start_date,
            "yes",
            "no"
          )
        ) %>%
        dplyr::select(-"last_drug_exposure_start_date")
    }
    x_overlap <- x_overlap %>%
      dplyr::ungroup() %>%
      dplyr::compute()
    x <- x %>%
      dplyr::anti_join(
        x_overlap,
        by = c(
          "subject_id", "cohort_start_date", "subexposure_id", "drug_exposure_id"
        )
      ) %>%
      dplyr::mutate(considered_subexposure = dplyr::if_else(
        .data$type_subexposure == "exposed" & is.na(.data$considered_subexposure),
        "yes",
        .data$considered_subexposure
      )) %>%
      dplyr::union_all(x_overlap) %>%
      dplyr::compute()
  }
  return(x)
}

#' @noRd
addGapDailyDose <- function(x, eraJoinMode) {
  x_gaps_dose <- x %>%
    dplyr::filter(.data$type_subexposure == "gap")
  if (eraJoinMode == "Zero") {
    x_gaps_dose <- x_gaps_dose %>%
      dplyr::mutate(daily_dose = as.numeric(0))
  } else if (eraJoinMode == "Previous") {
    x_gaps_dose <- x_gaps_dose %>%
      dplyr::select(-"daily_dose") %>%
      dplyr::inner_join(
        x %>%
          dplyr::mutate(subexposure_id = .data$subexposure_id + 1) %>%
          dplyr::filter(.data$considered_subexposure == "yes") %>%
          dplyr::group_by(
            .data$subject_id, .data$cohort_start_date, .data$subexposure_id
          ) %>%
          dplyr::mutate(daily_dose = sum(.data$daily_dose, na.rm = TRUE)) %>%
          dplyr::ungroup() %>%
          dplyr::select(
            "subject_id", "cohort_start_date", "cohort_end_date",
            "subexposure_id", "daily_dose"
          ) %>%
          dplyr::distinct(),
        by = c(
          "subject_id", "cohort_start_date", "cohort_end_date",
          "subexposure_id"
        )
      )
  } else if (eraJoinMode == "Subsequent") {
    x_gaps_dose <- x_gaps_dose %>%
      dplyr::select(-"daily_dose") %>%
      dplyr::inner_join(
        x %>%
          dplyr::mutate(subexposure_id = .data$subexposure_id - 1) %>%
          dplyr::filter(.data$considered_subexposure == "yes") %>%
          dplyr::group_by(
            .data$subject_id, .data$cohort_start_date, .data$subexposure_id
          ) %>%
          dplyr::mutate(daily_dose = sum(.data$daily_dose, na.rm = TRUE)) %>%
          dplyr::ungroup() %>%
          dplyr::select(
            "subject_id", "cohort_start_date", "cohort_end_date",
            "subexposure_id", "daily_dose"
          ) %>%
          dplyr::distinct(),
        by = c(
          "subject_id", "cohort_start_date", "cohort_end_date",
          "subexposure_id"
        )
      )
  }
  x <- x %>%
    dplyr::anti_join(
      x_gaps_dose,
      by = c(
        "subject_id", "cohort_start_date", "cohort_end_date",
        "subexposure_id"
      )
    ) %>%
    dplyr::union_all(
      x_gaps_dose %>% dplyr::mutate(considered_subexposure = "yes")
    ) %>%
    dplyr::compute()
  return(x)
}

#' @noRd
getConceptList <- function(conceptSetPath, ingredientConceptId, cdm) {
  if (!is.null(conceptSetPath)) {
    conceptSets <- dplyr::tibble(concept_set_path = .env$conceptSetPath) %>%
      dplyr::mutate(
        concept_set_name =
          tools::file_path_sans_ext(basename(.data$concept_set_path))
      ) %>%
      dplyr::mutate(cohort_definition_id = 1)

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
      dplyr::select("drug_concept_id") %>%
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
  } else {
    conceptList <- cdm[["drug_strength"]] %>%
      dplyr::filter(.data$ingredient_concept_id == .env$ingredientConceptId) %>%
      dplyr::select("drug_concept_id") %>%
      dplyr::collect()
  }
  return(conceptList)
}

#' @noRd
summariseCohort <- function(x) {
  x <- x %>%
    dplyr::mutate(exposed_dose = .data$daily_dose * .data$subexposed_days) %>%
    dplyr::compute() %>%
    dplyr::group_by(
      .data$subject_id, .data$cohort_start_date, .data$cohort_end_date
    ) %>%
    dplyr::summarise(
      gap_days = sum(
        .data$subexposed_days[.data$type_subexposure == "gap"],
        na.rm = TRUE
      ),
      unexposed_days = sum(
        .data$subexposed_days[.data$type_subexposure == "unexposed"],
        na.rm = TRUE
      ),
      not_considered_days = sum(
        .data$subexposed_days[
          .data$type_subexposure == "exposed" &
            .data$considered_subexposure == "no"
        ],
        na.rm = TRUE
      ),
      start_first_era = min(
        .data$subexposure_start_date[.data$era_id == 1],
        na.rm = TRUE
      ),
      end_first_era = max(
        .data$subexposure_end_date[.data$era_id == 1],
        na.rm = TRUE
      ),
      number_exposures = dplyr::n_distinct(.data$drug_exposure_id),
      number_subexposures = max(.data$subexposure_id, na.rm = TRUE),
      number_continuous_exposures = max(
        .data$continuous_exposure_id,
        na.rm = TRUE
      ),
      number_eras = max(.data$era_id, na.rm = TRUE),
      number_gaps = dplyr::n_distinct(
        .data$subexposure_id[.data$type_subexposure == "gap"]
      ),
      number_unexposed_periods = sum(
        dplyr::if_else(.data$type_subexposure == "unexposed", 1, 0),
        na.rm = TRUE
      ),
      number_subexposures_overlap = dplyr::n_distinct(
        .data$subexposure_id[.data$overlapping > 1]
      ),
      number_eras_overlap = dplyr::n_distinct(
        .data$era_id[.data$overlapping > 1]
      ),
      number_continuous_exposure_overlap = dplyr::n_distinct(
        .data$continuous_exposure_id[.data$overlapping > 1]
      ),
      cumulative_dose = sum(
        .data$exposed_dose[.data$considered_subexposure == "yes"],
        na.rm = TRUE
      ),
      initial_daily_dose = sum(
        .data$daily_dose[
          .data$subexposure_id == 1 & .data$considered_subexposure == "yes"
        ],
        na.rm = TRUE
      ),
      cumulative_gap_dose = sum(
        .data$exposed_dose[.data$type_subexposure == "gap"],
        na.rm = TRUE
      ),
      cumulative_not_considered_dose = sum(
        .data$exposed_dose[.data$considered_subexposure == "no"],
        na.rm = TRUE
      ),
      sum_all_exposed_dose = sum(
        .data$exposed_dose[.data$type_subexposure == "exposed"],
        na.rm = TRUE
      ),
      sum_all_exposed_days = sum(
        .data$subexposed_days[.data$type_subexposure == "exposed"],
        na.rm = TRUE
      ),
      .groups = "drop"
    ) %>%
    dplyr::compute() %>%
    # replace NA
    dplyr::mutate(
      gap_days = dplyr::if_else(
        is.na(.data$gap_days),
        0,
        .data$gap_days
      ),
      unexposed_days = dplyr::if_else(
        is.na(.data$unexposed_days),
        0,
        .data$unexposed_days
      ),
      not_considered_days = dplyr::if_else(
        is.na(.data$not_considered_days),
        0,
        .data$not_considered_days
      ),
      number_exposures = dplyr::if_else(
        is.na(.data$number_exposures),
        0,
        .data$number_exposures
      ),
      number_subexposures = dplyr::if_else(
        is.na(.data$number_subexposures),
        0,
        .data$number_subexposures
      ),
      number_continuous_exposures = dplyr::if_else(
        is.na(.data$number_continuous_exposures),
        0,
        .data$number_continuous_exposures
      ),
      number_eras = dplyr::if_else(
        is.na(.data$number_eras),
        0,
        .data$number_eras
      ),
      number_gaps = dplyr::if_else(
        is.na(.data$number_gaps),
        0,
        .data$number_gaps
      ),
      number_unexposed_periods = dplyr::if_else(
        is.na(.data$number_unexposed_periods),
        0,
        .data$number_unexposed_periods
      ),
      number_subexposures_overlap = dplyr::if_else(
        is.na(.data$number_subexposures_overlap),
        0,
        .data$number_subexposures_overlap
      ),
      number_eras_overlap = dplyr::if_else(
        is.na(.data$number_eras_overlap),
        0,
        .data$number_eras_overlap
      ),
      number_continuous_exposure_overlap = dplyr::if_else(
        is.na(.data$number_continuous_exposure_overlap),
        0,
        .data$number_continuous_exposure_overlap
      ),
      cumulative_dose = dplyr::if_else(
        is.na(.data$cumulative_dose),
        0,
        .data$cumulative_dose
      ),
      initial_daily_dose = dplyr::if_else(
        is.na(.data$initial_daily_dose),
        0,
        .data$initial_daily_dose
      ),
      cumulative_gap_dose = dplyr::if_else(
        is.na(.data$cumulative_gap_dose),
        0,
        .data$cumulative_gap_dose
      ),
      cumulative_not_considered_dose = dplyr::if_else(
        is.na(.data$cumulative_not_considered_dose),
        0,
        .data$cumulative_not_considered_dose
      )
    ) %>%
    # end replace NA
    dplyr::mutate(follow_up_days = !!CDMConnector::datediff(
      "cohort_start_date", "cohort_end_date"
    ) + 1) %>%
    dplyr::mutate(
      exposed_days =
        .data$follow_up_days - .data$unexposed_days - .data$gap_days
    ) %>%
    dplyr::mutate(
      number_subexposures_no_overlap =
        .data$number_subexposures - .data$number_subexposures_overlap
    ) %>%
    dplyr::mutate(
      number_eras_no_overlap =
        .data$number_eras - .data$number_eras_overlap
    ) %>%
    dplyr::mutate(
      number_continuous_exposures_no_overlap =
        .data$number_continuous_exposures - .data$number_continuous_exposure_overlap
    ) %>%
    dplyr::mutate(proportion_gap_dose = dplyr::if_else(
      .data$cumulative_dose == 0,
      as.numeric(NA),
      .data$cumulative_gap_dose / .data$cumulative_dose
    )) %>%
    dplyr::mutate(proportion_not_considered_dose = dplyr::if_else(
      .data$cumulative_dose == 0,
      as.numeric(NA),
      .data$cumulative_not_considered_dose / .data$cumulative_dose
    )) %>%
    dplyr::mutate(first_era_days = dplyr::if_else(
      is.na(.data$start_first_era),
      0,
      !!CDMConnector::datediff(
        "start_first_era",
        "end_first_era"
      ) + 1
    )) %>%
    dplyr::select(-"start_first_era", -"end_first_era") %>%
    dplyr::compute()

  return(x)
}

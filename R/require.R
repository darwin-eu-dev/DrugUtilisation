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

#' Require prior days with no exposure.
#'
#' @param cohort A cohort table in a cdm reference.
#' @param priorUseWashout The length of priorUseWashout to be applied. NOTE that
#' if priorUseWashout is infinity requireIsFirstDrugEntry will be called
#' instead.
#' @param cohortId IDs of the cohorts to modify. The default is NULL meaning all
#' cohorts will be used; otherwise, only the specified cohorts will be modified,
#' and the rest will remain unchanged.
#' @param name Name of the new cohort. Default name is the original cohort name.
#'
#' @return The cohort table object with the requirements applied.
#'
#' @export
#'
#' @examples
#' \donttest{
#' cdm <- mockDrugUtilisation()
#' cdm$cohort1 <- cdm$cohort1 |>
#'   requirePriorDrugWashout(priorUseWashout = 90, cohortId = c(2,3))
#' attrition(cdm$cohort1)
#' CDMConnector::cdmDisconnect(cdm = cdm)
#' }
#'
requirePriorDrugWashout <- function(cohort,
                                    priorUseWashout,
                                    cohortId = NULL,
                                    name = omopgenerics::tableName(cohort)) {
  # check inputs
  checkInputs(
    cohort = cohort, cohortId = cohortId, priorUseWashout = priorUseWashout,
    name = name
  )
  if (is.null(cohortId)) {
    cohortId <- settings(cohort) |> dplyr::pull("cohort_definition_id")
  }

  if (is.infinite(priorUseWashout)) {
    c("!" = "priorUseWashout is infinity -> calling requireIsFirstDrugEntry()") |>
      cli::cli_inform()
    cohort <- cohort |>
      requireIsFirstDrugEntry(cohortId = cohortId, name = name)
    return(cohort)
  }
  reason <- "require prior use priorUseWashout of {priorUseWashout} day{?s}"

  record_counts <- omopgenerics::cohortCount(cohort) |>
    dplyr::filter(.data$cohort_definition_id %in% cohortId) |>
    dplyr::pull("number_records")

  if (any(record_counts > 0) & priorUseWashout > 0) {
    cohort <- cohort |>
      dplyr::anti_join(
        cohort |>
          dplyr::filter(.data$cohort_definition_id %in% .env$cohortId) |>
          dplyr::group_by(.data$cohort_definition_id, .data$subject_id) |>
          dplyr::mutate("prior_end_date" = dplyr::lag(
            .data$cohort_end_date, order_by = .data$cohort_start_date)) |>
          dplyr::ungroup() %>%
          dplyr::mutate(prior_time = !!CDMConnector::datediff(
            "prior_end_date", "cohort_start_date")) |>
          dplyr::filter(.data$prior_time < .env$priorUseWashout) |>
          dplyr::select("cohort_definition_id", "subject_id", "cohort_start_date"),
        by = c("cohort_definition_id", "subject_id", "cohort_start_date")
      )
  }

  set <- settings(cohort) |>
    newSettings(
      col = "prior_use_washout", value = priorUseWashout, cohortId = cohortId
    )

  cohort <- cohort |>
    dplyr::compute(name = name, temporary = FALSE) |>
    omopgenerics::newCohortTable(.softValidation = TRUE, cohortSetRef = set) |>
    omopgenerics::recordCohortAttrition(reason = reason, cohortId = cohortId)

  return(cohort)
}

#' Require only first record per subject.
#'
#' @param cohort A cohort table in a cdm reference.
#' @param cohortId IDs of the cohorts to modify. The default is NULL meaning all
#' cohorts will be used; otherwise, only the specified cohorts will be modified,
#' and the rest will remain unchanged.
#' @param name Name of the new cohort. Default name is the original cohort name.
#'
#' @return The cohort table object with the requirements applied.
#'
#' @export
#'
#' @examples
#' \donttest{
#' cdm <- mockDrugUtilisation()
#' cdm$cohort1 <- cdm$cohort1 |>
#'   requireIsFirstDrugEntry()
#' attrition(cdm$cohort1)
#' CDMConnector::cdmDisconnect(cdm = cdm)
#' }
#'
requireIsFirstDrugEntry <- function(cohort,
                                    cohortId = NULL,
                                    name = omopgenerics::tableName(cohort)) {
  # check inputs
  checkInputs(cohort = cohort, cohortId = cohortId, name = name)
  if (is.null(cohortId)) {
    cohortId <- settings(cohort) |> dplyr::pull("cohort_definition_id")
  }

  reason <- "require is the first entry"

  record_counts <- omopgenerics::cohortCount(cohort) |>
    dplyr::filter(.data$cohort_definition_id %in% cohortId) |>
    dplyr::pull("number_records")

  if (any(record_counts > 0)) {
    cohort <- cohort |>
      dplyr::group_by(.data$subject_id,.data$cohort_definition_id) |>
      dplyr::filter(
        .data$cohort_start_date == min(.data$cohort_start_date, na.rm = TRUE) |
          (!.data$cohort_definition_id %in% .env$cohortId)
      ) |>
      dplyr::ungroup()
  }

  set <- settings(cohort) |>
    newSettings(col = "limit", value = "first_entry", cohortId = cohortId)

  cohort <- cohort |>
    dplyr::compute(name = name, temporary = FALSE) |>
    omopgenerics::newCohortTable(.softValidation = TRUE, cohortSetRef = set) |>
    omopgenerics::recordCohortAttrition(reason = reason, cohortId = cohortId)

  return(cohort)
}

#' Require number of days of observation before the drug starts.
#'
#' @param cohort A cohort table in a cdm reference.
#' @param priorObservation Number of days of prior observation so the records are
#' considered.
#' @param cohortId IDs of the cohorts to modify. The default is NULL meaning all
#' cohorts will be used; otherwise, only the specified cohorts will be modified,
#' and the rest will remain unchanged.
#' @param name Name of the new cohort. Default name is the original cohort name.
#'
#' @return The cohort table object with the requirements applied.
#'
#' @export
#'
#' @examples
#' \donttest{
#' cdm <- mockDrugUtilisation()
#' cdm$cohort1 <- cdm$cohort1 |>
#'   requireObservationBeforeDrug(priorObservation = 365, cohortId = 1)
#' attrition(cdm$cohort1)
#' CDMConnector::cdmDisconnect(cdm = cdm)
#' }
#'
requireObservationBeforeDrug <- function(cohort,
                                         priorObservation,
                                         cohortId = NULL,
                                         name = omopgenerics::tableName(cohort)) {
  # check inputs
  checkInputs(cohort = cohort, cohortId = cohortId, name = name)
  assertNumeric(priorObservation, integerish = T, length = 1, min = 0)
  if (is.null(cohortId)) {
    cohortId <- settings(cohort) |> dplyr::pull("cohort_definition_id")
  }

  reason <- "require prior observation of {priorObservation} day{?s}"

  record_counts <- omopgenerics::cohortCount(cohort) |>
    dplyr::filter(.data$cohort_definition_id %in% cohortId) |>
    dplyr::pull("number_records")

  if (any(record_counts > 0)) {
    id <- omopgenerics::uniqueId(exclude = colnames(cohort))
    cohort <- cohort |>
      PatientProfiles::addPriorObservationQuery(
        indexDate = "cohort_start_date", priorObservationName = id,
        priorObservationType = "days"
      ) |>
      dplyr::filter(
        .data[[id]] >= .env$priorObservation |
          (!.data$cohort_definition_id %in% .env$cohortId)
      ) |>
      dplyr::select(!dplyr::all_of(id))
  }

  set <- settings(cohort) |>
    newSettings(
      col = "prior_drug_observation", value = priorObservation,
      cohortId = cohortId
    )

  cohort <- cohort |>
    dplyr::compute(name = name, temporary = FALSE) |>
    omopgenerics::newCohortTable(.softValidation = TRUE, cohortSetRef = set) |>
    omopgenerics::recordCohortAttrition(reason = reason, cohortId = cohortId)

  return(cohort)
}

#' Restrict to observations that have an indexDate within a certain range.
#'
#' @param cohort A cohort table in a cdm reference.
#' @param dateRange Date interval to consider.
#' @param indexDate Column that points to a date column in cohort.
#' @param cohortId IDs of the cohorts to modify. The default is NULL meaning all
#' cohorts will be used; otherwise, only the specified cohorts will be modified,
#' and the rest will remain unchanged.
#' @param name Name of the new cohort. Default name is the original cohort name.
#'
#' @return The cohort table object with the requirements applied.
#'
#' @export
#'
#' @examples
#' \donttest{
#' cdm <- mockDrugUtilisation()
#' cdm$cohort1 <- cdm$cohort1 |>
#'   requireDrugInDateRange(
#'     dateRange = as.Date(c("2020-01-01", NA)), cohortId = 1)
#' attrition(cdm$cohort1)
#' CDMConnector::cdmDisconnect(cdm = cdm)
#' }
#'
requireDrugInDateRange <- function(cohort,
                                   dateRange,
                                   indexDate = "cohort_start_date",
                                   cohortId = NULL,
                                   name = omopgenerics::tableName(cohort)) {
  # check inputs
  checkInputs(cohort = cohort, cohortId = cohortId, name = name)
  assertCharacter(indexDate, length = 1)
  if (!indexDate %in% colnames(cohort)) {
    cli::cli_abort("{.var {indexDate}} (indexDate) is not a column in cohort.")
  }
  if (!inherits(dateRange, "Date") | length(dateRange) != 2) {
    cli::cli_abort("`dateRange` is not a date of length 2")
  }
  if (is.null(cohortId)) {
    cohortId <- settings(cohort) |> dplyr::pull("cohort_definition_id")
  }

  if (all(is.na(dateRange))) {
    reason <- "No date restrictions to {indexDate}"
  } else if (is.na(dateRange[1])) {
    reason <- "require {indexDate} before {dateRange[2]}"
    cohort <- cohort |>
      dplyr::filter(
        .data[[indexDate]] <= !!dateRange[2] |
          (!.data$cohort_definition_id %in% !!cohortId)
      )
  } else if (is.na(dateRange[2])) {
    reason <- "require {indexDate} after {dateRange[1]}"
    cohort <- cohort |>
      dplyr::filter(
        .data[[indexDate]] >= !!dateRange[1] |
          (!.data$cohort_definition_id %in% !!cohortId)
      )
  } else {
    reason <- "require {indexDate} between {dateRange[1]} to {dateRange[2]}"
    cohort <- cohort |>
      dplyr::filter(
        (.data[[indexDate]] >= !!dateRange[1] &
           .data[[indexDate]] <= !!dateRange[2]) |
          !(.data$cohort_definition_id %in% !!cohortId)
      )
  }

  set <- settings(cohort) |>
    newSettings(
      col = paste0("min_", indexDate), value = dateRange[1], cohortId = cohortId) |>
    newSettings(
      col = paste0("max_", indexDate), value = dateRange[2], cohortId = cohortId)

  cohort <- cohort |>
    dplyr::compute(name = name, temporary = FALSE) |>
    omopgenerics::newCohortTable(.softValidation = TRUE) |>
    omopgenerics::recordCohortAttrition(reason = reason, cohortId = cohortId)

  return(cohort)
}

newSettings <- function(set, col, value, cohortId) {
  if (!col %in% colnames(set)) {
    no <- "NA_character_"
  } else {
    no <- ".data[['{col}']]" |> glue::glue()
  }
  newCol <- "dplyr::if_else(.data$cohort_definition_id %in% .env$cohortId,
        as.character(value), {no})" |>
    glue::glue() |>
    rlang::parse_exprs() |>
    rlang::set_names(col)
  set <- set |> dplyr::mutate(!!!newCol)
  return(set)
}

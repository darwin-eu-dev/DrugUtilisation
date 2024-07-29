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


#' Restrict cohort to only cohort records with a given amount of time since the
#' last cohort record ended
#'
#' @description
#' Filter the cohort table keeping only the cohort records for which the
#' required amount of time has passed since the last cohort entry ended for that
#' individual.
#'
#' @param cohort A cohort table in a cdm reference.
#' @param days The number of days required to have passed since the last cohort
#' record finished. Any records with fewer days than this will be dropped. Note
#' that setting days to Inf will lead to the same result as that from using the
#' `requireIsFirstDrugEntry` function (with only an individual´s first cohort
#' record kept).
#' @param cohortId IDs of the cohorts to modify. The default is NULL meaning all
#' cohorts will be used; otherwise, only the specified cohorts will be modified,
#' and the rest will remain unchanged.
#' @param name Name of the table with the filtered cohort records. The default
#' name is the original cohort name, where the original table will be
#' overwritten.
#'
#' @return The cohort table having applied the washout requirement.
#'
#' @export
#'
#' @examples
#' \donttest{
#' library(DrugUtilisation)
#' library(dplyr)
#'
#' cdm <- mockDrugUtilisation()
#'
#' cdm$cohort1 <- cdm$cohort1 |>
#'   requirePriorDrugWashout(days = 90)
#'
#' attrition(cdm$cohort1) |> glimpse()
#' }
#'
requirePriorDrugWashout <- function(cohort,
                                    days,
                                    cohortId = NULL,
                                    name = omopgenerics::tableName(cohort)) {
  # check inputs
  checkInputs(
    cohort = cohort, cohortId = cohortId, priorUseWashout = days,
    name = name
  )
  if (is.null(cohortId)) {
    cohortId <- settings(cohort) |> dplyr::pull("cohort_definition_id")
  }

  if (is.infinite(days)) {
    c("!" = "days is infinity -> calling requireIsFirstDrugEntry()") |>
      cli::cli_inform()
    cohort <- cohort |>
      requireIsFirstDrugEntry(cohortId = cohortId, name = name)
    return(cohort)
  }
  reason <- "require prior use days of {days} day{?s}"

  record_counts <- omopgenerics::cohortCount(cohort) |>
    dplyr::filter(.data$cohort_definition_id %in% cohortId) |>
    dplyr::pull("number_records")

  if (any(record_counts > 0) & days > 0) {
    cohort <- cohort |>
      dplyr::anti_join(
        cohort |>
          dplyr::filter(.data$cohort_definition_id %in% .env$cohortId) |>
          dplyr::group_by(.data$cohort_definition_id, .data$subject_id) |>
          dplyr::mutate("prior_end_date" = dplyr::lag(
            .data$cohort_end_date,
            order_by = .data$cohort_start_date
          )) |>
          dplyr::ungroup() %>%
          dplyr::mutate(prior_time = !!CDMConnector::datediff(
            "prior_end_date", "cohort_start_date"
          )) |>
          dplyr::filter(.data$prior_time < .env$days) |>
          dplyr::select("cohort_definition_id", "subject_id", "cohort_start_date"),
        by = c("cohort_definition_id", "subject_id", "cohort_start_date")
      )
  }

  set <- settings(cohort) |>
    newSettings(
      col = "prior_use_washout", value = days, cohortId = cohortId
    )

  cohort <- cohort |>
    dplyr::compute(name = name, temporary = FALSE) |>
    omopgenerics::newCohortTable(.softValidation = TRUE, cohortSetRef = set) |>
    omopgenerics::recordCohortAttrition(reason = reason, cohortId = cohortId)

  return(cohort)
}

#' Restrict cohort to only the first cohort record per subject
#'
#' @description
#' Filter the cohort table keeping only the first cohort record per subject.
#'
#' @param cohort A cohort table in a cdm reference.
#' @param cohortId IDs of the cohorts to modify. The default is NULL meaning all
#' cohorts will be used; otherwise, only the specified cohorts will be modified,
#' and the rest will remain unchanged.
#' @param name Name of the table with the filtered cohort records. The default
#' name is the original cohort name, where the original table will be
#' overwritten.
#'
#' @return The cohort table having applied the first entry requirement.
#'
#' @export
#'
#' @examples
#' \donttest{
#' library(DrugUtilisation)
#' library(dplyr)
#'
#' cdm <- mockDrugUtilisation()
#'
#' cdm$cohort1 <- cdm$cohort1 |>
#'   requireIsFirstDrugEntry()
#'
#' attrition(cdm$cohort1) |> glimpse()
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
      dplyr::group_by(.data$subject_id, .data$cohort_definition_id) |>
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

#' Restrict cohort to only cohort records with the given amount of prior
#' observation time in the database
#'
#' @description
#' Filter the cohort table keeping only the cohort records for which the
#' individual has the required observation time in the database prior to their
#' cohort start date.
#'
#' @param cohort A cohort table in a cdm reference.
#' @param days Number of days of prior observation required before cohort start
#' date. Any records with fewer days will be dropped.
#' @param cohortId IDs of the cohorts to modify. The default is NULL meaning all
#' cohorts will be used; otherwise, only the specified cohorts will be modified,
#' and the rest will remain unchanged.
#' @param name Name of the table with the filtered cohort records. The default
#' name is the original cohort name, where the original table will be
#' overwritten.
#'
#' @return The cohort table having applied the prior observation requirement.
#'
#' @export
#'
#' @examples
#' \donttest{
#' library(DrugUtilisation)
#' library(dplyr)
#'
#' cdm <- mockDrugUtilisation()
#'
#' cdm$cohort1 <- cdm$cohort1 |>
#'   requireObservationBeforeDrug(days = 365)
#'
#' attrition(cdm$cohort1) |> glimpse()
#' }
#'
requireObservationBeforeDrug <- function(cohort,
                                         days,
                                         cohortId = NULL,
                                         name = omopgenerics::tableName(cohort)) {
  # check inputs
  checkInputs(cohort = cohort, cohortId = cohortId, name = name)
  assertNumeric(days, integerish = T, length = 1, min = 0)
  if (is.null(cohortId)) {
    cohortId <- settings(cohort) |> dplyr::pull("cohort_definition_id")
  }

  reason <- "require prior observation of {days} day{?s}"

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
        .data[[id]] >= .env$days |
          (!.data$cohort_definition_id %in% .env$cohortId)
      ) |>
      dplyr::select(!dplyr::all_of(id))
  }

  set <- settings(cohort) |>
    newSettings(
      col = "prior_drug_observation", value = days,
      cohortId = cohortId
    )

  cohort <- cohort |>
    dplyr::compute(name = name, temporary = FALSE) |>
    omopgenerics::newCohortTable(.softValidation = TRUE, cohortSetRef = set) |>
    omopgenerics::recordCohortAttrition(reason = reason, cohortId = cohortId)

  return(cohort)
}

#' Restrict cohort to only cohort records within a certain date range
#'
#' @description
#' Filter the cohort table keeping only the cohort records for which the
#' specified index date is within a specified date range.
#'
#' @param cohort A cohort table in a cdm reference.
#' @param dateRange Date interval to consider. Any records with the index date
#' outside of this range will be dropped.
#' @param indexDate The column containing the date that will be checked against
#' the date range.
#' @param cohortId IDs of the cohorts to modify. The default is NULL meaning all
#' cohorts will be used; otherwise, only the specified cohorts will be modified,
#' and the rest will remain unchanged.
#' @param name Name of the table with the filtered cohort records. The default
#' name is the original cohort name, where the original table will be
#' overwritten.
#'
#' @return The cohort table having applied the date requirement.
#'
#' @export
#'
#' @examples
#' \donttest{
#' library(DrugUtilisation)
#' library(dplyr)
#'
#' cdm <- mockDrugUtilisation()
#'
#' cdm$cohort1 <- cdm$cohort1 |>
#'   requireDrugInDateRange(
#'     dateRange = as.Date(c("2020-01-01", NA))
#'   )
#'
#' attrition(cdm$cohort1) |> glimpse()
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
      col = paste0("min_", indexDate), value = dateRange[1], cohortId = cohortId
    ) |>
    newSettings(
      col = paste0("max_", indexDate), value = dateRange[2], cohortId = cohortId
    )

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

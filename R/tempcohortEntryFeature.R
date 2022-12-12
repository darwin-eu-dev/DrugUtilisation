# Copyright 2022 DARWIN EU®
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

#' This subject if in a certain window the target cohort overlaps with the
#' overlap cohort
#'
#' @param cdm CDMConnector CDM reference object
#' @param targetCohortTable name of the cohort that we want to add the
#' overlapping
#' @param targetCohortId id or vector of ids, of the target cohort
#' @param overlapCohortTable name of the cohort that we want to check if
#' overlaps with the target cohort
#' @param overlapCohortId id or vector of ids, of the cohort we are interested
#' in
#' @param lookbackWindow lookback period window in days, a list consist of two
#' numbers
#'
#' @return it add a new column to target cohort for each id in overlap cohort.
#' The column is numeric and can be 0 if no overlap is observed and 1 if overlap
#' is oberved.
#' @export
#'
#' @examples
#' \dontrun{
#' library(DBI)
#' library(duckdb)
#' library(tibble)
#' cohort <- tibble::tibble(
#'   cohort_definition_id = c("1", "2", "1", "2", "2"),
#'   subject_id = c("1", "1", "2", "2", "3"),
#'   cohort_start_date = c(
#'     as.Date("2010-01-01"), as.Date("2009-12-01"),
#'     as.Date("2010-01-01"), as.Date("2009-01-01"),
#'     as.Date("2010-01-01")
#'   ), cohort_end_date = c(
#'     as.Date("2015-01-01"), as.Date("2013-01-01"),
#'     as.Date("2015-01-01"), as.Date("2009-01-02"),
#'     as.Date("2015-01-01")
#'   )
#' )
#' cdm <- mockDrugUtilisation(
#'   cohort = cohort
#' )
#' getOverlappingCohortSubjects(
#'   cdm = cdm,
#'   targetCohortTable = "cohort",
#'   targetCohortId = 1,
#'   overlapCohortTable = "cohort",
#'   overlapCohortId = c(2, 3),
#'   lookbackWindow = c(-180, 0)
#' )
#' }
#'
getOverlappingCohortSubjects <- function(cdm,
                                         targetCohortTable,
                                         targetCohortId = NULL,
                                         overlapCohortTable,
                                         overlapCohortId = NULL,
                                         lookbackWindow = 0) {
  if (is.character(targetCohortId)) {
    targetCohortId <- as.numeric(targetCohortId)
  }
  if (is.character(overlapCohortId)) {
    overlapCohortId <- as.numeric(overlapCohortId)
  }
  # checks
  errorMessage <- checkmate::makeAssertCollection()
  checkmate::assertTRUE(targetCohortTable %in% names(cdm), add = errorMessage)
  checkmate::assertIntegerish(
    targetCohortId,
    null.ok = TRUE, add = errorMessage
  )
  checkmate::assertTRUE(overlapCohortTable %in% names(cdm), add = errorMessage)
  checkmate::assertIntegerish(
    overlapCohortId,
    null.ok = TRUE, add = errorMessage
  )
  checkmate::assertNumeric(
    lookbackWindow, min.len = 1, max.len = 2, add = errorMessage
  )
  checkmate::reportAssertions(collection = errorMessage)

  if (length(lookbackWindow) == 1) {
    lookbackWindow <- c(lookbackWindow, lookbackWindow)
  }

  checkmate::assertTRUE(lookbackWindow[1] <= lookbackWindow[2])

  overlapCohort <- cdm[[overlapCohortTable]]

  if (is.null(targetCohortId)) {
    targetCohort <- cdm[[targetCohortTable]]
  } else {
    targetCohort <- cdm[[targetCohortTable]] %>%
      dplyr::filter(.data$cohort_definition_id %in% .env$targetCohortId)
  }
  if (is.null(overlapCohortId)) {
    overlapCohort <- cdm[[overlapCohortTable]]
  } else {
    overlapCohort <- cdm[[overlapCohortTable]] %>%
      dplyr::filter(.data$cohort_definition_id %in% .env$overlapCohortId)
  }

  overlapCohort <- overlapCohort %>%
    dplyr::rename(
      "overlap_start_date" = "cohort_start_date",
      "overlap_end_date" = "cohort_end_date",
      "overlap_id" = "cohort_definition_id"
    )

  result <- targetCohort %>%
    dplyr::left_join(overlapCohort, by = "subject_id") %>%
    dplyr::mutate(
      overlap_start_date = as.Date(dbplyr::sql(CDMConnector::dateadd(
        date = "overlap_start_date",
        number = !!-lookbackWindow[2]
      ))),
      overlap_end_date = as.Date(dbplyr::sql(CDMConnector::dateadd(
        date = "overlap_end_date",
        number = !!-lookbackWindow[1]
      )))
    ) %>%
    dplyr::filter(
      .data$cohort_start_date >= .data$overlap_start_date &
        .data$cohort_start_date <= .data$overlap_end_date
    ) %>%
    dplyr::select(
      "cohort_definition_id", "subject_id", "cohort_start_date",
      "cohort_end_date", "overlap_id"
    ) %>%
    dplyr::mutate(
      indicator = 1,
      overlap_id = as.numeric(.data$overlap_id),
      overlapCohortTableName = .env$overlapCohortTable
    ) %>%
    tidyr::pivot_wider(
      names_from = c("overlapCohortTableName", "overlap_id"),
      values_from = "indicator",
      names_glue = "overlap_{overlapCohortTableName}_{overlap_id}",
      values_fill = 0
    ) %>%
    dplyr::right_join(
      targetCohort,
      by = c(
        "cohort_definition_id", "subject_id", "cohort_start_date",
        "cohort_end_date"
      )
    ) %>%
    dplyr::mutate(dplyr::across(
      dplyr::starts_with("overlap"), ~ dplyr::if_else(is.na(.x), 0, .x)
    )) %>%
    dplyr::compute()

  return(result)
}

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

#' Get subjects in cohort1 that are also in cohort2 at the same time.
#'
#' @param cdm CDMConnector CDM reference object
#' @param targetCohortId id or list of ids, of the target cohort, including information we want to check our cohort against
#' @param interestCohortId id or list of ids, of the cohort we are interested in checking the prior history of each patient
#' @param targetCohortTable name of the target cohort table
#' @param interestCohortTable name of the interest cohort table
#' @param lookbackWindow lookback period window in days, a list consist of two numbers
#'
#' @return a tibble describing if the people in cohort1 where in cohort2 at that time (and some specified lookback period).
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
#'   cdm = cdm, targetCohortId = "2",
#'   targetCohortTable = "cohort",
#'   interestCohortId = "1",
#'   interestCohortTable = "cohort", lookbackWindow = c(0, 180)
#' )
#' }
#'
getOverlappingCohortSubjects <- function(cdm, targetCohortId, targetCohortTable,
                                         interestCohortId, interestCohortTable, lookbackWindow) {
  # checks
  errorMessage <- checkmate::makeAssertCollection()
  if (!is.null(targetCohortId) &&
    is.numeric(targetCohortId)) {
    cohortId <- as.character(targetCohortId)
  }
  checkmate::assertCharacter(targetCohortId, add = errorMessage)
  if (!is.null(interestCohortId) &&
    is.numeric(interestCohortId)) {
    cohortId <- as.character(interestCohortId)
  }
  checkmate::assertCharacter(interestCohortId, add = errorMessage)
  checkmate::check_true(targetCohortTable %in% names(cdm))
  checkmate::check_true(interestCohortTable %in% names(cdm))
  checkmate::assertNumeric(lookbackWindow, len = 2, add = errorMessage)
  checkmate::reportAssertions(collection = errorMessage)

  cohort1 <- cdm[[targetCohortTable]] %>%
    dplyr::filter(.data$cohort_definition_id %in% .env$targetCohortId) %>%
    dplyr::rename("target_start_date" = "cohort_start_date") %>%
    dplyr::rename("target_end_date" = "cohort_end_date")


  cohort2 <- cdm[[interestCohortTable]] %>%
    dplyr::filter(.data$cohort_definition_id == .env$interestCohortId) %>%
    dplyr::rename("interest_start_date" = "cohort_start_date") %>%
    dplyr::rename("interest_end_date" = "cohort_end_date")


  result <- cohort2 %>%
    dplyr::left_join(cohort1, by = "subject_id") %>%
    dplyr::mutate(
      overlap_end_date = dbplyr::sql(CDMConnector::dateadd(
        date = "interest_start_date",
        number = !!-lookbackWindow[1]
      )),
      overlap_start_date = dbplyr::sql(CDMConnector::dateadd(
        date = "interest_start_date",
        number = !!-lookbackWindow[2]
      ))
    ) %>%
    dplyr::group_by(.data$subject_id, .data$cohort_definition_id.y) %>%
    dplyr::mutate(
      target_end_date_max = max(.data$target_end_date, na.rm = TRUE),
      target_start_date_min = min(.data$target_start_date, na.rm = TRUE)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      indicator =
        dplyr::case_when(
          is.na(.data$target_start_date) ~ 0,
          is.na(.data$target_end_date) ~ 0,
          .data$target_end_date_max < .data$overlap_start_date ~ 0,
          .data$target_start_date_min > .data$overlap_end_date ~ 0,
          TRUE ~ 1
        )
    ) %>%
    dplyr::rename(
      "cohort_start_date" = "interest_start_date",
      "cohort_end_date" = "interest_end_date",
      "cohort_definition_id" = "cohort_definition_id.x",
      "target_cohort_definition_id" = "cohort_definition_id.y"
    ) %>%
    dplyr::select(
      "cohort_definition_id", "subject_id",
      "cohort_start_date", "cohort_end_date",
      "target_cohort_definition_id",
      "indicator"
    ) %>%
    dplyr::mutate(across(all_of("target_cohort_definition_id"), as.numeric)) %>%
    dplyr::compute()



  resultNotInTarget <- result %>%
    dplyr::filter(is.na(.data$target_cohort_definition_id)) %>%
    dplyr::select(cohort_definition_id, subject_id, cohort_start_date, cohort_end_date)
  resultInTarget <- result %>%
    dplyr::mutate(targetCohortTableName = .env$targetCohortTable) %>%
    dplyr::filter(!is.na(.data$target_cohort_definition_id)) %>%
    tidyr::pivot_wider(
      names_from = c(targetCohortTableName,target_cohort_definition_id),
      values_from = indicator,
      names_glue = "overlap_{targetCohortTableName}_{target_cohort_definition_id}"
    )

  result <- resultNotInTarget %>%
    dplyr::full_join(resultInTarget, by = c(
      "cohort_definition_id",
      "subject_id", "cohort_start_date",
      "cohort_end_date"
    )) %>%
    dplyr::mutate(
      across(starts_with("overlap"), ~ case_when(
        is.na(.) ~ 0,
        TRUE ~ .
      ))
    )


  return(result)
}

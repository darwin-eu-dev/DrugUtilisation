# Copyright 2022 DARWIN EU®
#
# This file is part of CohortProfile
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
#' @param targetCohortId id of the first cohort
#' @param interestCohortId id of the second cohort
#' @param targetCohortTable name of the first cohort table
#' @param interestCohortTable name of the second cohort table
#' @param lookbackWindow lookback period in days
#'
#' @return a tibble describing if the people in cohort1 where in cohort2 at that time (and some specified lookback period).
#' @export
#'
#' @examples
#' \dontrun{
#' library(DBI)
#' library(duckdb)
#' library(tibble)
#' library(CohortProfiles)
#' person_example <- tibble(
#'   person_id = "1",
#'   gender_concept_id = "8507",
#'   year_of_birth = 2000,
#'   month_of_birth = 06,
#'   day_of_birth = 01
#' )
#' observation_period_example <- tibble(
#'   observation_period_id = "1",
#'   person_id = "1",
#'   observation_period_start_date = as.Date("2010-01-01"),
#'   observation_period_end_date = as.Date("2015-06-01")
#' )
#' cdm <- generateMockCohortProfilesDb(
#'   person = person_example,
#'   observation_period = observation_period_example
#' )
#' getOverlappingCohortSubjects(cdm, "1", "cohort", "2", "cohort", 0)
#' }
#'
getOverlappingCohortSubjects <- function(cdm, targetCohortId, targetCohortTable,
                                         interestCohortId, interestCohortTable, lookbackWindow) {
  # checks
  errorMessage <- checkmate::makeAssertCollection()
  checkDbType(db = cdm, messageStore = errorMessage)
  if (!is.null(cohortId1) &&
      is.numeric(cohortId1)) {
    cohortId <- as.character(cohortId1)
  }
  checkCharacter(cohortId1, messageStore = errorMessage)
  if (!is.null(cohortId2) &&
      is.numeric(cohortId2)) {
    cohortId <- as.character(cohortId2)
  }
  checkCharacter(cohortId2, messageStore = errorMessage)
  checkmate::check_true(cohortTable1 %in% names(cdm))
  checkmate::check_true(cohortTable2 %in% names(cdm))
  checkNumeric(lookbackDays, messageStore = errorMessage)
  checkmate::reportAssertions(collection = errorMessage)

  cohort1 <- cdm[[cohortTable1]] %>%
    dplyr::filter(.data$cohort_definition_id == .env$cohortId1)

  cohort2 <- cdm[[cohortTable2]] %>%
    dplyr::filter(.data$cohort_definition_id == .env$cohortId2)

  # get rows in common based on subject_id and filter by cohort period
  cohort1 %>%
    dplyr::inner_join(cohort2, by = c("subject_id" = "subject_id")) %>%
    dplyr::collect() %>%
    dplyr::rowwise() %>%
    dplyr::mutate(cohort_diff = min(abs(as.numeric(.data$cohort_start_date.x - .data$cohort_start_date.y)),
                                    abs(as.numeric(.data$cohort_start_date.x - .data$cohort_end_date.y)))) %>%
    dplyr::filter(.data$cohort_diff <= .env$lookbackDays) %>%
    dplyr::ungroup() %>%
    dplyr::select(cohort_definition_id.x, subject_id, cohort_start_date.x, cohort_end_date.x, cohort_definition_id.y) %>%
    setNames(names(.) %>% stringr::str_replace(".x", "")) %>%
    setNames(names(.) %>% stringr::str_replace(".y", "2"))
}

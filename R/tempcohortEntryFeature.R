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
#' cohort <- tibble::tibble(
#' cohort_definition_id = c("1", "2","1", "2", "2"),
#' subject_id = c("1", "1", "2", "2", "3"),
#' cohort_start_date = c(
#' as.Date("2010-01-01"), as.Date("2009-12-01"),
#' as.Date("2010-01-01"), as.Date("2009-01-01"),
#' as.Date("2010-01-01")), cohort_end_date = c(
#' as.Date("2015-01-01"), as.Date("2013-01-01"),
#' as.Date("2015-01-01"), as.Date("2009-01-02"),
#' as.Date("2015-01-01")))
#' cdm <- mockDrugUtilisation(
#'   cohort = cohort)
#' getOverlappingCohortSubjects(cdm = cdm, targetCohortId = "2",
#' targetCohortTable = "cohort",
#' interestCohortId = "1",
#' interestCohortTable = "cohort", lookbackWindow = c(0,180))
#' }
#'
getOverlappingCohortSubjects <- function(cdm, targetCohortId, targetCohortTable,
                                         interestCohortId, interestCohortTable, lookbackWindow) {
  # checks
  errorMessage <- checkmate::makeAssertCollection()
  checkDbType(db = cdm, messageStore = errorMessage)
  if (!is.null(targetCohortId) &&
      is.numeric(targetCohortId)) {
    cohortId <- as.character(targetCohortId)
  }
  checkCharacter(targetCohortId, messageStore = errorMessage)
  if (!is.null(interestCohortId) &&
      is.numeric(interestCohortId)) {
    cohortId <- as.character(interestCohortId)
  }
  checkCharacter(interestCohortId, messageStore = errorMessage)
  checkmate::check_true(targetCohortTable %in% names(cdm))
  checkmate::check_true(interestCohortTable %in% names(cdm))
  checkmate::assertNumeric(lookbackWindow, len = 2, add = errorMessage)
  checkmate::reportAssertions(collection = errorMessage)

  cohort1 <- cdm[[targetCohortTable]] %>%
    dplyr::filter(.data$cohort_definition_id == .env$targetCohortId) %>%
    dplyr::rename("target_start_date" = "cohort_start_date") %>%
    dplyr::rename("target_end_date" = "cohort_end_date")


  cohort2 <- cdm[[interestCohortTable]] %>%
    dplyr::filter(.data$cohort_definition_id == .env$interestCohortId) %>%
    dplyr::rename("interest_start_date" = "cohort_start_date") %>%
    dplyr::rename("interest_end_date" = "cohort_end_date")


  result <- cohort2 %>% dplyr::left_join(cohort1, by = "subject_id") %>%
    dplyr::group_by(.data$subject_id) %>%
    dplyr::mutate(min_start_date = min(.data$interest_start_date, na.rm = TRUE))%>%
    dplyr::mutate(overlap_end_date = dbplyr::sql(sql_add_days(CDMConnector::dbms(attr(cdm, "dbcon")),
                                                              -lookbackWindow[1], "min_start_date")),
                  overlap_start_date = dbplyr::sql(sql_add_days(CDMConnector::dbms(attr(cdm, "dbcon")),
                                                                -lookbackWindow[2], "min_start_date")),
                  target_end_date_max = max(.data$target_end_date, na.rm = TRUE),
                  target_start_date_min = min(.data$target_start_date, na.rm = TRUE)) %>%
    dplyr::mutate(indicator =
                    dplyr::case_when(
                      is.na(.data$target_start_date) ~ 0,
                      is.na(.data$target_end_date) ~ 0,
                      .data$target_end_date_max < .data$overlap_start_date ~ 0,
                      .data$target_start_date_min > .data$overlap_end_date ~ 0,
                      TRUE ~ 1
                    )) %>%
    dplyr::rename(!!paste("overlap_", targetCohortTable, sep = "") := "indicator",
                  "cohort_start_date" = "interest_start_date",
                  "cohort_end_date" = "interest_end_date",
                  "cohort_definition_id" = "cohort_definition_id.x") %>%
    dplyr::ungroup() %>%
    dplyr::select("cohort_definition_id", "subject_id",
                  "cohort_start_date", "cohort_end_date",
                  paste("overlap_", targetCohortTable, sep = ""))
  return(result)
}

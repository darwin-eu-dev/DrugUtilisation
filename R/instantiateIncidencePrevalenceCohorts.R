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

#' Get the cohorts to compute incidence and prevalence
#'
#' @param cdm drug_exposure table from cdm
#' @param conceptIds conceptIds users want to compute era
#' @param gapEra Maxmium gap for the gap Era
#' @param incidencePrevalenceCohortName incidencePrevalenceCohortName
#' @param cohortDefinitionId cohortDefinitionId
#' @param overWrite overWrite
#' @param verbose verbose
#' @param append append or compute table in cdm
#'
#' @return
#' @export
#'
#' @examples
instantiateIncidencePrevalenceCohorts <- function(cdm,
                                                  conceptIds,
                                                  gapEra,
                                                  incidencePrevalenceCohortName,
                                                  cohortDefinitionId = NULL,
                                                  append = FALSE,
                                                  overWrite = TRUE,
                                                  verbose) {
  #CHECKS
  errorMessage <- checkmate::makeAssertCollection()
  checkmate::assert_int(gapEra, lower = 1)
  checkmate::assertDouble(conceptIds, lower = 1)
  checkmate::reportAssertions(collection = errorMessage)


  if (is.null(cohortDefinitionId)) {
    cohortDefinitionId <- 1
  }
  incidencePrevalenceCohort <- cdm[["drug_exposure"]] %>%
    dplyr::select(
      "person_id",
      "drug_concept_id",
      "drug_exposure_start_date",
      "drug_exposure_end_date"
    ) %>%
    dplyr::inner_join(dplyr::tibble(drug_concept_id = conceptIds),
                      by = "drug_concept_id",
                      copy = TRUE) %>%
    dplyr::select(-"drug_concept_id") %>%
    dplyr::compute()
  # get cohort_start_date
  incidencePrevalenceStart <- incidencePrevalenceCohort %>%
    dplyr::select("person_id",
                  "cohort_start_date" = "drug_exposure_start_date") %>%
    dplyr::distinct() %>%
    dplyr::union(
      incidencePrevalenceCohort %>%
        dplyr::select("person_id", "drug_exposure_end_date") %>%
        dplyr::distinct() %>%
        dplyr::mutate(cohort_start_date = as.Date(dbplyr::sql(
          sql_add_days(CDMConnector::dbms(attr(cdm, "dbcon")),
                       1,
                       "drug_exposure_end_date")
        ))) %>%
        dplyr::select(-"drug_exposure_end_date")
    ) %>%
    dplyr::group_by(.data$person_id) %>%
    dplyr::filter(.data$cohort_start_date < max(.data$cohort_start_date, na.rm = TRUE)) %>%
    dbplyr::window_order(.data$cohort_start_date) %>%
    dplyr::mutate(index = dplyr::row_number()) %>%
    dplyr::ungroup() %>%
    dplyr::compute()
  # get_cohort_end_date
  incidencePrevalenceEnd <- incidencePrevalenceCohort %>%
    dplyr::select("person_id", "drug_exposure_start_date") %>%
    dplyr::distinct() %>%
    dplyr::mutate(cohort_end_date = as.Date(dbplyr::sql(
      sql_add_days(CDMConnector::dbms(attr(cdm, "dbcon")),-1,
                   "drug_exposure_start_date")
    ))) %>%
    dplyr::select(-"drug_exposure_start_date") %>%
    dplyr::union(
      incidencePrevalenceCohort %>%
        dplyr::select("person_id", "cohort_end_date" = "drug_exposure_end_date") %>%
        dplyr::distinct()
    ) %>%
    dplyr::group_by(.data$person_id) %>%
    dplyr::filter(.data$cohort_end_date > min(.data$cohort_end_date, na.rm = TRUE)) %>%
    dbplyr::window_order(.data$cohort_end_date) %>%
    dplyr::mutate(index = dplyr::row_number()) %>%
    dplyr::ungroup() %>%
    dplyr::compute()
  # compute the overlapping periods joining start and end dates
  incidencePrevalenceCohort <- incidencePrevalenceStart %>%
    dplyr::inner_join(incidencePrevalenceEnd,
                      by = c("person_id", "index")) %>%
    dplyr::inner_join(incidencePrevalenceCohort, by = "person_id") %>%
    dplyr::filter(.data$drug_exposure_start_date <= .data$cohort_start_date) %>%
    dplyr::filter(.data$drug_exposure_end_date >= .data$cohort_end_date) %>%
    dplyr::select("person_id", "cohort_start_date", "cohort_end_date",) %>%
    dplyr::group_by(.data$person_id) %>%
    dbplyr::window_order(.data$cohort_end_date) %>%
    dplyr::mutate(index = dplyr::row_number()) %>%
    dplyr::ungroup() %>%
    dplyr::compute()
  # compute the eras
  incidencePrevalenceCohort <- incidencePrevalenceCohort %>%
    dplyr::left_join(
      incidencePrevalenceCohort %>%
        dplyr::mutate(index = .data$index + 1) %>%
        dplyr::rename("prev_exposure_end" = "cohort_end_date") %>%
        dplyr::select("person_id", "index", "prev_exposure_end"),
      by = c("person_id", "index")
    ) %>%
    dplyr::mutate(era_index = dplyr::if_else(
      is.na(.data$prev_exposure_end),
      1,
      dplyr::if_else(
        .data$cohort_start_date - .data$prev_exposure_end - 1 <= .env$gapEra,
        0,
        1
      )
    )) %>%
    dplyr::group_by(.data$person_id) %>%
    dbplyr::window_order(.data$index) %>%
    dplyr::mutate(era_group = cumsum(.data$era_index)) %>%
    dplyr::group_by(.data$person_id, .data$era_group) %>%
    dplyr::summarise(
      cohort_start_date = min(.data$cohort_start_date, na.rm = TRUE),
      cohort_end_date = max(.data$cohort_end_date, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::select("subject_id" = "person_id", "cohort_start_date", "cohort_end_date") %>%
    dplyr::compute()

  #create perm tables in database
  if (append == FALSE) {
    cdm <-
      SqlUtilities::computePermanent(
        incidencePrevalenceCohort,
        "incidencePrevalenceCohort",
        schema = attr(cdm, "write_schema"),
        overwrite = overWrite
      )
  }

  if (append == TRUE) {
    cdm <-
      SqlUtilities::appendPermanent(
        incidencePrevalenceCohort,
        "incidencePrevalenceCohort",
        schema = attr(cdm, "write_schema")
      )
  }

  # return cdm
  return(cdm)
}

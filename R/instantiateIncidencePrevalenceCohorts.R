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
#' @param cdm cdm
#' @param conceptIds conceptIds
#' @param gapEra gapEra
#' @param incidencePrevalenceCohortName incidencePrevalenceCohortName
#' @param cohortDefinitionId cohortDefinitionId
#' @param overWrite overWrite
#' @param verbose verbose
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
                                                  overWrite = TRUE,
                                                  verbose) {

  if (!is.list(conceptIds)){
    conceptIds <- list(conceptIds)
  }
  if (is.null(cohortDefinitionId)){
    cohortDefinitionId <- 1:length(conceptIds)
  }
  incidencePrevalenceCohort <- cdm[["drug_exposure"]] %>%
    dplyr::select(
      "person_id", "drug_concept_id", "drug_exposure_start_date",
      "drug_exposure_end_date"
    ) %>%
    dplyr::inner_join(
      dplyr::tibble(drug_concept_id = conceptIds),
      by = "drug_concept_id",
      copy = TRUE
    ) %>%
    dplyr::select(-"drug_concept_id") %>%
    dplyr::compute()
  # get cohort_start_date
  incidencePrevalenceStart <- incidencePrevalenceCohort %>%
    dplyr::select(
      "person_id",
      "cohort_start_date" = "drug_exposure_start_date"
    ) %>%
    dplyr::distinct() %>%
    dplyr::union(
      incidencePrevalenceCohort %>%
        dplyr::select(
          "person_id", "drug_exposure_end_date"
        ) %>%
        dplyr::distinct() %>%
        dplyr::mutate(
          cohort_start_date = as.Date(dbplyr::sql(sql_add_days(
            CDMConnector::dbms(attr(cdm, "dbcon")),
            1,
            "drug_exposure_end_date"
          )))
        ) %>%
        dplyr::select(-"drug_exposure_end_date")
    ) %>%
    dplyr::group_by(.data$person_id) %>%
    dplyr::filter(
      .data$cohort_start_date < max(.data$cohort_start_date, na.rm = TRUE)
    ) %>%
    dbplyr::window_order(.data$cohort_start_date) %>%
    dplyr::mutate(index = dplyr::row_number()) %>%
    dplyr::ungroup() %>%
    dplyr::compute()
  # get_cohort_end_date
  incidencePrevalenceEnd <- incidencePrevalenceCohort %>%
    dplyr::select("person_id", "drug_exposure_start_date") %>%
    dplyr::distinct() %>%
    dplyr::mutate(
      cohort_end_date = as.Date(dbplyr::sql(sql_add_days(
        CDMConnector::dbms(attr(cdm, "dbcon")),
        -1,
        "drug_exposure_start_date"
      )))
    ) %>%
    dplyr::select(-"drug_exposure_start_date") %>%
    dplyr::union(incidencePrevalenceCohort %>%
      dplyr::select("person_id", "cohort_end_date" = "drug_exposure_end_date") %>%
      dplyr::distinct()) %>%
    dplyr::group_by(.data$person_id) %>%
    dplyr::filter(
      .data$cohort_end_date > min(.data$cohort_end_date, na.rm = TRUE)
    ) %>%
    dbplyr::window_order(.data$cohort_end_date) %>%
    dplyr::mutate(index = dplyr::row_number()) %>%
    dplyr::ungroup() %>%
    dplyr::compute()
  # compute the overlapping periods joining start and end dates
  incidencePrevalenceCohort <- incidencePrevalenceStart %>%
    dplyr::inner_join(
      incidencePrevalenceEnd,
      by = c("person_id", "index")
    ) %>%
    dplyr::inner_join(incidencePrevalenceCohort, by = "person_id") %>%
    dplyr::filter(.data$drug_exposure_start_date <= .data$cohort_start_date) %>%
    dplyr::filter(.data$drug_exposure_end_date >= .data$cohort_end_date) %>%
    dplyr::select(
      "person_id", "cohort_start_date", "cohort_end_date",
    ) %>%
    dplyr::group_by(.data$person_id) %>%
    dbplyr::window_order(.data$cohort_end_date) %>%
    dplyr::mutate(index = dplyr::row_number()) %>%
    dplyr::ungroup() %>%
    dplyr::compute()
  # compute the eras
  incidencePrevalenceCohort <- incidencePrevalenceCohort %>%
    dplyr::left_join(
      incidencePrevalenceCohort %>%
        dplyr::mutate(index = .data$index - 1) %>%
        dplyr::rename("next_exposure" = "cohort_start_date") %>%
        dplyr::select("subject_id", "index", "next_exposure"),
      by = c("subject_id", "index")
    ) %>%
    dplyr::mutate(era_index = dplyr::if_else(
      is.na(.data$next_exposure),
      0,
      dplyr::if_else(
        .data$next_exposure - .data$cohort_end_date - 1 <= .env$gapEra,
        0,
        1
      )
    )) %>%
    dplyr::group_by(.data$subject_id) %>%
    dbplyr::window_order(.data$index) %>%
    dplyr::mutate(era_group = cumsum(.data$era_index)) %>%
    dplyr::summarise(
      cohort_start_date = min(.data$cohort_start_date, na.rm = TRUE),
      cohort_end_date = max(.data$cohort_end_date, na.rm = TRUE),
      number_index = dplyr::n(),
      .groups = "drop"
    ) %>%
    dplyr::select(
      "subject_id", "cohort_start_date", "cohort_end_date", "number_index"
    ) %>%
    dplyr::compute()

  # MISSING the instantiate part

  # # get the query to instantiate the table
  # sql_query <- paste0(
  #   "SELECT * INTO",
  #   attr(cdm, "write_schema"),
  #   ".",
  #   incidencePrevalenceCohortName,
  #   " FROM (",
  #   dbplyr::sql_render(PASC_cohort_table),
  #   ") AS from_table"
  # )
  # # execute the query to instantiate the table
  # DBI::dbExecute(db, as.character(sql_query))
  # # make the table visible in the current cdm object
  # cdm[[incidencePrevalenceCohortName]] <- dplyr::tbl(
  #   attr(cdm, "dbcon"),
  #   paste0(
  #     "SELECT * FROM ",
  #     attr(cdm, "write_schema"),
  #     ".",
  #     incidencePrevalenceCohortName
  #   )
  # )

  # return cdm
  return(cdm)
}

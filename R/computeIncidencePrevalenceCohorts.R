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
#' @param specifications specifications
#' @param gapEra gapEra
#' @param incidencePrevalenceCohortName incidencePrevalenceCohortName
#' @param verbose verbose
#'
#' @return
#' @export
#'
#' @examples
computeIncidencePrevalenceCohorts <- function(cdm,
                                              conceptIds,
                                              gapEra,
                                              incidencePrevalenceCohortName,
                                              verbose) {
  incidencePrevalenceCohort <- cdm[["drug_exposure"]] %>%
    dplyr::select(
      "person_id", "drug_concept_id", "drug_exposure_start_date",
      "drug_exposure_end_date"
    ) %>%
    dplyr::inner_join(
      dplyr::tibble(drug_concept_id = conceptIds),
      by = "drug_concept_id",
      copy = TRUE
    )
  # get the query to instantiate the table
  sql_query <- paste0(
    "SELECT * INTO",
    attr(cdm, "write_schema"),
    ".",
    incidencePrevalenceCohortName,
    " FROM (",
    dbplyr::sql_render(PASC_cohort_table),
    ") AS from_table"
  )
  # execute the query to instantiate the table
  DBI::dbExecute(db, as.character(sql_query))
  # make the table visible in the current cdm object
  cdm[[incidencePrevalenceCohortName]] <- dplyr::tbl(
    attr(cdm, "dbcon"),
    paste0(
      "SELECT * FROM ",
      attr(cdm, "write_schema"),
      ".",
      incidencePrevalenceCohortName
    )
  )
  # return cdm
  return(cdm)
}

#' Get the cohorts to compute incidence and prevalence
#'
#' @param table table
#' @param gapEra gapEra
#' @param verbose verbose
#'
#' @noRd
getEras <- function(table,
                    gapEra,
                    verbose){
  if (!("index" %in% colnames(table))){
    table <- table %>%
      dplyr::group_by(.data$person_id) %>%
      dplyr::arrange(.data$cohort_start_date) %>%
      dplyr::mutate(index = dplyr::row_number()) %>%
      dplyr::compute()
  }
  table <- table %>%
    dplyr::left_join(
      interestExposures %>%
        dplyr::mutate(index = .data$index - 1) %>%
        dplyr::rename("next_exposure" = "cohort_start_date") %>%
        dplyr::select("person_id", "index", "next_exposure"),
      by = c("person_id", "index")
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
    dplyr::group_by(.data$person_id) %>%
    dplyr::arrange(.data$overlap_group) %>%
    dplyr::mutate(era_group = cumsum(era_index)) %>%
    dplyr::summarise(
      cohort_start_date = min(.data$cohort_start_date, na.rm = TRUE),
      cohort_end_date = max(.data$cohort_end_date, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::rename("subject_id" = "person_id") %>%
    dplyr::select("subject_id", "cohort_start_date","cohort_end_date") %>%
    dplyr::compute()
  return(table)
}

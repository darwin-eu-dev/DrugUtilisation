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

#' This function is used to summarise the indication table over multiple
#' cohorts.
#'
#' @param cohort Cohort with indications and strata
#' @param cdm cdm_reference created by CDMConnector
#' @param strata Stratification list
#' @param indicationVariables Variables that point to an indication column
#' @param minCellCount Minimum counts that a group can have. Cohorts with
#' less counts than this value are obscured. By default: 5.
#'
#' @return A Tibble with 4 columns: cohort_definition_id, variable, estimate and
#' value. There will be one row for each cohort, variable and cohort
#' combination.
#'
#' @export
#'
#' @examples
summariseIndication <- function(cohort,
                                cdm,
                                strata = list(),
                                indicationVariables = indicationColumns(cohort),
                                minCellCount = 1) {
  # initialChecks
  checkInputs(
    cohort = cohort, cdm = cdm, strata = strata,
    indicationVariables = indicationVariables, minCellCount = minCellCount
  )

  # update cohort_names
  cohort <- cohort %>%
    dplyr::left_join(
      CDMConnector::cohortSet(cohort), by = "cohort_definition_id", copy = TRUE
    )

  # summarise indication columns
  result <- PatientProfiles::summariseResult(
    table = cohort, group = list("Cohort name" = "cohort_name"),
    strata = strata, variables = list(binaryVariables = indicationVariables),
    functions = list(binaryVariables = c("count", "%")),
    minCellCount = minCellCount
  ) %>%
    dplyr::mutate(
      cdm_name = dplyr::coalesce(CDMConnector::cdmName(cdm), as.character(NA)),
      generated_by = paste0(
        "DrugUtilisation_", utils::packageVersion("DrugUtilisation"),
        "_summariseIndication"
      )
    )

  return(result)
}

#' Obtain automatically the indication columns
#'
#' @param x Tibble
#'
#' @return Name of the indication columns
#'
#' @export
#'
#' @examples
indicationColumns <- function(x) {
  names <- colnames(x)[substr(colnames(x), 1, 15) == "indication_gap_"]
  return(names)
}

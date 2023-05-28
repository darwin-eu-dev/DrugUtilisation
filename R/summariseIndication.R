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
#' @param minimumCellCount Minimum counts that a group can have. Cohorts with
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
                                minimumCellCount = 5) {
  # initialChecks
  checkInputs(
    cohort = cohort, cdm = cdm, strata = strata, indicationVariables = indicationVariables,
    minimumCellCount = minimumCellCount
  )

  # summarise indication columns
  result <- summariseCohortIndication(
    cohort, strata, indicationVariables, minimumCellCount
  )

  # get denominator counts
  denominator <- getDenominatorCount(result)

  # get indication counts
  indication <- getIndicationCount(result)

  # tidy final result
  result <- indication %>%
    dplyr::inner_join(
      denominator, by = c("cohort_name", "strata_name", "strata_level")
    ) %>%
    dplyr::select(
      "cohort_name", "strata_name", "strata_level", "indication_gap",
      "indication_name", "count", "denominator", "%"
    ) %>%
    dplyr::mutate(
      cdm_name = dplyr::coalesce(CDMConnector::cdmName(cdm), as.character(NA)),
      generated_by = "DrugUtilisation_v0.2.0_summariseIndication"
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
  names <- names[!is.na(suppressWarnings(
    as.numeric(substr(names, 16, nchar(names)))
  ))]
  return(names)
}

#' @noRd
summariseCohortIndication <- function(x,
                                      strata,
                                      indicationVariables,
                                      minimumCellCount) {
  cs <- CDMConnector::cohortSet(x)
  cohortIds <- x %>%
    dplyr::select("cohort_definition_id") %>%
    dplyr::distinct() %>%
    dplyr::pull()
  result <- list()
  for (cohortId in cohortIds) {
    result[[cs$cohort_name[cs$cohort_definition_id == cohortId]]] <- x %>%
      dplyr::filter(.data$cohort_definition_id == .env$cohortId) %>%
      dplyr::collect() %>%
      PatientProfiles::summariseCharacteristics(
        strata = strata,
        variables = list(indications = indicationVariables),
        functions = list(indications = c("count", "%")),
        suppressCellCount = minimumCellCount
      )
  }
  result <- dplyr::bind_rows(result, .id = "cohort_name")
  return(result)
}

getDenominatorCount <- function(result) {
  result %>%
    dplyr::filter(.data$variable == "number records") %>%
    dplyr::select(
      "cohort_name", "strata_name", "strata_level", "denominator" = "value"
    )
}

getIndicationCount <- function(result) {
  result %>%
    dplyr::filter(!(.data$variable %in% c("number subjects", "number records"))) %>%
    tidyr::separate("estimate", c("indication_name", "estimate"), ": ") %>%
    tidyr::pivot_wider(names_from = "estimate", values_from = "value") %>%
    dplyr::mutate(
      indication_gap = as.numeric(gsub("indication_gap_", "", .data$variable))
    ) %>%
    dplyr::select(-"variable")
}

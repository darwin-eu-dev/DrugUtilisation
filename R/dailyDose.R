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

#' add daily dose information to a drug_exposure table
#'
#' @param table table to which add daily_dose
#' @param cdm cdm
#' @param ingredientConceptId ingredientConceptId for which to filter the
#' drugs of interest
#' @param tablePrefix The stem for the permanent tables that will
#' be created. If NULL, temporary tables will be used throughout.
#'
#'
#' @return table with added columns: days_exposed, daily_dose, unit
#' @export
#'
#' @examples
addDailyDose <- function(table,
                         cdm,
                         ingredientConceptId,
                         tablePrefix = NULL) {
  errorMessage <- checkmate::makeAssertCollection()
  # initial checks
  checkmate::assertClass(cdm, "cdm_reference", add = errorMessage)
  checkmate::assertCount(ingredientConceptId, add = errorMessage)
  checkmate::assertFALSE(c("daily_dose") %in% colnames(table), add = errorMessage)
  checkmate::assertTRUE(c("drug_strength") %in% names(cdm), add = errorMessage)
  # checks for tableprefix
  checkmate::assertCharacter(
    tablePrefix, len = 1, null.ok = TRUE, add = errorMessage
  )
  checkmate::reportAssertions(collection = errorMessage)

  if ("days_exposed" %in% colnames(table)) {
    warning("'days_exposed' will be overwritten.")
  }

  table <- table %>%
    dplyr::mutate(days_exposed = CDMConnector::datediff(
      start = "drug_exposure_start_date",
      end = "drug_exposure_end_date"
    ) + 1)

  table <- table %>%
    dplyr::left_join(
      table %>%
        dplyr::select(
          "days_exposed", "quantity", "drug_concept_id", "drug_exposure_id"
        ) %>%
        dplyr::distinct() %>%
        dplyr::inner_join(
          cdm$drug_strength %>%
            dplyr::filter(.data$ingredient_concept_id %in% .env$ingredientConceptId),
          by = c("drug_concept_id")
        ) %>%
        addPattern() %>%
        dplyr::mutate(
          # Change this with all different formula values
          daily_dose = dplyr::case_when(
            is.na(.data$quantity) ~ as.numeric(NA),
            .data$quantity < 0 ~ as.numeric(NA),
            .data$pattern_id %in% c(1:5) ~
              .data$amount_value * .data$quantity / .data$days_exposed,
            .data$pattern_id %in% c(6,7)  &&
              .data$denominator_value > 24
            ~ .data$numerator_value * 24 / .data$denominator_value,
            .data$pattern_id %in% c(6,7)  &&
              .data$denominator_value <= 24
            ~ .data$numerator_value,
            .data$pattern_id %in% c(8,9)
            ~ .data$numerator_value * 24,
            .default = as.numeric(NA)
          )
        ) %>%
        dplyr::mutate(daily_dose = dplyr::if_else(.data$daily_dose <= 0, NA, .data$daily_dose)) %>%
        dplyr::select(
          "days_exposed", "quantity", "drug_concept_id", "drug_exposure_id",
          "daily_dose", "unit"
        ),
      by = c("days_exposed", "quantity", "drug_concept_id", "drug_exposure_id")
    )

  if(is.null(tablePrefix)){
    table <- table %>%
      CDMConnector::computeQuery()
  } else {
    table <- table %>%
      CDMConnector::computeQuery(name = paste0(tablePrefix,
                                               "_person_sample"),
                                 temporary = FALSE,
                                 schema = attr(cdm, "write_schema"),
                                 overwrite = TRUE)
  }

  return(table)
}

#' Check coverage of daily dose computation in a sample of the cdm for selected
#' concept sets and ingredient
#'
#' @param cdm A cdm reference created using CDMConnector
#' @param sample A number indicating the size of the random sample to take from
#' the 'person' table of the cdm
#' @param ingredient Code indicating the ingredient of interest
#' @param conceptList List in the format given by 'conceptList.R' of concept
#' sets of interest
#' @param seed Seed for the random sample
#'
#' @return The function returns information of the coverage of computeDailyDose.R
#' for the selected ingredients and concept sets
#' @export
#'
#' @examples
dailyDoseCoverage <- function(cdm,
                              sample = NULL,
                              ingredient = NULL,
                              conceptList = NULL,
                              seed = 1) {

  if(!is.null(conceptList)) {
    checkInputs(
      cdm = cdm, sample = sample, ingredientConceptId = ingredient,
      conceptSetList = conceptList
    )
  } else {
    checkInputs(
      cdm = cdm, sample = sample, ingredientConceptId = ingredient
    )
  }

  set.seed(seed)

  if(!is.null(conceptList)) {
    concepts_interest <- unname(unlist(conceptList))
  } else {
    concepts_interest <- cdm$drug_exposure %>%
      dplyr::select("drug_concept_id") %>%
      dplyr::distinct() %>%
      dplyr::pull()
  }

  coverage_cohort <-  cdm$drug_exposure %>%
    dplyr::filter(.data$drug_concept_id %in% .env$concepts_interest) %>%
    dplyr::inner_join(
      cdm$person %>%
        dplyr::select("person_id") %>%
        dplyr::slice_sample(n = sample),
      by = "person_id"
    ) %>%
    dplyr::inner_join(
      cdm$drug_strength %>%
        dplyr::filter(.data$ingredient_concept_id == .env$ingredient) %>%
        dplyr::select("drug_concept_id"),
      by = "drug_concept_id"
    ) %>%
    addDailyDose(cdm, ingredient)

  coverage_num <- coverage_cohort %>%
    dplyr::filter(!is.na(.data$daily_dose)) %>%
    dplyr::tally() %>%
    dplyr::pull()

  coverage_den <- coverage_cohort %>%
    dplyr::tally() %>%
    dplyr::pull()

  if(coverage_den == 0) {
    coverage <- NA
  } else {
    coverage <- coverage_num/coverage_den*100
  }

  return(coverage)
}

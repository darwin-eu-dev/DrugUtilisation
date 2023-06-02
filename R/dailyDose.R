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
#' @param drugExposure drugExposure it must contain drug_concept_id, quantity,
#' drug_exposure_start_date and drug_exposure_end_date as columns
#' @param cdm cdm
#' @param ingredientConceptId ingredientConceptId for which to filter the
#' drugs of interest
#'
#' @return same input table
#' @export
#'
#' @examples
addDailyDose <- function(drugExposure,
                         cdm,
                         ingredientConceptId) {
  # initial checks
  checkInputs(
    drugExposure = drugExposure, cdm = cdm,
    ingredientConceptId = ingredientConceptId
  )

  # select only pattern_id and unit
  dailyDose <- drugExposure %>%
    dplyr::select(
      "drug_concept_id", "drug_exposure_start_date", "drug_exposure_end_date",
      "quantity"
    ) %>%
    dplyr::distinct() %>%
    dplyr::mutate(days_exposed = !!CDMConnector::datediff(
      start = "drug_exposure_start_date",
      end = "drug_exposure_end_date"
    ) + 1) %>%
    dplyr::inner_join(
      drugExposure %>%
        dplyr::select("drug_concept_id") %>%
        addPatternInternal(drugExposure, cdm, ingredientConceptId),
      by = "drug_concept_id"
    ) %>%
    standardUnits() %>%
    applyFormula() %>%
    dplyr::select(
      "drug_concept_id", "drug_exposure_start_date", "drug_exposure_end_date",
      "quantity", "daily_dose", "unit"
    ) %>%
    CDMConnector::computeQuery()

  # add the information back to the initial table
  drugExposure <- drugExposure %>%
    dplyr::left_join(
      dailyDose,
      by = c(
        "drug_concept_id", "drug_exposure_start_date", "drug_exposure_end_date",
        "quantity"
      )
    )

  return(drugExposure)
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

standardUnits <- function(drugExposure) {
  drugExposure <- drugExposure %>%
    dplyr::mutate(
      amount_value = ifelse(
        .data$amount_unit_concept_id == 9655,
        .data$amount_value / 1000, .data$amount_value
      ),
      numerator_value = ifelse(
        .data$numerator_unit_concept_id == 9655,
        .data$numerator_value / 1000, .data$numerator_value
      ),
      denominator_value = ifelse(
        .data$denominator_unit_concept_id == 8519,
        .data$denominator_value * 1000, .data$denominator_value
      ),
      numerator_value = ifelse(
        .data$numerator_unit_concept_id == 9439,
        .data$numerator_value / 1000000, .data$numerator_value
      )
    )
}

applyFormula <- function(drugExposure) {
  drugExposure <- drugExposure %>%
    dplyr::mutate(
      daily_dose = dplyr::case_when(
        is.na(.data$quantity) ~
          as.numeric(NA),
        .data$quantity < 0 ~
          as.numeric(NA),
        .data$pattern_id %in% c(1:5) ~
          .data$amount_value * .data$quantity / .data$days_exposed,
        .data$pattern_id %in% c(6,7)  & .data$denominator_value > 24 ~
          .data$numerator_value * 24 / .data$denominator_value,
        .data$pattern_id %in% c(6,7) & .data$denominator_value <= 24 ~
          .data$numerator_value,
        .data$pattern_id %in% c(8,9) ~
          .data$numerator_value * 24,
        .default = as.numeric(NA)
      )
    ) %>%
    dplyr::mutate(daily_dose = dplyr::if_else(
      .data$daily_dose <= 0, as.numeric(NA), .data$daily_dose
    ))
}

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
#' \donttest{
#' library(DrugUtilisation)
#' library(dplyr)
#'
#' cdm <- mockDrugUtilisation()
#'
#' cdm$drug_exposure %>%
#'   filter(drug_concept_id == 2905077) %>%
#'   addDailyDose(cdm, 1125315)
#' }
#'
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
        dplyr::distinct() %>%
        addPatternInternal(cdm, ingredientConceptId),
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
    ) %>%
    CDMConnector::computeQuery()

  return(drugExposure)
}

#' Check coverage of daily dose computation in a sample of the cdm for selected
#' concept sets and ingredient
#'
#' @param cdm A cdm reference created using CDMConnector
#' @param ingredientConceptId Code indicating the ingredient of interest
#' @param sample A number indicating the size of the random sample to take from
#' the 'person' table of the cdm
#' @param conceptSetList A concept list that we want to test
#' @param stratifyByConcept Whether to stratify the result by drug_concept_id
#' @param seed Seed for the random sample
#'
#' @return The function returns information of the coverage of computeDailyDose.R
#' for the selected ingredients and concept sets
#' @export
#'
#' @examples
#' \donttest{
#' library(DrugUtilisation)
#'
#' cdm <- mockDrugUtilisation()
#' dailyDoseCoverage(cdm, 1125315)
#' }
#'
dailyDoseCoverage <- function(cdm,
                              ingredientConceptId,
                              sample = NULL,
                              conceptSetList = NULL) {
  # initial checks
  checkInputs(
    cdm = cdm, sample = sample, ingredientConceptId = ingredientConceptId,
    conceptSetList = conceptSetList
  )

  # extract concept sets
  if (is.null(conceptSetList)) {
    conceptSet <- cdm[["drug_strength"]] %>%
      dplyr::filter(
        .data$ingredient_concept_id %in% .env$ingredientConceptId
      ) %>%
      dplyr::select(
        "concept_id" = "drug_concept_id", "ingredient_concept_id"
      )
  } else {
    conceptSet <- conceptSetFromConceptSetList(conceptSetList) %>%
      dplyr::rename("drug_concept_id" = "concept_id") %>%
      dplyr::inner_join(
        dplyr::tibble(
          concept_name = names(conceptSetList),
          ingredient_concept_id = ingredientConceptId
        ),
        by = "concept_name"
      ) %>%
      dplyr::select("drug_concept_id", "ingredient_concept_id")
  }

  # random sample
  if (!is.null(sample)) {
    cdm[["drug_exposure"]] <- cdm[["drug_exposure"]] %>%
      dplyr::inner_join(
        cdm[["person"]] %>%
          dplyr::select("person_id") %>%
          dplyr::slice_sample(n = sample),
        by = "person_id"
      )
  }

  # compute daily dose
  doseCoverage <- cdm[["drug_exposure"]] %>%
    dplyr::select(
      "drug_concept_id", "drug_exposure_start_date", "drug_exposure_end_date",
      "quantity"
    ) %>%
    dplyr::mutate(days_exposed = !!CDMConnector::datediff(
      start = "drug_exposure_start_date",
      end = "drug_exposure_end_date"
    ) + 1) %>%
    dplyr::inner_join() %>%
    addPatternInternal(cdm, ingredientConceptId) %>%
    standardUnits() %>%
    applyFormula() %>%
    dplyr::select(
      "drug_concept_id", "ingredient_concept_id", "daily_dose", "unit"
    ) %>%
    CDMConnector::computeQuery()

  # add route

  # collect
  doseCoverage <- doseCoverage %>% dplyr::collect()

  # add ingredient name
  ingredientNames <- cdm[["concept"]] %>%
    dplyr::filter(.data$concept_id %in% ingredientConceptId) %>%
    dplyr::collect() %>%
    dplyr::mutate(
      "ingredient" = "concept_name", "ingredient_concept_id" = "concept_id"
    )
  doseCoverage <- doseCoverage %>%
    dplyr::inner_join(ingredientNames, by = "ingredient_concept_id")

  # summarise coverage
  result <- doseCoverage %>%
    PatientProfiles::summariseResult(
      group = list("ingredient"),
      includeOverallGroup = FALSE,
      strata = list("unit", c("unit", "route")),
      includeOverallStrata = FALSE,
      variables = "daily_dose",
      functions = c(
        "missing", "mean", "sd", "min", "q05", "q25", "median", "q75", "q95",
        "max"
      )
    ) %>%
    PatientProfiles::addCdmName(cdm = cdm) %>%
    dplyr::mutate(result_type = "dose coverage") %>%
    dplyr::relocate(c("cdm_name", "result_type"))

  return(result)
}

standardUnits <- function(drugExposure) {
  drugExposure %>%
    dplyr::mutate(
      amount_value = dplyr::if_else(
        .data$amount_unit_concept_id == 9655,
        .data$amount_value / 1000, .data$amount_value
      ),
      numerator_value = dplyr::if_else(
        .data$numerator_unit_concept_id == 9655,
        .data$numerator_value / 1000, .data$numerator_value
      ),
      denominator_value = dplyr::if_else(
        .data$denominator_unit_concept_id == 8519,
        .data$denominator_value * 1000, .data$denominator_value
      ),
      numerator_value = dplyr::if_else(
        .data$numerator_unit_concept_id == 9439,
        .data$numerator_value / 1000000, .data$numerator_value
      )
    )
}

applyFormula <- function(drugExposure) {
  drugExposure %>%
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

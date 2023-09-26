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
#' concept_relationship <- dplyr::tibble(
#' concept_id_1 = c(2905077, 1516983, 2905075, 1503327, 1516978, 1503326, 1503328, 1516980,
#'                  29050773, 1125360, 15033297, 15030327, 15033427, 15036327, 15394662,
#'                  43135274, 11253605, 431352774, 431359274, 112530, 1539465, 29050772,
#'                  431352074, 15394062, 43135277, 15033327, 11253603, 15516980, 5034327,
#'                  1539462, 15033528, 15394636, 15176980, 1539463, 431395274, 15186980,
#'                  15316978),
#' concept_id_2 = c(19016586, 46275062, 35894935, 19135843, 19082107, 19011932, 19082108,
#'                  2008660,  2008661,  2008662, 19082109, 43126087, 19130307, 42629089,
#'                  19103220, 19082048, 19082049, 19082256, 19082050, 19082071, 19082072,
#'                  19135438, 19135446, 19135439, 19135440, 46234466, 19082653, 19057400,
#'                  19082227, 19082286, 19009068, 19082628, 19082224, 19095972, 19095973,
#'                  35604394, 702776 ),
#' relationship_id = c(rep("RxNorm has dose form", 37))
#' )
#'
#' cdm <- mockDrugUtilisation(extraTables = list("concept_relationship" = concept_relationship))
#'
#' cdm$drug_exposure %>%
#'   filter(drug_concept_id == 2905077) %>%
#'   addDailyDose(cdm, 1125315)
#' }
#'
addDailyDose <- function(drugExposure,
                         ingredientConceptId) {
  cdm <- attr(drugExposure, "cdm_reference")

  # initial checks
  checkInputs(
    drugExposure = drugExposure, ingredientConceptId = ingredientConceptId
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
      drugStrengthPattern(cdm = cdm, ingredientConceptId = ingredientConceptId),
      by = "drug_concept_id"
    ) %>%
    standardUnits() %>%
    applyFormula() %>%
    dplyr::select(
      "drug_concept_id", "drug_exposure_start_date", "drug_exposure_end_date",
      "quantity", "daily_dose", "unit", "route"
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
#' concept_relationship <- dplyr::tibble(
#' concept_id_1 = c(2905077, 1516983, 2905075, 1503327, 1516978, 1503326, 1503328, 1516980,
#'                  29050773, 1125360, 15033297, 15030327, 15033427, 15036327, 15394662,
#'                  43135274, 11253605, 431352774, 431359274, 112530, 1539465, 29050772,
#'                  431352074, 15394062, 43135277, 15033327, 11253603, 15516980, 5034327,
#'                  1539462, 15033528, 15394636, 15176980, 1539463, 431395274, 15186980,
#'                  15316978),
#' concept_id_2 = c(19016586, 46275062, 35894935, 19135843, 19082107, 19011932, 19082108,
#'                  2008660,  2008661,  2008662, 19082109, 43126087, 19130307, 42629089,
#'                  19103220, 19082048, 19082049, 19082256, 19082050, 19082071, 19082072,
#'                  19135438, 19135446, 19135439, 19135440, 46234466, 19082653, 19057400,
#'                  19082227, 19082286, 19009068, 19082628, 19082224, 19095972, 19095973,
#'                  35604394, 702776 ),
#' relationship_id = c(rep("RxNorm has dose form", 37))
#' )
#'
#' cdm <- mockDrugUtilisation(extraTables = list("concept_relationship" = concept_relationship))

#' dailyDoseCoverage(cdm, 1125315)
#' }
#'
dailyDoseCoverage <- function(cdm,
                              ingredientConceptId,
                              sample = NULL,
                              conceptSetList = NULL,
                              stratifyByConcept = TRUE,
                              seed = 1) {
  # add conceptSetList if needed
  if (is.null(conceptSetList)) {
    checkInputs(ingredientConceptId = ingredientConceptId, cdm = cdm)
    conceptSetList <- list(
      cdm[["drug_strength"]] %>%
        dplyr::filter(.data$ingredient_concept_id == .env$ingredientConceptId) %>%
        dplyr::pull("drug_concept_id")
    )
    names(conceptSetList) <- cdm[["concept"]] %>%
      dplyr::filter(.data$concept_id == .env$ingredientConceptId) %>%
      dplyr::pull("concept_name")
  }

  # initial checks
  checkInputs(
    cdm = cdm, sample = sample, ingredientConceptId = ingredientConceptId,
    conceptSetList = conceptSetList, stratifyByConcept = stratifyByConcept,
    seed = seed
  )

  # extract concept sets
  conceptSet <- conceptSetFromConceptSetList(conceptSetList) %>%
    dplyr::rename("drug_concept_id" = "concept_id")

  # random sample
  if (!is.null(sample)) {
    set.seed(seed)
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
    dplyr::inner_join(conceptSet, by = "drug_concept_id", copy = TRUE) %>%
    addDailyDose(cdm, ingredientConceptId)

  # summarise counts
  result <- doseCoverage %>%
    dplyr::mutate(
      daily_dose = dplyr::if_else(is.na(.data$daily_dose), 0, 1)
    ) %>%
    dplyr::group_by(.data$cohort_definition_id, .data$drug_concept_id) %>%
    dplyr::summarise(
      number_records = dplyr::n(),
      number_dose = sum(.data$daily_dose, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::collect() %>%
    dplyr::right_join(
      attr(conceptSet, "cohort_set"), by = "cohort_definition_id"
    ) %>%
    dplyr::mutate(
      number_records = dplyr::if_else(
        is.na(.data$number_records), 0, .data$number_records
      ),
      number_dose = dplyr::if_else(
        is.na(.data$number_dose), 0, .data$number_dose
      )
    )

  # get overall counts
  result <- result %>%
    dplyr::group_by(.data$cohort_definition_id, .data$cohort_name) %>%
    dplyr::summarise(
      number_records = sum(.data$number_records),
      number_dose = sum(.data$number_dose),
      .groups = "drop"
    ) %>%
    dplyr::mutate(drug_concept_id = as.numeric(NA)) %>%
    dplyr::union_all(result) %>%
    dplyr::mutate(coverage = .data$number_dose/.data$number_records) %>%
    dplyr::arrange(
      .data$cohort_definition_id, dplyr::desc(.data$number_records)
    ) %>%
    dplyr::select(
      "concept_set" = "cohort_name", "drug_concept_id", "number_records",
      "number_dose", "coverage"
    )

  if (stratifyByConcept == FALSE) {
    result <- result %>%
      dplyr::filter(is.na(.data$drug_concept_id))
  }

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
        .data$quantity <= 0 ~ # TO REMOVE
          as.numeric(NA),
        .data$formula_id == 1 ~
          .data$numerator_value * .data$quantity / .data$days_exposed,
        .data$formula_id == 2 ~
          .data$amount_value * .data$quantity / .data$days_exposed,
        .data$formula_id == 3  & .data$denominator_value > 24 ~
          .data$numerator_value * 24 / .data$denominator_value,
        .data$formula_id == 3 & .data$denominator_value <= 24 ~ # WHY?
          .data$numerator_value,
        .data$formula_id == 4 ~
          .data$numerator_value * 24,
        .default = as.numeric(NA)
      )
    ) %>%
    dplyr::mutate(daily_dose = dplyr::if_else(
      .data$daily_dose <= 0, as.numeric(NA), .data$daily_dose
    ))
}

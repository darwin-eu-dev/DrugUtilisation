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
#' @param cdm A cdm reference
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
#' cdm[["drug_exposure"]] %>%
#'   filter(drug_concept_id == 2905077) %>%
#'   addDailyDose(ingredientConceptId = 1125315)
#' }
#'
addDailyDose <- function(drugExposure,
                         cdm = attr(drugExposure, "cdm_reference"),
                         ingredientConceptId) {
  # initial checks
  checkInputs(
    drugExposure = drugExposure, ingredientConceptId = ingredientConceptId,
    cdm = cdm
  )

  nm <- uniqueTmpName()

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
      "quantity", "daily_dose", "unit"
    ) %>%
    dplyr::compute(temporary = FALSE, overwrite = TRUE, name = nm)

  # add the information back to the initial table
  drugExposure <- drugExposure %>%
    dplyr::left_join(
      dailyDose,
      by = c(
        "drug_concept_id", "drug_exposure_start_date", "drug_exposure_end_date",
        "quantity"
      )
    ) %>%
    dplyr::compute()

  cdm <- omopgenerics::dropTable(cdm = cdm, name = nm)

  return(drugExposure)
}

#' Check coverage of daily dose computation in a sample of the cdm for selected
#' concept sets and ingredient
#'
#' @param cdm A cdm reference created using CDMConnector
#' @param ingredientConceptId Code indicating the ingredient of interest
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
#'
#' dailyDoseCoverage(cdm, 1125315)
#' }
#'
dailyDoseCoverage <- function(cdm,
                              ingredientConceptId) {
  # initial checks
  checkInputs(cdm = cdm)

  # get daily dosage
  dailyDose <- cdm[["drug_exposure"]] %>%
    dplyr::inner_join(
      cdm[["concept_ancestor"]] %>%
        dplyr::filter(.data$ancestor_concept_id %in% .env$ingredientConceptId) %>%
        dplyr::select("drug_concept_id" = "descendant_concept_id") %>%
        dplyr::distinct(),
      by = "drug_concept_id"
    ) %>%
    dplyr::select(
      "drug_concept_id", "drug_exposure_start_date", "drug_exposure_end_date",
      "quantity"
    ) %>%
    dplyr::mutate(days_exposed = !!CDMConnector::datediff(
      start = "drug_exposure_start_date",
      end = "drug_exposure_end_date"
    ) + 1) %>%
    dplyr::left_join(
      drugStrengthPattern(cdm = cdm, ingredientConceptId = ingredientConceptId),
      by = "drug_concept_id"
    ) %>%
    standardUnits() %>%
    applyFormula() %>%
    dplyr::select(
      "drug_concept_id", "daily_dose", "unit", "pattern_id",
      "concept_id" =  "ingredient_concept_id"
    ) %>%
    addRoute() %>%
    dplyr::left_join(
      cdm[["concept"]] %>%
        dplyr::rename("ingredient_name" = "concept_name") %>%
        dplyr::select("concept_id", "ingredient_name"),
      by = "concept_id"
    ) %>%
    dplyr::collect()

  # summarise
  dailyDoseSummary <- dailyDose %>%
    dplyr::mutate(dplyr::across(
      c("route", "unit", "pattern_id", "ingredient_name"),
      ~ dplyr::if_else(is.na(.x), "NA", as.character(.x))
    )) |>
    PatientProfiles::summariseResult(
      group = list("ingredient_name"),
      includeOverallGroup = FALSE,
      strata = list("unit", c("route", "unit"), c("unit", "route", "pattern_id")),
      includeOverallStrata = TRUE,
      variables = "daily_dose",
      estimates = c(
        "count_missing", "percentage_missing", "mean", "sd", "min", "q05",
        "q25", "median", "q75", "q95", "max"
      )
    ) %>%
    dplyr::filter(
      !(.data$strata_name %in% c("Overall", "route")) |
        .data$variable_name != "daily_dose" |
        .data$estimate_name %in% c("count", "percentage")
    ) |>
    dplyr::mutate("cdm_name" = omopgenerics::cdmName(cdm))
  dailyDoseSummary <- dailyDoseSummary |>
    omopgenerics::newSummarisedResult(settings = dplyr::tibble(
      "result_id" = unique(dailyDoseSummary$result_id),
      "package_name" = "DrugUtilisation",
      "package_version" = as.character(utils::packageVersion("DrugUtilisation")),
      "result_type" = "dose_coverage"
    ))

  return(dailyDoseSummary)
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
        .data$quantity <= 0 ~
          as.numeric(NA),
        .data$days_exposed <= 0 ~
          as.numeric(NA),
        .data$denominator_value <= 0 ~
          as.numeric(NA),
        .data$numerator_value <= 0 ~
          as.numeric(NA),
        .data$amount_value <= 0 ~
          as.numeric(NA),
        .data$formula_name == "concentration formulation" ~
          .data$numerator_value * .data$quantity / .data$days_exposed,
        .data$formula_name == "fixed amount formulation" ~
          .data$amount_value * .data$quantity / .data$days_exposed,
        .data$formula_name == "time based with denominator" & .data$denominator_value > 24 ~
          .data$numerator_value * 24 / .data$denominator_value,
        .data$formula_name == "time based with denominator" & .data$denominator_value <= 24 ~
          .data$numerator_value,
        .data$formula_name == "time based no denominator" ~
          .data$numerator_value * 24,
        .default = as.numeric(NA)
      )
    )
}

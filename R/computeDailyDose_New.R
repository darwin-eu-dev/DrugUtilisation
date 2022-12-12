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

#' Explain function
#'
#' @param table table
#' @param cdm cdm
#' @param ingredientConceptId ingredientConceptId
#'
#' @return
#' @export
#'
#' @examples
addDailyDose <- function(table,
                         cdm,
                         ingredientConceptId) {
  errorMessage <- checkmate::makeAssertCollection()
  # initial checks
  checkmate::assertClass(cdm, "cdm_reference", add = errorMessage)
  checkmate::assertCount(ingredientConceptId, add = errorMessage)
  checkmate::assertFALSE(c("daily_dose") %in% colnames(table), add = errorMessage)
  checkmate::assertTRUE(c("drug_strength") %in% names(cdm), add = errorMessage)
  checkmate::reportAssertions(collection = errorMessage)

  table <- table %>%
    dplyr::left_join(
      table %>%
        dplyr::mutate(days_exposed = CDMConnector::datediff(
          start = "drug_exposure_start_date",
          end = "drug_exposure_end_date"
        ) + 1) %>%
        dplyr::select(
          "person_id", "days_exposed", "quantity", "drug_concept_id", "drug_exposure_id"
        ) %>%
        dplyr::inner_join(
          cdm$drug_strength,
          by = c("drug_concept_id")
        ) %>%
        dplyr::mutate(drugDoseType := dplyr::case_when(
          # 1. Tablets and other fixed amount formulations
          is.na(denominator_unit_concept_id) == TRUE ~ "tablets",
          # 2. Puffs of an inhaler
          denominator_unit_concept_id == 45744809  ~ "puffs",
          # 3. Quantified Drugs which are formulated as a concentration
          denominator_unit_concept_id %in% c(8576, 8587) && denominator_value != 1 && is.na(denominator_value)==FALSE ~ "quantified",
          # 4. Drugs with the total amount provided in quantity, e.g. chemotherapeutics
          denominator_unit_concept_id %in% c(8576, 8587) && (denominator_value == 1 | is.na(denominator_value)==TRUE) ~ "quantity",
          # 5. Compounded drugs
          denominator_unit_concept_id == 8576 & amount_value == 1 ~ "compounded",
          # 6. Drugs with the active ingredient released over time, e.g. patches
          denominator_unit_concept_id == 8505 ~ "timeBased")
        ) %>%
        dplyr::mutate(
          daily_dose := dplyr::case_when(is.na(drugDoseType) ~ NA,
                                         days_exposed == 0 ~ NA,
                                         drugDoseType == "tablets" ~  .data$quantity * .data$amount_value / .data$days_exposed,
                                         drugDoseType == "quantified" ~ .data$quantity * .data$numerator_value / .data$days_exposed,
                                         drugDoseType == "puffs" ~ .data$quantity * .data$numerator_value / .data$days_exposed,
                                         drugDoseType == "compounded" ~ .data$quantity * .data$numerator_value / .data$days_exposed,
                                         drugDoseType == "quantity" ~ .data$quantity * .data$numerator_value / .data$days_exposed,
                                         drugDoseType == "timeBased" ~ 24 * .data$numerator_value)
          ) %>%
        # dplyr::mutate(ingredient_concept_id = ingredient_concept_id) %>%
        dplyr::select(
          "person_id", "drugDoseType", "daily_dose", "drug_concept_id", "ingredient_concept_id",
          "drug_exposure_id"
        ),
      by = c(
        "person_id", "drug_concept_id",
        "drug_exposure_id"
      )
    ) %>%
    dplyr::compute()

  return(table)
}

#' Explain function
#'
#' @param cdm cdm
#' @param tableName tableName
#' @param ingredientConceptId ingredientConceptId
#'
#' @return
#' @export
#'
#' @examples
computeDailyDose <- function(cdm,
                             tableName,
                             ingredientConceptId) {
  errorMessage <- checkmate::makeAssertCollection()
  # initial checks
  checkmate::assertClass(cdm, "cdm_reference", add = errorMessage)
  checkmate::assertCount(ingredientConceptId, add = errorMessage)
  checkmate::assertFALSE(c("daily_dose") %in% colnames(table), add = errorMessage)
  checkmate::assertTRUE(c("drug_strength") %in% names(cdm), add = errorMessage)
  checkmate::reportAssertions(collection = errorMessage)

  cdm[[tableName]] <- addDailyDose(
    table = cdm[[tableName]],
    cdm = cdm,
    ingredientConceptId = ingredientConceptId
  )

  return(cdm)
}

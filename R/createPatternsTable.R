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

#' Function to create a tibble with the patterns from current drug strength table
#'
#' @param cdm 'cdm' object created with CDMConnector::cdm_from_con(). It must
#' must contain 'drug_strength' and 'concept' tables.
#'
#' @return The function creates a tibble with the different patterns found in
#' the table, plus a column of potentially valid and invalid combinations.
#' @export
#'
#' @examples
createPatternsTable <- function(cdm) {
  # Check errors in input
  checkCdm(cdm, tables = c("drug_strength", "concept"))

  # Start code
  amount_unit_db <- cdm$concept %>%
    dplyr::select(
      "amount_unit_concept_id" = "concept_id",
      "amount_unit" = "concept_name"
    ) %>%
    CDMConnector::computeQuery()

  numerator_unit_db <- cdm$concept %>%
    dplyr::select(
      "numerator_unit_concept_id" = "concept_id",
      "numerator_unit" = "concept_name"
    ) %>%
    CDMConnector::computeQuery()

  denominator_unit_db <- cdm$concept %>%
    dplyr::select(
      "denominator_unit_concept_id" = "concept_id",
      "denominator_unit" = "concept_name"
    ) %>%
    CDMConnector::computeQuery()

  ingredient_db <- cdm$concept %>%
    dplyr::select(
      "ingredient_concept_id" = "concept_id",
      "ingredient_name" = "concept_name"
    ) %>%
    CDMConnector::computeQuery()

  drug_db <- cdm$concept %>%
    dplyr::select(
      "drug_concept_id" = "concept_id",
      "drug_name" = "concept_name"
    ) %>%
    CDMConnector::computeQuery()

  x <- cdm$drug_strength %>%
    dplyr::collect() %>%
    dplyr::left_join(drug_db, by = "drug_concept_id") %>%
    dplyr::left_join(ingredient_db, by = "ingredient_concept_id") %>%
    dplyr::mutate(amount = ifelse(is.na(.data$amount_value), NA, "numeric")) %>%
    dplyr::left_join(amount_unit_db, by = "amount_unit_concept_id") %>%
    dplyr::mutate(numerator = ifelse(is.na(.data$numerator_value), NA, "numeric")) %>%
    dplyr::left_join(numerator_unit_db, by = "numerator_unit_concept_id") %>%
    dplyr::mutate(denominator = ifelse(is.na(.data$denominator_value), NA, "numeric")) %>%
    dplyr::left_join(denominator_unit_db, by = "denominator_unit_concept_id") %>%
    dplyr::select(
      "drug_concept_id", "ingredient_concept_id", "amount", "amount_unit", "amount_unit_concept_id", "numerator",
      "numerator_unit", "numerator_unit_concept_id", "denominator", "denominator_unit", "amount_value",
      "numerator_value", "denominator_value", "denominator_unit_concept_id", "ingredient_name", "drug_name"
    ) %>%
    CDMConnector::computeQuery()

  patternfile <- x %>%
    dplyr::group_by(.data$amount, .data$amount_unit, .data$amount_unit_concept_id, .data$numerator, .data$numerator_unit,
                    .data$denominator, .data$denominator_unit, .data$numerator_unit_concept_id, .data$denominator_unit_concept_id) %>%
    dplyr::summarise(
      number_concepts = dplyr::n_distinct(.data$drug_concept_id),
      number_ingredients = dplyr::n_distinct(.data$ingredient_concept_id),
      .groups = "drop"
    ) %>%
    dplyr::mutate(pattern_id = dplyr::row_number()) %>%
    dplyr::relocate(.data$pattern_id) %>% dplyr::collect()

  # Here add logic valid column of patterns
  patternfile <- patternfile %>%
    dplyr::mutate(
      valid =
        ifelse((!is.na(.data$amount) & grepl("gram|curie|international unit|mole|liter|milliequivalent", .data$amount_unit) & is.na(.data$denominator_unit) & is.na(.data$numerator_unit)) |
                          (!is.na(.data$numerator) & grepl("gram|curie|international unit|mole|liter|milliequivalent", .data$numerator_unit) &
                             grepl("gram|hour|liter|Actuation", .data$denominator_unit)), TRUE, FALSE))

  #validpattern <- subset(patternfile, valid==TRUE) %>% arrange(amount, denominator)

  return(patternfile)
}

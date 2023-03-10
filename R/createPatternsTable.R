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
#' the table, plus a column of potentialy valid and unvalid combinations.
#' @export
#'
#' @examples

createPatternsTable <- function(cdm) {
  # Check errors in input

  errorMessage <- checkmate::makeAssertCollection()
  checkmate::assertClass(
    cdm,
    classes = "cdm_reference",
    add = errorMessage
  )

  checkmate::reportAssertions(collection = errorMessage)
  errorMessage <- checkmate::makeAssertCollection()

  checkmate::assertTRUE(
    all(c("drug_strength", "concept") %in% names(cdm)),
    add = errorMessage
  )

  # Start code
  amount_unit_db <- cdm$concept %>%
    dplyr::select(
      "amount_unit_concept_id" = "concept_id",
      "amount_unit" = "concept_name"
    ) %>%
    compute()

  numerator_unit_db <- cdm$concept %>%
    dplyr::select(
      "numerator_unit_concept_id" = "concept_id",
      "numerator_unit" = "concept_name"
    ) %>%
    compute()

  denominator_unit_db <- cdm$concept %>%
    dplyr::select(
      "denominator_unit_concept_id" = "concept_id",
      "denominator_unit" = "concept_name"
    ) %>%
    compute()

  ingredient_db <- cdm$concept %>%
    dplyr::select(
      "ingredient_concept_id" = "concept_id",
      "ingredient_name" = "concept_name"
    ) %>%
    compute()

  drug_db <- cdm$concept %>%
    dplyr::select(
      "drug_concept_id" = "concept_id",
      "drug_name" = "concept_name"
    ) %>%
    compute()

  x <- cdm$drug_strength %>%
    dplyr::left_join(drug_db, by = "drug_concept_id") %>%
    dplyr::left_join(ingredient_db, by = "ingredient_concept_id") %>%
    dplyr::mutate(amount = if_else(is.na(amount_value), NA, "numeric")) %>%
    dplyr::left_join(amount_unit_db, by = "amount_unit_concept_id") %>%
    dplyr::mutate(numerator = if_else(is.na(numerator_value), NA, "numeric")) %>%
    dplyr::left_join(numerator_unit_db, by = "numerator_unit_concept_id") %>%
    dplyr::mutate(denominator = if_else(is.na(denominator_value), NA, "numeric")) %>%
    dplyr::left_join(denominator_unit_db, by = "denominator_unit_concept_id") %>%
    dplyr::select(
      drug_concept_id, ingredient_concept_id, amount, amount_unit, amount_unit_concept_id, numerator,
      numerator_unit, numerator_unit_concept_id, denominator, denominator_unit, amount_value,
      numerator_value, denominator_value, denominator_unit_concept_id, ingredient_name, drug_name
    ) %>%
    compute()

  patternfile <- x %>%
    dplyr::group_by(amount, amount_unit, amount_unit_concept_id, numerator, numerator_unit,
                    denominator, denominator_unit, numerator_unit_concept_id, denominator_unit_concept_id) %>%
    dplyr::summarise(
      number_concepts = n_distinct(drug_concept_id),
      number_ingredients = n_distinct(ingredient_concept_id),
      .groups = "drop"
    ) %>%
    dplyr::mutate(pattern_id = row_number()) %>%
    relocate(pattern_id) %>% collect()

  # Here add logic valid column of patterns
  patternfile <- patternfile %>%
    dplyr::mutate(
      valid =
        dplyr::if_else((!is.na(amount) & grepl("gram|curie|international unit|mole|liter|milliequivalent", amount_unit) & is.na(denominator_unit) & is.na(numerator_unit)) |
                          (!is.na(numerator) & grepl("gram|curie|international unit|mole|liter|milliequivalent", numerator_unit) &
                             grepl("gram|hour|liter|Actuation", denominator_unit)), TRUE, FALSE))

  #validpattern <- subset(patternfile, valid==TRUE) %>% arrange(amount, denominator)

  return(patternfile)
}

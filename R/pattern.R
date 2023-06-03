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

#' add pattern info to a table containing drug_strength information
#'
#' @param drugList Table in the cdm that has contain drug_concept_id
#' @param cdm cdm_reference
#' @param ingredientConceptId ingredientConceptId
#'
#' @return It adds pattern_id and unit to the current table
#' @export
#'
#' @examples
addPattern <- function(drugList, cdm, ingredientConceptId) {
  # initial checks
  checkInputs(
    drugList = drugList, cdm = cdm, ingredientConceptId = ingredientConceptId
  )

  # select only pattern_id and unit
  drugList <- drugList %>%
    dplyr::left_join(
      drugList %>%
        dplyr::select("drug_concept_id") %>%
        dplyr::distinct() %>%
        addPatternInternal(cdm, ingredientConceptId) %>%
        dplyr::select("drug_concept_id", "pattern_id", "unit"),
      by = "drug_concept_id"
    ) %>%
    CDMConnector::computeQuery()

  return(drugList)
}

#' @noRd
addPatternInternal <- function(drugList, cdm, ingredientConceptId) {
  drugList %>%
    dplyr::inner_join(
      cdm[["drug_strength"]] %>%
        dplyr::filter(ingredient_concept_id == .env$ingredientConceptId) %>%
        dplyr::mutate(
          amount_numeric = dplyr::if_else(!is.na(.data$amount_value), 1, 0),
          numerator_numeric = dplyr::if_else(
            !is.na(.data$numerator_value), 1, 0
          ),
          denominator_numeric = dplyr::if_else(
            !is.na(.data$denominator_value), 1, 0
          )
        ) %>%
        dplyr::inner_join(
          patternfile,
          by = c(
            "amount_numeric", "amount_unit_concept_id", "numerator_numeric",
            "numerator_unit_concept_id", "denominator_numeric",
            "denominator_unit_concept_id"
          ), copy = TRUE, na_matches = "na"
        ) %>%
        dplyr::select(
          "drug_concept_id", "amount_value", "numerator_value",
          "denominator_value", "pattern_id", "unit", "amount_unit_concept_id",
          "numerator_unit_concept_id", "denominator_unit_concept_id"
        ),
      by = "drug_concept_id"
    )
}

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
patternTable <- function(cdm, counts = TRUE) {
  # Initial chekc on inputs
  checkInputs(cdm = cdm)

  # create patterns
  x <- cdm[["drug_strength"]] %>%
    dplyr::left_join(
      cdm[["concept"]] %>%
        dplyr::select(
          "amount_unit_concept_id" = "concept_id",
          "amount_unit" = "concept_name"
        ),
      by = "amount_unit_concept_id"
    ) %>%
    dplyr::left_join(
      cdm[["concept"]] %>%
        dplyr::select(
          "numerator_unit_concept_id" = "concept_id",
          "numerator_unit" = "concept_name"
        ),
      by = "numerator_unit_concept_id"
    ) %>%
    dplyr::left_join(
      cdm[["concept"]] %>%
        dplyr::select(
          "denominator_unit_concept_id" = "concept_id",
          "denominator_unit" = "concept_name"
        ),
      by = "denominator_unit_concept_id"
    ) %>%
    dplyr::mutate(
      amount_numeric = ifelse(is.na(.data$amount_value), 0, 1),
      numerator_numeric = ifelse(is.na(.data$numerator_value), 0, 1),
      denominator_numeric = ifelse(is.na(.data$denominator_value), 0, 1)
    ) %>%
    dplyr::select(
      "drug_concept_id", "ingredient_concept_id", "amount_numeric",
      "amount_unit", "amount_unit_concept_id", "numerator_numeric",
      "numerator_unit", "numerator_unit_concept_id", "denominator_numeric",
      "denominator_unit", "denominator_unit_concept_id"
    ) %>%
    CDMConnector::computeQuery()

  # get pattern
  pattern <- x %>%
    dplyr::select(-"drug_concept_id", -"ingredient_concept_id") %>%
    dplyr::distinct() %>%
    dplyr::collect()

  # get counts
  if (counts) {
    patternCounts <- x %>%
      dplyr::group_by(
        .data$amount_numeric, .data$amount_unit_concept_id,
        .data$numerator_numeric, .data$numerator_unit_concept_id,
        .data$denominator_numeric, .data$denominator_unit_concept_id
      ) %>%
      dplyr::summarise(
        number_concepts = dplyr::n_distinct(.data$drug_concept_id),
        number_ingredients = dplyr::n_distinct(.data$ingredient_concept_id),
        .groups = "drop"
      ) %>%
      dplyr::collect()
    pattern <- pattern %>%
      dplyr::left_join(
        patternCounts,
        by = c(
          "amount_numeric", "amount_unit_concept_id", "numerator_numeric",
          "numerator_unit_concept_id", "denominator_numeric",
          "denominator_unit_concept_id"
        )
      )
  }

  # present patterns
  presentPattern <- pattern %>%
    dplyr::inner_join(
      patternfile %>%
        dplyr::select(
          "amount_numeric", "amount_unit_concept_id", "numerator_numeric",
          "numerator_unit_concept_id", "denominator_numeric",
          "denominator_unit_concept_id", "pattern_id"
        ),
      by = c(
        "amount_numeric", "amount_unit_concept_id", "numerator_numeric",
        "numerator_unit_concept_id", "denominator_numeric",
        "denominator_unit_concept_id"
      )
    ) %>%
    dplyr::mutate(
      consideration = dplyr::if_else(
        is.na(pattern_id),
        "Considered in DrugUtilisation but no formula provided",
        "Pattern and formula identified"
      )
    )

  # not present / new patterns
  newPattern <- pattern %>%
    dplyr::anti_join(
      patternfile,
      by = c(
        "amount_numeric", "amount_unit_concept_id", "numerator_numeric",
        "numerator_unit_concept_id", "denominator_numeric",
        "denominator_unit_concept_id"
      )
    ) %>%
    dplyr::mutate(
      patern_id = as.character(NA),
      consideration = "This pattern is not considered in DrugUtilisation"
    )

  if (nrow(newPattern)) {
    cli::cli_alert_info(
      "This cdm contains non standard patterns please inform the mantainer of
      the package or open an issue in
      https://github.com/darwin-eu-dev/DrugUtilisation so your new patterns
      could be supported"
    )
  }

  # join supported and non supported patterns
  pattern <- dplyr::union_all(presentPattern, newPattern)

  return(pattern)
}

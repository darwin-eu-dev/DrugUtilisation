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
          amount_num = dplyr::if_else(is.numeric(.data$amount_value), 1, 0),
          numerator_num = dplyr::if_else(
            is.numeric(.data$numerator_value), 1, 0
          ),
          denominator_num = dplyr::if_else(
            is.numeric(.data$denominator_value), 1, 0
          )
        ) %>%
        dplyr::inner_join(
          patternfile,
          by = c(
            "amount_num", "amount_unit_concept_id", "numerator_num",
            "numerator_unit_concept_id", "denominator_num",
            "denominator_unit_concept_id"
          ), copy = TRUE
        ) %>%
        dplyr::select(
          "drug_concept_id", "amount_value", "numerator_value",
          "denominator_value", "patern_id", "unit"
        ),
      by = "drug_concept_id"
    )
}

#' Function to compare a given tibble of drug strength patterns with the
#' "current one" used in the DrugUtilisation package
#'
#' @param pattern_tibble 'tibble' containing 'drug_strength' pattern information
#' to compare with current data in DrugUtilisation package.
#' @param addId whether to add pattern ids from tibble in DrugUtilisation
#' to the given tibble
#'
#' @return List with three arguments:
#' "diff_patterns" Tibble of differences in patterns between the two tibbles
#' "diff_numbers" Tibble of differences in numbers of concepts and ingredients
#'  between the two tibbles
#' "pattern_tibble" If asked, also given tibble with the added column of pattern
#' numbers used in DrugUtilisation
#' @export
#'
#' @examples
comparePatternsTable <- function(pattern_tibble, addId = TRUE) {
  # Check errors in input
  checkPatternTibble(pattern_tibble)
  if(!(addId %in% c(TRUE, FALSE)) || length(addId) != 1) {cli::cli_abort("{addId} is not a boolean variable of length 1")}

  # Start code
  # First check patterns
  different_patterns <- patternfile %>%
    dplyr::mutate(tibble = "DU") %>%
    dplyr::select("amount", "amount_unit", "numerator", "numerator_unit", "denominator", "denominator_unit") %>%
    dplyr::anti_join(pattern_tibble %>%
                       dplyr::mutate(tibble = "new") %>%
                       dplyr::select("amount", "amount_unit", "numerator", "numerator_unit", "denominator", "denominator_unit"),
                     by = c("amount", "amount_unit", "numerator", "numerator_unit", "denominator", "denominator_unit"))

  # Then check number concepts and number ingredients
  different_numbers <- patternfile %>%
    dplyr::mutate(tibble = "DU") %>%
    dplyr::select("number_concepts", "number_ingredients") %>%
    dplyr::anti_join(pattern_tibble %>%
                       dplyr::mutate(tibble = "new") %>%
                       dplyr::select("number_concepts", "number_ingredients"),
                     by = c("number_concepts", "number_ingredients"))

  list_output <- list()
  list_output[["diff_patterns"]] <- different_patterns
  list_output[["diff_numbers"]] <- different_numbers

  if(addId) {
    # Add pattern ids
    pattern_tibble <- pattern_tibble %>% dplyr::left_join(
      patternfile %>%
        dplyr::select("amount", "amount_unit", "numerator", "numerator_unit", "denominator", "denominator_unit", "pattern_id"),
      by = c("amount", "amount_unit", "numerator", "numerator_unit", "denominator", "denominator_unit")
    )
    list_output[["pattern_tibble"]] <- pattern_tibble
  }

  return(list_output)
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
createPatternsTable <- function(cdm) {
  # Check errors in input
  checkCdm(cdm)

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
      amount = ifelse(is.na(.data$amount_value), NA, "numeric"),
      numerator = ifelse(is.na(.data$numerator_value), NA, "numeric"),
      denominator = ifelse(is.na(.data$denominator_value), NA, "numeric")
    ) %>%
    dplyr::select(
      "drug_concept_id", "ingredient_concept_id", "amount", "amount_unit",
      "amount_unit_concept_id", "numerator", "numerator_unit",
      "numerator_unit_concept_id", "denominator", "denominator_unit",
      "amount_value", "numerator_value", "denominator_value",
      "denominator_unit_concept_id"
    ) %>%
    computeTable(cdm)

  patternfile <- x %>%
    dplyr::group_by(
      .data$amount, .data$amount_unit, .data$amount_unit_concept_id,
      .data$numerator, .data$numerator_unit, .data$denominator,
      .data$denominator_unit, .data$numerator_unit_concept_id,
      .data$denominator_unit_concept_id
    ) %>%
    dplyr::summarise(
      number_concepts = dplyr::n_distinct(.data$drug_concept_id),
      number_ingredients = dplyr::n_distinct(.data$ingredient_concept_id),
      .groups = "drop"
    ) %>%
    dplyr::mutate(pattern_id = dplyr::row_number()) %>%
    dplyr::relocate("pattern_id") %>%
    dplyr::collect()

  # Here add logic valid column of patterns
  patternfile <- patternfile %>%
    dplyr::mutate(valid = dplyr::if_else (
      (
        !is.na(.data$amount) &
          grepl("gram|international unit|liter|milliequivalent", .data$amount_unit) &
          is.na(.data$denominator_unit) &
          is.na(.data$numerator_unit)
      ) | (
        !is.na(.data$numerator) &
          grepl("gram|international unit|liter|milliequivalent", .data$numerator_unit) &
          grepl("hour", .data$denominator_unit)
      ),
      TRUE,
      FALSE
    ))

  return(patternfile)
}

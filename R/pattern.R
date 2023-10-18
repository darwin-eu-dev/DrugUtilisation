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

#' @noRd
addUnit <- function(x, cdm = attr(x, "cdm_reference"), ingredientConceptId = NULL) {
  x %>%
    dplyr::left_join(
      drugStrengthPattern(
        cdm = cdm, ingredientConceptId = ingredientConceptId, pattern = FALSE,
        patternDetails = FALSE, unit = TRUE, route = FALSE, formula = FALSE,
        ingredient = FALSE
      ),
      by = "drug_concept_id"
    )
}

#' @noRd
addPattern <- function(x, cdm = attr(x, "cdm_reference"), ingredientConceptId = NULL) {
  x %>%
    dplyr::left_join(
      drugStrengthPattern(
        cdm = cdm, ingredientConceptId = ingredientConceptId, pattern = TRUE,
        patternDetails = FALSE, unit = FALSE, route = FALSE, formula = FALSE,
        ingredient = FALSE
      ),
      by = "drug_concept_id"
    )
}

#' @noRd
addFormula <- function(x, cdm = attr(x, "cdm_reference"), ingredientConceptId = NULL) {
  x %>%
    dplyr::left_join(
      drugStrengthPattern(
        cdm = cdm, ingredientConceptId = ingredientConceptId, pattern = FALSE,
        patternDetails = FALSE, unit = FALSE, route = FALSE, formula = TRUE,
        ingredient = FALSE
      ),
      by = "drug_concept_id"
    )
}

#' @noRd
drugStrengthPattern <- function(cdm,
                                ingredientConceptId = NULL,
                                pattern = TRUE,
                                patternDetails = TRUE,
                                unit = TRUE,
                                route = TRUE,
                                formula = TRUE,
                                ingredient = TRUE) {
  # start table
  drugStrengthRelated <- cdm[["drug_strength"]]

  # filter ingredients
  if (!is.null(ingredientConceptId)) {
    drugStrengthRelated <- drugStrengthRelated %>%
      dplyr::filter(.data$ingredient_concept_id %in% .env$ingredientConceptId)
  }

  # add route
  if (route | formula | unit) {
    drugStrengthRelated <- drugStrengthRelated %>% addRoute()
  }

  # add pattern
  if (pattern | patternDetails | unit | formula) {
    drugStrengthRelated <- drugStrengthRelated %>%
      dplyr::mutate(
        amount_numeric = dplyr::if_else(!is.na(.data$amount_value), 1, 0),
        numerator_numeric = dplyr::if_else(
          !is.na(.data$numerator_value), 1, 0
        ),
        denominator_numeric = dplyr::if_else(
          !is.na(.data$denominator_value), 1, 0
        )
      ) %>%
      dplyr::left_join(
        patterns,
        by = c(
          "amount_numeric", "amount_unit_concept_id", "numerator_numeric",
          "numerator_unit_concept_id", "denominator_numeric",
          "denominator_unit_concept_id"
        ), copy = TRUE, na_matches = "na"
      )
  }

  # add formula
  if (formula | unit) {
    drugStrengthRelated <- drugStrengthRelated %>%
      dplyr::left_join(formulas, by = c("pattern_id", "route"), copy = TRUE)
  }

  # select desired columns
  variables <- c(
    {if (pattern) "pattern_id"},
    {if (patternDetails) c(
      "amount_value", "numerator_value", "denominator_value",
      "amount_unit_concept_id", "numerator_unit_concept_id",
      "denominator_unit_concept_id", "pattern_file_id"
    )},
    {if (unit) "unit"},
    {if (route) "route"},
    {if (formula) "formula_id"},
    {if (ingredient) "ingredient_concept_id"}
  )
  drugStrengthRelated <- drugStrengthRelated %>%
    dplyr::select(dplyr::all_of(c("drug_concept_id", variables)))

  return(drugStrengthRelated)
}

#' add route column to a table containing drug_exposure information
#'
#' @param drugTable Table in the cdm that must contain drug_concept_id
#' @param cdm 'cdm' object created with CDMConnector::cdm_from_con(). It must
#' must contain 'concept_relationship' table.
#'
#' @return It adds route to the current table
#'
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
#'   addRoute()
#' }
#'
addRoute <- function(drugTable, cdm = attr(drugTable, "cdm_reference")) {
  drugTable %>%
    dplyr::left_join(
      cdm[["concept_relationship"]] %>%
        dplyr::select(c("concept_id_1", "concept_id_2", "relationship_id")) %>%
        dplyr::filter(.data$relationship_id == "RxNorm has dose form") %>%
        dplyr::select(-"relationship_id") %>%
        dplyr::rename(
          "drug_concept_id" = "concept_id_1",
          "dose_form_concept_id" = "concept_id_2"
        ),
      by = "drug_concept_id"
    ) %>%
    dplyr::left_join(routes, by = "dose_form_concept_id", copy = TRUE) %>%
    dplyr::select(-"dose_form_concept_id")
}

#' Function to create a tibble with the patterns from current drug strength table
#'
#' @param cdm 'cdm' object created with CDMConnector::cdm_from_con(). It must
#' must contain 'drug_strength' and 'concept' tables.
#'
#' @return The function creates a tibble with the different patterns found in
#' the table, plus a column of potentially valid and invalid combinations.
#'
#' @export
#'
#' @examples
#' \donttest{
#' library(DrugUtilisation)
#'
#' cdm <- mockDrugUtilisation()
#'
#' patternTable(cdm)
#' }
#'
patternTable <- function(cdm) {
  # Initial chekc on inputs
  checkInputs(cdm = cdm)

  # drug strength pattern
  drugStrengthPattern <- drugStrengthPattern(
    cdm = cdm, route = FALSE, unit = FALSE
  ) %>%
    CDMConnector::computeQuery()

  # counts concepts and ingredients
  pattern <- drugStrengthPattern %>%
    dplyr::group_by(
      .data$pattern_id, .data$formula_id, .data$pattern_file_id
    ) %>%
    dplyr::summarise(
      number_concepts = as.numeric(dplyr::n_distinct(.data$drug_concept_id)),
      number_ingredients = as.numeric(dplyr::n_distinct(.data$ingredient_concept_id)),
      .groups = "drop"
    ) %>%
    dplyr::collect()

  # create patterns
  pattern <- pattern %>%
    dplyr::full_join(
      cdm[["drug_exposure"]] %>%
        dplyr::left_join(drugStrengthPattern, by = "drug_concept_id") %>%
        dplyr::group_by(
          .data$pattern_id, .data$formula_id, .data$pattern_file_id
        ) %>%
        dplyr::summarise(
          number_records = as.numeric(dplyr::n()), .groups = "drop"
        ) %>%
        dplyr::collect(),
      by = c("pattern_id", "formula_id", "pattern_file_id")
    ) %>%
    dplyr::mutate(number_records = dplyr::if_else(
      is.na(.data$number_records), 0, .data$number_records
    )) %>%
    dplyr::arrange(.data$pattern_file_id) %>%
    dplyr::left_join(patterns, by = c("pattern_id", "pattern_file_id"))

  # not present / new patterns
  newPattern <- pattern %>%
    dplyr::filter(is.na(.data$pattern_file_id)) %>%
    dplyr::mutate(validity = dplyr::if_else(
      is.na(.data$amount_numeric) & is.na(.data$amount_unit_concept_id) &
        is.na(.data$numerator_numeric) &
        is.na(.data$numerator_unit_concept_id) &
        is.na(.data$denominator_numeric) &
        is.na(.data$denominator_unit_concept_id),
      "no pattern", "new pattern"
    ))

  # new patterns
  newNewPattern <- newPattern %>%
    dplyr::filter(.data$validity == "new pattern")
  if (nrow(newNewPattern) > 0) {
    cli::cli_alert_info(
      "This cdm contains non standard patterns please inform the mantainer of
      the package or open an issue in
      https://github.com/darwin-eu/DrugUtilisation so your new patterns could
      be supported"
    )
  }

  # join supported and non supported patterns
  pattern <- pattern %>%
    dplyr::filter(!is.na(.data$pattern_file_id)) %>%
    dplyr::mutate(validity = dplyr::if_else(
      is.na(.data$pattern_id),
      "pattern with no formula",
      dplyr::if_else(
        is.na(.data$formula_id), "pattern wrong route", "pattern with formula"
      )
    )) %>%
    dplyr::union_all(newPattern) %>%
    dplyr::relocate(dplyr::starts_with("number")) %>%
    dplyr::relocate(
      c("pattern_file_id", "pattern_id", "formula_id", "validity")
    )

  return(pattern)
}

#' Function to stratify a conceptSet by unit
#'
#' @param conceptSet List of concept sets
#' @param cdm cdm reference
#' @param ingredientConceptId ConceptId that refers to an ingredient
#'
#' @return The conceptSet stratified by unit
#'
#' @export
#'
#' @examples
#' \donttest{
#' library(DrugUtilisation)
#' library(CodelistGenerator)
#'
#' cdm <- mockDrugUtilisation()
#'
#' codelist <- getDrugIngredientCodes(cdm, "acetaminophen")
#'
#' codelistStratified <- stratifyByUnit(codelist, cdm, 1125315)
#'
#' codelistStratified
#' }
#'
stratifyByUnit <- function(conceptSet, cdm , ingredientConceptId) {
  # check initial inputs
  checkInputs(
    conceptSet = conceptSet, cdm = cdm,
    ingredientConceptId = ingredientConceptId
  )

  # add the conceptSet to a tibble
  x <- lapply(conceptSet, function(x){
    x <- dplyr::tibble(drug_concept_id = x) %>%
      dplyr::inner_join(
        drugStrengthPattern(
          cdm = cdm, ingredientConceptId = ingredientConceptId, pattern = FALSE,
          patternDetails = FALSE, unit = TRUE, route = FALSE, formula = FALSE,
          ingredient = FALSE
        ) %>%
          dplyr::collect(),
        by = c("drug_concept_id")
      ) %>%
      dplyr::filter(!is.na(.data$unit))
    split(x, x$unit) %>%
      lapply(dplyr::pull, var = "drug_concept_id")
  })

  # rename
  result <- unlist(
    lapply(names(x), function(nam) {
      names(x[[nam]]) <- paste(nam, names(x[[nam]]), sep = " unit: ")
      x[[nam]]
    }),
    recursive = FALSE
  )

  return(result)
}

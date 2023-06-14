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
#' cdm$drug_exposure %>%
#'   addPattern(cdm, 1125315)
#'
#' cdm$concept %>%
#'   filter(domain_id == "Drug") %>%
#'   select(drug_concept_id = concept_id) %>%
#'   addPattern(cdm, 1125315)
#' }
#'
addPattern <- function(drugList, cdm, ingredientConceptId) {
  # initial checks
  checkInputs(
    drugList = drugList, cdm = cdm, ingredientConceptId = ingredientConceptId
  )

  # insert as temporal if it is a local tbl
  if (!("tbl_sql" %in% class(drugList))) {
    name <- CDMConnector::uniqueTableName()
    DBI::dbWriteTable(attr(cdm, "dbcon"), name, as.data.frame(drugList))
    drugList <- dplyr::tbl(attr(cdm, "dbcon"), name)
  }

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
        dplyr::filter(
          .data$ingredient_concept_id == .env$ingredientConceptId
        ) %>%
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
#' @param recordCount Whether number of records per pattern should be computed
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
patternTable <- function(cdm, recordCount = FALSE) {
  # Initial chekc on inputs
  checkInputs(cdm = cdm, recordCount = recordCount)

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

  pattern <- x %>%
    dplyr::group_by(
      .data$amount_numeric, .data$amount_unit, .data$amount_unit_concept_id,
      .data$numerator_numeric, .data$numerator_unit,
      .data$numerator_unit_concept_id, .data$denominator_numeric,
      .data$denominator_unit, .data$denominator_unit_concept_id
    ) %>%
    dplyr::summarise(
      number_concepts = dplyr::n_distinct(.data$drug_concept_id),
      number_ingredients = dplyr::n_distinct(.data$ingredient_concept_id),
      .groups = "drop"
    ) %>%
    dplyr::collect()

  # get recordCount
  if (recordCount) {
    recordCounts <- cdm[["drug_exposure"]] %>%
      dplyr::inner_join(x, by = "drug_concept_id") %>%
      dplyr::group_by(
        .data$amount_numeric, .data$amount_unit_concept_id,
        .data$numerator_numeric, .data$numerator_unit_concept_id,
        .data$denominator_numeric, .data$denominator_unit_concept_id
      ) %>%
      dplyr::summarise(number_records = dplyr::n(), .groups = "drop") %>%
      dplyr::collect()
    pattern <- pattern %>%
      dplyr::left_join(
        recordCounts,
        by = c(
          "amount_numeric", "amount_unit_concept_id", "numerator_numeric",
          "numerator_unit_concept_id", "denominator_numeric",
          "denominator_unit_concept_id"
        )
      ) %>%
      dplyr::mutate(number_records = dplyr::if_else(
        is.na(.data$number_records), 0, .data$number_records
      ))
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
      validity = dplyr::if_else(
        is.na(.data$pattern_id), "no formula provided", "valid"
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
      pattern_id = as.numeric(NA),
      validity = "new pattern, inform please"
    )

  if (nrow(newPattern) > 0) {
    cli::cli_alert_info(
      "This cdm contains non standard patterns please inform the mantainer of
      the package or open an issue in
      https://github.com/darwin-eu/DrugUtilisation so your new patterns could
      be supported"
    )
  }

  # join supported and non supported patterns
  pattern <- dplyr::union_all(presentPattern, newPattern) %>%
    dplyr::relocate(dplyr::starts_with("number")) %>%
    dplyr::relocate(c("pattern_id", "validity"))

  return(pattern)
}

#' Function to stratify a conceptSetList by unit
#'
#' @param conceptSetList List of concept sets
#' @param cdm cdm reference
#' @param ingredientConceptId ConceptId that refers to an ingredient
#'
#' @return The conceptSetList stratified by unit
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
stratifyByUnit <- function(conceptSetList, cdm, ingredientConceptId) {
  # check initial inputs
  checkInputs(
    conceptSetList = conceptSetList, cdm = cdm,
    ingredientConceptId = ingredientConceptId
  )

  # add the conceptSet to a tibble
  x <- lapply(conceptSetList, function(x){
    x <- dplyr::tibble(drug_concept_id = x) %>%
      addPattern(cdm, ingredientConceptId) %>%
      dplyr::filter(!is.na(.data$unit)) %>%
      dplyr::select("drug_concept_id", "unit") %>%
      dplyr::collect()
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

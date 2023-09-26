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
addPattern <- function(drugList, cdm = attr(drugList, "cdm_reference"), ingredientConceptId) {
  # initial checks
  checkInputs(
    drugList = drugList, cdm = cdm, ingredientConceptId = ingredientConceptId
  )

  # insert as temporal if it is a local tbl
  if (!("tbl_sql" %in% class(drugList))) {
    name <- CDMConnector::uniqueTableName()
    DBI::dbWriteTable(attr(cdm, "dbcon"), name, as.data.frame(drugList), temporary = TRUE)
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

#' @noRd
addFormulaInternal <- function(drugList, cdm, ingredientConceptId) {
  drugList %>%
    addRoute(cdm) %>%
    dplyr::left_join(
      cdm[["drug_strength"]] %>%
        dplyr::filter(
          .data$ingredient_concept_id %in% .env$ingredientConceptId
        ) %>%
        dplyr::mutate(
          amount = dplyr::if_else(!is.na(.data$amount_value), "numeric", NA),
          numerator = dplyr::if_else(
            !is.na(.data$numerator_value), "numeric", NA
          ),
          denominator = dplyr::if_else(
            !is.na(.data$denominator_value), "numeric", NA
          )
        ) %>%
        dplyr::left_join(
          formulafile,
          by = c(
            "numerator",
            "numerator_unit_concept_id", "denominator",
            "denominator_unit_concept_id",
            "amount", "amount_unit_concept_id"
          ), copy = TRUE, na_matches = "na"
        ) %>%
        dplyr::select(
          "drug_concept_id", "numerator_value", "denominator_value", "amount_value",
          "numerator_unit_concept_id", "denominator_unit_concept_id", "amount_unit_concept_id",
          "unit", "formula_id", "route"
        ),
      by = c("drug_concept_id", "route")
    )
}

#' add route info to a table containing drug_exposure information
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
#'   addRoute(cdm)
#' }
#'
addRoute <- function(drugTable, cdm) {

  drugTable %>%
    dplyr::left_join(
      cdm$concept_relationship %>%
        dplyr::select(c("concept_id_1", "concept_id_2", "relationship_id")) %>%
        dplyr::group_by(.data$relationship_id) %>%
        dplyr::filter(.data$relationship_id == "RxNorm has dose form") %>%
        dplyr::ungroup() %>%
        dplyr::select(-"relationship_id") %>%
        dplyr::rename(
          "drug_concept_id" = "concept_id_1",
          "source_concept_id" = "concept_id_2"
        ),
      by = "drug_concept_id",
      copy = TRUE
    ) %>%
    dplyr::left_join(decisiontable %>%
                       dplyr::select(c("route", "source_concept_id")),
                     by = "source_concept_id",
                     copy = TRUE
    ) %>%
    dplyr::select(-"source_concept_id")

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
      number_concepts = as.numeric(dplyr::n_distinct(.data$drug_concept_id)),
      number_ingredients = as.numeric(dplyr::n_distinct(.data$ingredient_concept_id)),
      .groups = "drop"
    ) %>%
    dplyr::collect()

  # get recordCount
  if (recordCount) {
    recordCounts <- cdm[["drug_exposure"]] %>%
      dplyr::left_join(x, by = "drug_concept_id") %>%
      dplyr::group_by(
        .data$amount_numeric, .data$amount_unit_concept_id,
        .data$numerator_numeric, .data$numerator_unit_concept_id,
        .data$denominator_numeric, .data$denominator_unit_concept_id
      ) %>%
      dplyr::summarise(number_records = as.numeric(dplyr::n()), .groups = "drop") %>%
      dplyr::collect()
    pattern <- pattern %>%
      dplyr::full_join(
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
    dplyr::mutate(validity = dplyr::if_else(
      is.na(.data$pattern_id), "no formula provided", "valid"
    )) %>%
    dplyr::arrange(.data$pattern_id)

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
    ) %>%
    dplyr::mutate(validity = dplyr::if_else(
      is.na(.data$amount_numeric) & is.na(.data$amount_unit_concept_id) &
        is.na(.data$numerator_numeric) &
        is.na(.data$numerator_unit_concept_id) &
        is.na(.data$denominator_numeric) &
        is.na(.data$denominator_unit_concept_id),
      "records with no pattern", .data$validity
    ))

  # new patterns
  newNewPattern <- newPattern %>%
    dplyr::filter(.data$validity != "records with no pattern")
  if (nrow(newNewPattern) > 0) {
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
stratifyByUnit <- function(conceptSetList, cdm , ingredientConceptId) {
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

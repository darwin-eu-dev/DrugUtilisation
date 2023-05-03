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
#' @param tablePrefix The stem for the permanent tables that will
#' be created. If NULL, temporary tables will be used throughout.
#'
#'
#' @return
#' @export
#'
#' @examples
addDailyDose <- function(table,
                         cdm,
                         ingredientConceptId,
                         tablePrefix = NULL) {
  errorMessage <- checkmate::makeAssertCollection()
  # initial checks
  checkmate::assertClass(cdm, "cdm_reference", add = errorMessage)
  checkmate::assertCount(ingredientConceptId, add = errorMessage)
  checkmate::assertFALSE(c("daily_dose") %in% colnames(table), add = errorMessage)
  checkmate::assertTRUE(c("drug_strength") %in% names(cdm), add = errorMessage)
  # checks for tableprefix
  checkmate::assertCharacter(
    tablePrefix, len = 1, null.ok = TRUE, add = errorMessage
  )
  checkmate::reportAssertions(collection = errorMessage)



  if ("days_exposed" %in% colnames(table)) {
    warning("'days_exposed' will be overwritten.")
  }

  table <- table %>%
    dplyr::mutate(days_exposed = CDMConnector::datediff(
      start = "drug_exposure_start_date",
      end = "drug_exposure_end_date"
    ) + 1)

  table <- table %>%
    dplyr::left_join(
      table %>%
        dplyr::select(
          "days_exposed", "quantity", "drug_concept_id", "drug_exposure_id"
        ) %>%
        dplyr::distinct() %>%
        dplyr::inner_join(
          cdm$drug_strength,
          by = c("drug_concept_id")
        ) %>% addPattern() %>%
        dplyr::mutate(
          # Change this with all different formula values
          daily_dose = dplyr::case_when(
            is.na(.data$quantity) ~ as.numeric(NA),
            .data$quantity < 0 ~ as.numeric(NA),
            .data$pattern_id %in% c(1:6) ~
              .data$amount_value * .data$quantity / .data$days_exposed,
            .data$pattern_id == 9  &&
              (.data$denominator_value * .data$quantity / 24) > .data$days_exposed
            ~ .data$numerator_value * 24 / (.data$quantity * .data$denominator_value / 24),
            .data$pattern_id == 9  &&
              (.data$denominator_value * .data$quantity / 24) <= .data$days_exposed
            ~ .data$numerator_value * 24 ,
            .data$pattern_id %in% c(14,16) && .data$quantity < 1 &&
              (.data$numerator_value * .data$quantity) > .data$denominator_value
            ~ .data$denominator_value / .data$days_exposed,
            .data$pattern_id %in% c(14,16) && .data$quantity < 1 &&
              (.data$numerator_value * .data$quantity) <= .data$denominator_value
            ~ .data$numerator_value * .data$quantity / .data$days_exposed,
            .data$pattern_id %in% c(7,8,10:13,15,17:20,22:33) ~
              .data$numerator_value * .data$quantity / .data$days_exposed,
            .data$pattern_id == 21 ~
              .data$numerator_value * 24
          )
        ) %>%
        dplyr::mutate(daily_dose = dplyr::if_else(.data$daily_dose <= 0, NA, .data$daily_dose)) %>%
        # dplyr::mutate(ingredient_concept_id = ingredient_concept_id) %>%
        dplyr::select(
          "days_exposed", "quantity", "drug_concept_id", "drug_exposure_id",
          "daily_dose", "unit"
        ),
      by = c("days_exposed", "quantity", "drug_concept_id", "drug_exposure_id")
    )

  if(is.null(tablePrefix)){
    table <- table %>%
      CDMConnector::computeQuery()
  } else {
    table <- table %>%
      CDMConnector::computeQuery(name = paste0(tablePrefix,
                                               "_person_sample"),
                                 temporary = FALSE,
                                 schema = attr(cdm, "write_schema"),
                                 overwrite = TRUE)
  }

  return(table)
}

#' @noRd
addPattern <- function(table) {
  # Join table with pattern table in DUS, add "pattern_id" and "unit" columns
  table <- table %>%
    dplyr::mutate(amount = ifelse(is.na(.data$amount_value), NA, "numeric")) %>%
    dplyr::mutate(numerator = ifelse(is.na(.data$numerator_value), NA, "numeric")) %>%
    dplyr::mutate(denominator = ifelse(is.na(.data$denominator_value), NA, "numeric")) %>%
    dplyr::left_join(patternfile, by = c(
    "amount", "amount_unit_concept_id",
    "numerator", "numerator_unit_concept_id",
    "denominator","denominator_unit_concept_id"), copy = TRUE, na_matches = c("na"))

  # Make standardised values
  table <- table %>%
    dplyr::mutate(amount_value = ifelse(
      .data$amount_unit_concept_id == 9655,
      .data$amount_value / 1000, .data$amount_value)) %>%
    dplyr::mutate(numerator_value = ifelse(
      .data$numerator_unit_concept_id == 9655,
      .data$numerator_value / 1000, .data$numerator_value)) %>%
    dplyr::mutate(denominator_value = ifelse(
      .data$denominator_unit_concept_id == 8519,
      .data$denominator_value * 1000, .data$denominator_value)) %>%
    dplyr::mutate(numerator_value = ifelse(
      .data$numerator_unit_concept_id == 9439,
      .data$numerator_value / 1000000, .data$numerator_value)) %>%
    dplyr::compute()

  return(table)
}

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
#' @param table table to which add daily_dose
#' @param cdm cdm
#' @param ingredientConceptId ingredientConceptId for which to filter the
#' drugs of interest
#' @param tablePrefix The stem for the permanent tables that will
#' be created. If NULL, temporary tables will be used throughout.
#'
#'
#' @return table with added columns: days_exposed, daily_dose, unit
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
          cdm$drug_strength %>%
            dplyr::filter(.data$ingredient_concept_id %in% .env$ingredientConceptId),
          by = c("drug_concept_id")
        ) %>% addPattern() %>%
        dplyr::mutate(
          # Change this with all different formula values
          daily_dose = dplyr::case_when(
            is.na(.data$quantity) ~ as.numeric(NA),
            .data$quantity < 0 ~ as.numeric(NA),
            .data$pattern_id %in% c(1:5) ~
              .data$amount_value * .data$quantity / .data$days_exposed,
            .data$pattern_id %in% c(6,7)  &&
              .data$denominator_value > 24
            ~ .data$numerator_value * 24 / .data$denominator_value,
            .data$pattern_id %in% c(6,7)  &&
              .data$denominator_value <= 24
            ~ .data$numerator_value,
            .data$pattern_id %in% c(8,9)
            ~ .data$numerator_value * 24,
            .default = as.numeric(NA)
          )
        ) %>%
        dplyr::mutate(daily_dose = dplyr::if_else(.data$daily_dose <= 0, NA, .data$daily_dose)) %>%
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

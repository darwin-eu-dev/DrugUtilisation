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
#' @param verbose verbose
#'
#' @return
#' @export
#'
#' @examples
computeDailyDose <- function(table,
                             cdm = cdm,
                             verbose = FALSE) {
  # initial checks
  checkmate::assertTibble(table)
  checkmate::assertClass(cdm, "cdm_reference")
  checkmate::assertLogical(verbose)
  checkmate::assertTRUE(
    all(c(
      "person_id", "quantity", "drug_concept_id", "days_supply",
      "ingredient_concept_id"
    ) %in% colnames(table))
  )
  checkmate::assertFALSE(c("daily_dose") %in% colnames(table))
  checkmate::assertTRUE(c("drug_strength") %!in% names(cdm))
  # add daily dose column
  table <- table %>%
    dplyr::left_join(
      table %>%
        dplyr::select("person_id", "days_supply") %>%
        dplyr::inner_join(
          cdm$drug_strength,
          by = c("drug_concept_id", "ingredient_concept_id")
        ) %>%
        dplyr::mutate(
          daily_dose = .data$quantity * .data$amount_value / .data$days_supply
        ) %>%
        dplyr::select("person_id", "daily_dose", "drug_concept_id"),
      by = c("person_id", "drug_concept_id")
    ) %>%
    dplyr::compute()

  return(table)
}

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
#' @param table table
#'
#' @return table with a pattern_id and unit columns
#' @export
#'
#' @examples
addPattern <- function(table) {
  checkPatternTibble(table)
  # Join table with pattern table in DUS, add "pattern_id" and "unit" columns
  table <- table %>%
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

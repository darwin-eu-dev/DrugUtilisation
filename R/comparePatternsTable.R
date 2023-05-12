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

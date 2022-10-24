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
#' @param tableName tableName
#' @param verbose verbose
#'
#' @return
#' @export
#'
#' @examples
computeDailyDose <- function(table,
                             verbose = FALSE) {
  checkmate::assertTibble(table)
  checkmate::assertLogical(verbose)
  checkmate::assertTRUE(
    c(
      "person_id", "quantity", "drug_concept_id", "drug_exposure_start_date",
      "drug_exposure_end_date", "ingredient_concept_id"
    ) %in% colnames(table)
  )

  table <- table %>%
    dplyr::mutate(daily_dose = 50) %>%
    dplyr::compute()

  return(table)
}

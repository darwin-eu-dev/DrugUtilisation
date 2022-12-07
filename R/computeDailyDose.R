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
#' @return
#' @export
#'
#' @examples
addDailyDose <- function(table,
                         cdm,
                         ingredientConceptId) {
  errorMessage <- checkmate::makeAssertCollection()
  # initial checks
  checkmate::assertClass(cdm, "cdm_reference", add = errorMessage)
  checkmate::assertCount(ingredientConceptId, add = errorMessage)
  checkmate::assertFALSE(c("daily_dose") %in% colnames(table), add = errorMessage)
  checkmate::assertTRUE(c("drug_strength") %in% names(cdm), add = errorMessage)
  checkmate::reportAssertions(collection = errorMessage)

  table <- table %>%
    dplyr::mutate(daily_dose = 100)

  return(table)
}

#' Explain function
#'
#' @param cdm cdm
#' @param tableName tableName
#' @param ingredientConceptId ingredientConceptId
#'
#' @return
#' @export
#'
#' @examples
computeDailyDose <- function(cdm,
                             tableName,
                             ingredientConceptId) {
  errorMessage <- checkmate::makeAssertCollection()
  # initial checks
  checkmate::assertClass(cdm, "cdm_reference", add = errorMessage)
  checkmate::assertCount(ingredientConceptId, add = errorMessage)
  checkmate::assertFALSE(c("daily_dose") %in% colnames(table), add = errorMessage)
  checkmate::assertTRUE(c("drug_strength") %in% names(cdm), add = errorMessage)
  checkmate::reportAssertions(collection = errorMessage)

  cdm[[tableName]] <- addDailyDose(
    table = cdm[[tableName]],
    cdm = cdm,
    ingredientConceptId = ingredientConceptId
  )

  return(cdm)
}

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

#' This function is used to summarise the dose table over multiple cohorts.
#'
#' @param cohort Cohort with drug use variables and strata.
#' @param strata Stratification list.
#' @param window Window where to summarise the treatments.
#' @param tretmentCohortName Name of a cohort in the cdm that contains the
#' interest treatments.
#' @param tretmentConceptSet Concept set list to summarise.
#' @param combination Whether to include combination treatments.
#' @param unexposed Whether to include unexposed option.
#' @param minCellCount Below this number counts will be suppressed.
#'
#' @return A summary of the drug use stratified by cohort_name and strata_name
#'
#' @export
#'
summariseTreatment<- function(cohort,
                              strata = list(),
                              window,
                              tretmentCohortName = NULL,
                              tretmentCohortId = NULL,
                              tretmentConceptSet = NULL,
                              combination = TRUE,
                              unexposed = TRUE,
                              minCellCount = 5) {
  # initial checks
  checkmate::checkClass(cohort, "generated_cohort_set")
  checkmate::checkList(strata, types = "character")
  checkmate::checkTRUE(all(unlist(strata) %in% colnames(cohort)))
  checkmate::checkCharacter(treatmentCohortName, null.ok = TRUE)

  # combination
  if (combination) {
    cdm <- CohortConstructor::generateCombinationCohortSet(
      cdm = cdm,
      targetCohortName = tretmentCohortName,
      targetCohortId = tretmentCohortId,
      mutuallyEclusive = FALSE
    )
  }

  # unexposed
  if (unexposed) {
    cdm <- CohortConstructor::generateUnexposedCohortSet()
  }

}

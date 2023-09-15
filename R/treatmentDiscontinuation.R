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

#' Compute the treatment dicontinuation.
#'
#' @param cohort GeneratedCohortSet object.
#' @param cohortId cohort_definition_id to target.
#' @param strata List of columns or combinations of columns to stratify for.
#' @param censorOnDate ##COPY FROM CS##
#' @param folloUpDays ##COPY FROM CS##
#' @param timeGap ##COPY FROM CS##
#' @param times ##COPY FROM CS##
#' @param minCellCount ##COPY FROM CS##
#'
#' @export
#'
#' @examples
#' \donttest{
#' library(DrugUtilisation)
#'
#' cdm <- mockDrugUtilisation()
#'
#' cdm <- generateDrugUtilisationCohortSet(
#'   cdm = cdm, name = "cohort", conceptSet = list("acetaminophen" = 1214)
#' )
#'
#' treatmentDiscontinuation(cdm$cohort)
#' }
#'
treatmentDiscontinuation <- function(cohort,
                                     cohortId = NULL,
                                     strata = list(),
                                     censorOnDate = NULL,
                                     followUpDays = Inf,
                                     timeGap = c(1, 7, 30, 365),
                                     times = NULL,
                                     minCellCount = 5) {
  cdm <- attr(cohort, "cdm_reference")
  checkInputs(cohort = cohort, cdm = cdm)
  cdm[["drug_cohort"]] <- cohort
  result <- CohortSurvival::estimateSingleEventSurvival(
    cdm = cdm, targetCohortTable = "drug_cohort", targetCohortId = cohortId,
    outcomeCohortTable = "drug_cohort", outcomeCohortId = cohortId,
    outcomeDateVariable = "cohort_end_date", censorOnDate = censorOnDate,
    followUpDays = followUpDays, strata = strata, timeGap = timeGap,
    times = times, minCellCount = minCellCount
  )
  return(result)
}

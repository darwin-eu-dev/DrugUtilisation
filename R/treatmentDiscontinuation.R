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
#' @param censorOnDate f not NULL, an individual's follow up will be censored at
#' the given date
#' @param followUpDays Number of days to follow up individuals (lower bound 1,
#' upper bound Inf)
#' @param timeGap Days between time points for which to report survival
#' estimates. First day will be day zero with risk estimates provided for times
#' up to the end of follow-up, with a gap in days equivalent to timeGap.
#' @param minCellCount The minimum number of events to reported, below which
#' results will be obscured. If 0, all results will be reported.
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
                                     strata = NULL,
                                     censorOnDate = NULL,
                                     followUpDays = Inf,
                                     timeGap = 100,
                                     minCellCount = 5) {
  cdm <- attr(cohort, "cdm_reference")
  checkInputs(cohort = cohort, cdm = cdm)
  cdm[["drug_cohort"]] <- cohort
  if (is.null(cohortId)) {
    cohortId <- CDMConnector::cohort_set(cdm[["drug_cohort"]]) %>%
      dplyr::pull("cohort_definition_id")
  }
  result <- list()
  for (ci in cohortId) {
    result[[ci]] <- suppressMessages(CohortSurvival::estimateSingleEventSurvival(
      cdm = cdm, targetCohortTable = "drug_cohort", targetCohortId = ci,
      outcomeCohortTable = "drug_cohort", outcomeCohortId = ci,
      outcomeDateVariable = "cohort_end_date", censorOnDate = censorOnDate,
      followUpDays = followUpDays, strata = strata, timeGap = timeGap,
      minCellCount = minCellCount
    ))
  }
  result <- dplyr::bind_rows(result) %>%
    dplyr::mutate(
      "result_type" = "Treatment discontinuation",
      "variable" = "Prabability to be on treatment",
      "variable_level" = .data$time
    ) %>%
    dplyr::select(-c("time", "analysis_type", "outcome"))
  return(result)
}

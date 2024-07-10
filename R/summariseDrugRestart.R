# Copyright 2024 DARWIN EU (C)
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

#' Summarise the drug restart per window.
#'
#' `r lifecycle::badge("experimental")`
#'
#' @param cohort A cohort table
#' @param window A list of different windows to analyse.
#' @param restrictToFirstDiscontinuation Whether to consider only the first
#' discontinuation epidose or all of them.
#' @param indexDate Date of discontinuation, it has to be a column in cohort.
#' @param switchConceptSet A conceptSet that refers to possible alternative
#' treatments.
#' @param switchCohortTable A cohort table in the cdm that contains possible
#' alternative treatments.
#' @param switchCohortId The cohort ids to be used from switchCohortTable. If
#' NULL all cohort definition ids are used.
#'
#' @return A summarised_result object with the percentages of restart, swicth
#' and not exposed per window.
#' @export
#'
summariseDrugRestart <- function(cohort,
                                 window,
                                 restrictToFirstDiscontinuation = TRUE,
                                 indexDate = "cohort_end_date",
                                 switchConceptSet = NULL,
                                 switchCohortTable = NULL,
                                 switchCohortId = NULL) {
  # check input
  cdm <- omopgenerics::cdmReference(cohort)
  if (is.null(switchCohortId)) {
    switchCohortId <- omopgenerics::settings(cdm[[switchCohortTable]]) |>
      dplyr::pull("cohort_definition_id")
  }

  # warnings
  if (!is.null(switchConceptSet) & !is.null(switchCohortTable)) {
    cli::cli_warn(c("!" = "It is not recommended to use conceptSet and CohortTable at the same time"))
  }
  if (is.null(switchCohortTable) & !is.null(switchCohortId)) {
    cli::cli_warn(c("!" = "cohortId specification only make sense if cohortTable is populated, the argument will be ignored"))
  }

  conceptSet <- list("switch" = unique(unlist(switchConceptSet)))
  if (!is.null(switchCohortTable)) {
    nm <- omopgenerics::uniqueTableName(omopgenerics::tmpPrefix())
  }


}

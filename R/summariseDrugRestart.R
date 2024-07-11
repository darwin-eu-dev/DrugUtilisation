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
#' @param followUpDays A vector of number of days to follow up. It can be
#' multiple values.
#' @param restrictToFirstDiscontinuation Whether to consider only the first
#' discontinuation epidose or all of them.
#' @param indexDate Date of discontinuation, it has to be a column in cohort.
#' @param censorDate Date of censoring. Individuals are always censored at the
#' end of observation.
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
                                 followUpDays = Inf,
                                 restrictToFirstDiscontinuation = TRUE,
                                 indexDate = "cohort_end_date",
                                 censorDate = NULL,
                                 switchConceptSet = NULL,
                                 switchCohortTable = NULL,
                                 switchCohortId = NULL) {
  # check input
  cdm <- omopgenerics::cdmReference(cohort)
  assertClass(cohort, class = "cohort_table")
  assertNumeric(followUpDays, integerish = TRUE, min = 0)
  assertLogical(restrictToFirstDiscontinuation, length = 1)
  assertCharacter(indexDate, length = 1)
  assertCharacter(censorDate, length = 1, null = TRUE)
  if (!is.null(switchConceptSet)) {
    switchConceptSet <- omopgenerics::newCodelist(switchConceptSet)
  }
  if (!is.null(switchCohortTable)) {
    assertCharacter(switchCohortTable, length = 1)
    assertClass(cdm[[switchCohortTable]], class = "cohort_table")
    if (is.null(switchCohortId)) {
      switchCohortId <- omopgenerics::settings(cdm[[switchCohortTable]]) |>
        dplyr::pull("cohort_definition_id")
    } else {
      assertNumeric(switchCohortId, integerish = TRUE, min = 1)
      checkIds <- omopgenerics::settings(cdm[[switchCohortTable]]) |>
        dplyr::pull("cohort_definition_id")
      checkIds <- !switchCohortId %in% checkIds
      if (any(checkIds)) {
        cli::cli_warn(c("!" = "switchCohortId not present in settings of {switchCohortTable}: {switchCohortId[checkIds]}"))
        switchCohortId <- switchCohortId[!checkIds]
      }
    }
  }
  # warnings
  if (!is.null(switchConceptSet) & !is.null(switchCohortTable)) {
    cli::cli_warn(c("!" = "It is not recommended to use conceptSet and CohortTable at the same time"))
  }
  if (is.null(switchCohortTable) & !is.null(switchCohortId)) {
    cli::cli_warn(c("!" = "cohortId specification only make sense if cohortTable is populated, the argument will be ignored"))
  }
  conceptSet <- list("switch" = unique(unlist(switchConceptSet)))
  prefix <- omopgenerics::tmpPrefix()
  tempName <- omopgenerics::uniqueTableName(prefix = prefix)

  # producing switch cohorts
  if (!is.null(switchConceptSet)) {
    results <- cohort |>
      PatientProfiles::addConceptIntersectDays(
        conceptSet = conceptSet,
        indexDate = indexDate,
        censorDate = censorDate,
        window = list(c(0, Inf)),
        nameStyle = "switch_days",
        name = tempName
      )
  }

  # producing switch cohorts from cohort table
  if (!is.null(switchCohortTable)) {
    results <- cohort |>
      PatientProfiles::addCohortIntersectDays(
        targetCohortTable = switchCohortTable,
        targetCohortId = switchCohortId,
        indexDate = indexDate,
        censorDate = censorDate,
        window = list(c(0, Inf)),
        nameStyle = "switch_{cohort_name}",
        name = tempName
      ) |>
      dplyr::mutate(
        switch_days = dplyr::across(
          .cols = dplyr::starts_with("switch"),
          .fn = ~ min(.x)
        )
      )
  }
  results <-
    results |>
    dplyr::group_by(.data$cohort_definition_id, .data$subject_id) %>%
    dplyr::mutate(
      restart_days = dplyr::lead(.data$cohort_start_date),
      restart_days = !!CDMConnector::datediff(indexDate, "restart_days")
    ) |>
    dplyr::ungroup() |>
    dplyr::compute(name = tempName, temporary = FALSE)

  omopgenerics::dropTable(cdm = cdm, name = dplyr::starts_with(prefix))
}

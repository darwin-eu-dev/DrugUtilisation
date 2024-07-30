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
#' @param cohort A cohort table.
#' @param switchCohortTable A cohort table in the cdm that contains possible
#' alternative treatments.
#' @param switchCohortId The cohort ids to be used from switchCohortTable. If
#' NULL all cohort definition ids are used.
#' @param strata List with column names or vectors of column names groups to
#' stratify results by.
#' @param followUpDays A vector of number of days to follow up. It can be
#' multiple values.
#' @param restrictToFirstDiscontinuation Whether to consider only the first
#' discontinuation episode or all of them.
#' @param censorDate Date of censoring. Individuals are always censored at the
#' end of observation.
#'
#' @return A summarised_result object with the percentages of restart, switch
#' and not exposed per window.
#'
#' @export
#'
#' @examples
#' \donttest{
#' library(DrugUtilisation)
#'
#' cdm <- mockDrugUtilisation()
#'
#' conceptlist <- list("a" = 1125360, "b" = c(1503297, 1503327))
#' cdm <- generateDrugUtilisationCohortSet(
#'   cdm = cdm,
#'   name = "switch_cohort",
#'   conceptSet = conceptlist
#' )
#'
#' result <- cdm$cohort1 |>
#'   summariseDrugRestart(switchCohortTable = "switch_cohort")
#'
#' tableDrugRestart(result)
#'
#' CDMConnector::cdmDisconnect(cdm = cdm)
#' }
summariseDrugRestart <- function(cohort,
                                 switchCohortTable,
                                 switchCohortId = NULL,
                                 strata = list(),
                                 followUpDays = Inf,
                                 censorDate = NULL,
                                 restrictToFirstDiscontinuation = TRUE) {
  # check input
  cdm <- omopgenerics::cdmReference(cohort)
  assertClass(cohort, class = "cohort_table")
  assertLogical(restrictToFirstDiscontinuation, length = 1)
  assertCharacter(censorDate, length = 1, null = TRUE)
  assertNumeric(followUpDays, integerish = TRUE, min = 0)
  # if (!is.null(switchConceptSet)) {
  #   switchConceptSet <- omopgenerics::newCodelist(switchConceptSet)
  # }
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
      if (length(switchCohortId) == 0) {
        cli::cli_abort(c("x" = "Please provide valid ids, or leave `switchCohortId` as NULL to consider all cohorts."))
      }
    }
  }
  # if (!is.null(switchConceptSet) & !is.null(switchCohortTable)) {
  #   cli::cli_abort(c("x" = "It is not allowed to use conceptSet and CohortTable at the same time"))
  # }
  # if (is.null(switchConceptSet) & is.null(switchCohortTable)) {
  #   cli::cli_abort(c("x" = "One of `switchConceptSet` or `switchCohortTable` arguments must be populated."))
  # }
  # warnings
  # if (is.null(switchCohortTable) & !is.null(switchCohortId)) {
  #   cli::cli_warn(c("!" = "cohortId specification only make sense if cohortTable is populated, the argument will be ignored"))
  # }

  # conceptSet <- list("switch" = unique(unlist(switchConceptSet)))
  prefix <- omopgenerics::tmpPrefix()
  tempName <- omopgenerics::uniqueTableName(prefix = prefix)
  workingCohort <- cohort |> dplyr::compute(name = tempName, temporary = FALSE)

  # remove cohort entries ending before censor date and throw warning saying how many
  if (!is.null(censorDate)) {
    censorDateSym <- rlang::sym(censorDate)
    nBefore <- workingCohort |>
      dplyr::ungroup() |>
      dplyr::tally() |>
      dplyr::pull("n")
    workingCohort <- workingCohort |>
      dplyr::filter(!!censorDateSym > .data$cohort_end_date) |>
      dplyr::compute(name = tempName, temporary = FALSE)
    nAfter <- workingCohort |>
      dplyr::ungroup() |>
      dplyr::tally() |>
      dplyr::pull("n")
    if (nBefore != nAfter) {
      cli::cli_warn(c(
        "!" = "{nBefore-nAfter} record{?s} {?was/were} dropped because {?it/their} ended before {censorDate}."
      ))
    }
  }

  # get first switch - concept
  # if (!is.null(switchConceptSet)) {
  #   results <- workingCohort |>
  #     PatientProfiles::addConceptIntersectDays(
  #       conceptSet = conceptSet,
  #       indexDate = "cohort_end_date",
  #       censorDate = censorDate,
  #       window = list(c(0, Inf)),
  #       nameStyle = "switch_days",
  #       name = tempName
  #     )
  # }

  # get first switch - cohort
  cols <- c(
    "cohort_definition_id", "subject_id", "cohort_start_date",
    "cohort_end_date", censorDate, unique(unlist(strata))
  )
  results <- workingCohort |>
    dplyr::select(dplyr::all_of(cols)) |>
    PatientProfiles::addCohortIntersectDays(
      targetCohortTable = switchCohortTable,
      targetCohortId = switchCohortId,
      indexDate = "cohort_end_date",
      censorDate = censorDate,
      window = list(c(0, Inf)),
      nameStyle = "cohort_switch_{cohort_name}",
      name = tempName
    ) |>
    tidyr::pivot_longer(
      cols = dplyr::starts_with("cohort_switch_"),
      values_to = "switch_days",
      names_to = "cohort_switch_name"
    ) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(cols))) |>
    dplyr::summarise(
      switch_days = min(.data$switch_days, na.rm = TRUE), .groups = "drop"
    ) |>
    dplyr::compute(name = tempName, temporary = FALSE)

  results <- results |>
    dplyr::group_by(.data$cohort_definition_id, .data$subject_id) |>
    dplyr::arrange(.data$cohort_start_date) |>
    dplyr::mutate(internal_order_id = dplyr::row_number()) |>
    dplyr::ungroup() |>
    dplyr::compute(name = tempName, temporary = FALSE)

  if (!is.null(censorDate)) {
    nextExposure <- results |>
      dplyr::filter(.data$cohort_start_date <= !!censorDateSym)
  } else {
    nextExposure <- results
  }

  variables <- tolower(glue::glue("{followUpDays} days"))
  results <- results |>
    PatientProfiles::addCohortName() |>
    dplyr::left_join(
      nextExposure |>
        dplyr::mutate(internal_order_id = .data$internal_order_id - 1) |>
        dplyr::select(
          "cohort_definition_id", "subject_id", "internal_order_id",
          "restart_days" = "cohort_start_date",
        ),
      by = c("cohort_definition_id", "subject_id", "internal_order_id")
    ) %>%
    dplyr::mutate(
      restart_days = !!CDMConnector::datediff("cohort_end_date", "restart_days"),
      !!!windowEvents(followUpDays, variables)
    ) |>
    dplyr::compute(name = tempName, temporary = FALSE)

  # restrict to first
  if (restrictToFirstDiscontinuation) {
    results <- results |>
      dplyr::filter(
        .data$cohort_start_date == min(.data$cohort_start_date, na.rm = TRUE),
        .by = c("cohort_definition_id", "subject_id")
      ) |>
      dplyr::compute(name = tempName, temporary = FALSE)
  }

  results <- results |>
    dplyr::collect() |>
    PatientProfiles::summariseResult(
      group = list("cohort_name"),
      includeOverallGroup = FALSE,
      strata = strata,
      includeOverallStrata = TRUE,
      variables = variables,
      estimates = c("count", "percentage"),
      counts = TRUE
    ) |>
    suppressMessages() |>
    dplyr::select(-"cdm_name") |>
    PatientProfiles::addCdmName(cdm = cdm)

  # add empty categories with 0:
  zeroResults <- results |>
    dplyr::filter(.data$variable_name == "number records") |>
    dplyr::distinct(
      .data$result_id, .data$cdm_name, .data$group_name, .data$group_level,
      .data$strata_name, .data$strata_level, .data$additional_name, .data$additional_level
    ) |>
    dplyr::cross_join(dplyr::tibble(variable_name = variables)) |>
    dplyr::cross_join(
      dplyr::tibble(
        variable_level = rep(c("not treated", "restart", "switch", "restart and switch"), 2),
        estimate_name = c(rep("count", 4), rep("percentage", 4)),
        estimate_type = c(rep("integer", 4), rep("percentage", 4)),
        estimate_value = "0"
      )
    ) |>
    dplyr::anti_join(
      results,
      by = c(
        "result_id", "cdm_name", "group_name", "group_level", "strata_name",
        "strata_level", "variable_name", "variable_level",
        "additional_name", "additional_level"
      )
    )

  # summarised result
  if (is.null(censorDate)) {
    censorDate <- NA
  }
  results <- dplyr::bind_rows(results, zeroResults) |>
    dplyr::filter(!.data$variable_name %in% c("number subjects", "number records")) |>
    dplyr::mutate(
      variable_name = factor(
        .data$variable_name,
        levels = variables
      ),
      variable_level = factor(
        .data$variable_level,
        levels = c("restart", "switch", "restart and switch", "not treated")
      ),
      group_level = factor(.data$group_level),
      strata_name = factor(.data$strata_name),
      strata_level = factor(.data$strata_level),
      estimate_name = factor(.data$estimate_name)
    ) |>
    dplyr::arrange(
      .data$group_level, .data$strata_name, .data$strata_level, .data$variable_name,
      .data$variable_level, .data$estimate_name
    ) |>
    dplyr::mutate(
      dplyr::across(.cols = c(!"result_id"), ~ as.character(.x)),
      variable_name = dplyr::if_else(
        .data$variable_name == "inf days",
        "End of observation", .data$variable_name
      )
    ) |>
    omopgenerics::newSummarisedResult(
      settings = dplyr::tibble(
        result_id = unique(results$result_id),
        result_type = "summarise_drug_restart",
        restrict_to_first_discontinuation = restrictToFirstDiscontinuation,
        censor_date = censorDate
      )
    )

  omopgenerics::dropTable(cdm = cdm, name = dplyr::starts_with(prefix))

  return(results)
}

windowEvents <- function(followUpDays, variables) {
  followUpDaysNum <- followUpDays
  followUpDaysNum[is.infinite(followUpDays)] <- 99999999999999
  glue::glue(
    "dplyr::case_when(",
    ".data$restart_days <= {followUpDaysNum} & .data$switch_days <= {followUpDaysNum} ~ 'restart and switch', ",
    ".data$restart_days <= {followUpDaysNum} ~ 'restart', ",
    ".data$switch_days <= {followUpDaysNum} ~ 'switch', ",
    ".default = 'not treated')"
  ) |>
    rlang::parse_exprs() |>
    rlang::set_names(variables)
}

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

#' Summarise proportion Of patients covered
#'
#' @description Gives the proportion of patients still in observation who are
#' in the cohort on any given day following their first cohort entry. This is
#' known as the “proportion of patients covered” (PPC) method for assessing
#' treatment persistence.
#'
#' @param cohort A cohort table
#' @param cohortId Cohort definition ID of interest. If NUll, results for all
#' cohorts will be returned.
#' @param strata List of variables to stratify by.
#' @param followUpDays Number of days to follow up individuals for. If NULL the
#' maximum amount of days from an individuals first cohort start date to their
#' last cohort end date will be used
#'
#' @return A summarised result
#' @export
#'
summariseProportionOfPatientsCovered <- function(cohort,
                                                 cohortId = NULL,
                                                 strata = list(),
                                                 followUpDays = NULL) {
  checkmate::assert_integerish(followUpDays,
    lower = 1,
    len = 1,
    null.ok = TRUE
  )

  checkmate::checkList(strata, types = "character")
  if (isFALSE(all(unlist(strata) %in% colnames(cohort)))) {
    cli::cli_abort("strata not found in cohort table")
  }

  cohortIds <- omopgenerics::settings(cohort) |>
    dplyr::pull("cohort_definition_id")
  if (!is.null(cohortId)) {
    cohortIds <- cohortIds[cohortIds %in% cohortId]
  }
  if (length(cohortIds) == 0) {
    cli::cli_abort("Cohort ID not found")
  }

  cdm <- omopgenerics::cdmReference(cohort)

  analysisSettings <- dplyr::tibble(
    "result_id" = 1L,
    "result_type" = "summarise_proportion_of_patients_covered",
    "package_name" = "DrugUtilisation",
    "package_version" = as.character(utils::packageVersion("DrugUtilisation"))
  )
  if (nrow(cohort |> utils::head(1) |> dplyr::collect()) == 0) {
    cli::cli_warn("No records found in cohort table")
    return(omopgenerics::emptySummarisedResult(settings = analysisSettings))
  }

  if (is.null(followUpDays)) {
    cli::cli_inform("Setting followUpDays to maximum time from first cohort entry to last cohort exit per cohort")
    maxDays <- cohort %>%
      dplyr::mutate(days_in_cohort = as.integer(
        !!CDMConnector::datediff(
          start = "cohort_start_date",
          end = "cohort_end_date",
          interval = "day"
        )
      )) %>%
      dplyr::group_by(.data$cohort_definition_id, .data$subject_id) |>
      dplyr::summarise(days = sum(.data$days_in_cohort, na.rm = TRUE)) |>
      dplyr::group_by(.data$cohort_definition_id) |>
      dplyr::summarise(max_days = as.integer(max(.data$days, na.rm = TRUE))) |>
      dplyr::collect()
  } else {
    maxDays <- omopgenerics::settings(cohort) |>
      dplyr::mutate(max_days = .env$followUpDays) |>
      dplyr::select("cohort_definition_id", "max_days")
  }

  ppc <- list()
  for (j in seq_along(cohortIds)) {
    workingCohortId <- cohortIds[j]
    workingMaxDays <- maxDays |>
      dplyr::filter(.data$cohort_definition_id == .env$workingCohortId) |>
      dplyr::pull()

    ppc[[j]] <- getPPC(cohort,
      cohortId = workingCohortId,
      strata = strata,
      days = workingMaxDays
    )
  }

  ppc <- dplyr::bind_rows(ppc)

  if (nrow(ppc) == 0) {
    cli::cli_inform(c(
      "i" =
        "No results found for any cohort, returning an empty summarised result"
    ))
    return(omopgenerics::newSummarisedResult(omopgenerics::emptySummarisedResult(),
      settings = analysisSettings
    ))
  }

  ppc <- ppc |>
    dplyr::mutate(ppc = round((.data$outcome_count / .data$denominator_count) * 100, 2)) |>
    tidyr::pivot_longer(
      c(
        "outcome_count",
        "denominator_count",
        "ppc"
      ),
      names_to = "estimate_name",
      values_to = "estimate_value"
    ) |>
    dplyr::mutate(estimate_type = dplyr::if_else(.data$estimate_name == "ppc",
      "percentage", "integer"
    ))


  ppc <- ppc |>
    dplyr::mutate(
      result_id = 1L,
      cdm_name = omopgenerics::cdmName(cdm),
      group_name = "cohort",
      group_level = .data$cohort_name,
      strata_name = .data$strata_name,
      strata_level = .data$strata_level,
      variable_name = "overall",
      variable_level = "overall",
      estimate_name = .data$estimate_name,
      estimate_type = .data$estimate_type,
      estimate_value = as.character(.data$estimate_value),
      additional_name = "time",
      additional_level = as.character(.data$time)
    ) |>
    dplyr::select(omopgenerics::resultColumns())


  ppc <- omopgenerics::newSummarisedResult(ppc,
    settings = analysisSettings
  )

  ppc
}

getPPC <- function(cohort, cohortId, strata, days) {
  result <- list()

  workingCohortName <- omopgenerics::settings(cohort) |>
    dplyr::filter(.data$cohort_definition_id == .env$cohortId) |>
    dplyr::pull("cohort_name")
  cli::cli_inform(glue::glue("Getting PPC for cohort {workingCohortName}"))

  cli::cli_inform("Collecting cohort into memory")
  workingCohort <- cohort |>
    dplyr::filter(.data$cohort_definition_id == .env$cohortId) |>
    PatientProfiles::addFutureObservationQuery(
      futureObservationName = "observation_end_date",
      futureObservationType = "date"
    ) |>
    dplyr::collect()

  if (nrow(workingCohort) == 0) {
    cli::cli_inform(c("i" = "No records found for {workingCohortName}"))
    return(NULL)
  }

  workingCohort <- workingCohort |>
    dplyr::group_by(.data$subject_id) |>
    dplyr::mutate(min_cohort_start_date = min(.data$cohort_start_date,
      na.rm = TRUE
    )) |>
    dplyr::ungroup()

  cli::cli_inform(glue::glue("Geting PPC over {days} days following first cohort entry"))
  result[["overall_time_0"]] <-
    getOverallStartingCount(workingCohort) |>
    dplyr::mutate(time = 0)
  for (j in seq_along(strata)) {
    result[[paste0("strata_", j, "_time_0")]] <-
      getStratifiedStartingCount(
        workingCohort,
        strata[[j]]
      ) |>
      dplyr::mutate(time = 0)
  }

  cli::cli_progress_bar(
    .envir = parent.frame(),
    total = days,
    format = " -- getting PPC for {cli::pb_bar} {cli::pb_current} of {cli::pb_total} days"
  )
  for (i in seq_along(1:days)) {
    cli::cli_progress_update(.envir = parent.frame())
    c <- workingCohort |>
      dplyr::mutate(working_date = clock::add_days(.data$min_cohort_start_date, i)) |>
      dplyr::mutate(
        in_cohort = dplyr::if_else(
          .data$cohort_start_date <= .data$working_date &
            .data$cohort_end_date >= .data$working_date,
          1,
          0
        ),
        in_observation = dplyr::if_else(.data$observation_end_date >= .data$working_date, 1, 0)
      )

    # overall
    result[[paste0("overall_time_", i)]] <-
      getOverallCounts(workingCohort = c) |>
      dplyr::mutate(time = i)

    # stratified results
    for (j in seq_along(strata)) {
      result[[paste0("strata_", j, "_", i)]] <-
        getStratifiedCounts(c, strata[[j]]) |>
        dplyr::mutate(time = i)
    }
  }
  cli::cli_progress_done(.envir = parent.frame())

  result <- dplyr::bind_rows(result) %>%
    dplyr::mutate(denominator_count = dplyr::if_else(is.na(.data$denominator_count),
      0, .data$denominator_count
    )) |>
    dplyr::mutate(outcome_count = dplyr::if_else(is.na(.data$outcome_count),
      0, .data$outcome_count
    )) |>
    dplyr::mutate(cohort_name = .env$workingCohortName)

  result
}

getOverallStartingCount <- function(workingCohort) {
  startN <- workingCohort |>
    dplyr::select("subject_id") |>
    dplyr::distinct() |>
    dplyr::tally() |>
    dplyr::pull("n")
  dplyr::tibble(
    denominator_count = .env$startN,
    outcome_count = .env$startN,
    strata_name = "overall",
    strata_level = "overall"
  )
}

getOverallCounts <- function(workingCohort) {
  dplyr::tibble(
    # people still in observation
    denominator_count = workingCohort |>
      dplyr::filter(.data$in_observation == 1) |>
      dplyr::summarise(n = dplyr::n_distinct(.data$subject_id)) |>
      dplyr::pull("n"),
    # people in cohort on date
    outcome_count = workingCohort |>
      dplyr::filter(.data$in_cohort == 1) |>
      dplyr::summarise(n = dplyr::n_distinct(.data$subject_id)) |>
      dplyr::pull("n"),
    strata_name = "overall",
    strata_level = "overall"
  )
}

getStratifiedStartingCount <- function(workingCohort, workingStrata) {
  workingCohort %>%
    dplyr::select(c("subject_id", dplyr::all_of(workingStrata))) |>
    dplyr::distinct() |>
    dplyr::group_by(dplyr::pick(.env$workingStrata)) %>%
    dplyr::summarise(
      denominator_count = dplyr::n(),
      outcome_count = dplyr::n()
    ) %>%
    dplyr::ungroup() %>%
    tidyr::unite("strata_level",
      c(dplyr::all_of(.env$workingStrata)),
      remove = FALSE,
      sep = " &&& "
    ) %>%
    dplyr::mutate(strata_name = !!paste0(workingStrata, collapse = " &&& ")) %>%
    dplyr::relocate("strata_level", .after = "strata_name") %>%
    dplyr::select(!dplyr::any_of(workingStrata))
}

getStratifiedCounts <- function(workingCohort, workingStrata) {
  workingCohort |>
    dplyr::group_by(dplyr::pick(.env$workingStrata)) %>%
    # so that we get empty result if no records
    dplyr::summarise(placeholder = dplyr::n()) |>
    dplyr::select(!"placeholder") |>
    dplyr::full_join(
      workingCohort |>
        dplyr::filter(.data$in_observation == 1) |>
        dplyr::group_by(dplyr::pick(.env$workingStrata)) %>%
        dplyr::summarise(denominator_count = dplyr::n_distinct(.data$subject_id)),
      by = workingStrata
    ) |>
    dplyr::full_join(
      workingCohort |>
        dplyr::filter(.data$in_cohort == 1) |>
        dplyr::group_by(dplyr::pick(.env$workingStrata)) %>%
        dplyr::summarise(outcome_count = dplyr::n_distinct(.data$subject_id)),
      by = workingStrata
    ) %>%
    dplyr::ungroup() |>
    tidyr::unite("strata_level",
      c(dplyr::all_of(.env$workingStrata)),
      remove = FALSE,
      sep = " &&& "
    ) %>%
    dplyr::mutate(strata_name = !!paste0(workingStrata, collapse = " &&& ")) %>%
    dplyr::relocate("strata_level", .after = "strata_name") %>%
    dplyr::select(!dplyr::any_of(workingStrata))
}

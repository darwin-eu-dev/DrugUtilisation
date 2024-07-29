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

#' This function is used to summarise treatments received
#'
#' @param cohort A cohort table in a cdm reference.
#' @param window Time window over which to summarise the treatments.
#' @param treatmentCohortName Name of a cohort in the cdm that contains the
#'  treatments of interest.
#' @param treatmentCohortId Cohort definition id of interest from
#' treatmentCohortName.
#' @param strata List with column names or vectors of column names groups to
#' stratify results by.
#' @param indexDate Variable in x that contains the date to compute the
#' intersection.
#' @param censorDate Whether to censor overlap events at a specific date or a
#' column date of x. If NULL, end of observation will be used.
#' @param minCellCount ```r lifecycle::badge("deprecated")```
#'
#' @return A summary of treatments stratified by cohort_name and strata_name
#'
#' @export
#'
#' @examples
#' \donttest{
#' library(DrugUtilisation)
#'
#' cdm <- mockDrugUtilisation()
#' cdm$cohort1 |>
#'   summariseTreatment(
#'     treatmentCohortName = "cohort2",
#'     window = list(c(0, 30), c(31, 365))
#'   )
#' }
#'
summariseTreatment <- function(cohort,
                               window,
                               treatmentCohortName,
                               treatmentCohortId = NULL,
                               strata = list(),
                               indexDate = "cohort_start_date",
                               censorDate = NULL,
                               minCellCount = lifecycle::deprecated()) {
  if (lifecycle::is_present(minCellCount)) {
    lifecycle::deprecate_warn("0.7.0", "summariseCodeUse(minCellCount)", with = "omopgenerics::suppress()")
  }

  return(summariseTreatmentInternal(
    cohort = cohort,
    strata = strata,
    window = window,
    indexDate = indexDate,
    censorDate = censorDate,
    treatmentCohortName = treatmentCohortName,
    treatmentCohortId = treatmentCohortId,
    combination = FALSE
  ))
}


summariseTreatmentInternal <- function(cohort,
                                       strata = list(),
                                       window,
                                       indexDate,
                                       censorDate,
                                       treatmentCohortName = NULL,
                                       treatmentCohortId = NULL,
                                       treatmentConceptSet = NULL,
                                       combination = FALSE,
                                       call = parent.frame()) {
  if (!is.list(window)) window <- list(window)
  cdm <- omopgenerics::cdmReference(cohort)
  # initial checks
  checkmate::checkList(strata, types = "character")
  checkmate::checkTRUE(all(unlist(strata) %in% colnames(cohort)))
  checkmate::checkCharacter(treatmentCohortName, null.ok = TRUE)
  checkmate::checkCharacter(censorDate, null.ok = TRUE)
  checkmate::checkCharacter(indexDate)

  cohortNames <- settings(cohort) |> dplyr::pull("cohort_name")

  # correct window names
  if (!is.null(names(window))) {
    namesWindow <- names(window)
  } else {
    namesWindow <- lapply(window, function(x) {
      paste0(as.character(x[1]), " to ", as.character(x[2]))
    }) |>
      unlist()
  }
  names(window) <- paste0("window", seq_along(window))

  # interest variables
  cohort <- cohort |>
    dplyr::select(dplyr::all_of(c(
      "cohort_definition_id", "subject_id", "cohort_start_date",
      "cohort_end_date", unique(unname(unlist(strata)))
    )))

  # add cohort intersect
  cohort <- cohort |>
    PatientProfiles::addCohortIntersectFlag(
      targetCohortTable = treatmentCohortName,
      targetCohortId = treatmentCohortId,
      targetEndDate = NULL,
      indexDate = indexDate,
      censorDate = censorDate,
      window = window,
      nameStyle = "{window_name}_{cohort_name}"
    )

  # create untreated
  for (win in names(window)) {
    cohort <- cohort |>
      dplyr::mutate(!!!untreated(colnames(cohort), win))
  }
  cohort <- cohort |>
    PatientProfiles::addCohortName() |>
    dplyr::collect()

  # summarise
  newCols <- colnames(cohort)
  newCols <- newCols[startsWith(newCols, "window")]
  result <- PatientProfiles::summariseResult(
    table = cohort,
    group = list("cohort_name"),
    strata = strata,
    variables = newCols,
    estimates = c("count", "percentage"),
    counts = FALSE
  ) |>
    suppressMessages()
  cols <- colnames(result)

  treatments <- settings(cdm[[treatmentCohortName]])
  if (!is.null(treatmentCohortId)) {
    treatments <- treatments |>
      dplyr::filter(.data$cohort_definition_id %in% .env$treatmentCohortId)
  }
  treatments <- treatments |> dplyr::pull("cohort_name")
  treatments <- c(treatments, "untreated")

  # correct names
  result <- result |>
    tidyr::separate_wider_delim(
      cols = "variable_name",
      delim = "_",
      names = c("window", "variable_name"),
      too_few = "align_end",
      too_many = "merge",
      cols_remove = TRUE
    ) |>
    dplyr::left_join(
      dplyr::tibble(
        window = paste0("window", seq_along(namesWindow)),
        window_name = namesWindow
      ),
      by = "window"
    ) |>
    dplyr::select(-c(
      "cdm_name", "additional_name", "additional_level", "window"
    )) |>
    treatmentCombinations(
      namesWindow = namesWindow,
      treatments = treatments,
      cohortNames = cohortNames
    ) |>
    visOmopResults::uniteAdditional(cols = "window_name") |>
    PatientProfiles::addCdmName(cdm = cdm)

  result <- result |>
    omopgenerics::newSummarisedResult(settings = dplyr::tibble(
      "result_id" = unique(result$result_id),
      "result_type" = "summarise_treatment",
      "package_name" = "DrugUtilisation",
      "package_version" = as.character(utils::packageVersion("DrugUtilisation"))
    ))

  return(result)
}

untreated <- function(cols, w) {
  col <- cols[startsWith(cols, w)]
  sum <- paste0(".data[[\"", col, "\"]]", collapse = " + ")
  paste0("dplyr::if_else(", sum, " > 0, 0, 1)") |>
    rlang::parse_exprs() |>
    rlang::set_names(paste0(w, "_untreated"))
}
treatmentCombinations <- function(result, namesWindow, treatments, cohortNames) {
  treatments <- dplyr::tibble(
    "variable_name" = treatments, "variable_level" = NA_character_
  ) |>
    dplyr::mutate(order_treatment = dplyr::row_number())
  strata <- result |>
    dplyr::select("result_id", "strata_name", "strata_level") |>
    dplyr::distinct() |>
    dplyr::mutate(order_strata = dplyr::row_number())
  cohorts <- dplyr::tibble(
    group_name = "cohort_name", group_level = cohortNames
  ) |>
    dplyr::mutate(order_group = dplyr::row_number())
  windows <- dplyr::tibble(window_name = namesWindow) |>
    dplyr::mutate(order_window = dplyr::row_number())
  order <- strata |>
    dplyr::cross_join(cohorts) |>
    dplyr::cross_join(windows) |>
    dplyr::cross_join(treatments) |>
    dplyr::arrange(
      .data$order_group, .data$order_strata, .data$order_window,
      .data$order_treatment
    ) |>
    dplyr::mutate("id" = dplyr::row_number()) |>
    dplyr::select(!dplyr::starts_with("order"))
  cols <- colnames(result)
  cols <- cols[!startsWith(cols, "estimate")]
  toAdd <- order |>
    dplyr::mutate(
      "estimate_value" = "0",
      "estimate_name" = "count",
      "estimate_type" = "integer"
    ) |>
    dplyr::union_all(
      order |>
        dplyr::mutate(
          "estimate_value" = "0",
          "estimate_name" = "percentage",
          "estimate_type" = "percentage"
        )
    ) |>
    dplyr::select(-"id") |>
    dplyr::anti_join(result, by = cols)
  result <- result |>
    dplyr::union_all(toAdd) |>
    dplyr::left_join(order, by = cols) |>
    dplyr::arrange(.data$id, .data$estimate_name) |>
    dplyr::select(-"id")
  return(result)
}

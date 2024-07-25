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
                               minCellCount = lifecycle::deprecated()){

  if (lifecycle::is_present(minCellCount)) {
    lifecycle::deprecate_warn("0.7.0", "summariseCodeUse(minCellCount)", with = "omopgenerics::suppress()")
  }

  return(summariseTreatmentInternal(cohort = cohort,
                                    strata = strata,
                                    window = window,
                                    indexDate = indexDate,
                                    censorDate = censorDate,
                                    treatmentCohortName = treatmentCohortName,
                                    treatmentCohortId   = treatmentCohortId,
                                    combination  = FALSE))

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
                               minCellCount = lifecycle::deprecated()) {
  if (!is.list(window)) {
    window <- list(window)
  }
  cdm <- attr(cohort, "cdm_reference")
  # initial checks
  checkmate::checkList(strata, types = "character")
  checkmate::checkTRUE(all(unlist(strata) %in% colnames(cohort)))
  checkmate::checkCharacter(treatmentCohortName, null.ok = TRUE)
  checkmate::checkCharacter(censorDate, null.ok = TRUE)
  checkmate::checkCharacter(indexDate)

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
  if (!is.null(treatmentCohortName)) {
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
    if (is.null(treatmentCohortId)) {
      treatmentCohortId <- omopgenerics::settings(cdm[[treatmentCohortName]])$cohort_definition_id |> sort()
    }
    variableLevel <- omopgenerics::settings(cdm[[treatmentCohortName]]) |>
      dplyr::arrange(.data$cohort_definition_id) |>
      dplyr::pull("cohort_name")
    variableLevel <- c(variableLevel[treatmentCohortId], "untreated")
  }

  # add concept intersect
  # if (!is.null(treatmentConceptSet)) {
  #   cohort <- cohort |>
  #     PatientProfiles::addConceptIntersectFlag(
  #       conceptSet = treatmentConceptSet,
  #       indexDate = indexDate,
  #       censorDate = censorDate,
  #       window = window,
  #       nameStyle = "{window_name}_{concept_name}"
  #     )
  #   variableLevel <- c(names(treatmentConceptSet), "untreated")
  # }

  # create untreated
  for (win in names(window)) {
    cohort <- cohort |>
      dplyr::mutate(!!!untreated(colnames(cohort), win))
  }
  cohort <- cohort |>
    dplyr::compute() |>
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
    estimates = c("count", "percentage")
  )
  cols <- colnames(result)

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
    dplyr::filter(!is.na(.data$window)) |>
    dplyr::left_join(
      dplyr::tibble(
        "window" = paste0("window", seq_along(window)),
        "window_name" = namesWindow
      ),
      by = "window"
    ) |>
    dplyr::arrange(
      .data$group_level, .data$strata_name, .data$strata_level,
      .data$window_name, .data$variable_name
    ) |>
    dplyr::select(-c("window", "additional_name", "additional_level")) |>
    visOmopResults::uniteAdditional(cols = "window_name") |>
    PatientProfiles::addCdmName(cdm = cdm) |>
    dplyr::mutate(variable_name = factor(.data$variable_name, levels = variableLevel)) |>
    dplyr::group_by_at(c(
      "result_id", "cdm_name", "group_name",  "group_level", "strata_name",
      "strata_level", "additional_name", "additional_level"
    )) |>
    dplyr::arrange(.data$variable_name, .by_group = TRUE) |>
    dplyr::ungroup() |>
    dplyr::mutate(variable_name = as.character(.data$variable_name))

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

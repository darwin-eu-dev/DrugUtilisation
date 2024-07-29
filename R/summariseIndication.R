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

#' Summarise the indications of individuals in a drug cohort
#'
#' @description
#' Summarise the observed indications of patients in a drug cohort based on
#' their presence in an indication cohort in a specified time window. If an
#' individual is not in one of the indication cohorts, they will be considered
#' to have an unknown indication if they are present in one of the specified
#' OMOP CDM clinical tables. Otherwise, if they  are neither in an indication
#' cohort or a clinical table they will be considered as having no observed
#' indication.
#'
#' @param cohort A cohort table in a cdm reference.
#' @param strata List of variables to stratify results by. These variables
#' must be present in the cohort table.
#' @param indicationCohortName Name of the cohort table with potential
#' indications.
#' @param indicationCohortId The target cohort ID to add indication. If NULL all
#' cohorts will be considered.
#' @param indicationWindow The time window over which to identify indications.
#' @param unknownIndicationTable Tables in the OMOP CDM to search for unknown
#' indications.
#' @param indexDate A date variable in the cohort table for which indications
#' will be found relative to.
#' @param censorDate After that day no indication will be considered.
#'
#' @return A summarised result
#'
#' @export
#'
#' @examples
#' \donttest{
#' library(DrugUtilisation)
#' library(CDMConnector)
#' library(dplyr)
#'
#' cdm <- mockDrugUtilisation()
#'
#' indications <- list("headache" = 378253, "asthma" = 317009)
#' cdm <- generateConceptCohortSet(cdm, indications, "indication_cohorts")
#'
#' cdm <- generateIngredientCohortSet(
#'   cdm = cdm, name = "drug_cohort",
#'   ingredient = "acetaminophen"
#' )
#'
#' cdm$drug_cohort |>
#'   summariseIndication(
#'     indicationCohortName = "indication_cohorts",
#'     unknownIndicationTable = "condition_occurrence",
#'     indicationWindow = list(c(-Inf, 0))
#'   ) |>
#'   glimpse()
#' }
#'
summariseIndication <- function(cohort,
                                strata = list(),
                                indicationCohortName,
                                indicationCohortId = NULL,
                                indicationWindow = list(c(0, 0)),
                                unknownIndicationTable = NULL,
                                indexDate = "cohort_start_date",
                                censorDate = NULL) {
  # initialChecks
  cdm <- omopgenerics::cdmReference(cohort)
  checkInputs(cohort = cohort, cdm = cdm, strata = strata)

  tablePrefix <- omopgenerics::tmpPrefix()

  if (!is.list(indicationWindow)) indicationWindow <- list(indicationWindow)
  if (is.null(names(indicationWindow))) {
    names(indicationWindow) <- rep("", length(indicationWindow))
  }
  windowNames <- lapply(seq_along(indicationWindow), function(k) {
    if (names(indicationWindow)[k] == "") {
      return(windowName(indicationWindow[[k]]))
    } else {
      return(names(indicationWindow)[k])
    }
  }) |>
    unlist()
  names(indicationWindow) <- paste0("win", seq_along(indicationWindow))

  cohort <- cohort |>
    dplyr::select(!dplyr::any_of("cohort_name")) |>
    PatientProfiles::addCohortName() |>
    dplyr::select(dplyr::all_of(c("subject_id", indexDate, "cohort_name", censorDate, unique(unlist(strata))))) |>
    addIndication(
      indicationCohortName = indicationCohortName,
      indicationCohortId = indicationCohortId,
      indicationWindow = indicationWindow,
      unknownIndicationTable = unknownIndicationTable,
      indexDate = indexDate,
      censorDate = censorDate,
      name = omopgenerics::uniqueTableName(tablePrefix)
    ) |>
    dplyr::collect()

  indicationVariables <- colnames(cohort)
  indicationVariables <- indicationVariables[startsWith(indicationVariables, "indication_")]

  q <- paste0(
    "dplyr::case_when(",
    paste0(
      ".data$variable_name == 'indication_", names(indicationWindow), "' ~ 'Indication ",
      windowNames, "'",
      collapse = ", "
    ),
    ", .default = .data$variable_name)"
  ) |>
    rlang::parse_exprs() |>
    rlang::set_names("variable_name")

  # summarise indication columns
  suppressMessages(
    result <- PatientProfiles::summariseResult(
      table = cohort,
      group = list("cohort_name"),
      includeOverallGroup = FALSE,
      includeOverallStrata = TRUE,
      strata = strata,
      variables = indicationVariables,
      estimates = c("count", "percentage")
    )
  )
  result <- result |>
    dplyr::select(-"cdm_name") |>
    PatientProfiles::addCdmName(cdm = cdm) |>
    dplyr::mutate(!!!q) |>
    # make sure all indications are reported
    indicationCombinations(
      settings(cdm[[indicationCohortName]]), indicationCohortId, unknownIndicationTable
    )

  result <- result |>
    omopgenerics::newSummarisedResult(
      settings = dplyr::tibble(
        result_id = unique(result$result_id),
        result_type = "summarise_indication",
        package_name = "DrugUtilisation",
        package_version = as.character(utils::packageVersion("DrugUtilisation"))
      )
    )

  return(result)
}

temporalWord <- function(x) {
  if (x < 0) {
    return("before")
  } else {
    return("after")
  }
}
daysWord <- function(d) {
  if (is.infinite(d)) {
    return("any time")
  } else {
    nm <- cli::cli_text("{abs(d)} day{?s}") |>
      cli::cli_fmt() |>
      paste0(collapse = " ")
    return(nm)
  }
}
windowName <- function(win) {
  min <- win[1]
  max <- win[2]
  if (min == 0 & max == 0) {
    nm <- "on index date"
  } else if (is.infinite(min) & max == 0) {
    nm <- "any time before or on index date"
  } else if (min == 0 & is.infinite(max)) {
    nm <- "any time after or on index date"
  } else if (is.infinite(min) & is.infinite(max)) {
    nm <- "any time"
  } else if (min == 0) {
    nm <- glue::glue("from index date to {daysWord(max)} after")
  } else if (max == 0) {
    nm <- glue::glue("from {daysWord(min)} before to the index date")
  } else {
    nm <- glue::glue("from {daysWord(min)} {temporalWord(min)} to {daysWord(max)} {temporalWord(max)} the index date")
  }
  return(nm)
}
indicationCombinations <- function(result, set, indicationCohortId, unknownIndicationTable) {
  vars <- result$variable_name |> unique()
  vars <- vars[startsWith(vars, "Indication")]
  if (!is.null(indicationCohortId)) {
    set <- set |>
      dplyr::filter(.data$cohort_definition_id %in% .env$indicationCohortId)
  }
  indications <- set$cohort_name |>
    sort() |>
    rev()
  indications <- c(getCombinations(set$cohort_name), ifelse(length(unknownIndicationTable) > 0, "unknown", character()), "none")
  allcombs <- dplyr::tibble(
    "variable_name" = c("number records", "number subjects"),
    "variable_level" = NA_character_
  ) |>
    dplyr::union_all(tidyr::expand_grid(
      "variable_name" = vars, "variable_level" = indications
    ))
  order <- result |>
    dplyr::select(
      "result_id", "cdm_name", "group_name", "group_level", "strata_name",
      "strata_level", "additional_name", "additional_level"
    ) |>
    dplyr::distinct() |>
    dplyr::cross_join(allcombs) |>
    dplyr::mutate("order_id" = dplyr::row_number())
  cols <- colnames(result)
  cols <- cols[!cols %in% c("estimate_name", "estimate_type", "estimate_value")]
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
    dplyr::select(-"order_id") |>
    dplyr::anti_join(result, by = cols)
  result <- result |>
    dplyr::union_all(toAdd) |>
    dplyr::left_join(order, by = cols) |>
    dplyr::arrange(.data$order_id, .data$estimate_name) |>
    dplyr::select(-"order_id")
  return(result)
}
getCombinations <- function(indications) {
  indications <- sort(indications)
  combs <- rep(list(c(1, 0)), length(indications))
  names(combs) <- indications
  sumOp <- paste0('.data[["', indications, '"]]', collapse = "+") |>
    rlang::parse_exprs() |>
    rlang::set_names("sum")
  combs <- tidyr::expand_grid(!!!combs) |>
    dplyr::mutate(!!!sumOp) |>
    dplyr::arrange(dplyr::pick(dplyr::all_of(c("sum", rev(indications))))) |>
    dplyr::select(-"sum")
  indications <- character()
  for (k in 2:nrow(combs)) {
    cols <- combs[k, ] |>
      as.list() |>
      unlist()
    nms <- names(cols)[cols == 1]
    indications <- c(indications, paste0(nms, collapse = " and "))
  }
  return(indications)
}

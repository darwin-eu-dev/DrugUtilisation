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
#' cdm <- generateIngredientCohortSet(cdm = cdm, name = "drug_cohort",
#'                                    ingredient = "acetaminophen")
#'
#' cdm$drug_cohort |>
#'   summariseIndication(
#'     indicationCohortName = "indication_cohorts",
#'     unknownIndicationTable = "condition_occurrence",
#'     indicationWindow = list(c(-Inf, 0))) |>
#'  glimpse()
#'
#' }
#'
summariseIndication <- function(cohort,
                                strata = list(),
                                indicationCohortName,
                                indicationCohortId = NULL,
                                indicationWindow = list(c(0, 0)),
                                unknownIndicationTable = NULL,
                                indexDate = "cohort_start_date") {
  # initialChecks
  cdm <- omopgenerics::cdmReference(cohort)
  checkInputs(cohort = cohort, cdm = cdm, strata = strata)

  tablePrefix <- omopgenerics::tmpPrefix()

  if (!is.list(indicationWindow)) indicationWindow <- list(indicationWindow)
  if (is.null(names(indicationWindow))) {
    names(indicationWindow) <- rep("", length(indicationWindow))
  }
  windowNames <- names(indicationWindow)
  for (k in seq_along(windowNames)) {
    if (windowNames[k] == "") {
      windowNames[k] <- windowName(indicationWindow[[k]])
    }
  }
  names(indicationWindow) <- paste0("win", seq_along(indicationWindow))

  cohort <- cohort |>
    PatientProfiles::addCohortName() |>
    dplyr::select(dplyr::any_of(c("subject_id", "person_id", indexDate, "cohort_name"))) |>
    addIndication(
      indicationCohortName = indicationCohortName,
      indicationCohortId = indicationCohortId,
      indicationWindow = indicationWindow,
      unknownIndicationTable = unknownIndicationTable,
      indexDate = indexDate,
      name = omopgenerics::uniqueTableName(tablePrefix),
      nameStyle = "ind_{window_name}_{cohort_name}"
    )
  indicationVariables <- indicationColumns(cohort)

  # update cohort_names
  cohort <-
    cohort |> PatientProfiles::addCohortName() |> dplyr::collect()

  # summarise indication columns
  result <- PatientProfiles::summariseResult(
    table = cohort,
    group = list("cohort_name"),
    includeOverallGroup = FALSE,
    includeOverallStrata = TRUE,
    strata = strata,
    variables = indicationVariables,
    estimates = c("count", "percentage")
  ) |>
    PatientProfiles::addCdmName(cdm = cdm) |>
    dplyr::mutate(
      variable_name = dplyr::if_else(
        substr(.data$variable_name, 1, 11) == "indication_",
        lapply(strsplit(.data$variable_name, "_"), function(x) {
          x <- paste0(x[1:min(4, length(x))], collapse = "_")
          x <- indicationColumnName(x)
        }) |>
          unlist(),
        .data$variable_name
      )
    )

  result <- result |>
    omopgenerics::newSummarisedResult(
      settings = dplyr::tibble(
        result_id = unique(result$result_id),
        result_type = "summarised_indication",
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


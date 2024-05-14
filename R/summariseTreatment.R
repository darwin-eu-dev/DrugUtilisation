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

#' This function is used to summarise the dose table over multiple cohorts.
#'
#' @param cohort Cohort with drug use variables and strata.
#' @param strata Stratification list.
#' @param window Window where to summarise the treatments.
#' @param treatmentCohortName Name of a cohort in the cdm that contains the
#' interest treatments.
#' @param treatmentCohortId Cohort definition id of interest from
#' treatmentCohortName.
#' @param combination Whether to include combination treatments.
#' @param minCellCount Below this number counts will be suppressed.
#'
#' @return A summary of the drug use stratified by cohort_name and strata_name
#'
#' @export
#'
#' @examples
#' \donttest{
#' library(DrugUtilisation)
#' library(PatientProfiles)
#' library(CodelistGenerator)
#'
#' cdm <- mockDrugUtilisation()
#' cdm <- generateDrugUtilisationCohortSet(
#'   cdm, "dus_cohort", getDrugIngredientCodes(cdm, "acetaminophen")
#' )
#' cdm[["dus_cohort"]] <- cdm[["dus_cohort"]] %>%
#'   addDrugUse(ingredientConceptId = 1125315)
#' result <- summariseDrugUse(cdm[["dus_cohort"]])
#' print(result)
#'
#' cdm[["dus_cohort"]] <- cdm[["dus_cohort"]] %>%
#'   addSex() %>%
#'   addAge(ageGroup = list("<40" = c(0, 30), ">40" = c(40, 150)))
#'
#' summariseDrugUse(
#'   cdm[["dus_cohort"]], strata = list(
#'    "age_group" = "age_group", "sex" = "sex",
#'    "age_group and sex" = c("age_group", "sex")
#'   )
#' )
#' }
#'
summariseTreatmentFromCohort <- function(cohort,
                                         strata = list(),
                                         window,
                                         treatmentCohortName,
                                         treatmentCohortId = NULL,
                                         combination = FALSE,
                                         minCellCount = 5){
  return(summariseTreatment(cohort = cohort,
                            strata = strata,
                            window = window,
                            treatmentCohortName = treatmentCohortName,
                            treatmentCohortId   = treatmentCohortId,
                            combination  = combination,
                            minCellCount = minCellCount))

}

#'This function is used to summarise the dose table over multiple cohorts.
#'
#' @param cohort Cohort with drug use variables and strata.
#' @param strata Stratification list.
#' @param window Window where to summarise the treatments.
#' @param treatmentConceptSet Concept set list to summarise.
#' @param combination Whether to include combination treatments.
#' @param minCellCount Below this number counts will be suppressed.
#'
#' @return A summary of the drug use stratified by cohort_name and strata_name
#'
#' @export
#'
summariseTreatmentFromConceptSet <- function(cohort,
                                             strata = list(),
                                             window,
                                             treatmentConceptSet,
                                             combination = FALSE,
                                             minCellCount = 5){
  return(summariseTreatment(cohort = cohort,
                            strata = strata,
                            window = window,
                            treatmentConceptSet = treatmentConceptSet,
                            combination  = combination,
                            minCellCount = minCellCount))
}


summariseTreatment <- function(cohort,
                               strata = list(),
                               window,
                               treatmentCohortName = NULL,
                               treatmentCohortId = NULL,
                               treatmentConceptSet = NULL,
                               combination = FALSE,
                               minCellCount = 5) {
  if (!is.list(window)) {
    window <- list(window)
  }
  cdm <- attr(cohort, "cdm_reference")
  # initial checks
  checkmate::checkList(strata, types = "character")
  checkmate::checkTRUE(all(unlist(strata) %in% colnames(cohort)))
  checkmate::checkCharacter(treatmentCohortName, null.ok = TRUE)

  # combination
  if (combination) {
    cli::cli_warn("Combination is not implemented yet")
  }

  # correct window names
  if (!is.null(names(window))) {
    namesWindow <- names(window)
  } else {
    namesWindow <- lapply(window, function(x) {
      paste0(as.character(x[1]), " to ", as.character(x[2]))
    }) %>%
      unlist()
  }
  names(window) <- paste0("window", seq_along(window))

  # interest variables
  cohort <- cohort %>%
    dplyr::select(dplyr::all_of(c(
      "cohort_definition_id", "subject_id", "cohort_start_date",
      "cohort_end_date", unique(unname(unlist(strata)))
    )))

  # add cohort intersect
  if (!is.null(treatmentCohortName)) {
    cohort <- cohort %>%
      PatientProfiles::addCohortIntersectFlag(
        targetCohortTable = treatmentCohortName,
        targetCohortId = treatmentCohortId,
        targetEndDate = NULL,
        window = window,
        nameStyle = "{window_name}_{cohort_name}"
      )
  }

  # add concept intersect
  if (!is.null(treatmentConceptSet)) {
    cohort <- cohort %>%
      PatientProfiles::addConceptIntersectFlag(
        conceptSet = treatmentConceptSet,
        window = window,
        nameStyle = "{window_name}_{concept_name}"
      )
  }

  # create untreated
  for (win in names(window)) {
    cohort <- cohort %>%
      dplyr::mutate(!!!untreated(colnames(cohort), win))
  }
  cohort <- cohort %>%
    dplyr::compute() %>%
    PatientProfiles::addCohortName() %>%
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
  result <- result %>%
    tidyr::separate_wider_delim(
      cols = "variable_name",
      delim = "_",
      names = c("window_name", "new_variable_name"),
      too_few = "align_end",
      too_many = "merge",
      cols_remove = TRUE
    ) %>%
    dplyr::rename("variable_name" = "new_variable_name") %>%
    dplyr::filter(!is.na(.data$window_name)) |>
    dplyr::left_join(
      dplyr::tibble(
        "window_name" = paste0("window", seq_along(window)),
        "window" = namesWindow
      ),
      by = "window_name"
    ) %>%
    dplyr::arrange(
      .data$group_level, .data$strata_name, .data$strata_level,
      .data$window_name, .data$variable_name
    ) %>%
    dplyr::select(-c("window_name", "additional_name", "additional_level")) %>%
    visOmopResults::uniteAdditional(cols = "window") |>
    PatientProfiles::addCdmName(cdm = cdm)

  result <- result |>
    omopgenerics::newSummarisedResult(settings = dplyr::tibble(
      "result_id" = unique(result$result_id),
      "result_type" = "summarised_treatment",
      "package_name" = "DrugUtilisation",
      "package_version" = as.character(utils::packageVersion("DrugUtilisation"))
    ))

  return(result)
}

untreated <- function(cols, w) {
  col <- cols[startsWith(cols, w)]
  sum <- paste0(".data[[\"", col, "\"]]", collapse = " + ")
  paste0("dplyr::if_else(", sum, " > 0, 0, 1)") %>%
    rlang::parse_exprs() %>%
    rlang::set_names(paste0(w, "_untreated"))
}

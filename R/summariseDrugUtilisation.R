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

#' This function is used to summarise the dose utilisation table over multiple
#' cohorts.
#'
#' @param cohort Cohort with drug use variables and strata.
#' @param strata Stratification list.
#' @param drugUseEstimates Estimates that we want for the columns.
#' @param indexDate Name of a column that indicates the date to start the
#' analysis.
#' @param censorDate Name of a column that indicates the date to stop the
#' analysis, if NULL end of individuals observation is used.
#' @param ingredientConceptId Ingredient OMOP concept that we are interested for
#' the study. It is a compulsory input, no default value is provided.
#' @param conceptSet List of concepts to be included. If NULL all the
#' descendants of ingredient concept id will be used.
#' ingredientConceptId = NULL,
#' @param restrictIncident Whether to include only incident prescriptions in the
#' analysis. If FALSE all prescriptions that overlap with the study period will
#' be included.
#' @param gapEra Number of days between two continuous exposures to be
#' considered in the same era.
#' @param numberExposures Whether to add a column with the number of exposures.
#' @param numberEras Whether to add a column with the number of eras.
#' @param exposedTime Whether to add a column with the number of exposed days.
#' @param timeToExposure Whether to add a column with the number of days between
#' indexDate and start of the first exposure.
#' @param initialQuantity Whether to add a column with the initial quantity.
#' @param cumulativeQuantity Whether to add a column with the cumulative
#' quantity of the identified prescription.
#' @param initialDailyDose Whether to add a column with the initial daily dose.
#' @param cumulativeDose Whether to add a column with the cumulative dose.
#'
#' @return A summary of drug utilisation stratified by cohort_name and strata_name
#'
#' @export
#' @examples
#' \donttest{
#' library(DrugUtilisation)
#' library(CodelistGenerator)
#'
#' cdm <- mockDrugUtilisation()
#' cdm <- generateDrugUtilisationCohortSet(
#'   cdm, "dus_cohort", getDrugIngredientCodes(cdm, name = "acetaminophen")
#' )
#' cdm[["dus_cohort"]] %>%
#'   summariseDrugUtilisation(ingredientConceptId = 1125315)
#' }
#'
summariseDrugUtilisation <- function(cohort,
                                     strata = list(),
                                     drugUseEstimates = c(
                                       "q25", "median", "q75", "mean", "sd",
                                       "count_missing", "percentage_missing"
                                     ),
                                     indexDate = "cohort_start_date",
                                     censorDate = "cohort_end_date",
                                     ingredientConceptId = NULL,
                                     conceptSet = NULL,
                                     restrictIncident = TRUE,
                                     gapEra = 1,
                                     numberExposures = TRUE,
                                     numberEras = TRUE,
                                     exposedTime = TRUE,
                                     timeToExposure = TRUE,
                                     initialQuantity = TRUE,
                                     cumulativeQuantity = TRUE,
                                     initialDailyDose = TRUE,
                                     cumulativeDose = TRUE) {

  # checks
  checkInputs(cohort = cohort, strata = strata, drugUseEstimates = drugUseEstimates)
  cdm <- omopgenerics::cdmReference(cohort)
  ingredientConceptId <- validateIngredientConceptId(ingredientConceptId, cdm)
  conceptSet <- validateConceptSet(conceptSet, ingredientConceptId, cdm)

  # concept dictionary
  dic <- dplyr::tibble(concept_name = names(conceptSet)) |>
    dplyr::mutate(
      variable_level_id = paste0("xxid", dplyr::row_number(), "xx"),
      variable_level = paste0("id", dplyr::row_number())
      )
  names(conceptSet) <- dic$variable_level_id



  # add drug utilisation
  cohort <- cohort |>
    dplyr::select(dplyr::all_of(c(
      "cohort_definition_id", "subject_id", "cohort_start_date",
      "cohort_end_date", unique(unlist(strata))
      ))) |>
    PatientProfiles::addCohortName() |>
    addDrugUseInternal(
      indexDate = indexDate,
      censorDate = censorDate,
      conceptSet = conceptSet,
      ingredientConceptId = ingredientConceptId,
      restrictIncident = restrictIncident,
      numberExposures = numberExposures,
      numberEras = numberEras,
      exposedTime = exposedTime,
      timeToExposure = timeToExposure,
      initialQuantity = initialQuantity,
      cumulativeQuantity = cumulativeQuantity,
      initialDailyDose = initialDailyDose,
      cumulativeDose = cumulativeDose,
      gapEra = gapEra,
      nameStyle = "{value}_{concept_name}_{ingredient}",
      name = NULL) |>
    dplyr::collect()

  initialCols <- c(
    "cohort_definition_id", "subject_id", "cohort_start_date",
    "cohort_end_date", unique(unlist(strata))
  )
  drugUseCols <- colnames(cohort)
  drugUseCols <- drugUseCols[!drugUseCols %in% initialCols]

  variableNames <- c(
    "number_exposures_", "time_to_exposure_", "cumulative_quantity_",
    "initial_quantity_", "number_eras_", "exposed_time_", "cumulative_dose_",
    "initial_daily_dose_"
  )



  # summarise drug use columns
  result <- PatientProfiles::summariseResult(
    table = cohort, group = list("cohort_name" = "cohort_name"),
    strata = strata, variables = drugUseCols,
    estimates = drugUseEstimates
  ) %>%
    dplyr::mutate(
      cdm_name = dplyr::coalesce(omopgenerics::cdmName(cdm), as.character(NA)),
      unit =  gsub("_xx.*", "", gsub(paste0(variableNames, collapse = "|"), "", .data$variable_name)),
      variable_level = dplyr::if_else(
        .data$variable_name %in% c("number records", "number subjects"),
        NA,
        gsub(".*_xx|xx_.*|xx.*", "", .data$variable_name)
        ),
      ingredient = gsub(".*xx_|.*xx", "", .data$variable_name),
      ingredient = dplyr::if_else(nchar(.data$ingredient) == 0, NA, .data$ingredient),
      !!!variableNameExp(variableNames)
    ) |>
    dplyr::left_join(dic, by = "variable_level") |>
    dplyr::mutate(
      variable_level = dplyr::case_when(
        !is.na(.data$concept_name) & is.na(.data$ingredient) ~ .data$concept_name,
        !is.na(.data$concept_name) & !is.na(.data$ingredient) ~ paste0(.data$concept_name, " - ", .data$ingredient),
        .default = .data$variable_level
      )
    ) |>
    dplyr::select(omopgenerics::resultColumns()) |>
    dplyr::arrange(.data$result_id, .data$group_name, .data$group_level, .data$strata_name, .data$strata_level)

  result <- result |>
    omopgenerics::newSummarisedResult(settings = dplyr::tibble(
      result_id = unique(result$result_id),
      result_type = "drug_utilisation",
      package_name = "DrugUtilisation",
      package_version = as.character(utils::packageVersion("DrugUtilisation"))
    ))

  return(result)
}

variableNameExp <- function(variableNames) {
  expr <- "dplyr::case_when("
  for (var in variableNames) {
    if (!grepl("dose", var)) {
      expr <- paste0(expr, "grepl('", var, "', .data$variable_name) ~ '", gsub("_", " ", substring(var, 0, nchar(var)-1)), "',")
    } else {
      expr <- paste0(expr, "grepl('", var, "', .data$variable_name) ~ ", "paste0('", gsub("_", " ", var), "',", "'(', .data$unit, ')')", ",")
    }
  }
  expr <- paste0(expr, ".default = .data$variable_name)") |> rlang::parse_exprs() |> rlang::set_names("variable_name")
}

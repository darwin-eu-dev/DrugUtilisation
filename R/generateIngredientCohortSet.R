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

#' Generate a set of drug cohorts based on drug ingredients
#'
#' @description
#' Adds a new cohort table to the cdm reference with individuals who have drug
#' exposure records with the specified drug ingredient. Cohort start and end
#' dates will be based on drug record start and end dates, respectively. Records
#' that overlap or have fewer days between them than the specified gap era will
#' be concatenated into a single cohort entry.
#'
#' @param cdm A cdm reference.
#' @param name The name of the new cohort table to add to the cdm reference.
#' @param ingredient Accepts both vectors and named lists of ingredient names.
#' For a vector input, e.g., c("acetaminophen", "codeine"), it generates a
#' cohort table with descendant concept codes for each ingredient, assigning
#' unique cohort_definition_id. For a named list input, e.g., list(
#' "test_1" = c("simvastatin", "acetaminophen"), "test_2" = "metformin"),
#' it produces a cohort table based on the structure of the input, where
#' each name leads to a combined set of descendant concept codes for the
#' specified ingredients, creating distinct cohort_definition_id for each
#' named group.
#' @param doseForm Only descendants codes with the specified dose form
#' will be returned. If NULL, descendant codes will be returned regardless
#' of dose form.
#' @param doseUnit Only descendants codes with the specified dose unit
#' will be returned. If NULL, descendant codes will be returned regardless
#' of dose unit
#' @param routeCategory Only descendants codes with the specified route
#' will be returned. If NULL, descendant codes will be returned regardless
#' of route category.
#' @param ingredientRange Used to restrict descendant codes to those
#' associated with a specific number of ingredients. Must be a vector of length
#' two with the first element the minimum number of ingredients allowed and
#' the second the maximum. A value of c(2, 2) would restrict to only concepts
#' associated with two ingredients.
#' @param gapEra Number of days between two continuous exposures to be
#' considered in the same era. Records that have fewer days between them than
#' this gap will be concatenated into the same cohort record.
#' @param durationRange Deprecated.
#' @param imputeDuration Deprecated.
#' @param priorUseWashout Deprecated
#' @param priorObservation Deprecated.
#' @param cohortDateRange Deprecated.
#' @param limit Deprecated.
#'
#' @return The function returns the cdm reference provided with the addition of
#' the new cohort table.
#'
#' @export
#'
#' @examples
#' \donttest{
#' library(DrugUtilisation)
#' library(dplyr)
#'
#' cdm <- mockDrugUtilisation()
#'
#' cdm <- generateIngredientCohortSet(
#'   cdm = cdm,
#'   ingredient = "acetaminophen",
#'   name = "acetaminophen"
#' )
#'
#' cdm$acetaminophen |>
#'   glimpse()
#' }
#'
generateIngredientCohortSet <- function(cdm,
                                        name,
                                        ingredient = NULL,
                                        doseForm = NULL,
                                        doseUnit = NULL,
                                        routeCategory = NULL,
                                        ingredientRange = c(1, Inf),
                                        gapEra = 1,
                                        durationRange = lifecycle::deprecated(),
                                        imputeDuration = lifecycle::deprecated(),
                                        priorUseWashout = lifecycle::deprecated(),
                                        priorObservation = lifecycle::deprecated(),
                                        cohortDateRange = lifecycle::deprecated(),
                                        limit = lifecycle::deprecated()) {
  if (lifecycle::is_present(durationRange)) {
    lifecycle::deprecate_warn(
      when = "0.7.0",
      what = "generateIngredientCohortSet(durationRange = )"
    )
  }
  if (lifecycle::is_present(imputeDuration)) {
    lifecycle::deprecate_warn(
      when = "0.7.0", what = "generateIngredientCohortSet(imputeDuration = )"
    )
  }
  if (lifecycle::is_present(priorUseWashout)) {
    lifecycle::deprecate_warn(
      when = "0.7.0",
      what = "generateIngredientCohortSet(priorUseWashout = )",
      with = "requirePriorDrugWashout()"
    )
  }
  if (lifecycle::is_present(priorObservation)) {
    lifecycle::deprecate_warn(
      when = "0.7.0",
      what = "generateIngredientCohortSet(priorObservation = )",
      with = "requireObservationBeforeDrug()"
    )
  }
  if (lifecycle::is_present(cohortDateRange)) {
    lifecycle::deprecate_warn(
      when = "0.7.0",
      what = "generateIngredientCohortSet(cohortDateRange = )",
      with = "requireDrugInDateRange()"
    )
  }
  if (lifecycle::is_present(limit)) {
    lifecycle::deprecate_warn(
      when = "0.7.0",
      what = "generateIngredientCohortSet(limit = )",
      with = "requireIsFirstDrugEntry()"
    )
  }

  if (!is.list(ingredient)) {
    conceptSet <- CodelistGenerator::getDrugIngredientCodes(
      cdm = cdm,
      name = ingredient,
      doseForm = doseForm,
      ingredientRange = ingredientRange,
      doseUnit = doseUnit,
      routeCategory = routeCategory
    )
  } else {
    conceptSet <- lapply(ingredient, function(values) {
      lapply(values, function(value) {
        CodelistGenerator::getDrugIngredientCodes(
          cdm = cdm,
          name = value,
          doseForm = doseForm,
          ingredientRange = ingredientRange,
          doseUnit = doseUnit,
          routeCategory = routeCategory
        )
      }) |>
        unname() |>
        unlist() |>
        unique()
    })
  }

  cdm <- DrugUtilisation::generateDrugUtilisationCohortSet(
    cdm = cdm,
    name = name,
    conceptSet = conceptSet,
    gapEra = gapEra
  )

  cdm[[name]] <- cdm[[name]] |>
    omopgenerics::newCohortTable(
      cohortSetRef = settings(cdm[[name]]) |>
        dplyr::mutate(
          "dose_form" = paste0(.env$doseForm, collapse = " + "),
          "ingredient_range_min" = as.character(ingredientRange[1]),
          "ingredient_range_max" = as.character(ingredientRange[2])
        )
    )

  return(cdm)
}

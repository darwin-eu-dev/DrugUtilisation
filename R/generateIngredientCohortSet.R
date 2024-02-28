# Copyright 2022 DARWIN EU (C)
#
# This file is part of CohrotSymmetry
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

#' Generates a cohort of the drug use of a certain list of concepts.
#'
#' @param cdm A cdm_reference object.
#' @param name Name of the GeneratedCohortSet
#' @param ingredient Names of ingredients of interest. For example, c("acetaminophen",
#' "codeine"), would result in a list of length two with the descendant
#' concepts for these two particular drug ingredients.
#' @param durationRange Range between the duration must be comprised. It should
#' be a numeric vector of length two, with no NAs and the first value should be
#' equal or smaller than the second one. It is only required if imputeDuration
#' = TRUE. If NULL no restrictions are applied.
#' @param imputeDuration Whether/how the duration should be imputed
#' "none", "median", "mean", "mode", or it can be a count
#' @param gapEra Number of days between two continuous exposures to be
#' considered in the same era.
#' @param priorUseWashout Prior days without exposure.
#' @param priorObservation Minimum number of days of prior observation
#' required for the incident eras to be considered.
#' @param cohortDateRange Range for cohort_start_date and cohort_end_date
#' @param limit Choice on how to summarise the exposures. There are
#' two options:
#' "all" we summarise the output will be a summary of the exposed eras of
#' each individual. Each individual can contribute multiple times.
#' "first" we only consider the first observable era of each individual that fulfills the criteria provided
#' in previous parameters. In this case each individual can not contribute with multiple rows.
#' @param doseForm Only descendants codes with the specified dose form
#' will be returned. If NULL, descendant codes will be returned regardless
#' of dose form.
#' @param ingredientRange Used to restrict descendant codes to those
#' associated with a specific number of ingredients. Must be a vector of length
#' two with the first element the minimum number of ingredients allowed and
#' the second the maximum. A value of c(2, 2) would restrict to only concepts
#' associated with two ingredients.
#' @param withConceptDetails If FALSE, each item in the list of results (one per
#' ingredient) will contain a vector of concept IDs for each ingredient. If
#' TRUE each item in the list of results will contain a tibble with additional
#' information on the identified concepts.
#' @return The function returns the 'cdm' object with the created cohorts as
#' references of the object.
#'
#' @export
#'
#' @examples
#' \donttest{
#'  cdm <- DrugUtilisation::mockDrugUtilisation()
#'  cdm <- generateIngredientCohortSet(cdm,
#'  ingredient = "acetaminophen",
#'  name = "test")
#' }

#'
#'

generateIngredientCohortSet <- function(cdm,
                                        name,
                                        ingredient = NULL,
                                        durationRange = c(1, Inf),
                                        imputeDuration = "none",
                                        gapEra = 0,
                                        priorUseWashout = 0,
                                        priorObservation = 0,
                                        cohortDateRange = as.Date(c(NA, NA)),
                                        limit = "all",
                                        doseForm = NULL,
                                        ingredientRange = c(1, Inf),
                                        withConceptDetails = FALSE) {

  conceptSet <- CodelistGenerator::getDrugIngredientCodes(cdm,
                                                          name = ingredient,
                                                          doseForm,
                                                          ingredientRange,
                                                          withConceptDetails)

  cdm <- DrugUtilisation::generateDrugUtilisationCohortSet(
    cdm,
    name,
    conceptSet,
    durationRange,
    imputeDuration,
    gapEra,
    priorUseWashout,
    priorObservation,
    cohortDateRange,
    limit
  )

  return(cdm)
}



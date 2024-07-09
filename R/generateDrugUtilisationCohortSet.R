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

#' Generates a cohort of the drug use of a certain list of concepts.
#'
#' @param cdm A cdm_reference object.
#' @param name Name of the GeneratedCohortSet
#' @param conceptSet Named list of concept sets.
#' @param durationRange Deprecated.
#' @param imputeDuration Deprecated.
#' @param gapEra Number of days between two continuous exposures to be
#' considered in the same era.
#' @param priorUseWashout Prior days without exposure.
#' @param priorObservation Deprecated.
#' @param cohortDateRange Deprecated.
#' @param limit Deprecated.
#' @return The function returns the 'cdm' object with the created tables as
#' references of the object.
#'
#' @export
#'
#' @examples
#' \donttest{
#' library(CDMConnector)
#' library(DrugUtilisation)
#'
#' cdm <- mockDrugUtilisation()
#'
#' druglist <- getDrugIngredientCodes(cdm, c("acetaminophen", "metformin"))
#'
#' cdm <- generateDrugUtilisationCohortSet(
#'   cdm = cdm,
#'   name = "drug_cohorts",
#'   conceptSet = druglist
#' )
#'
#' cdm[["drug_cohorts"]]
#'
#' settings(cdm[["drug_cohorts"]])
#'
#' cohortCount(cdm[["drug_cohorts"]])
#'
#' attrition(cdm[["drug_cohorts"]])
#' }
#'
generateDrugUtilisationCohortSet <- function(cdm,
                                             name,
                                             conceptSet,
                                             durationRange = lifecycle::deprecated(),
                                             imputeDuration = lifecycle::deprecated(),
                                             gapEra = 0,
                                             priorUseWashout = 0,
                                             priorObservation = lifecycle::deprecated(),
                                             cohortDateRange = lifecycle::deprecated(),
                                             limit = lifecycle::deprecated()) {

  if (lifecycle::is_present(durationRange)) {
    lifecycle::deprecate_warn(
      when = "0.6.2", what = "generateDrugUtilisationCohortSet(durationRange = )"
    )
  }
  if (lifecycle::is_present(imputeDuration)) {
    lifecycle::deprecate_warn(
      when = "0.6.2", what = "generateDrugUtilisationCohortSet(imputeDuration = )"
    )
  }
  if (lifecycle::is_present(priorObservation)) {
    lifecycle::deprecate_warn(
      when = "0.6.2", what = "generateDrugUtilisationCohortSet(priorObservation = )"
    )
  }
  if (lifecycle::is_present(cohortDateRange)) {
    lifecycle::deprecate_warn(
      when = "0.6.2", what = "generateDrugUtilisationCohortSet(cohortDateRange = )"
    )
  }
  if (lifecycle::is_present(limit)) {
    lifecycle::deprecate_warn(
      when = "0.6.2", what = "generateDrugUtilisationCohortSet(limit = )"
    )
  }

  priorObservation <- 0
  cohortDateRange <- as.Date(c(NA, NA))

  checkInputs(
    cdm = cdm, name = name, conceptSet = conceptSet,
    limit = "all", priorObservation = priorObservation, gapEra = gapEra,
    priorUseWashout = priorUseWashout, cohortDateRange = cohortDateRange,
    imputeDuration = "none", durationRange = c(1, Inf)
  )

  # get conceptSet
  cohortSet <- dplyr::tibble(cohort_name = names(conceptSet)) |>
    dplyr::mutate(cohort_definition_id = dplyr::row_number()) |>
    dplyr::select("cohort_definition_id", "cohort_name") |>
    dplyr::mutate(
      duration_range_min = as.character(1),
      duration_range_max = as.character(Inf),
      impute_duration = "none",
      gap_era = as.character(.env$gapEra),
      prior_use_washout = as.character(.env$priorUseWashout),
      prior_observation = as.character(dplyr::coalesce(
        .env$priorObservation, as.numeric(NA)
      )),
      cohort_date_range_start = as.character(.env$cohortDateRange[1]),
      cohort_date_range_end = as.character(.env$cohortDateRange[2]),
      limit = "all"
    )

  conceptSet <- conceptSetFromConceptSetList(conceptSet, cohortSet)

  cdm[[name]] <- subsetTables(
    cdm, conceptSet, name
  ) |>
    omopgenerics::newCohortTable(cohortSetRef = cohortSet) |>
    erafyCohort(gapEra) |>
    requirePriorUseWashout(priorUseWashout) |>
    requirePriorObservation(priorObservation) |>
    trimStartDate(cohortDateRange[1]) |>
    trimEndDate(cohortDateRange[2])

  dropTmpTables(cdm)

  return(cdm)
}

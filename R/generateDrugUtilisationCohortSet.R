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

#' Generates a cohort of the drug use of a certain list of concepts.
#'
#' @param cdm A cdm_reference object.
#' @param name Name of the GeneratedCohortSet
#' @param conceptSet Named list of concept sets.
#' @param gapEra Number of days between two continuous exposures to be
#' considered in the same era.
#' @param durationRange Deprecated.
#' @param imputeDuration Deprecated.
#' @param priorUseWashout Deprecated.
#' @param priorObservation Deprecated.
#' @param cohortDateRange Deprecated.
#' @param limit Deprecated.
#'
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
      what = "generateDrugUtilisationCohortSet(durationRange = )"
    )
  }
  if (lifecycle::is_present(imputeDuration)) {
    lifecycle::deprecate_warn(
      when = "0.7.0", what = "generateDrugUtilisationCohortSet(imputeDuration = )"
    )
  }
  if (lifecycle::is_present(priorUseWashout)) {
    lifecycle::deprecate_warn(
      when = "0.7.0",
      what = "generateDrugUtilisationCohortSet(priorUseWashout = )",
      with = "requirePriorDrugWashout()"
    )
  }
  if (lifecycle::is_present(priorObservation)) {
    lifecycle::deprecate_warn(
      when = "0.7.0",
      what = "generateDrugUtilisationCohortSet(priorObservation = )",
      with = "requireObservationBeforeDrug()"
    )
  }
  if (lifecycle::is_present(cohortDateRange)) {
    lifecycle::deprecate_warn(
      when = "0.7.0",
      what = "generateDrugUtilisationCohortSet(cohortDateRange = )",
      with = "requireDrugInDateRange()"
    )
  }
  if (lifecycle::is_present(limit)) {
    lifecycle::deprecate_warn(
      when = "0.7.0",
      what = "generateDrugUtilisationCohortSet(limit = )",
      with = "requireIsFirstDrugEntry()"
    )
  }

  checkInputs(cdm = cdm, name = name, conceptSet = conceptSet, gapEra = gapEra)

  # get conceptSet
  cohortSet <- dplyr::tibble(cohort_name = names(conceptSet)) |>
    dplyr::mutate(cohort_definition_id = dplyr::row_number()) |>
    dplyr::select("cohort_definition_id", "cohort_name") |>
    dplyr::mutate(gap_era = as.character(.env$gapEra))

  conceptSet <- conceptSetFromConceptSetList(conceptSet, cohortSet)

  cohortCodelistAttr <- cohortSet |>
    dplyr::select("cohort_definition_id", "codelist_name" = "cohort_name") |>
    dplyr::inner_join(
      conceptSet |>
        dplyr::rename("concept_id" = "drug_concept_id"),
      by = "cohort_definition_id",
      relationship = "one-to-many"
    ) |>
    dplyr::mutate("type" = "index event")

  cdm[[name]] <- subsetTables(cdm, conceptSet, name) |>
    omopgenerics::newCohortTable(
      cohortSetRef = cohortSet, cohortCodelistRef = cohortCodelistAttr
    ) |>
    erafyCohort(gapEra)

  dropTmpTables(cdm)

  return(cdm)
}

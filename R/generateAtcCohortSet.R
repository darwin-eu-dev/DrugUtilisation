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

#' Generates a cohort of the drug use of ATC name(s) of interest.
#'
#' @param cdm A cdm_reference object.
#' @param name Name of the GeneratedCohortSet
#' @param atcName Names of ATC of interest.
#' @param durationRange Deprecated.
#' @param imputeDuration Deprecated.
#' @param gapEra Number of days between two continuous exposures to be
#' considered in the same era.
#' @param priorUseWashout Prior days without exposure.
#' @param priorObservation Deprecated.
#' @param cohortDateRange Deprecated.
#' @param limit Deprecated.
#' @param level ATC level. Can be one or more of "ATC 1st", "ATC 2nd",
#' "ATC 3rd", "ATC 4th", and "ATC 5th"
#' @param doseForm Only descendants codes with the specified dose form
#' will be returned. If NULL, descendant codes will be returned regardless
#' of dose form.
#' @return The function returns the 'cdm' object with the created cohorts as
#' references of the object.
#' @export
#' @examples
#' \donttest{
#' library(DrugUtilisation)
#' cdm <- mockDrugUtilisation()
#' cdm <- generateAtcCohortSet(cdm, name =  "test", priorObservation = 365)
#' cdm
#' cdm$test
#' settings(cdm$test)
#' }
generateAtcCohortSet <- function(cdm,
                                 name,
                                 atcName = NULL,
                                 durationRange = lifecycle::deprecated(),
                                 imputeDuration = lifecycle::deprecated(),
                                 gapEra = 0,
                                 priorUseWashout = 0,
                                 priorObservation = lifecycle::deprecated(),
                                 cohortDateRange = lifecycle::deprecated(),
                                 limit = lifecycle::deprecated(),
                                 level = c("ATC 1st"),
                                 doseForm = NULL) {

  if (lifecycle::is_present(durationRange)) {
    lifecycle::deprecate_warn(
      when = "0.6.2", what = "generateAtcCohortSet(durationRange = )"
    )
  }
  if (lifecycle::is_present(imputeDuration)) {
    lifecycle::deprecate_warn(
      when = "0.6.2", what = "generateAtcCohortSet(imputeDuration = )"
    )
  }
  if (lifecycle::is_present(priorObservation)) {
    lifecycle::deprecate_warn(
      when = "0.6.2", what = "generateAtcCohortSet(priorObservation = )"
    )
  }
  if (lifecycle::is_present(cohortDateRange)) {
    lifecycle::deprecate_warn(
      when = "0.6.2", what = "generateAtcCohortSet(cohortDateRange = )"
    )
  }
  if (lifecycle::is_present(limit)) {
    lifecycle::deprecate_warn(
      when = "0.6.2", what = "generateDrugUtilisationCohortSet(limit = )"
    )
  }

  conceptSet <- CodelistGenerator::getATCCodes(cdm,
    name = atcName,
    level = level,
    doseForm = doseForm,
    withConceptDetails = FALSE
  )

  cdm <- DrugUtilisation::generateDrugUtilisationCohortSet(
    cdm = cdm,
    name = name,
    conceptSet = conceptSet,
    priorUseWashout = priorUseWashout,
    gapEra = gapEra
  )

  cdm[[name]] <- cdm[[name]] |>
    omopgenerics::newCohortTable(
      cohortSetRef = settings(cdm[[name]]) |>
        dplyr::mutate("dose_form" = paste0(.env$doseForm, collapse = " "))
    )

  return(cdm)
}


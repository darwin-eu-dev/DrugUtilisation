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

#' Generate a set of drug cohorts based on ATC classification
#'
#' @description
#' Adds a new cohort table to the cdm reference with individuals who have drug
#' exposure records that belong to the specified Anatomical Therapeutic Chemical
#' (ATC) classification. Cohort start and end dates will be based on drug record
#' start and end dates, respectively. Records that overlap or have fewer days
#' between them than the specified gap era will be concatenated into a single
#' cohort entry.
#'
#' @param cdm A cdm reference.
#' @param name The name of the new cohort table to add to the cdm reference.
#' @param atcName Names of ATC classification of interest.
#' @param level ATC level. Can be one or more of "ATC 1st", "ATC 2nd",
#' "ATC 3rd", "ATC 4th", and "ATC 5th"
#' @param doseForm Only descendants codes with the specified dose form
#' will be returned. If NULL, descendant codes will be returned regardless
#' of dose form.
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
#' cdm <- generateAtcCohortSet(
#'   cdm = cdm,
#'   atcName = "alimentary tract and metabolism",
#'   name = "drugs"
#' )
#'
#' cdm$drugs |>
#'   glimpse()
#' }
generateAtcCohortSet <- function(cdm,
                                 name,
                                 atcName = NULL,
                                 level = c("ATC 1st"),
                                 doseForm = NULL,
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
      what = "generateAtcCohortSet(durationRange = )"
    )
  }
  if (lifecycle::is_present(imputeDuration)) {
    lifecycle::deprecate_warn(
      when = "0.7.0", what = "generateAtcCohortSet(imputeDuration = )"
    )
  }
  if (lifecycle::is_present(priorUseWashout)) {
    lifecycle::deprecate_warn(
      when = "0.7.0",
      what = "generateAtcCohortSet(priorUseWashout = )",
      with = "requirePriorDrugWashout()"
    )
  }
  if (lifecycle::is_present(priorObservation)) {
    lifecycle::deprecate_warn(
      when = "0.7.0",
      what = "generateAtcCohortSet(priorObservation = )",
      with = "requireObservationBeforeDrug()"
    )
  }
  if (lifecycle::is_present(cohortDateRange)) {
    lifecycle::deprecate_warn(
      when = "0.7.0",
      what = "generateAtcCohortSet(cohortDateRange = )",
      with = "requireDrugInDateRange()"
    )
  }
  if (lifecycle::is_present(limit)) {
    lifecycle::deprecate_warn(
      when = "0.7.0",
      what = "generateAtcCohortSet(limit = )",
      with = "requireIsFirstDrugEntry()"
    )
  }

  conceptSet <- CodelistGenerator::getATCCodes(cdm,
    name = atcName,
    level = level,
    doseForm = doseForm
  )

  cdm <- DrugUtilisation::generateDrugUtilisationCohortSet(
    cdm = cdm,
    name = name,
    conceptSet = conceptSet,
    gapEra = gapEra
  )

  cdm[[name]] <- cdm[[name]] |>
    omopgenerics::newCohortTable(
      cohortSetRef = settings(cdm[[name]]) |>
        dplyr::mutate("dose_form" = paste0(.env$doseForm, collapse = " "))
    )

  return(cdm)
}

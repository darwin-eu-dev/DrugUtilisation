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
#' @param atcName Names of ATC of interest.
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
#' cdm <- generateAtcCohortSet(cdm, name =  "test")
#' cdm
#' cdm$test
#' settings(cdm$test)
#' }
generateAtcCohortSet <- function(cdm,
                                 name,
                                 atcName = NULL,
                                 durationRange = c(1, Inf),
                                 imputeDuration = "none",
                                 gapEra = 0,
                                 priorUseWashout = 0,
                                 priorObservation = 0,
                                 cohortDateRange = as.Date(c(NA, NA)),
                                 limit = "all",
                                 level = c("ATC 1st"),
                                 doseForm = NULL) {
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
    durationRange = durationRange,
    imputeDuration = imputeDuration,
    gapEra = gapEra,
    priorUseWashout = priorUseWashout,
    priorObservation = priorObservation,
    cohortDateRange = cohortDateRange,
    limit = limit
  )

  cdm[[name]] <- cdm[[name]] |>
    omopgenerics::newCohortTable(
      cohortSetRef = settings(cdm[[name]]) |>
        dplyr::mutate("dose_form" = paste0(.env$doseForm, collapse = " "))
    )

  return(cdm)
}


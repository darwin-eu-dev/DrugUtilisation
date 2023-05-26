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

#' This function is used to summarise the dose and/or indication over multiple
#' cohorts.
#'
#' @param cohort A cohort in the cdm
#' @param cdm A cdm_reference created by CDMConnector
#' @param strata Stratification list
#' @param ageGroup A list of age groups.
#' @param windowVisitOcurrence Window to count visit occurrences.
#' @param covariates Named list of windows to check covariates. The name must
#' point to a cohortTableName in the cdm.
#' @param minimumCellCount minimum counts due to obscure
#'
#' @return A summary of the characteristics of the individuals
#'
#' @export
#'
#' @examples
#' \donttest{
#' cdm <- mockDrugUtilisation(numberIndividuals = 1000)
#' summariseTableOne(
#'   cdm$cohort1,
#'   cdm,
#'   ageGroup = list(c(0, 19), c(20, 39), c(40, 59), c(60, 79), c(80, 150)),
#'   windowVisitOcurrence = c(-180, 0)
#' )
#' }
summariseTableOne <- function(cohort,
                              cdm,
                              strata = list(),
                              ageGroup = NULL,
                              windowVisitOcurrence = NULL,
                              covariates = list(),
                              minimumCellCount = 5) {
  # check initial tables
  checkInputs(
    cohort = cohort, cdm = cdm, strata = strata, ageGroup = ageGroup,
    windowVisitOcurrence = windowVisitOcurrence, covariates = covariates,
    minimumCellCount = minimumCellCount
  )

  # add characteristics
  cohort <- cohort %>%
    PatientProfiles::addDemographics(cdm) %>%
    PatientProfiles::addTableIntersectFlag(
      cdm, "visit_occurrence", window = windowVisitOcurrence
    )
  for (k in seq_along(covariates)) {
    cohort <- cohrot %>%
      PatientProfiles::addCohortIntersect(
        cdm, names(covariates)[k], window = covariates[[k]]
      )
  }

  # summarise results
  results <- cohort %>%
    dplyr::collect() %>%
    PatientProfiles::summariseCharacteristics(
      cdm = cdm, suppressCellCount = minimumCellCount
    )

  return(results)
}

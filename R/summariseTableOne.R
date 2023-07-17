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

#' Summarise characteristics of individuals
#' It is an alias of [PatientProfiles::summariseCharacteristics()]
#'
#' @param cohort A cohort in the cdm
#' @param cdm A cdm reference.
#' @param strata Stratification list
#' @param ageGroup A list of age groups.
#' @param tableIntersect A list of arguments that uses addTableIntersect
#' function to add covariates and comorbidities.
#' @param cohortIntersect A list of arguments that uses addCohortIntersect
#' function to add covariates and comorbidities.
#' @param minCellCount minimum counts due to obscure
#'
#' @return A summary of the characteristics of the individuals
#'
#' @export
#'
#' @examples
#' \donttest{
#' library(DrugUtilisation)
#'
#' cdm <- mockDrugUtilisation()
#'
#' summariseTableOne(
#'   cohort = cdm$cohort1,
#'   ageGroup = list(c(0, 19), c(20, 39), c(40, 59), c(60, 79), c(80, 150)),
#'   tableIntersect = list(
#'     "Visits" = list(
#'       tableName = "visit_occurrence", value = "count", window = c(-365, 0)
#'      )
#'   ),
#'   cohortIntersect = list(
#'     "Medications" = list(
#'       targetCohortTable = "cohort2", value = "flag", window = c(-365, 0)
#'     )
#'   )
#' )
#' }
summariseTableOne <- function(cohort,
                              cdm = attr(cohort, "cdm_reference"),
                              strata = list(),
                              ageGroup = NULL,
                              tableIntersect = list(
                                "Visit" = list(
                                  tableName = "visit_occurrence",
                                  value = "count", window = c(-365, 0)
                                )
                              ),
                              cohortIntersect = list(),
                              minCellCount = 5) {
  PatientProfiles::summariseCharacteristics(
    cohort = cohort, cdm = cdm, strata = strata, ageGroup = ageGroup,
    tableIntersect = tableIntersect, cohortIntersect = cohortIntersect,
    minCellCount = minCellCount
  )
}

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
#' @param cohort A cohort table in a cdm reference.
#' @param priorUseWashout The length of priorUseWashout to be applied.
#' @param cohortId IDs of the cohorts to modify. The default is NULL meaning all
#' cohorts will be used; otherwise, only the specified cohorts will be modified,
#' and the rest will remain unchanged.
#' @param name Name of the new cohort with the priorUseWashout Default name is the original
#' cohort name.
#' @return The function returns the cohort after applying the specified priorUseWashout.
#'
#' @export
#'
#' @examples
#' \donttest{
#' cdm <- mockDrugUtilisation()
#' cdm$cohort1 <- cdm$cohort1 |>
#'   requirePriorDrugWashout(priorUseWashout = 90, cohortId = c(2,3))
#' attrition(cdm$cohort1)
#' CDMConnector::cdmDisconnect(cdm = cdm)
#' }
#'
requirePriorDrugWashout <- function(cohort,
                                    priorUseWashout,
                                    cohortId = NULL,
                                    name = omopgenerics::tableName(cohort)) {
  # check inputs
  cdm <- omopgenerics::cdmReference(cohort)
  checkInputs(
    cohort = cohort, cohortId = cohortId, priorUseWashout = priorUseWashout, name = name
  )
  if (is.null(cohortId)) {
    cohortId <- settings(cohort) |> dplyr::pull("cohort_definition_id")
  }

  reason <- "require prior use priorUseWashout of {priorUseWashout} day{?s}"

  record_counts <- omopgenerics::cohortCount(cohort) |>
    dplyr::filter(.data$cohort_definition_id %in% cohortId) |>
    dplyr::pull("number_records")

  if (any(record_counts > 0) & priorUseWashout > 0) {
    cohort <- cohort |>
      dplyr::anti_join(
        cohort |>
          dplyr::filter(.data$cohort_definition_id %in% .env$cohortId) |>
          dplyr::group_by(.data$cohort_definition_id, .data$subject_id) |>
          dplyr::mutate("prior_end_date" = dplyr::lead(
            .data$cohort_end_date, order_by = .data$cohort_start_date)) |>
          dplyr::ungroup() %>%
          dplyr::mutate(prior_time = !!CDMConnector::datediff(
            "prior_end_date", "cohort_start_date")) |>
          dplyr::filter(.data$prior_time < .env$priorUseWashout) |>
          dplyr::select("cohort_definition_id", "subject_id", "cohort_start_date"),
        by = c("cohort_definition_id", "subject_id", "cohort_start_date")
      )
  }

  cohort <- cohort |>
    dplyr::compute(name = name, temporary = FALSE) |>
    omopgenerics::recordCohortAttrition(reason = reason)

  return(cohort)
}

#' Title
#'
#' @param cohort
#' @param cohortId
#' @param name
#'
#' @return
#' @export
#'
#' @examples
requireIsFirstDrugEntry <- function(cohort,
                                    cohortId = NULL,
                                    name = omopgenerics::tableName(cohort)) {

}

#' Title
#'
#' @param cohort
#' @param prioObservation
#' @param cohortId
#' @param name
#'
#' @return
#' @export
#'
#' @examples
requireObservationBeforeDrug <- function(cohort,
                                         prioObservation,
                                         cohortId = NULL,
                                         name = omopgenerics::tableName(cohort)) {

}

#' Title
#'
#' @param cohort
#' @param dateRange
#' @param cohortId
#' @param name
#'
#' @return
#' @export
#'
#' @examples
requireDrugInDateRange <- function(cohort,
                                   dateRange,
                                   cohortId = NULL,
                                   name = omopgenerics::tableName(cohort)) {

}

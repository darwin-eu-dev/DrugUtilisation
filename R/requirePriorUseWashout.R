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
#' requirePriorUseWashout(cohort = cdm$cohort1, priorUseWashout = 90, cohortId = c(2,3))
#' CDMConnector::cdmDisconnect(cdm = cdm)
#' }
#'
requirePriorUseWashout <- function(cohort,
                                   priorUseWashout,
                                   cohortId = NULL,
                                   name = tableName(cohort)) {

  cdm <- omopgenerics::cdmReference(cohort)

  checkInputs(
    cohort = cohort, cohortId = cohortId, priorUseWashout = priorUseWashout, name = name
  )

  if (is.null(cohortId)){
    record_counts <- omopgenerics::cohortCount(cohort) |>
      dplyr::pull("number_records")
  } else {
    record_counts <- omopgenerics::cohortCount(cohort) |>
      dplyr::filter(.data$cohort_definition_id %in% cohortId) |>
      dplyr::pull("number_records")
  }

  if (all(record_counts == 0)){
    cli::cli_inform("The specified cohorts are all empty so priorUseWashout
    cannot be applied. Suggest rechecking the cohort.")
    if (name == tableName(cohort)) {
      return(cohort)
    } else if (name != tableName(cohort)){
      cohort <- cohort |>
        dplyr::compute(name = name, temporary = FALSE, overwrite = TRUE)
      return(cohort)
    }
  } else if(priorUseWashout == 0){
    cli::cli_inform("Washout specified is 0 meaning nothing will be applied.
                   Suggest rechecking the priorUseWashout.")
    if (name == tableName(cohort)) {
      return(cohort)
    } else if (name != tableName(cohort)){
      cohort <- cohort |>
        dplyr::compute(name = name, temporary = FALSE, overwrite = TRUE)
      return(cohort)
    }
  } else if (is.null(cohortId)){
    cohort <- cohort |>
      dplyr::group_by(.data$cohort_definition_id, .data$subject_id) |>
      dplyr::arrange(.data$cohort_start_date) |>
      dplyr::mutate(id = dplyr::row_number()) |>
      dplyr::compute(name = uniqueTmpName(), temporary = FALSE, overwrite = TRUE)
    cohort <- cohort |>
      dplyr::left_join(
        cohort |>
          dplyr::mutate(id = .data$id + 1) |>
          dplyr::select(
            "cohort_definition_id", "subject_id", "id",
            "prior_date" = "cohort_end_date"
          ),
        by = c("cohort_definition_id", "subject_id", "id")
      ) %>%
      dplyr::mutate(
        prior_time = !!CDMConnector::datediff("prior_date", "cohort_start_date")
      )
    if (is.infinite(priorUseWashout)) {
      cohort <- cohort |>
        dplyr::filter(is.na(.data$prior_date))
    } else {
      cohort <- cohort |>
        dplyr::filter(
          is.na(.data$prior_date) | .data$prior_time >= .env$priorUseWashout
        )
    }
    cohort <- cohort |>
      dplyr::select(-c("id", "prior_date", "prior_time")) |>
      dplyr::arrange() |>
      dplyr::ungroup() |>
      dplyr::compute(name = name, temporary = FALSE, overwrite = TRUE)
    res <- paste("require prior use priorUseWashout of", priorUseWashout, "days")
    cohort <- cohort |>
      omopgenerics::recordCohortAttrition(reason = res)

    omopgenerics::dropTable(cdm = cdm, name = dplyr::starts_with("tmp_"))
    return(cohort)
  } else {
    presentIds <- omopgenerics::settings(cohort) |>
      dplyr::pull("cohort_definition_id")
    excludedIds <- setdiff(presentIds, cohortId)
    included <- cohort |>
      CohortConstructor::subsetCohorts(cohortId = cohortId, name = "tmp_included")
    excluded <- cohort |>
      CohortConstructor::subsetCohorts(cohortId = excludedIds, name = "tmp_excluded")

    included <- included |>
      dplyr::group_by(.data$cohort_definition_id, .data$subject_id) |>
      dplyr::arrange(.data$cohort_start_date) |>
      dplyr::mutate(id = dplyr::row_number()) |>
      dplyr::compute(name = uniqueTmpName(), temporary = FALSE, overwrite = TRUE)
    included <- included |>
      dplyr::left_join(
        included |>
          dplyr::mutate(id = .data$id + 1) |>
          dplyr::select(
            "cohort_definition_id", "subject_id", "id",
            "prior_date" = "cohort_end_date"
          ),
        by = c("cohort_definition_id", "subject_id", "id")
      ) %>%
      dplyr::mutate(
        prior_time = !!CDMConnector::datediff("prior_date", "cohort_start_date")
      )
    if (is.infinite(priorUseWashout)) {
      included <- included |>
        dplyr::filter(is.na(.data$prior_date))
    } else {
      included <- included |>
        dplyr::filter(
          is.na(.data$prior_date) | .data$prior_time >= .env$priorUseWashout)
    }
    included <- included |>
        dplyr::select(-c("id", "prior_date", "prior_time")) |>
        dplyr::arrange() |>
        dplyr::ungroup() |>
        dplyr::compute(name = uniqueTmpName(), temporary = FALSE, overwrite = TRUE)
      res <- paste("require prior use priorUseWashout of", priorUseWashout, "days")
      included <- included |>
        omopgenerics::recordCohortAttrition(reason = res)

      cdm <- omopgenerics::bind(included, excluded, name = name)

      cohort <- cdm[[name]]

      omopgenerics::dropTable(cdm = cdm, name = dplyr::starts_with("tmp_"))

      return(cohort)
    }
}

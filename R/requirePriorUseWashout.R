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
#' @param washout The length of washout to be applied.
#' @param name Name of the new cohort with the washout. Default name is the original
#' cohort name.
#' @return The function returns the cohort after applying the specified washout.
#'
#' @export
#'
#' @examples
#' \donttest{
#' cdm <- mockDrugUtilisation()
#' requirePriorUseWashout(cohort = cdm$cohort1, washout = 90)
#' CDMConnector::cdmDisconnect(cdm = cdm)
#' }
#'
requirePriorUseWashout <- function(cohort,
                                   washout,
                                   name = tableName(cohort)) {
  checkInputs(
    cohort = cohort, priorUseWashout = washout, name = name
  )

  if ((sumcounts(cohort) == 0) & (name !=tableName(cohort))){
    cli::cli_abort("Cohort is empty so washout cannot be applied.
                   Suggest rechecking the cohort.")
  }

  if (washout == 0 & (name !=tableName(cohort))){
    cli::cli_abort("Washout specified is 0 meaning nothing will be applied.
                   Suggest rechecking the washout.")
  }

  if (sumcounts(cohort) > 0 & washout > 0) {
    cohort <- cohort %>%
      dplyr::group_by(.data$cohort_definition_id, .data$subject_id) %>%
      dplyr::arrange(.data$cohort_start_date) %>%
      dplyr::mutate(id = dplyr::row_number()) %>%
      dplyr::compute(name = uniqueTmpName(), temporary = FALSE, overwrite = TRUE)
    cohort <- cohort %>%
      dplyr::left_join(
        cohort %>%
          dplyr::mutate(id = .data$id + 1) %>%
          dplyr::select(
            "cohort_definition_id", "subject_id", "id",
            "prior_date" = "cohort_end_date"
          ),
        by = c("cohort_definition_id", "subject_id", "id")
      ) %>%
      dplyr::mutate(
        prior_time = !!CDMConnector::datediff("prior_date", "cohort_start_date")
      )
    if (is.infinite(washout)) {
      cohort <- cohort %>%
        dplyr::filter(is.na(.data$prior_date))
    } else {
      cohort <- cohort %>%
        dplyr::filter(
          is.na(.data$prior_date) | .data$prior_time >= .env$washout
        )
    }
    cohort <- cohort %>%
      dplyr::select(-c("id", "prior_date", "prior_time")) %>%
      dplyr::arrange() %>%
      dplyr::ungroup() %>%
      dplyr::compute(name = name, temporary = FALSE, overwrite = TRUE)
    res <- paste("require prior use washout of", washout, "days")
    cohort <- cohort |>
      omopgenerics::recordCohortAttrition(reason = res)
  }
  return(cohort)
}

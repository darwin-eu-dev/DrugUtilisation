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

#' summarise proportion Of patients covered
#'
#' @param cohort A cohort table
#' @param cohortId Cohort definition id of interest. If NUll all cohorts will
#' be considered.
#' @param strata List of variables to stratify by.
#' @param followUpDays Number of days to follow up individuals for. If NULL the
#' maximum amount of days from an individuals first cohort start date to their
#' last cohort end date will be used
#'
#' @return A summarised result
#' @export
#'
summariseProportionOfPatientsCovered <- function(cohort,
                                                 cohortId = NULL,
                                                 strata = list(),
                                                 followUpDays = NULL){

 cohortIds <- omopgenerics::settings(cohort) |>
    dplyr::pull("cohort_definition_id")
 if(!is.null(cohortId)){
   cohortIds <- cohortIds[cohortIds %in% cohortId]
 }
 if(length(cohortIds) == 0){
   cli::cli_abort("Cohort ID not found")
 }

 cdm <- omopgenerics::cdmReference(cohort)

 analysisSettings <- dplyr::tibble(
   "result_id" = 1L,
   "result_type" = "proportion_of_patients_covered",
   "package_name" = "DrugUtilisation",
   "package_version" = as.character(utils::packageVersion("DrugUtilisation"))
 )
 if(nrow(cohort |> utils::head(1) |> dplyr::collect()) == 0){
   cli::cli_warn("No records found in cohort table")
  return(
    omopgenerics::emptySummarisedResult(settings = analysisSettings)
     )
 }

 if(is.null(followUpDays)){
  # get maximum days of time in cohort following first entry for each cohort
 maxDays <- cohort %>%
    dplyr::mutate(days_in_cohort = as.integer(!!CDMConnector::datediff(
      start = "cohort_start_date", end = "cohort_end_date", interval = "day"
    ))) %>%
   dplyr::group_by(.data$cohort_definition_id, .data$subject_id) |>
   dplyr::summarise(days = sum(.data$days_in_cohort, na.rm = TRUE)) |>
   dplyr::group_by(.data$cohort_definition_id)  |>
   dplyr::summarise(max_days = max(.data$days, na.rm = TRUE)) |>
   dplyr::collect()
 } else {
   maxDays <- omopgenerics::settings(cohort) |>
     dplyr::mutate(max_days = .env$followUpDays) |>
     dplyr::select("cohort_definition_id", "max_days")
 }

 #
 ppc <- list()
 for(j in seq_along(cohortIds)){
 workingCohortId <- cohortIds[j]
 workingMaxDays <- maxDays |>
   dplyr::filter(.data$cohort_definition_id == .env$workingCohortId) |>
   dplyr::pull()

 ppc[[j]] <- getPPC(cohort,
        cohortId = workingCohortId,
        days = workingMaxDays)
 }

 ppc <- dplyr::bind_rows(ppc) |>
   dplyr::mutate(ppc = .data$numerator_count / .data$denominator_count) |>
   tidyr::pivot_longer(c("numerator_count",
                         "denominator_count",
                         "ppc"),
                       names_to = "estimate_type",
                       values_to = "estimate_value")


 ppc <- ppc |>
   dplyr::mutate(
     result_id = 1L,
     cdm_name = omopgenerics::cdmName(cdm),
     group_name = "cohort",
     group_level = .data$cohort_name,
     strata_name = "overall",
     strata_level = "overall",
     variable_name = "overall",
     variable_level = "overall",
     estimate_name = .data$estimate_type,
     estimate_type = "numeric",
     estimate_value = as.character(.data$estimate_value),
     additional_name = "time",
     additional_level = as.character(.data$time)) |>
   dplyr::select(omopgenerics::resultColumns())


 ppc <- omopgenerics::newSummarisedResult(ppc,
                                          settings = analysisSettings)

 ppc

 }


getPPC <- function(cohort, cohortId, days){

  result <- list()

  workingCohort <- cohort |>
    dplyr::filter(.data$cohort_definition_id == .env$cohortId) |>
    PatientProfiles::addFutureObservationQuery(futureObservationName = "observation_end_date",
                                               futureObservationType = "date") |>
    dplyr::collect()

  workingCohortName <- omopgenerics::settings(cohort) |>
    dplyr::filter(.data$cohort_definition_id == .env$cohortId) |>
    dplyr::pull("cohort_name")

  print(paste0("Getting PPC for cohort ", workingCohortName))

  # add result for day zero
  startN <- workingCohort |>
    dplyr::select("subject_id")|>
    dplyr::distinct() |>
    dplyr::tally() |>
    dplyr::pull("n")
  result[["time_0"]] <- dplyr::tibble(time = 0,
                                    numerator_count = .env$startN,
                                    denominator_count = .env$startN)
  for(i in seq_along(1:days)){
   c <- workingCohort |>
      dplyr::mutate(working_date = clock::add_days(.data$cohort_start_date, i)) |>
      dplyr::mutate(in_cohort = dplyr::if_else(
        .data$cohort_start_date <= .data$working_date &
          .data$cohort_end_date >= .data$working_date, 1, 0
      ),
      in_observation =  dplyr::if_else(
        .data$observation_end_date >= .data$working_date, 1, 0
      ))


    # people still in observation
    denominator <- c |>
      dplyr::filter(.data$in_observation == 1) |>
      dplyr::select("subject_id") |>
      dplyr::distinct() |>
      dplyr::tally() |>
      dplyr::pull("n")

    # people in cohort on date
    numerator <- c |>
      dplyr::filter(.data$in_cohort == 1) |>
      dplyr::select("subject_id") |>
      dplyr::distinct() |>
      dplyr::tally() |>
      dplyr::pull("n")

    result[[paste0("time_", i)]] <- dplyr::tibble(time = i,
                  numerator_count = .env$numerator,
                  denominator_count = .env$denominator)

  }


  result <- dplyr::bind_rows(result)

  result <- result |>
    dplyr::mutate(cohort_name = .env$workingCohortName)

  result

}

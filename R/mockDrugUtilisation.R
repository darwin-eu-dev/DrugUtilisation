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

#' It creates a mock database for testing drugutilisation package
#'
#' @param connectionDetails Details of the connection
#' @param drug_exposure default null user can define its own table
#' @param drug_strength default null user can define its own table
#' @param observation_period default null user can define its own table
#' @param condition_occurrence default null user can define its own table
#' @param visit_occurrence default null user can define its own visit_occurrence table
#' @param person default null user can define its own table
#' @param drug_concept_id_size number of unique drug concept id
#' @param ingredient_concept_id_size number of unique drug ingredient concept id
#' @param drug_exposure_size number of unique drug exposure
#' @param patient_size number of unique patient
#' @param min_drug_exposure_start_date user define minimum drug exposure start date
#' @param max_drug_exposure_start_date user define maximium drug exposure start date
#' @param seed seed
#' @param condition_concept_id_size number of unique row in the condition concept table
#' @param visit_concept_id_size number of unique visit concept id
#' @param visit_occurrence_id_size number of unique visit occurrence id
#' @param earliest_date_of_birth the earliest date of birth of patient in person table format "dd-mm-yyyy"
#' @param latest_date_of_birth the latest date of birth for patient in person table format "dd-mm-yyyy"
#' @param earliest_observation_start_date the earliest observation start date for patient format "dd-mm-yyyy"
#' @param latest_observation_start_date the latest observation start date for patient format "dd-mm-yyyy"
#' @param min_days_to_observation_end the minimum number of days of the observational integer
#' @param max_days_to_observation_end the maximum number of days of the observation period integer
#' @param earliest_condition_start_date the earliest condition start date for patient format "dd-mm-yyyy"
#' @param earliest_visit_start_date the earliest visit start date for patient format "dd-mm-yyyy"
#' @param latest_condition_start_date the latest condition start date for patient format "dd-mm-yyyy"
#' @param latest_visit_start_date the latest visit start date for patient format "dd-mm-yyyy"
#' @param min_days_to_condition_end the minimum number of days of the condition integer
#' @param min_days_to_visit_end the minimum number of days of the visit integer
#' @param max_days_to_condition_end the maximum number of days of the condition integer
#' @param max_days_to_visit_end the maximum number of days of the visit integer

#' @param concept_ancestor the concept ancestor table
#' @param ancestor_concept_id_size the size of ceoncept ancestor table

#' @param cohort1 cohort table for test to run in getindication
#' @param cohort2 cohort table for test to run in getindication
#' @return
#' @export
#'
#' @examples
mockDrugUtilisation <- function(connectionDetails = list(
                                  db = DBI::dbConnect(duckdb::duckdb(), ":memory:"),
                                  writeSchema = "main",
                                  writePrefix = NULL
                                ),
                                numberIndividuals = 10,
                                seed = 1,
                                concept = NULL,
                                concept_ancestor = NULL,
                                drug_strength = NULL,
                                person = NULL,
                                observation_period = NULL,
                                drug_exposure = NULL,
                                condition_occurrence = NULL,
                                ...) {

  # get vocabulary
  vocab <- vocabularyTables(concept, concept_ancestor, drug_strength)
  concept <- vocab$concept
  concept_ancestor <- vocab$concept_ancestor
  drug_strength <- vocab$drug_strength

  # set seed
  set.seed(seed)

  # create person if NULL
  if (is.null(person)) {
    person <- createPersonTable(numberIndividuals)
  }

  # create observation_period if NULL
  if (is.null(observation_period)) {
    observation_period <- createObservationPeriod(person)
  }

  # create drug_exposure if NULL
  if (is.null(drug_exposure)) {
    drug_exposure <- createDrugExposure(person, concept, drug_strength)
  }

  # create condition_occurrence if NULL
  if (is.null(condition_occurrence)) {
    condition_occurrence <- createConditionOccurrence(person, concept)
  }

  visit_occurrence <- createVisitOccurrence(condition_occurrence, drug_exposure)

  cohorts <- list(...)

  listTables <- c(
    "concept", "concept_ancestor", "drug_strength", "person",
    "observation_period", "drug_exposure", "condition_occurrence",
    "visit_occurrence", names(cohorts)
  )

  for (newTable in listTables) {
    DBI::dbWriteTable(
      db, CDMConnector::inSchema(writeSchema, newTable, CDMConnector::dbms(db)),
      eval(parse(text = newTable)), overwrite = TRUE
    )
  }

  cdm <- CDMConnector::cdm_from_con(
    db,
    cdm_schema = writeSchema,
    cdm_tables = listTables[!(listTables %in% names(cohorts))],
    write_schema = writeSchema,
    cohort_tables = names(cohorts),
    write_prefix = connectionDetails$writePrefix
  )

  return(cdm)
}


#' To create the vocabulary tables
#' @noRd
vocabularyTables <- function(concept, concept_ancestor, drug_strength) {
  if (is.null(concept)) {
    concept <- mockConcept
  }
  if (is.null(concept_ancestor)) {
    concept_ancestor <- mockConceptAncestor
  }
  if (is.null(drug_strength)) {
    drug_strength <- mockDrugStrength
  }
  return(list(concept = concept, concept_ancestor = concept_ancestor, drug_strength = drug_strength))
}

#' To add the cohort set if NULL
#' @noRd
addCohortSet <- function(cohort) {
  attr(cohort, "cohort_set") <- cohort %>%
    dplyr::select("cohort_definition_id") %>%
    dplyr::distinct() %>%
    dplyr::mutate("cohort_name" = paste0(
      "cohort_",
      .data$cohort_definition_id
    ))
  return(cohort)
}

#' To add the cohort count if NULL
#' @noRd
addCohortCount <- function(cohort) {
  attr(cohort, "cohort_count") <- cohort %>%
    dplyr::group_by(.data$cohort_definition_id) %>%
    dplyr::summarise(
      number_records = dplyr::n(),
      number_subjects = dplyr::n_distinct(.data$subject_id),
      .groups = "drop"
    )
  return(cohort)
}

#' To add the cohort attrition if NULL
#' @noRd
addCohortAttrition <- function(cohort) {
  attr(cohort, "cohort_attrition") <- cohort %>%
    dplyr::group_by(.data$cohort_definition_id) %>%
    dplyr::summarise(
      number_records = dplyr::n(),
      number_subjects = dplyr::n_distinct(.data$subject_id),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      reason_id = 1, reason = "Qualifying initial records",
      excluded_records = 0, excluded_subjects = 0
    )
  return(cohort)
}

#' To create the person tables
#' @noRd
createPersonTable <- function(numberIndividuals) {
  person <- dplyr::tibble(
    person_id = 1:numberIndividuals,
    gender_concept_id = sample(c(8507, 8532), numberIndividuals, T),
    year_of_birth = sample(1950:2020, numberIndividuals, T),
    month_of_birth = sample(1:12, numberIndividuals, T),
    day_of_birth = sample(1:31, numberIndividuals, T),
    birth_datetime = as.Date(NA),
    race_concept_id = as.numeric(NA),
    ethnicity_concept_id = as.numeric(NA),
    location_id = as.numeric(NA),
    provider_id = as.numeric(NA),
    care_site_id = as.numeric(NA)
  ) %>%
    dplyr::mutate(
      birth_datetime = as.Date(
        paste0(.data$year_of_birth, "-01-01"), "%Y-%m-%d"
      ) +
      months(.data$month_of_birth - 1) +
      lubridate::days(.data$day_of_birth - 1)
    ) %>%
    dplyr::mutate(
      year_of_birth = lubridate::year(.data$birth_datetime),
      month_of_birth = lubridate::month(.data$birth_datetime),
      day_of_birth = lubridate::day(.data$birth_datetime)
    )
  return(person)
}

#' To create the observation period tables
#' @noRd
createObservationPeriod <- function(observation_period) {

}

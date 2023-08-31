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
#' @param numberIndividuals Number of individuals in the mock cdm
#' @param seed Seed for the random numbers
#' @param concept A concept tibble, if NULL a mock one is created
#' @param concept_ancestor A concept_ancestor tibble, if NULL a mock one is
#' created
#' @param drug_strength A drug_strength tibble, if NULL a mock one is created
#' @param person A person tibble, if NULL a mock one is created
#' @param observation_period A observation_period tibble, if NULL a mock one is
#' created
#' @param drug_exposure A drug_exposure tibble, if NULL a mock one is created
#' @param condition_occurrence A condition_occurrence tibble, if NULL a mock one
#' is created
#' @param observation A observation tibble, if NULL a mock one is created
#' @param extraTables Extra tibbles to be instantiated that are not cohorts or
#' cdm tables
#' @param ... Cohorts can be added to the cdm reference, cohort1 and cohort2
#' will be created if not provided
#'
#' @return A cdm reference with the mock tables
#'
#' @export
#'
#' @examples
#' \donttest{
#' library(DrugUtilisation)
#'
#' cdm <- mockDrugUtilisation()
#'
#' cdm
#' }
#'
mockDrugUtilisation <- function(connectionDetails = list(
                                  con = DBI::dbConnect(duckdb::duckdb(), ":memory:"),
                                  writeSchema = "main",
                                  cdmPrefix = NULL,
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
                                observation = NULL,
                                extraTables = list(),
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
  } else {
    person <- person %>%
      dplyr::mutate(birth_datetime = as.Date(
        paste(
          .data$year_of_birth,
          dplyr::if_else(is.na(.data$month_of_birth), 1, .data$month_of_birth),
          dplyr::if_else(is.na(.data$day_of_birth), 1, .data$day_of_birth),
          sep = "-"
        ),
        "%Y-%m-%d"
      ))
  }

  # create observation_period if NULL
  if (is.null(observation_period)) {
    observation_period <- createObservationPeriod(person)
  }

  # create drug_exposure if NULL
  if (is.null(drug_exposure)) {
    drug_exposure <- createDrugExposure(observation_period, concept)
  }

  # create condition_occurrence if NULL
  if (is.null(condition_occurrence)) {
    condition_occurrence <- createConditionOccurrence(
      observation_period, concept
    )
  }

  # create observation if NULL
  if (is.null(observation)) {
    observation <- createObservation(observation_period, concept)
  }

  visit_occurrence <- createVisitOccurrence(condition_occurrence, drug_exposure)

  cohorts <- list(...)
  cohorts <- createCohorts(cohorts, observation_period)

  listTables <- list(
    concept = concept, concept_ancestor = concept_ancestor,
    drug_strength = drug_strength, person = person,
    observation_period = observation_period, drug_exposure = drug_exposure,
    condition_occurrence = condition_occurrence,
    visit_occurrence = visit_occurrence, observation = observation
  )

  con <- connectionDetails$con
  writeSchema <- strsplit(connectionDetails[["writeSchema"]], "\\.")[[1]]
  if (length(writeSchema) == 2) {
    writeSchema <- c(
      catlog = writeSchema[1], schema = writeSchema[2],
      prefix = connectionDetails$writePrefix
    )
    cdmSchema <- c(
      catlog = writeSchema[1], schema = writeSchema[2],
      prefix = connectionDetails$cdmPrefix
    )
  } else {
    writeSchema <- c(
      schema = writeSchema[1], prefix = connectionDetails$writePrefix
    )
    cdmSchema <- c(
      schema = writeSchema[1], prefix = connectionDetails$cdmPrefix
    )
  }

  for (newTable in names(listTables)) {
    writeTable(con, cdmSchema, newTable, listTables[[newTable]])
  }
  for (nam in names(cohorts)) {
    writeTable(con, writeSchema, nam, cohorts[[nam]])
    writeTable(
      con, writeSchema, paste0(nam, "_set"), attr(cohorts[[nam]], "cohort_set")
    )
    writeTable(
      con, writeSchema, paste0(nam, "_attrition"),
      attr(cohorts[[nam]], "cohort_attrition")
    )
    writeTable(
      con, writeSchema, paste0(nam, "_count"),
      attr(cohorts[[nam]], "cohort_count")
    )
  }

  cdm <- CDMConnector::cdm_from_con(
    con,
    cdm_schema = cdmSchema,
    write_schema = writeSchema,
    cohort_tables = names(cohorts),
    cdm_name = "DUS MOCK"
  )

  for (newTable in names(extraTables)) {
    writeTable(con, writeSchema, newTable, extraTables[[newTable]])
    cdm[[newTable]] <- dplyr::tbl(
      con, CDMConnector::inSchema(writeSchema, newTable)
    )
  }

  return(cdm)
}

#' To write a table in the mock database
#' @noRd
writeTable <- function(con, writeSchema, name, x) {
  name <- CDMConnector::inSchema(writeSchema, name)
  DBI::dbWriteTable(conn = con, name = name, as.data.frame(x), overwrite = TRUE)
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
    day_of_birth = sample(1:365, numberIndividuals, T),
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
createObservationPeriod <- function(person) {
  person %>%
    dplyr::select("person_id", "birth_datetime") %>%
    dplyr::mutate(upper_limit = as.Date("2023-01-01")) %>%
    createDate(
      "observation_period_start_date", "birth_datetime", "upper_limit"
    ) %>%
    createDate(
      "observation_period_end_date", "observation_period_start_date",
      "upper_limit"
    ) %>%
    dplyr::mutate(
      observation_period_id = dplyr::row_number(),
      period_type_concept_id = 44814724
    ) %>%
    dplyr::select(
      "observation_period_id", "person_id", "observation_period_start_date",
      "observation_period_end_date", "period_type_concept_id"
    )
}

#' To add the attributes to the cohorts
#' @noRd
addCohortAttributes <- function(cohort) {
  if (is.null(attr(cohort, "cohort_set"))) {
    cohort <- addCohortSet(cohort)
  }
  if (is.null(attr(cohort, "cohort_count"))) {
    cohort <- addCohortCount(cohort)
  }
  if (is.null(attr(cohort, "cohort_attrition"))) {
    cohort <- addCohortAttrition(cohort)
  }
  return(cohort)
}

#' To create the cohorts or add the attributes to the existing ones
#' @noRd
createCohorts <- function(cohorts, observation_period) {
  if (!("cohort1" %in% names(cohorts))) {
    cohorts[["cohort1"]] <- createCohort(observation_period)
  }
  if (!("cohort2" %in% names(cohorts))) {
    cohorts[["cohort2"]] <- createCohort(observation_period)
  }
  for (name in names(cohorts)) {
    cohorts[[name]] <- addCohortAttributes(cohorts[[name]])
  }
  return(cohorts)
}

#' To create a random cohort from observation period
#' @noRd
createCohort <- function(observation_period) {
  cohort <- observation_period %>%
    dplyr::group_by(.data$person_id) %>%
    dplyr::filter(dplyr::row_number() == 1) %>%
    dplyr::ungroup() %>%
    createDate(
      "cohort_start_date", "observation_period_start_date",
      "observation_period_end_date"
    ) %>%
    createDate(
      "cohort_end_date", "cohort_start_date", "observation_period_end_date"
    )
  cohort <- cohort %>%
    dplyr::mutate(
      cohort_definition_id = sample(1:3, nrow(cohort), replace = T)
    ) %>%
    dplyr::select(
      "cohort_definition_id", "subject_id" = "person_id", "cohort_start_date",
      "cohort_end_date"
    )
}

#' To create a random date between two dates
#' @noRd
createDate <- function(x, newColumn, lowerLimit, upperLimit) {
  x %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      !!newColumn := .data[[lowerLimit]] + sample(
        0:difftime(.data[[upperLimit]], .data[[lowerLimit]], units = "days"), 1
      )
    ) %>%
    dplyr::ungroup()
}

#' To create a mock drug_exposure table
#' @noRd
createDrugExposure <- function(observation_period, concept) {
  concepts <- concept %>%
    dplyr::filter(.data$domain_id == "Drug") %>%
    dplyr::filter(.data$concept_class_id != "Ingredient") %>%
    dplyr::pull("concept_id")
  if (length(concepts) > 0) {
    drug_exposure <- observation_period %>%
      dplyr::mutate(number_records = stats::rpois(dplyr::n(), 3)) %>%
      tidyr::uncount(.data$number_records) %>%
      createDate(
        "drug_exposure_start_date", "observation_period_start_date",
        "observation_period_end_date"
      ) %>%
      createDate(
        "drug_exposure_end_date", "drug_exposure_start_date",
        "observation_period_end_date"
      ) %>%
      dplyr::mutate(
        drug_exposure_id = dplyr::row_number(),
        drug_type_concept_id = 38000177
      )
    drug_exposure <- drug_exposure %>%
      dplyr::mutate(
        drug_concept_id = sample(concepts, nrow(drug_exposure), replace = T),
        quantity = sample(
          c(1, seq(5, 50, 5), seq(60, 100, 10)), nrow(drug_exposure),
          replace = T
        )
      ) %>%
      dplyr::select(
        "drug_exposure_id", "person_id", "drug_concept_id",
        "drug_exposure_start_date", "drug_exposure_end_date",
        "drug_type_concept_id", "quantity"
      )
  } else {
    drug_exposure <- dplyr::tibble(
      drug_exposure_id = numeric(), person_id = numeric(),
      drug_concept_id = numeric(),
      drug_exposure_start_date = as.Date(x = integer(0), origin = "1970-01-01"),
      drug_exposure_end_date = as.Date(x = integer(0), origin = "1970-01-01"),
      drug_type_concept_id = numeric(), quantity = numeric()
    )
  }
  return(drug_exposure)
}

#' To create a condition_occurrence table based on observation_period
#' @noRd
createConditionOccurrence <- function(observation_period, concept) {
  concepts <- concept %>%
    dplyr::filter(.data$domain_id == "Condition") %>%
    dplyr::pull("concept_id")
  if (length(concepts) > 0) {
    condition_occurrence <- observation_period %>%
      dplyr::mutate(number_records = stats::rpois(dplyr::n(), 2)) %>%
      tidyr::uncount(.data$number_records) %>%
      createDate(
        "condition_start_date", "observation_period_start_date",
        "observation_period_end_date"
      ) %>%
      createDate(
        "condition_end_date", "condition_start_date",
        "observation_period_end_date"
      ) %>%
      dplyr::mutate(
        condition_occurrence_id = dplyr::row_number(),
        condition_type_concept_id = 32020
      )
    condition_occurrence <- condition_occurrence %>%
      dplyr::mutate(condition_concept_id = sample(
        concepts, nrow(condition_occurrence), replace = T
      )) %>%
      dplyr::select(
        "condition_occurrence_id", "person_id", "condition_concept_id",
        "condition_start_date", "condition_end_date",
        "condition_type_concept_id"
      )
  } else {
    condition_occurrence <- dplyr::tibble(
      condition_occurrence_id = numeric(), person_id = numeric(),
      condition_concept_id = numeric(),
      condition_start_date = as.Date(x = integer(0), origin = "1970-01-01"),
      condition_end_date = as.Date(x = integer(0), origin = "1970-01-01"),
      condition_type_concept_id = numeric()
    )
  }
  condition_occurrence <- condition_occurrence  %>%
    dplyr::mutate(
      condition_start_datetime = as.Date(NA),
      condition_end_datetime = as.Date(NA),
      condition_status_concept_id = as.numeric(NA),
      stop_reason = NA,
      provider_id = NA,
      visit_occurrence_id = NA,
      visit_detail_id = NA,
      condition_source_value = NA,
      condition_source_concept_id = NA,
      condition_status_source_value = NA
    )
  return(condition_occurrence)
}

#' To create visit occurrence from condition_occurrence and drug_exposure
#' @noRd
createVisitOccurrence <- function(condition_occurrence, drug_exposure) {
  condition_occurrence %>%
    dplyr::select(
      "person_id", "visit_start_date" = "condition_start_date",
      "visit_end_date" = "condition_end_date"
    ) %>%
    dplyr::union_all(
      drug_exposure %>%
        dplyr::select(
          "person_id", "visit_start_date" = "drug_exposure_start_date",
          "visit_end_date" = "drug_exposure_end_date"
        )
    ) %>%
    dplyr::mutate(
      visit_occurrence_id = dplyr::row_number(),
      visit_concept_id = 9202
    ) %>%
    dplyr::select(
      "visit_occurrence_id", "person_id", "visit_concept_id",
      "visit_start_date", "visit_end_date"
    )
}

#' To create observation table based on observation_period
#' @noRd
createObservation <- function(observation_period, concept) {
  concepts <- concept %>%
    dplyr::filter(.data$domain_id == "Observation") %>%
    dplyr::pull("concept_id")
  if (length(concepts) > 0) {
    observation <- observation_period %>%
      dplyr::mutate(number_records = stats::rpois(dplyr::n(), 2)) %>%
      tidyr::uncount(.data$number_records) %>%
      createDate(
        "observation_date", "observation_period_start_date",
        "observation_period_end_date"
      ) %>%
      dplyr::mutate(
        observation_id = dplyr::row_number(),
        observation_type_concept_id = 32020
      )
    observation <- observation %>%
      dplyr::mutate(observation_concept_id = sample(
        concepts, nrow(observation), replace = T
      )) %>%
      dplyr::select(
        "observation_id", "person_id", "observation_concept_id",
        "observation_date", "observation_type_concept_id"
      )
  } else {
    observation <- dplyr::tibble(
      observation_id = numeric(), person_id = numeric(),
      observation_concept_id = numeric(),
      observation_date = as.Date(x = integer(0), origin = "1970-01-01"),
      observation_type_concept_id = numeric()
    )
  }
  return(observation)
}

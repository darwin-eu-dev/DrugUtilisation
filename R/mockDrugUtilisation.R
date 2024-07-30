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

#' It creates a mock database for testing DrugUtilisation package
#'
#' @param con A DBIConnection object to a database. If NULL a new duckdb
#' connection will be used.
#' @param writeSchema A schema with writing permissions to copy there the cdm
#' tables.
#' @param numberIndividuals Number of individuals in the mock cdm.
#' @param seed Seed for the random numbers. If NULL no seed is used.
#' @param ... Tables to use as basis to create the mock. If some tables are
#' provided they will be used to construct the cdm object.
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
mockDrugUtilisation <- function(con = NULL,
                                writeSchema = NULL,
                                numberIndividuals = 10,
                                seed = NULL,
                                ...) {
  tables <- list(...)
  assertNumeric(seed, length = 1, null = TRUE)
  assertNumeric(numberIndividuals, length = 1, integerish = TRUE, min = 1)
  assertList(tables, named = TRUE, class = "data.frame")

  if (is.null(con)) con <- duckdb::dbConnect(duckdb::duckdb(), ":memory:")
  if (is.null(writeSchema)) writeSchema <- c(schema = "main", prefix = "mock_")

  # get vocabulary
  vocab <- vocabularyTables(
    tables[["concept"]], tables[["concept_ancestor"]], tables[["drug_strength"]],
    tables[["concept_relationship"]]
  )
  tables$concept <- vocab$concept
  tables$concept_ancestor <- vocab$concept_ancestor
  tables$drug_strength <- vocab$drug_strength
  tables$concept_relationship <- vocab$concept_relationship

  # set seed
  if (!is.null(seed)) set.seed(seed)

  if (!all(c("person", "observation_period") %in% names(tables))) {
    minDates <- calculateMinDate(tables)
  }

  # create person if NULL
  if (!"person" %in% names(tables)) {
    tables$person <- createPersonTable(numberIndividuals, tables) |>
      correctPersonDates(minDates)
  } else {
    tables$person <- tables$person |>
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
  if (!"observation_period" %in% names(tables)) {
    tables$observation_period <- createObservationPeriod(tables$person) |>
      correctObsDates(minDates)
  }

  # create drug_exposure if NULL
  if (!"drug_exposure" %in% names(tables)) {
    tables$drug_exposure <- createDrugExposure(tables$observation_period, tables$concept)
  }

  # create condition_occurrence if NULL
  if (!"condition_occurrence" %in% names(tables)) {
    tables$condition_occurrence <- createConditionOccurrence(
      tables$observation_period, tables$concept
    )
  }

  # create observation if NULL
  if (!"observation" %in% names(tables)) {
    tables$observation <- createObservation(
      tables$observation_period, tables$concept
    )
  }

  tables$visit_occurrence <- createVisitOccurrence(
    tables$condition_occurrence, tables$drug_exposure
  )

  cohortPos <- lapply(tables, isCohort) |> unlist()
  cohorts <- tables[cohortPos] |> createCohorts(tables$observation_period)
  tables <- tables[!cohortPos]

  cdm <- omopgenerics::cdmFromTables(
    tables = tables, cdmName = "DUS MOCK", cohortTables = cohorts
  )

  writeSchema <- strsplit(writeSchema, "\\.")[[1]]
  suppressMessages(
    cdm <- CDMConnector::copyCdmTo(
      con = con, cdm = cdm, schema = writeSchema, overwrite = TRUE
    )
  )

  return(cdm)
}

# To create the vocabulary tables
vocabularyTables <- function(concept, concept_ancestor, drug_strength, concept_relationship) {
  if (is.null(concept)) {
    concept <- mockConcept
  }
  if (is.null(concept_ancestor)) {
    concept_ancestor <- mockConceptAncestor
  }
  if (is.null(drug_strength)) {
    drug_strength <- mockDrugStrength
  }
  if (is.null(concept_relationship)) {
    concept_relationship <- dplyr::tibble(
      concept_id_1 = c(
        2905077, 1516983, 2905075, 1503327, 1516978, 1503326, 1503328, 1516980,
        29050773, 1125360, 15033297, 15030327, 15033427, 15036327, 15394662,
        43135274, 11253605, 431352774, 431359274, 112530, 1539465, 29050772,
        431352074, 15394062, 43135277, 15033327, 11253603, 15516980, 5034327,
        1539462, 15033528, 15394636, 15176980, 1539463, 431395274, 15186980,
        15316978
      ),
      concept_id_2 = c(
        19016586, 46275062, 35894935, 19135843, 19082107, 19011932, 19082108,
        2008660, 2008661, 2008662, 19082109, 43126087, 19130307, 42629089,
        19103220, 19082048, 19082049, 19082256, 19082050, 19082071, 19082072,
        19135438, 19135446, 19135439, 19135440, 46234466, 19082653, 19057400,
        19082227, 19082286, 19009068, 19082628, 19082224, 19095972, 19095973,
        35604394, 702776
      ),
      relationship_id = c(rep("RxNorm has dose form", 37)),
      valid_start_date = as.Date("1970-01-01"),
      valid_end_date = as.Date("2100-01-01")
    )
  }
  return(
    list(
      concept = concept, concept_ancestor = concept_ancestor,
      drug_strength = drug_strength, concept_relationship = concept_relationship
    )
  )
}

# To create the person tables
createPersonTable <- function(numberIndividuals, tables) {
  persons <- integer()
  for (k in seq_along(tables)) {
    cols <- colnames(tables[[k]])
    if ("subject_id" %in% cols) {
      persons <- c(persons, tables[[k]]$subject_id) |> unique()
    }
    if ("person_id" %in% cols) {
      persons <- c(persons, tables[[k]]$person_id) |> unique()
    }
  }
  if (length(persons) == 0) {
    persons <- seq_len(numberIndividuals) |> as.integer()
  }
  numberIndividuals <- length(persons)
  person <- dplyr::tibble(
    person_id = persons,
    gender_concept_id = sample(c(8507, 8532), numberIndividuals, T),
    year_of_birth = sample(1950:2020, numberIndividuals, T),
    day_of_birth = sample(1:365, numberIndividuals, T),
    birth_datetime = as.Date(NA),
    race_concept_id = as.numeric(NA),
    ethnicity_concept_id = as.numeric(NA),
    location_id = as.numeric(NA),
    provider_id = as.numeric(NA),
    care_site_id = as.numeric(NA)
  ) |>
    dplyr::mutate(
      birth_datetime = clock::add_days(
        as.Date(paste0(.data$year_of_birth, "-01-01"), "%Y-%m-%d"),
        .data$day_of_birth - 1
      )
    ) |>
    dplyr::mutate(
      year_of_birth = clock::get_year(.data$birth_datetime),
      month_of_birth = clock::get_month(.data$birth_datetime),
      day_of_birth = clock::get_day(.data$birth_datetime)
    )
  return(person)
}

# To create the observation period tables
createObservationPeriod <- function(person) {
  obs <- person |>
    dplyr::select("person_id", "birth_datetime") |>
    dplyr::mutate(upper_limit = as.Date("2023-01-01")) |>
    createDate(
      "observation_period_start_date", "birth_datetime", "upper_limit"
    ) |>
    createDate(
      "observation_period_end_date", "observation_period_start_date",
      "upper_limit"
    ) |>
    dplyr::mutate(
      observation_period_id = dplyr::row_number(),
      period_type_concept_id = 44814724
    ) |>
    dplyr::select(
      "observation_period_id", "person_id", "observation_period_start_date",
      "observation_period_end_date", "period_type_concept_id"
    )
  return(obs)
}

calculateMinDate <- function(tables) {
  # correct for current observations
  minDate <- dplyr::tibble(person_id = integer(), date = as.Date(character()))
  for (k in seq_along(tables)) {
    tab <- tables[[k]]
    cols <- colnames(tab)
    id <- c("person_id", "subject_id")
    id <- id[id %in% cols]
    cdates <- cols[endsWith(cols, "_date")]
    if (length(id) == 1) {
      for (col in cdates) {
        minDate <- minDate |>
          dplyr::union_all(
            tab |>
              dplyr::select(
                "person_id" = dplyr::all_of(id), "date" = dplyr::all_of(col)
              ) |>
              dplyr::mutate(
                "person_id" = as.integer(.data$person_id),
                "date" = as.Date(.data$date)
              ) |>
              dplyr::distinct()
          )
      }
    }
  }
  if (nrow(minDate) > 0) {
    minDate <- minDate |>
      dplyr::group_by(.data$person_id) |>
      dplyr::summarise(
        "date_min" = min(.data$date, na.rm = TRUE),
        "date_max" = max(.data$date, na.rm = TRUE)
      )
  } else {
    minDate <- dplyr::tibble(
      person_id = integer(),
      date_min = as.Date(character()),
      date_max = as.Date(character())
    )
  }
  return(minDate)
}

# To create the cohorts or add the attributes to the existing ones
createCohorts <- function(cohorts, observation_period) {
  if (!("cohort1" %in% names(cohorts))) {
    cohorts[["cohort1"]] <- createCohort(observation_period)
  }
  if (!("cohort2" %in% names(cohorts))) {
    cohorts[["cohort2"]] <- createCohort(observation_period)
  }
  return(cohorts)
}

# To create a random cohort from observation period
createCohort <- function(observation_period) {
  cohort <- observation_period |>
    dplyr::group_by(.data$person_id) |>
    dplyr::filter(dplyr::row_number() == 1) |>
    dplyr::ungroup() |>
    createDate(
      "cohort_start_date", "observation_period_start_date",
      "observation_period_end_date"
    ) |>
    createDate(
      "cohort_end_date", "cohort_start_date", "observation_period_end_date"
    )
  cohort <- cohort |>
    dplyr::mutate(
      cohort_definition_id = sample(1:3, nrow(cohort), replace = T)
    ) |>
    dplyr::select(
      "cohort_definition_id",
      "subject_id" = "person_id", "cohort_start_date",
      "cohort_end_date"
    )
}

# To create a random date between two dates
createDate <- function(x, newColumn, lowerLimit, upperLimit) {
  if (nrow(x) == 0) {
    return(x |> dplyr::mutate(!!newColumn := as.Date(character())))
  }
  x |>
    dplyr::rowwise() |>
    dplyr::mutate(
      !!newColumn := .data[[lowerLimit]] + sample(
        0:difftime(.data[[upperLimit]], .data[[lowerLimit]], units = "days"), 1
      )
    ) |>
    dplyr::ungroup()
}

# To create a mock drug_exposure table
createDrugExposure <- function(observation_period, concept) {
  concepts <- concept |>
    dplyr::filter(.data$domain_id == "Drug") |>
    dplyr::filter(.data$concept_class_id != "Ingredient") |>
    dplyr::pull("concept_id")
  if (length(concepts) > 0) {
    drug_exposure <- observation_period |>
      dplyr::mutate(number_records = stats::rpois(dplyr::n(), 3)) |>
      tidyr::uncount(.data$number_records) |>
      createDate(
        "drug_exposure_start_date", "observation_period_start_date",
        "observation_period_end_date"
      ) |>
      createDate(
        "drug_exposure_end_date", "drug_exposure_start_date",
        "observation_period_end_date"
      ) |>
      dplyr::mutate(
        drug_exposure_id = dplyr::row_number(),
        drug_type_concept_id = 38000177
      )
    drug_exposure <- drug_exposure |>
      dplyr::mutate(
        drug_concept_id = sample(concepts, nrow(drug_exposure), replace = T),
        quantity = sample(
          c(1, seq(5, 50, 5), seq(60, 100, 10)), nrow(drug_exposure),
          replace = T
        )
      ) |>
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

# To create a condition_occurrence table based on observation_period
createConditionOccurrence <- function(observation_period, concept) {
  concepts <- concept |>
    dplyr::filter(.data$domain_id == "Condition") |>
    dplyr::pull("concept_id")
  if (length(concepts) > 0) {
    condition_occurrence <- observation_period |>
      dplyr::mutate(number_records = stats::rpois(dplyr::n(), 2)) |>
      tidyr::uncount(.data$number_records) |>
      createDate(
        "condition_start_date", "observation_period_start_date",
        "observation_period_end_date"
      ) |>
      createDate(
        "condition_end_date", "condition_start_date",
        "observation_period_end_date"
      ) |>
      dplyr::mutate(
        condition_occurrence_id = dplyr::row_number(),
        condition_type_concept_id = 32020
      )
    condition_occurrence <- condition_occurrence |>
      dplyr::mutate(condition_concept_id = sample(
        concepts, nrow(condition_occurrence),
        replace = T
      )) |>
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
  condition_occurrence <- condition_occurrence |>
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

# To create visit occurrence from condition_occurrence and drug_exposure
createVisitOccurrence <- function(condition_occurrence, drug_exposure) {
  condition_occurrence |>
    dplyr::select(
      "person_id",
      "visit_start_date" = "condition_start_date",
      "visit_end_date" = "condition_end_date"
    ) |>
    dplyr::union_all(
      drug_exposure |>
        dplyr::select(
          "person_id",
          "visit_start_date" = "drug_exposure_start_date",
          "visit_end_date" = "drug_exposure_end_date"
        )
    ) |>
    dplyr::mutate(
      visit_occurrence_id = dplyr::row_number(),
      visit_concept_id = 9202,
      visit_type_concept_id = 0
    ) |>
    dplyr::select(
      "visit_occurrence_id", "person_id", "visit_concept_id",
      "visit_start_date", "visit_end_date", "visit_type_concept_id"
    )
}

# To create observation table based on observation_period
createObservation <- function(observation_period, concept) {
  concepts <- concept |>
    dplyr::filter(.data$domain_id == "Observation") |>
    dplyr::pull("concept_id")
  if (length(concepts) > 0) {
    observation <- observation_period |>
      dplyr::mutate(number_records = stats::rpois(dplyr::n(), 2)) |>
      tidyr::uncount(.data$number_records) |>
      createDate(
        "observation_date", "observation_period_start_date",
        "observation_period_end_date"
      ) |>
      dplyr::mutate(
        observation_id = dplyr::row_number(),
        observation_type_concept_id = 32020
      )
    observation <- observation |>
      dplyr::mutate(observation_concept_id = sample(
        concepts, nrow(observation),
        replace = T
      )) |>
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

correctPersonDates <- function(tab, minDates) {
  if (nrow(minDates) > 0) {
    tab <- tab |>
      dplyr::left_join(minDates, by = "person_id") |>
      dplyr::mutate(
        id = .data$date_min < as.Date(.data$birth_datetime),
        year_of_birth = dplyr::if_else(.data$id, format(.data$date_min, "%Y") |> as.integer(), .data$year_of_birth),
        month_of_birth = dplyr::if_else(.data$id, format(.data$date_min, "%m") |> as.integer(), .data$month_of_birth),
        day_of_birth = dplyr::if_else(.data$id, format(.data$date_min, "%d") |> as.integer(), .data$day_of_birth)
      ) |>
      dplyr::select(-"id", -"date_min", -"date_max")
  }
  return(tab)
}

correctObsDates <- function(tab, datesRange) {
  if (nrow(datesRange) > 0) {
    tab <- tab |>
      dplyr::left_join(datesRange, by = "person_id") |>
      dplyr::mutate(
        "observation_period_start_date" = dplyr::if_else(
          .data$date_min < .data$observation_period_start_date,
          .data$date_min,
          .data$observation_period_start_date
        ),
        "observation_period_end_date" = dplyr::if_else(
          .data$date_max > .data$observation_period_end_date,
          .data$date_max,
          .data$observation_period_end_date
        )
      ) |>
      dplyr::select(-"date_min", -"date_max")
  }
  return(tab)
}

isCohort <- function(x) {
  all(c("cohort_definition_id", "subject_id", "cohort_start_date", "cohort_end_date") %in% names(x))
}

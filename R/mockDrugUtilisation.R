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
#'
#' @param drug_exposure default null user can define its own table
#' @param drug_strength default null user can define its own table
#' @param observation_period default null user can define its own table
#' @param condition_occurrence default null user can define its own table
#' @param person default null user can define its own table
#' @param drug_concept_id_size number of unique drug concept id
#' @param ingredient_concept_id_size number of unique drug ingredient concept id
#' @param drug_exposure_size number of unique drug exposure
#' @param patient_size number of unique patient
#' @param min_drug_exposure_start_date user define minimum drug exposure start date
#' @param max_drug_exposure_start_date user define maximium drug exposure start date
#' @param seed seed
#' @param condition_concept_id_size number of unique row in the concept table
#' @param earliest_date_of_birth the earliest date of birth of patient in person table format "dd-mm-yyyy"
#' @param latest_date_of_birth the latest date of birth for patient in person table format "dd-mm-yyyy"
#' @param earliest_observation_start_date the earliest observation start date for patient format "dd-mm-yyyy"
#' @param latest_observation_start_date the latest observation start date for patient format "dd-mm-yyyy"
#' @param min_days_to_observation_end the minimum number of days of the observational integer
#' @param max_days_to_observation_end the maximum number of days of the observation period integer
#' @param earliest_condition_start_date the earliest condition start date for patient format "dd-mm-yyyy"
#' @param latest_condition_start_date the latest condition start date for patient format "dd-mm-yyyy"
#' @param min_days_to_condition_end the minimum number of days of the condition integer
#' @param max_days_to_condition_end the maximum number of days of the condition integer
#'
#' @return
#' @export
#'
#' @examples
mockDrugUtilisation <- function(drug_exposure = NULL,
                             drug_strength = NULL,
                             observation_period = NULL,
                             condition_occurrence = NULL,
                             person = NULL,
                             drug_concept_id_size = 5,
                             condition_concept_id_size = 5,
                             ingredient_concept_id_size = 1,
                             drug_exposure_size = 10,
                             patient_size = 1,
                             min_drug_exposure_start_date = "2000-01-01",
                             max_drug_exposure_start_date = "2020-01-01",
                             earliest_date_of_birth = NULL,
                             latest_date_of_birth = NULL,
                             earliest_observation_start_date = NULL,
                             latest_observation_start_date = NULL,
                             min_days_to_observation_end = NULL,
                             max_days_to_observation_end = NULL,
                             earliest_condition_start_date = NULL,
                             latest_condition_start_date = NULL,
                             min_days_to_condition_end = NULL,
                             max_days_to_condition_end = NULL,
                             seed = 1) {
  #checks
  errorMessage <- checkmate::makeAssertCollection()
  checkmate::assert_int(drug_exposure_size, lower = 1)
  checkmate::assert_int(patient_size, lower = 1)
  checkmate::assert_int(drug_concept_id_size, lower = 1)
  checkmate::assert_int(ingredient_concept_id_size, lower = 1)
  checkmate::assertTRUE(drug_exposure_size >= patient_size)
  checkmate::assert_tibble(person, null.ok = TRUE)
  checkmate::assert_tibble(observation_period, null.ok = TRUE)
  checkmate::assert_tibble(drug_exposure, null.ok = TRUE)
  checkmate::assert_tibble(condition_occurrence, null.ok = TRUE)
  checkmate::assert_tibble(drug_strength, null.ok = TRUE)
  checkmate::assert_int(seed, lower = 1)
  checkmate::assertDate(as.Date(earliest_date_of_birth), null.ok = TRUE)
  checkmate::assertDate(as.Date(latest_date_of_birth), null.ok = TRUE)
  checkmate::assertDate(as.Date(earliest_observation_start_date), null.ok = TRUE)
  checkmate::assertDate(as.Date(latest_observation_start_date), null.ok = TRUE)
  checkmate::assertDate(as.Date(earliest_condition_start_date), null.ok = TRUE)
  checkmate::assertDate(as.Date(latest_condition_start_date), null.ok = TRUE)
  checkmate::assert_int(min_days_to_observation_end, lower = 1, null.ok = TRUE)
  checkmate::assert_int(max_days_to_observation_end, lower = 1, null.ok = TRUE)
  checkmate::assert_int(min_days_to_condition_end, lower = 1, null.ok = TRUE)
  checkmate::assert_int(max_days_to_condition_end, lower = 1, null.ok = TRUE)
  if (!is.null(latest_date_of_birth) &
      !is.null(earliest_date_of_birth)) {
    checkmate::assertTRUE(latest_date_of_birth >= earliest_date_of_birth)
  }
  if (!is.null(earliest_observation_start_date) &
      !is.null(latest_observation_start_date)) {
    checkmate::assertTRUE(latest_observation_start_date >= earliest_observation_start_date)
  }
  if (!is.null(min_days_to_observation_end) &
      !is.null(max_days_to_observation_end)) {
    checkmate::assertTRUE(max_days_to_observation_end >= min_days_to_observation_end)
  }
  if (!is.null(earliest_condition_start_date) &
      !is.null(latest_condition_start_date)) {
    checkmate::assertTRUE(latest_condition_start_date >= earliest_condition_start_date)
  }
  if (!is.null(min_days_to_condition_end) &
      !is.null(max_days_to_condition_end)) {
    checkmate::assertTRUE(max_days_to_condition_end >= min_days_to_condition_end)
  }
  checkmate::reportAssertions(collection = errorMessage)



  set.seed(seed)#set seeds

  #create drug strength table
  if (is.null(drug_strength)) {
    drug_concept_id <-
      seq(1:drug_concept_id_size)#create unique drug concept id
    ingredient_concept_id <-
      seq(1:ingredient_concept_id_size)#create ingredient concept id
    amount_value <-
      c(
        rep(NA, each = ingredient_concept_id_size),
        #ingredient have missing amount value
        sample(c("10", "20", "30"),
               drug_concept_id_size - 1,
               replace = TRUE)
      ) # compute amount value
    amount_unit_concept_id <-

      sample(c("8576"),
             drug_concept_id_size,
             replace = TRUE)#  compute unit id


    drug_strength <-
      data.frame(
        drug_concept_id = as.numeric(drug_concept_id),
        ingredient_concept_id = as.numeric(
          sample(ingredient_concept_id, drug_concept_id_size, replace = TRUE)
        ),
        amount_value = as.numeric(amount_value),
        amount_unit_concept_id = as.numeric(amount_unit_concept_id)
        # numerator_value = numeric(),
        # numerator_unit_concept_id = numeric(),
        # denominator_value = numeric(),
        # denominator_unit_concept_id = numeric(),
        # box_size = numeric(),
        # valid_start_date = as.Date(character()),
        # valid_end_date = as.Date(character()),
        # invalid_reason = character()
      )
  }





   #drug_exposure
  if (is.null(drug_exposure)) {
    drug_exposure_id <-
      as.integer(seq(1:drug_exposure_size)) #generate number of unique drug_exposure_id
    person_id <-
      as.integer(sample(seq(1:patient_size),
                        drug_exposure_size,
                        replace = TRUE)) #generate number of unique patient id
    drug_concept_id <-
      as.integer(sample(
        drug_strength$drug_concept_id,
        drug_exposure_size,
        replace = TRUE
      )) #assign drug concept id to to each drug exposure

    # generate drug exposure start date
    drug_exposure_start_date <-
      sample(seq(
        as.Date(min_drug_exposure_start_date),
        as.Date(max_drug_exposure_start_date),
        by = "day"
      ),
      drug_exposure_size,
      replace = TRUE)
    # generate drug exposure end date to happens after drug exposure start date
    drug_exposure_end_date  <-
      drug_exposure_start_date + lubridate::days(sample(c(0, 7, 14, 21, 28, 30, 60, 90)
                                                        ,
                                                        drug_exposure_size,
                                                        replace = TRUE))

    days_supply <-
      as.integer(difftime(drug_exposure_end_date, drug_exposure_start_date, units = "days"))

    quantity <- days_supply+1



    # putting into drug_exposure table
    drug_exposure <-
      data.frame(
        drug_exposure_id = as.numeric(drug_exposure_id),
        person_id = as.numeric(person_id),
        drug_concept_id = as.numeric(drug_concept_id),
        drug_exposure_start_date = drug_exposure_start_date,
        drug_exposure_end_date = drug_exposure_end_date,
        quantity = as.numeric(quantity)
      ##  days_supply = as.numeric(days_supply)

      )
  }


  if (is.null(person) | is.null(observation_period)) {
    # person table
    id <- sample(seq(1:patient_size))
    # person gender
    gender_id <- sample(c("8507", "8532"),
                        patient_size,
                        replace = TRUE)

    # Define earliest possible date of birth for person table
    if (is.null(earliest_date_of_birth)) {
      earliest_date_of_birth <- as.Date("1920-01-01")
    }
    # Define latest possible date of birth for person table
    if (is.null(latest_date_of_birth)) {
      latest_date_of_birth <- as.Date("2000-01-01")
    }

    DOB <- sample(seq(
      as.Date(earliest_date_of_birth),
      as.Date(latest_date_of_birth),
      by = "day"
    ),
    patient_size,
    replace = TRUE)
    # year, month, day
    DOB_year <- as.numeric(format(DOB, "%Y"))
    DOB_month <- as.numeric(format(DOB, "%m"))
    DOB_day <- as.numeric(format(DOB, "%d"))

    # observation_period table
    # create a list of observational_period_id

    # define earliest and latest observation start date for obs table
    # if not specified by user
    if (is.null(earliest_observation_start_date)) {
      earliest_observation_start_date <- as.Date("2005-01-01")
    }
    if (is.null(latest_observation_start_date)) {
      latest_observation_start_date <- as.Date("2020-01-01")
    }
    obs_start_date <-
      sample(seq(
        as.Date(earliest_observation_start_date),
        as.Date(latest_observation_start_date),
        by = "day"
      ),
      patient_size,
      replace = TRUE) # start date for the period


    # define min and max day to condition end
    if (is.null(min_days_to_observation_end)) {
      min_days_to_observation_end <- 1
    }
    if (is.null(max_days_to_observation_end)) {
      max_days_to_observation_end <- 1000
    }

    obs_end_date <-
      obs_start_date + lubridate::days(
        sample(
          min_days_to_observation_end:max_days_to_observation_end,
          patient_size,
          replace = TRUE
        )
      )

  }



  if (is.null(person) | is.null(condition_occurrence)) {
    # define earliest and latest condition start date for obs table
    # if not specified by user
    if (is.null(earliest_condition_start_date)) {
      earliest_condition_start_date <- as.Date("2005-01-01")
    }
    if (is.null(latest_condition_start_date)) {
      latest_condition_start_date <- as.Date("2020-01-01")
    }
    condition_start_date <-
      sample(seq(
        as.Date(earliest_condition_start_date),
        as.Date(latest_condition_start_date),
        by = "day"
      ),
      patient_size,
      replace = TRUE) # start date for the period


    # define min and max day to condition end
    if (is.null(min_days_to_condition_end)) {
      min_days_to_condition_end <- 1
    }
    if (is.null(max_days_to_condition_end)) {
      max_days_to_condition_end <- 1000
    }

    condition_end_date <-
      condition_start_date + lubridate::days(
        sample(
          min_days_to_condition_end:max_days_to_condition_end,
          patient_size,
          replace = TRUE
        )
      )

    c_concept_id <-
      seq(1:condition_concept_id_size)
    concept_concept_id <- sample(c_concept_id,
                                 patient_size,
                                 replace = TRUE)

  }


    if (is.null(person)) {
      person <- tibble::tibble(
        person_id = id,
        gender_concept_id = gender_id,
        year_of_birth = DOB_year,
        month_of_birth = DOB_month,
        day_of_birth = DOB_day
      )
    }

    if (is.null(observation_period)) {
      observation_period <- tibble::tibble(
        observation_period_id = id,
        person_id = id,
        observation_period_start_date = obs_start_date,
        observation_period_end_date = obs_end_date
      )
    }



  if (is.null(condition_occurrence)) {
    condition_occurrence <- tibble::tibble(
      condition_occurrence_id = id,
      person_id = id,
      condition_concept_id = concept_concept_id,
      condition_start_date = condition_start_date,
      condition_end_date = condition_end_date
    )
  }





  # into in-memory database
  db <- DBI::dbConnect(duckdb::duckdb(), ":memory:")


  DBI::dbWithTransaction(db, {
    DBI::dbWriteTable(db, "drug_strength",
                      drug_strength,
                      overwrite = TRUE)
  })

  DBI::dbWithTransaction(db, {
    DBI::dbWriteTable(db, "drug_exposure",
                      drug_exposure,
                      overwrite = TRUE)
  })

  DBI::dbWithTransaction(db, {
    DBI::dbWriteTable(db, "person",
                      person,
                      overwrite = TRUE)
  })

  DBI::dbWithTransaction(db, {
    DBI::dbWriteTable(db, "observation_period",
                      observation_period,
                      overwrite = TRUE)
  })

  DBI::dbWithTransaction(db, {
    DBI::dbWriteTable(db, "condition_occurrence",
                      condition_occurrence,
                      overwrite = TRUE)
  })

  cdm <- CDMConnector::cdm_from_con(
    db,
    cdm_tables = c(
      "drug_strength",
      "drug_exposure",
      "person",
      "observation_period",
      "condition_occurrence"
    )
  )

  return(cdm)
}

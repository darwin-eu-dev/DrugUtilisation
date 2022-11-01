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
#' @param drug_concept_id_size number of unique drug concept id
#' @param ingredient_concept_id_size number of unique drug ingredient concept id
#' @param drug_exposure_size number of unique drug exposure
#' @param patient_size number of unique patient
#' @param min_drug_exposure_start_date user define minimum drug exposure start date
#' @param max_drug_exposure_start_date user define maximium drug exposure start date
#' @param seed seed
#'
#'
#'
mockDrugExposure <- function(drug_exposure = NULL,
                             drug_strength = NULL,
                             drug_concept_id_size = 5,
                             ingredient_concept_id_size = 1,
                             drug_exposure_size = 10,
                             patient_size = 1,
                             min_drug_exposure_start_date = "2000-01-01",
                             max_drug_exposure_start_date = "2020-01-01",
                             seed = 1) {
  #checks
  errorMessage <- checkmate::makeAssertCollection()
  checkmate::assert_int(drug_exposure_size, lower = 1)
  checkmate::assert_int(patient_size, lower = 1)
  checkmate::assert_int(drug_concept_id_size, lower = 1)
  checkmate::assert_int(ingredient_concept_id_size, lower = 1)
  checkmate::assertTRUE(drug_exposure_size >= patient_size)
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

    quantity <- days_supply



    # putting into drug_exposure table
    drug_exposure <-
      data.frame(
        drug_exposure_id = as.numeric(drug_exposure_id),
        person_id = as.numeric(person_id),
        drug_concept_id = as.numeric(drug_concept_id),
        drug_exposure_start_date = drug_exposure_start_date,
        drug_exposure_end_date = drug_exposure_end_date,
        quantity = as.numeric(quantity),
        days_supply = as.numeric(days_supply)

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

  cdm <- CDMConnector::cdm_from_con(db,
                                    cdm_tables = c("drug_strength",
                                                   "drug_exposure"))

  return(cdm)
}

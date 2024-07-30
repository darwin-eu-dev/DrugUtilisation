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
#' Run benchmark of drug utilisation cohort generation
#' @param cdm A CDM reference object
#' @param numberOfCohort Number of cohort to generate for benchmarking. An integer or a vector of integers
#' @param indicationCohortName  Name of indication cohort table
#' @param ingredientId Ingredient OMOP concept that we are interested for
#' the study. It is a compulsory input, no default value is provided.
#' @param drugExposureName Name of drug_exposure table in cdm, the table must contain drug_concept_id, quantity,
#' drug_exposure_start_date and drug_exposure_end_date as columns
#' @return a tibble with time taken for different analyses
#' @export
#' @examples
#' \donttest{
#' library(DrugUtilisation)
#'
#' cdm <- mockDrugUtilisation()
#'
#' timings <- benchmarkDrugUtilisation(cdm)
#'
#' timings
#' }
benchmarkDrugUtilisation <- function(cdm,
                                     numberOfCohort = 1:4,
                                     indicationCohortName = "cohort1",
                                     ingredientId = 1125315,
                                     drugExposureName = "drug_exposure") {
  rlang::check_installed("tictoc")

  errorMessage <- checkmate::makeAssertCollection()

  checkmate::assertIntegerish(ingredientId,
    len = 1,
    add = errorMessage,
    null.ok = FALSE
  )

  checkmate::assertIntegerish(numberOfCohort,
    lower = 1,
    add = errorMessage,
    null.ok = FALSE
  )

  checkmate::assertCharacter(indicationCohortName,
    len = 1,
    add = errorMessage,
    null.ok = FALSE
  )


  checkmate::assertCharacter(drugExposureName,
    len = 1,
    add = errorMessage,
    null.ok = FALSE
  )

  checkmate::reportAssertions(collection = errorMessage)

  conceptSet <- CodelistGenerator::getDrugIngredientCodes(cdm)

  checkmate::assertTRUE(length(conceptSet) >= max(numberOfCohort),
    add = errorMessage
  )

  time_record <- list()

  for (j in numberOfCohort) {
    conceptSetList <- conceptSet[c(1:j)]

    name <- paste0("dus_", j)

    tictoc::tic()


    cdm <- generateDrugUtilisationCohortSet(
      cdm = cdm,
      name = name,
      conceptSet = conceptSetList
    )

    omopgenerics::cohortCount(cdm[[name]])

    t <- tictoc::toc(quiet = TRUE)

    time_record[[paste0("DUS cohorts generation ", j)]] <- dplyr::tibble(
      task = paste0("DUS ", j, " cohorts"),
      time_taken_secs = as.numeric(t$toc - t$tic)
    )


    tictoc::tic()

    cdm[[name]] |>
      addIndication(
        indicationCohortName = indicationCohortName,
        indicationWindow = list(c(0, 0)),
        unknownIndicationTable = NULL
      )

    t <- tictoc::toc(quiet = TRUE)

    time_record[[paste0("add indication ", j)]] <- dplyr::tibble(
      task = paste0("add indication ", j, " cohorts"),
      time_taken_secs = as.numeric(t$toc - t$tic)
    )

    tictoc::tic()

    x <- cdm[[name]] |>
      addDrugUtilisation(ingredientConceptId = ingredientId, gapEra = 30)

    t <- tictoc::toc(quiet = TRUE)

    time_record[[paste0("add drug utilisation ", j)]] <- dplyr::tibble(
      task = paste0("add drug utilisation for ", j, " cohorts"),
      time_taken_secs = as.numeric(t$toc - t$tic)
    )


    tictoc::tic()

    cdm[[name]] |>
      summariseDrugUtilisation(ingredientConceptId = ingredientId, gapEra = 30)

    time_record[[paste0("summarise drug utilisation ", j)]] <- dplyr::tibble(
      task = paste0("summarise drug utilisation for ", j, " cohorts"),
      time_taken_secs = as.numeric(t$toc - t$tic)
    )
  }

  tictoc::tic()

  time_record <- dplyr::bind_rows(time_record) |>
    dplyr::mutate(time_taken_secs = round(.data$time_taken_secs, 2)) |>
    dplyr::mutate(time_taken_mins = round(.data$time_taken_secs / 60, 2)) |>
    dplyr::mutate(time_taken_hours = round(.data$time_taken_mins / 60, 2)) |>
    dplyr::mutate(person_n = cdm$person |>
      dplyr::count() |>
      dplyr::pull())

  return(time_record)
}

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

#' Generate a set of drug cohorts based on given concepts
#'
#' @description
#' Adds a new cohort table to the cdm reference with individuals who have drug
#' exposure records with the specified concepts. Cohort start and end dates will
#' be based on drug record start and end dates, respectively. Records that
#' overlap or have fewer days between them than the specified gap era will be
#' concatenated into a single cohort entry.
#'
#' @param cdm A cdm reference.
#' @param name The name of the new cohort table to add to the cdm reference.
#' @param conceptSet The concepts used to create the cohort, provide as a
#' codelist or concept set expression.
#' @param gapEra Number of days between two continuous exposures to be
#' considered in the same era. Records that have fewer days between them than
#' this gap will be concatenated into the same cohort record.
#' @param durationRange Deprecated.
#' @param imputeDuration Deprecated.
#' @param priorUseWashout Deprecated.
#' @param priorObservation Deprecated.
#' @param cohortDateRange Deprecated.
#' @param limit Deprecated.
#'
#' @return The function returns the cdm reference provided with the addition of
#' the new cohort table.
#'
#' @export
#'
#' @examples
#' \donttest{
#' library(CDMConnector)
#' library(DrugUtilisation)
#' library(dplyr)
#'
#' cdm <- mockDrugUtilisation()
#'
#' druglist <- CodelistGenerator::getDrugIngredientCodes(
#'   cdm, c("acetaminophen", "metformin")
#' )
#'
#' cdm <- generateDrugUtilisationCohortSet(
#'   cdm = cdm,
#'   name = "drug_cohorts",
#'   conceptSet = druglist
#' )
#'
#' cdm$drug_cohorts |>
#'   glimpse()
#' }
#'
generateDrugUtilisationCohortSet <- function(cdm,
                                             name,
                                             conceptSet,
                                             gapEra = 1,
                                             durationRange = lifecycle::deprecated(),
                                             imputeDuration = lifecycle::deprecated(),
                                             priorUseWashout = lifecycle::deprecated(),
                                             priorObservation = lifecycle::deprecated(),
                                             cohortDateRange = lifecycle::deprecated(),
                                             limit = lifecycle::deprecated()) {
  if (lifecycle::is_present(durationRange)) {
    lifecycle::deprecate_warn(
      when = "0.7.0",
      what = "generateDrugUtilisationCohortSet(durationRange = )"
    )
  }
  if (lifecycle::is_present(imputeDuration)) {
    lifecycle::deprecate_warn(
      when = "0.7.0", what = "generateDrugUtilisationCohortSet(imputeDuration = )"
    )
  }
  if (lifecycle::is_present(priorUseWashout)) {
    lifecycle::deprecate_warn(
      when = "0.7.0",
      what = "generateDrugUtilisationCohortSet(priorUseWashout = )",
      with = "requirePriorDrugWashout()"
    )
  }
  if (lifecycle::is_present(priorObservation)) {
    lifecycle::deprecate_warn(
      when = "0.7.0",
      what = "generateDrugUtilisationCohortSet(priorObservation = )",
      with = "requireObservationBeforeDrug()"
    )
  }
  if (lifecycle::is_present(cohortDateRange)) {
    lifecycle::deprecate_warn(
      when = "0.7.0",
      what = "generateDrugUtilisationCohortSet(cohortDateRange = )",
      with = "requireDrugInDateRange()"
    )
  }
  if (lifecycle::is_present(limit)) {
    lifecycle::deprecate_warn(
      when = "0.7.0",
      what = "generateDrugUtilisationCohortSet(limit = )",
      with = "requireIsFirstDrugEntry()"
    )
  }

  checkInputs(cdm = cdm, name = name, conceptSet = conceptSet, gapEra = gapEra)

  # get conceptSet
  cohortSet <- dplyr::tibble(cohort_name = names(conceptSet)) |>
    dplyr::mutate(cohort_definition_id = dplyr::row_number()) |>
    dplyr::select("cohort_definition_id", "cohort_name") |>
    dplyr::mutate(gap_era = as.character(.env$gapEra))

  conceptSet <- conceptSetFromConceptSetList(conceptSet, cohortSet)

  cohortCodelistAttr <- cohortSet |>
    dplyr::select("cohort_definition_id", "codelist_name" = "cohort_name") |>
    dplyr::inner_join(
      conceptSet |>
        dplyr::rename("concept_id" = "drug_concept_id"),
      by = "cohort_definition_id",
      relationship = "one-to-many"
    ) |>
    dplyr::mutate("type" = "index event")

  cdm[[name]] <- subsetTables(cdm, conceptSet, name) |>
    omopgenerics::newCohortTable(
      cohortSetRef = cohortSet, cohortCodelistRef = cohortCodelistAttr
    ) |>
    erafyCohort(gapEra)

  dropTmpTables(cdm)

  return(cdm)
}


#' Get the gapEra used to create a cohort
#'
#' @param cohort A `cohort_table` object.
#' @param cohortId Integer vector refering to cohortIds from cohort. If NULL all
#' cohort definition ids in settings will be used.
#'
#' @return gapEra values for the specific cohortIds
#'
#' @export
#'
#' @examples
#' \donttest{
#' library(CDMConnector)
#' library(DrugUtilisation)
#' library(dplyr)
#'
#' cdm <- mockDrugUtilisation()
#'
#' druglist <- CodelistGenerator::getDrugIngredientCodes(
#'   cdm, c("acetaminophen", "metformin")
#' )
#'
#' cdm <- generateDrugUtilisationCohortSet(
#'   cdm = cdm,
#'   name = "drug_cohorts",
#'   conceptSet = druglist,
#'   gapEra = 100
#' )
#'
#' cohortGapEra(cdm$drug_cohorts)
#' }
#'
cohortGapEra <- function(cohort, cohortId = NULL) {
  assertClass(cohort, class = "cohort_table")
  assertNumeric(cohortId, integerish = TRUE, null = TRUE)
  set <- settings(cohort)
  opts <- set |> dplyr::pull("cohort_definition_id")
  if (is.null(cohortId)) cohortId <- opts
  removed <- cohortId[!cohortId %in% opts]
  if (length(removed) > 0) {
    cli::cli_warn(c("!" = "cohortIds: {removed} not present in settings."))
  }
  cohortId <- cohortId[cohortId %in% opts]
  if ("gap_era" %in% colnames(set)) {
    gapEra <- set |>
      dplyr::select("gap_era", "cohort_definition_id") |>
      dplyr::inner_join(
        dplyr::tibble(
          "cohort_definition_id" = cohortId, "order" = seq_along(cohortId)
        ),
        by = "cohort_definition_id"
      ) |>
      dplyr::arrange(.data$order) |>
      dplyr::pull("gap_era") |>
      as.integer()
  } else {
    cli::cli_inform("`gap_era` not present in settings, returning NULL.")
    gapEra <- NULL
  }
  return(gapEra)
}

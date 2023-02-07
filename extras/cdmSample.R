# Copyright 2022 DARWIN EU®
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

#' To subset the cdm object tables to the individuals present in a certain
#' cohort.
#'
#' @param cdm cdm object.
#' @param cohortName name of the cohort in the cdm reference.
#' @param cohortId id of the cohort that we want to subset from the cohort
#' table. If NULL all cohorts in cohort table are considered.
#' @param tablesToSubset set of tables to subset. If NULL all tables in cdm
#' object with person_id column are going to be subseted.
#'
#' @return
#'
#' @export
#'
#' @examples
#'
cdmSubset <- function(cdm,
                      cohortName,
                      cohortId = NULL,
                      tablesToSubset = NULL) {
  checkmate::assertClass(cdm, "cdm_reference")
  checkmate::assertCharacter(cohortName, len = 1)
  checkmate::assertTRUE(cohortName %in% names(cdm))
  checkmate::assertIntegerish(cohortId, len = 1, null.ok = TRUE)

  if (!("subject_id" %in% colnames(cdm[[cohortName]]))) {
    stop("subject_id column is not present in cdm[[cohortName]]")
  }
  if (!("cohort_definition_id" %in% colnames(cdm[[cohortName]])) &&
      !is.null(cohortId)) {
    stop("cohort_definition_id column is not present in cdm[[cohortName]]")
  }

  subjects <- cdm[[cohortName]]
  if (!is.null(cohortId)) {
    subjects <- subjects %>%
      dplyr::filter(.data$cohort_definition_id == .env$cohortId)
  }
  subjects <- subjects %>%
    dplyr::select("person_id" = "subject_id") %>%
    dplyr::distinct() %>%
    dplyr::compute()

  if (subjects %>% dplyr::tally() %>% dplyr::pull("n") == 0) {
    stop("No individual selected with these specifications.")
  }

  if (subjects %>%
      dplyr::tally() %>%
      dplyr::pull("n") !=
      subjects %>%
      dplyr::inner_join(cdm$person, by = "person_id") %>%
      dplyr::tally() %>%
      dplyr::pull("n")) {
    warning("Not all subjects are present in person table.")
  }

  cdm <- cdmSubsetSubjects(cdm, subjects, tablesToSubset)

  return(cdm)
}

#' To subset the cdm object to a certain number of individuals.
#'
#' @param cdm cdm object.
#' @param numberIndividuals number of individuals that are going to be selected
#' from person table.
#' @param tablesToSubset set of tables to subset. If NULL all tables in cdm
#' object with person_id column are going to be subseted.
#'
#' @return
#'
#' @export
#'
#' @examples
#'
cdmSample <- function(cdm,
                      numberIndividuals = 500000,
                      tablesToSubset = NULL) {
  checkmate::assertClass(cdm, "cdm_reference")
  checkmate::assertIntegerish(numberIndividuals, len = 1)

  numberIndividuals <- as.numeric(numberIndividuals)

  if (numberIndividuals > cdm$person %>% dplyr::tally() %>% dplyr::pull("n")) {
    stop("numberIndividuals is bigger than individuals in the cdm reference.")
  }

  subjects <- cdm$person %>%
    dplyr::select("person_id") %>%
    dplyr::slice_sample(n = numberIndividuals) %>%
    dplyr::compute()

  cdm <- cdmSubsetSubjects(cdm, subjects, tablesToSubset)

  return(cdm)
}

#' @noRd
cdmSubsetSubjects <- function(cdm,
                              subjects,
                              tablesToSubset) {
  checkmate::assertCharacter(tablesToSubset, null.ok = TRUE)
  if (is.null(tablesToSubset)) {
    tablesToSubset <- lapply(names(cdm), function(x) {
      x <- cdm[[x]]
      if ("tbl" %in% class(x)) {
        return("person_id" %in% colnames(x))
      } else {
        return(FALSE)
      }
    }) %>%
      unlist()
    tablesToSubset <- names(cdm)[tablesToSubset]
  }
  if(length(tablesToSubset) == 0) {
    stop("No table found to be subseted.")
  }
  checkmate::assertTRUE(all(tablesToSubset %in% names(cdm)))

  newCdmTables <- lapply(tablesToSubset, function(x){
    cdm[[x]] %>%
      dplyr::inner_join(subjects, by = "person_id") %>%
      dplyr::compute()
  })

  for (k in 1:length(tablesToSubset)) {
    cdm[[tablesToSubset[k]]] <- newCdmTables[[k]]
  }

  return(cdm)
}

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

#' Check coverage of daily dose computation in a sample of the cdm for selected
#' concept sets and ingredient
#'
#' @param cdm A cdm reference created using CDMConnector
#' @param sample A number indicating the size of the random sample to take from
#' the 'person' table of the cdm
#' @param ingredient Code indicating the ingredient of interest
#' @param conceptList List in the format given by 'conceptList.R' of concept
#' sets of interest
#' @param seed Seed for the random sample
#'
#' @return The function returns information of the coverage of computeDailyDose.R
#' for the selected ingredients and concept sets
#' @export
#'
#' @examples
dailyDoseCoverage <- function(cdm,
                              sample = NULL,
                              ingredient = NULL,
                              conceptList = NULL,
                              seed = 1) {

  if(!is.null(conceptList)) {
    checkInputs(
      cdm = cdm, sample = sample, ingredientConceptId = ingredient,
      conceptSetList = conceptList
    )
  } else {
    checkInputs(
      cdm = cdm, sample = sample, ingredientConceptId = ingredient
    )
  }

  set.seed(seed)

  if(!is.null(conceptList)) {
    concepts_interest <- unname(unlist(conceptList))
  } else {
    concepts_interest <- cdm$drug_exposure %>%
      dplyr::select("drug_concept_id") %>%
      dplyr::distinct() %>%
      dplyr::pull()
  }

  coverage_cohort <-  cdm$drug_exposure %>%
    dplyr::filter(.data$drug_concept_id %in% .env$concepts_interest) %>%
    dplyr::inner_join(
      cdm$person %>%
        dplyr::select("person_id") %>%
        dplyr::slice_sample(n = sample),
      by = "person_id"
    ) %>%
    dplyr::inner_join(
      cdm$drug_strength %>%
        dplyr::filter(.data$ingredient_concept_id == .env$ingredient) %>%
        dplyr::select("drug_concept_id"),
      by = "drug_concept_id"
    ) %>%
    addDailyDose(cdm, ingredient)

  coverage_num <- coverage_cohort %>%
    dplyr::filter(!is.na(.data$daily_dose)) %>%
    dplyr::tally() %>%
    dplyr::pull()

  coverage_den <- coverage_cohort %>%
    dplyr::tally() %>%
    dplyr::pull()

  if(coverage_den == 0) {
    coverage = 0
  } else {
    coverage <- coverage_num/coverage_den*100
  }

  return(coverage)
}



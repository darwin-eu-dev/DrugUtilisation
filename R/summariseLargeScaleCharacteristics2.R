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

#' This function is used to summarise the large scale characteristics of a
#' cohort table
#'
#' @param cohort The cohort to characterise.
#' @param strata Stratification list.
#' @param window Temporal windows that we want to characterize.
#' @param standardConcept Tables to characterise the concept_id.
#' @param sourceConcept Tables to characterise the source_concept_id.
#' @param concepSetList List of concep sets to be assessed.
#' @param atcLevel ATC levels to characterise.
#' @param icd10Level ICD10 Levels to characterise.
#' @param restrictToIncident Whether you want to consider incidence events only
#' (restrictToIncident = TRUE) or ongoing events (restrictToIncident = FALSE).
#' @param minCellCount All counts lower than minCellCount will be obscured
#' changing its value by NA.
#' @param cdm A cdm reference.
#'
#' @return The output of this function is a `ResultSummary` contsining the
#' relevant information.
#'
#' @noRd
#'
#' @examples
#' \donttest{
#' library(DrugUtilisation)
#'
#' cdm <- mockDrugUtilisation()
#'
#' cdm$cohort1 %>%
#'   summariseLargeScaleCharacteristics(
#'     window = list(c(-180, -1), c(0, 0), c(1, 180)),
#'     standardConcept = c("drug_exposure", "condition_occurrence")
#'   )
#' }
#'
summariseLargeScaleCharacteristics2 <- function(cohort,
                                               strata = list(),
                                               window = list(
                                                 c(-Inf, -366), c(-365, -91),
                                                 c(-365, -31), c(-90, -1),
                                                 c(-30, -1), c(0, 0), c(1, 30),
                                                 c(1, 90), c(31, 365),
                                                 c(91, 365), c(366, Inf)
                                               ),
                                               standardConcept = NULL,
                                               sourceConcept = NULL,
                                               concepSetList = NULL,
                                               atcLevel = NULL,
                                               icd10Level = NULL,
                                               restrictToIncident = TRUE,
                                               minCellCount = 5,
                                               cdm = attr(cohort, "cdm_reference")) {
  # initial checks
  checkInputs(
    cohort = cohort, strata = strata, window = window,
    standardConcept = standardConcept, sourceConcept = sourceConcept,
    concepSetList = concepSetList, atcLevel = atcLevel, icd10Level = icd10Level,
    restrictToIncident = restrictToIncident, minCellCount = minCellCount,
    cdm = cdm
  )

  # add windoName
  window <- windowName(window)

  # create codelist
  codelist <- c(
    codelist,
    ifelse(
      !is.null(atcLevel),
      CodelistGenerator::getATCCodes(cdm = cdm, level = atcLevel),
      NULL
    ),
    ifelse(
      !is.null(icd10Codelist),
      CodelistGenerator::getICD10StandardCodes(cdm = cdm, level = icd10Level),
      NULL
    )
  )
  codelist <- lapply(names(codelist), function(x) {
    dplyr::tibble(concept_id = codelist[[x]], variable_level = x)
  }) %>%
    dplyr::bind_rows() %>%
    dplyr::distinct() %>%
    addDomain(cdm)

  # make unique codelist


}

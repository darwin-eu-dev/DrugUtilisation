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

#' Explain function
#'
#' @param cdm 'cdm' object created with CDMConnector::cdm_from_con(). It must
#' must contain the 'targetCohort' table and all the tables that we want to
#' characterize. It is a compulsory input, no default value is provided.
#' @param targetCohortName Name of the table in the cdm that contains the
#' target cohort that we want to characterize. It is a compulsory input, no
#' default value is provided.
#' @param targetCohortId Cohort definition id for the analyzed target cohorts.
#' It can be a vector or a number. If it is NULL all cohorts are analyzed. By
#' default: NULL.
#' @param temporalWindow Temporal windows that we want to characterize. It must
#' be a list of numeric vectors of length two. The tables will be characterized
#' between the first element and the second element respect to the
#' cohort_start_date of each individual. To refer to any time prior set NA the
#' first element of the vector. To refer to any time after the index date set NA
#' the second element of the vector. By default: list(c(NA, -366), c(-365, -90),
#' c(-365, -31), c(-90, -1), c(-30, -1), c(0, 0), c(1, 30), c(1, 90),
#' c(31, 365), c(91, 365), c(366, NA)).
#' @param tablesToCharacterize Name of the tables in the cdm that we want to
#' summarize. The available tables to characterize are: "visit_occurrence",
#' "condition_occurrence", "drug_exposure", "procedure_occurrence",
#' "device_exposure", "measurement", "observation", "drug_era", "condition_era"
#' and "specimen". By default: c("condition_occurrence", "drug_era",
#' "procedure_occurrence", "measurement").
#' @param overlap Whether you want to consider overlapping events (overlap =
#' TRUE) or only incident ones (overlap = FALSE).
#' @param summarise Whether we want patient-level data (FALSE) or summarised
#' aggregated data (TRUE).
#' @param minimumCellCount All counts lower than minimumCellCount will be
#' obscured changing its value by NA. 'obscured' column of characterization
#' tibble is TRUE when a count has been obscured. Otherwise it is FALSE.
#'
#' @return The output of this function is a 3 elements list. First
#' ("Characterization") is a reference to a temporal table in the database. It
#' contains the characterization of the desired cohorts of interest. The cohorts
#' of interest are specified using 'targetCohortId' and 'targetCohortName'. The
#' characterized tables are the ones specified in 'tablesToChacaterize'. Second
#' ("temporalWindow") contains the windows used to do the characaterization.
#' Finally "overlap" is also included in the list.
#'
#' @export
#'
#' @examples
summariseLargeScaleCharacteristics <- function(cohort,
                                               cdm,
                                               window = list(
                                                 c(-Inf, -366), c(-365, -91),
                                                 c(-365, -31), c(-90, -1),
                                                 c(-30, -1), c(0, 0), c(1, 30),
                                                 c(1, 90), c(31, 365),
                                                 c(91, 365), c(366, Inf)
                                               ),
                                               tablesToCharacterize = c(
                                                 "condition_occurrence",
                                                 "drug_era",
                                                 "procedure_occurrence",
                                                 "measurement"
                                               ),
                                               overlap = TRUE,
                                               minimumCellCount = 5) {
  checkInputs(
    cohort = cohort, cdm = cdm, window = window,
    tablesToCharacterize = tablesToCharacterize, overlap = overlap,
    minimumCellCount = minimumCellCount
  )

  # correct overlap
  if (length(overlap) == 1) {
    overlap <- rep(overlap, length(tablesToCharacterize))
  }

  # select only the cohort variables
  cohort <- cohort %>%
    dplyr::select("cohort_definition_id", "subject_id", "cohort_start_date")

  # add flags
  for (k in seq_along(tablesToCharacterize)) {
    cohort <- cohort %>%
      PatientProfiles::addIntersect(
        cdm, tablesToCharacterize[k], window = window,
        filterVariable = PatientProfiles::getConceptName(tablesToCharacterize[k]),
        value = "flag", targetEndDate = ifelse(
          overlap[k] == FALSE,
          PatientProfiles::getEndName(tablesToCharacterize[k]),
          NULL
        ), nameStyle = paste0(tablesToCharacterize[k], "-{id_name}-{widow_name}")
      )
  }

  # summarise
  result <- PatientProfiles::summariseCharacteristics(
    cohort, variables = variables, functions = list(numeric = "count")
  )

  return(result)
}

# Copyright 2022 DARWIN EU (C)
#
# This file is part of DrugUtilizationCharacteristics
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
#' @param cdm A cdm object created with CDMConnector. No default.
#' @param targetCohortName A taget cohort instantiated in the database.
#' @param targetCohortId The cohort definition id of the cohort of interest. By
#' default NULL. NULL only valid when targetCohort contains only one cohort.
#' @param sex The sex stratification. By default = NULL.
#' @param ageGroup The age groups stratification. By default = NULL.
#' @param indexYearGroup The index Years stratification. By default = NULL.
#' @param indicationTable The indication table that contains the different
#' indications.
#' @param oneStrata Weather we want to stratify one strata (TRUE) each or
#' combine in multiple stratas using expand_grid (FALSE). By deafault = FALSE.
#'
#' @return Multiple cohorts as a temporal table in the database. The
#' stratification for each cohort can be see as attribute ("strata").
#' @export
#'
#' @examples
getStratification <- function(cdm,
                              targetCohortName,
                              targetCohortId = NULL,
                              sex = NULL,
                              ageGroup = NULL,
                              indexYearGroup = NULL,
                              indicationTable = NULL,
                              oneStrata = FALSE) {
  # initial checks
  errorMessage <- checkmate::makeAssertCollection()
  checkmate::assertClass(cdm, "cdm_reference", add = errorMessage)
  checkmate::assertCharacter(
    targetCohortName,
    any.missing = FALSE,
    len = 1,
    add = errorMessage
  )
  checkmate::assertCount(targetCohortId, null.ok = TRUE, add = errorMessage)
  checkmate::assertCharacter(
    sex,
    any.missing = FALSE, unique = TRUE, null.ok = TRUE, add = errorMessage
  )
  checkmate::assertList(
    ageGroup,
    c("numeric", "logical"),
    min.len = 1,
    any.missing = FALSE,
    null.ok = TRUE,
    add = errorMessage
  )
  checkmate::assertList(
    indexYearGroup,
    "numeric",
    min.len = 1,
    any.missing = FALSE,
    null.ok = TRUE,
    add = errorMessage
  )
  checkmate::assertList(
    indicationTable,
    min.len = 1,
    null.ok = TRUE,
    any.missing = FALSE,
    add = errorMessage
  )
  checkmate::reportAssertions(collection = errorMessage)

  # second round of checks
  checkmate::assertFALSE(
    is.null(sex) && is.null(ageGroup) && is.null(indexYearGroup) &&
      is.null(indicationTable),
    add = errorMessage
  )
  checkmate::assertTRUE(targetCohortName %in% names(cdm), add = errorMessage)
  if (!is.null(sex)) {
    checkmate::assertTRUE(
      all(sex %in% c("Both", "Male", "Female")),
      add = errorMessage
    )
  }
  if (!is.null(ageGroup)) {
    if (!all(unlist(lapply(ageGroup, length)) == 2)) {
      errorMessage$push("Length of all elements in ageGroup should be 2.")
    }
    if (!all(unlist(lapply(ageGroup, function(x) {
      all(is.na(x) | x >= 0)
    })))) {
      errorMessage$push(
        "Length of all elements in ageGroup should be NA or >= 0."
      )
    }
    if (!all(unlist(lapply(ageGroup, function(x) {
      is.na(x[1]) | is.na(x[2]) | x[1] <= x[2]
    })))) {
      errorMessage$push(paste0(
        "The first value of each element in ageGroup shoudl be smaller or equal ",
        "than the second one."
      ))
    }
  }
  if (!is.null(indexYearGroup)) {
    checkmate::assertNumeric(
      unlist(indexYearGroup),
      any.missing = FALSE,
      add = errorMessage
    )
  }
  if (!is.null(indicationTable)) {
    if (!all(unlist(lapply(indicationTable, function(x) {
      c(
        "indication_id", "subject_id", "cohort_start_date", "cohort_end_date"
      ) %in% colnames(x)
    })))) {
      errorMessage$push(paste0(
        "All indicationTable elements must be temp tables",
        " with columns: 'indication_id', 'subject_id', 'cohort_start_date', ",
        "'cohort_end_date'."
      ))
      errorMessage$push("Use getIndication function to obtain indicationTable.")
    }
  }
  checkmate::reportAssertions(collection = errorMessage)

  targetCohort <- cdm[[targetCohortName]]
  checkmate::assertTRUE(all(c(
    "cohort_definition_id", "subject_id", "cohort_start_date", "cohort_end_date"
  ) %in% colnames(targetCohort)),
  add = errorMessage
  )
  if (is.null(targetCohortId)) {
    targetCohortId <- targetCohort %>%
      dplyr::select("cohort_definition_id") %>%
      dplyr::distinct() %>%
      dplyr::pull()
    if (length(targetCohortId) > 1) {
      errorMessage$push(paste0(
        "targetCohortId should be provided when ",
        "targetCohort contains more than one cohort_definition_id."
      ))
    }
  }
  checkmate::reportAssertions(collection = errorMessage)
  if (targetCohort %>%
    dplyr::filter(.data$cohort_definition_id == .env$targetCohortId) %>%
    dplyr::tally() %>%
    dplyr::pull("n") == 0) {
    errorMessage$push("No counts in the targetCohortId")
  }
  checkmate::reportAssertions(collection = errorMessage)
}

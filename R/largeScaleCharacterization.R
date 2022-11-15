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
#' @param temporalWindows Temporal windows that we want to characterize. It must
#' be a list of numeric vectors of length two. The tables will be characterized
#' between the first element and the second element respect to the
#' cohort_start_date of each individual. To refer to any time prior set NA the
#' first element of the vector. To refer to any time after the index date set NA
#' the second element of the vector. By default: list(c(NA, -366), c(-365, -90),
#' c(-365, -31), c(-90, -1), c(-30, -1), c(0, 0), c(1, 30), c(1, 90),
#' c(31, 365), c(91, 365), c(366, NA)).
#' @param tablesToCharacterize Name of the tables in the cdm that we want to
#' summarize. The available tables to characterize are: "observation_period",
#' "visit_occurrence", "condition_occurrence", "drug_exposure",
#' "procedure_occurrence", "device_exposure", "measurement", "observation",
#' "drug_era", "condition_era", "specimen" and "death". By default:
#' c("condition_occurrence", "drug_era", "procedure_occurrence", "measurement").
#' @param characterizationTableName Name of the table that contains the large
#' scale characterization. This name will be used to access to the reference of
#' the temporal table that this function creates in the 'cdm' object. By
#' default: "characterization".
#' @param verbose Whether you want that the function explain the different steps
#' that follows.
#'
#' @return The output of this function is the 'cdm' object with an extra
#' reference that points to a temporal table in the database that contains the
#' characterization of the specified tables.
#'
#' @export
#'
#' @examples
largeScaleCharacterization <- function(cdm,
                                       targetCohortName,
                                       targetCohortId = NULL,
                                       temporalWindows = list(
                                         c(NA, -366), c(-365, -91),
                                         c(-365, -31), c(-90, -1), c(-30, -1),
                                         c(0, 0), c(1, 30), c(1, 90),
                                         c(31, 365), c(91, 365), c(366, NA)
                                       ),
                                       tablesToCharacterize = c(
                                         "condition_occurrence", "drug_era",
                                         "procedure_occurrence", "measurement"
                                         ),
                                       characterizationTableName =
                                         "characterization",
                                       verbose = FALSE) {
  errorMessage <- checkmate::makeAssertCollection()

  # check cdm
  checkmate::assertClass(cdm, "cdm_reference", add = errorMessage)

  # check targetCohortName
  checkmate::assertCharacter(targetCohortName, len = 1, add = errorMessage)

  # check that targetCohortName point to a table that is a cohort

  # check targetCohortId
  if (is.numeric(targetCohortId)) {
    targetCohortId <- as.character(targetCohortId)
  }
  checkmate::assertCharacter(targetCohortId, null.ok = TRUE, add = errorMessage)

  # check temporalWindows
  checkmate::assertList(temporalWindows, min.len = 1, add = errorMessage)
  checkmate::assertTRUE(
    all(unlist(lapply(temporalWindows, length)) == 2),
    add = errorMessage
  )

  # check tablesToCharacterize
  checkmate::assertCharacter(
    tablesToCharacterize,
    min.len = 1, add = errorMessage
  )
  checkmate::assertTRUE(
    all(tablesToCharacterize %in% names(cdm)),
    add = errorMessage
  )

  # check characterizationTableName
  checkmate::assertCharacter(
    characterizationTableName,
    len = 1, add = errorMessage
  )

  # check verbose
  checkmate::assertLogical(verbose, add = errorMessage)

  # report collection of errors
  checkmate::reportAssertions(collection = errorMessage)

  # setTemporalWindows inf to 40000
  temporalWindows <- lapply(temporalWindows, function(x) {
    nam <- paste0(
      ifelse(is.na(x[1]), "Any", x[1]),
      ";",
      ifelse(is.na(x[2]), "Any", x[2])
    )
    x <- dplyr::as_tibble(t(x))
    colnames(x) <- c("windowStart", "windowEnd")
    x <- dplyr::mutate(x, windowName = nam)
    return(x)
  }) %>%
    dplyr::bind_rows() %>%
    dplyr::mutate(windowId = dplyr::row_number()) %>%
    dplyr::select("windowId", "windowName", "windowStart", "windowEnd")

  if (!is.null(targetCohortId)) {
    target_cohort <- cdm[[targetCohortName]] %>%
      dplyr::filter(.data$cohort_definition_id %in% .env$targetCohortId)
  } else {
    target_cohort <- cdm[[targetCohortName]]
  }

  target_cohort <- target_cohort %>%
    dplyr::compute()

  subjects <- target_cohort %>%
    dplyr::select("person_id" = "subject_id", "cohort_start_date") %>%
    dplyr::distinct() %>%
    dplyr::compute()

  get_start_date <- list(
    "observation_period" = "observation_period_start_date",
    "visit_occurrence" = "visit_start_date",
    "condition_occurrence" = "condition_start_date",
    "drug_exposure" = "drug_exposure_start_date",
    "procedure_occurrence" = "procedure_date",
    "device_exposure" = "device_exposure_start_date",
    "measurement" = "measurement_date",
    "observation" = "observation_date",
    "drug_era" = "drug_era_start_date",
    "condition_era" = "condition_era_start_date",
    "specimen" = "specimen_date",
    "death" = "death_date"
  )

  get_end_date <- list(
    "observation_period" = "observation_period_end_date",
    "visit_occurrence" = "visit_end_date",
    "condition_occurrence" = "condition_end_date",
    "drug_exposure" = "drug_exposure_start_date",
    "procedure_occurrence" = NULL,
    "device_exposure" = "device_exposure_end_date",
    "measurement" = NULL,
    "observation" = NULL,
    "drug_era" = "drug_era_end_date",
    "condition_era" = "condition_era_end_date",
    "specimen" = NULL,
    "death" = NULL
  )

  get_concept <- list(
    "observation_period" = NULL,
    "visit_occurrence" = "visit_concept_id",
    "condition_occurrence" = "condition_start_date",
    "drug_exposure" = "drug_exposure_concept_id",
    "procedure_occurrence" = "procedure_concept_id",
    "device_exposure" = "device_concept_id",
    "measurement" = "measurement_concept_id",
    "observation" = "observation_concept_id",
    "drug_era" = "drug_concept_id",
    "condition_era" = "condition_concept_id",
    "specimen" = "specimen_concept_id",
    "death" = NULL
  )

  characterizedTable <- lapply(tablesToCharacterize, function(table_name) {
    start_date <- get_start_date[[table_name]]
    end_date <- get_end_date[[table_name]]
    concept_id <- get_concept[[table_name]]

    study_table <- cdm[[table_name]] %>%
      dplyr::inner_join(target_cohort %>%
        mutate(person_id = subject_id),
      by = "person_id"
      ) %>%
      dplyr::rename("start_date" = .env$start_date)
    if (is.null(end_date)) {
      study_table <- study_table %>%
        dplyr::mutate(end_date = .data$start_date)
    } else {
      study_table <- study_table %>%
        dplyr::rename("end_date" = .env$end_date)
    }
    study_table <- study_table %>%
      dplyr::rename("covariate" = .env$concept_id) %>%
      dplyr::select(
        "person_id", "start_date", "end_date", "concept_id", "cohort_start_date"
      ) %>%
      dplyr::mutate(table_id = dplyr::row_number()) %>%
      dplyr::compute()

    temporalWindows <- tidyr::expand_grid(
      study_table$table_id,
      temporalWindows$windowId
    ) %>%
      dplyr::left_join(temporalWindows, by = "windowId")

    study_table <- study_table %>%
      dplyr::left_join(temporalWindows,
        by = "table_id",
        copy = TRUE
      ) %>%
      dplyr::filter(.data$start_date <= .data$cohort_start_date - .data$window_end) %>%
      dplyr::filter(.data$end_date >= .data$cohort_start_date - .data$window_start) %>%
      dplyr::left_join(cdm[["concept"]],
        by = "concept_id"
      ) %>%
      dplyr::mutate(covariate = paste0(
        .env$table_name, ": ", .data$concept_name
      )) %>%
      dplyr::select(
        "person_id",
        "cohort_start_date",
        "covariate",
        "concept_id",
        "window_name"
      ) %>%
      dplyr::compute()

    return(study_table)
  })

  if (length(characterizedTable) > 1) {
    namesTables <- names(characterizedTable)
    characterizedTables <- characterizedTable[[namesTables[1]]]
    for (i in 2:length(characterizedTable)) {
      characterizedTables <- characterizedTables %>%
        dplyr::union(characterizedTable[[namesTables[i]]])
    }
    characterizedTables <- characterizedTables %>%
      dplyr::compute()
  }

  cdm[[characterizationTableName]] <- characterizedTables

  return(cdm)
}

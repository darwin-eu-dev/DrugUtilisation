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
#' ("temporalWindows") contains the windows used to do the characaterization.
#' Finally "overlap" is also included in the list.
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
                                       overlap = TRUE,
                                       summarise = TRUE,
                                       minimumCellCount = 5) {
  get_start_date <- list(
    "visit_occurrence" = "visit_start_date",
    "condition_occurrence" = "condition_start_date",
    "drug_exposure" = "drug_exposure_start_date",
    "procedure_occurrence" = "procedure_date",
    "device_exposure" = "device_exposure_start_date",
    "measurement" = "measurement_date",
    "observation" = "observation_date",
    "drug_era" = "drug_era_start_date",
    "condition_era" = "condition_era_start_date",
    "specimen" = "specimen_date"
  )

  get_end_date <- list(
    "visit_occurrence" = "visit_end_date",
    "condition_occurrence" = "condition_end_date",
    "drug_exposure" = "drug_exposure_start_date",
    "procedure_occurrence" = NULL,
    "device_exposure" = "device_exposure_end_date",
    "measurement" = NULL,
    "observation" = NULL,
    "drug_era" = "drug_era_end_date",
    "condition_era" = "condition_era_end_date",
    "specimen" = NULL
  )

  get_concept <- list(
    "visit_occurrence" = "visit_concept_id",
    "condition_occurrence" = "condition_concept_id",
    "drug_exposure" = "drug_exposure_concept_id",
    "procedure_occurrence" = "procedure_concept_id",
    "device_exposure" = "device_concept_id",
    "measurement" = "measurement_concept_id",
    "observation" = "observation_concept_id",
    "drug_era" = "drug_concept_id",
    "condition_era" = "condition_concept_id",
    "specimen" = "specimen_concept_id"
  )

  errorMessage <- checkmate::makeAssertCollection()

  # check cdm
  checkmate::assertClass(cdm, "cdm_reference", add = errorMessage)

  # check targetCohortName
  checkmate::assertCharacter(targetCohortName, len = 1, add = errorMessage)

  # check that targetCohortName point to a table that is a cohort
  checkmate::assertTRUE(
    all(c(
      "cohort_definition_id",
      "subject_id",
      "cohort_start_date",
      "cohort_end_date"
    ) %in% colnames(cdm[[targetCohortName]])),
    add = errorMessage
  )

  # check targetCohortId
  checkmate::assertIntegerish(
    targetCohortId,
    lower = 1,
    null.ok = TRUE,
    add = errorMessage
  )

  # check temporalWindows
  checkmate::assertList(temporalWindows, min.len = 1, add = errorMessage)
  checkmate::assertTRUE(
    all(unlist(lapply(temporalWindows, length)) == 2),
    add = errorMessage
  )

  # check tablesToCharacterize
  checkmate::assertCharacter(
    tablesToCharacterize,
    min.len = 1,
    add = errorMessage
  )
  checkmate::assertTRUE(
    all(tablesToCharacterize %in% names(cdm)),
    add = errorMessage
  )
  checkmate::assertTRUE(
    all(tablesToCharacterize %in% c(
      "visit_occurrence", "condition_occurrence", "drug_exposure",
      "procedure_occurrence", "device_exposure", "measurement", "observation",
      "drug_era", "condition_era", "specimen"
    )),
    add = errorMessage
  )

  # overlap
  checkmate::assertLogical(overlap, len = 1, add = errorMessage)

  # summarise
  checkmate::assertLogical(summarise, len = 1, add = errorMessage)

  # minimumCellCount
  checkmate::assertCount(minimumCellCount, add = errorMessage)

  # report collection of errors
  checkmate::reportAssertions(collection = errorMessage)

  # setTemporalWindows inf to 40000
  temporalWindows <- lapply(temporalWindows, function(x) {
    nam <- paste0(
      ifelse(is.na(x[1]), "Any", x[1]),
      ";",
      ifelse(is.na(x[2]), "Any", x[2])
    )
    x <- dplyr::tibble(windowStart = x[1], windowEnd = x[2], windowName = nam)
    return(x)
  }) %>%
    dplyr::bind_rows() %>%
    dplyr::mutate(windowId = dplyr::row_number()) %>%
    dplyr::select("windowId", "windowName", "windowStart", "windowEnd")

  if (!is.null(targetCohortId)) {
    targetCohort <- cdm[[targetCohortName]] %>%
      dplyr::filter(.data$cohort_definition_id %in% .env$targetCohortId)
  } else {
    targetCohort <- cdm[[targetCohortName]]
    targetCohortId <- targetCohort %>%
      dplyr::select("cohort_definition_id") %>%
      dplyr::distinct() %>%
      dplyr::pull()
  }

  subjects <- targetCohort %>%
    dplyr::select("person_id" = "subject_id", "cohort_start_date") %>%
    dplyr::distinct()

  characterizedTable <- lapply(tablesToCharacterize, function(table_name) {
    start_date <- get_start_date[[table_name]]
    end_date <- get_end_date[[table_name]]
    concept_id <- get_concept[[table_name]]

    study_table <- cdm[[table_name]] %>%
      dplyr::inner_join(subjects, by = "person_id") %>%
      dplyr::rename("start_date" = .env$start_date)
    if (is.null(end_date)) {
      study_table <- study_table %>%
        dplyr::mutate(end_date = .data$start_date)
    } else {
      study_table <- study_table %>%
        dplyr::rename("end_date" = .env$end_date)
    }
    study_table <- study_table %>%
      dplyr::rename("concept_id" = .env$concept_id) %>%
      dplyr::select(
        "person_id", "start_date", "end_date", "concept_id", "cohort_start_date"
      ) %>%
      dplyr::mutate(days_difference_start = dbplyr::sql(sqlDiffDays(
        CDMConnector::dbms(attr(cdm, "dbcon")),
        "cohort_start_date",
        "start_date"
      )))
    if (isTRUE(overlap)) {
      study_table <- study_table %>%
        dplyr::mutate(days_difference_end = dbplyr::sql(sqlDiffDays(
          CDMConnector::dbms(attr(cdm, "dbcon")),
          "cohort_start_date",
          "end_date"
        )))
    } else {
      study_table <- study_table %>%
        dplyr::mutate(days_difference_end = .data$days_difference_start)
    }
    study_table <- study_table %>%
      dplyr::compute()

    for (i in 1:nrow(temporalWindows)) {
      windowStart <- temporalWindows$windowStart[i]
      windowEnd <- temporalWindows$windowEnd[i]
      windowId <- temporalWindows$windowId[i]
      if (!is.na(windowEnd) & is.na(windowStart)) {
        study_table_i <- study_table %>%
          dplyr::filter(.data$days_difference_start <= .env$windowEnd)
      } else if (is.na(windowEnd) & !is.na(windowStart)) {
        study_table_i <- study_table %>%
          dplyr::filter(.data$days_difference_end >= .env$windowStart)
      } else if (!is.na(windowEnd) & !is.na(windowStart)) {
        study_table_i <- study_table %>%
          dplyr::filter(
            .data$days_difference_start <= .env$windowEnd &
              .data$days_difference_end >= .env$windowStart
          )
      }
      study_table_i <- study_table_i %>%
        dplyr::select("person_id", "concept_id", "cohort_start_date") %>%
        dplyr::distinct() %>%
        dplyr::mutate(window_id = .env$windowId) %>%
        dplyr::compute()
      if (i == 1) {
        study_tab <- study_table_i
      } else {
        study_tab <- study_tab %>%
          dplyr::union_all(study_table_i)
      }
    }
    study_tab <- study_tab %>% dplyr::compute()
    return(study_tab)
  })

  for (i in 1:length(characterizedTable)) {
    if (i == 1) {
      characterizedTables <- characterizedTable[[i]] %>%
        dplyr::mutate(table_id = .env$i)
    } else {
      characterizedTables <- characterizedTables %>%
        dplyr::union_all(
          characterizedTable[[i]] %>%
            dplyr::mutate(table_id = .env$i)
        )
    }
  }

  characterizedTables <- characterizedTables %>% dplyr::compute()

  if (summarise == TRUE) {
    for (k in 1:length(targetCohortId)) {
      characterizedCohort <- targetCohort %>%
        dplyr::filter(.data$cohort_definition_id == !!targetCohortId[k]) %>%
        dplyr::select("person_id" = "subject_id", "cohort_start_date") %>%
        dplyr::inner_join(
          characterizedTables,
          by = c("person_id", "cohort_start_date")
        ) %>%
        dplyr::group_by(.data$concept_id, .data$window_id, .data$table_id) %>%
        dplyr::tally() %>%
        dplyr::ungroup() %>%
        dplyr::collect() %>%
        dplyr::mutate(cohort_definition_id = targetCohortId[k])
      if (k == 1) {
        characterizedCohortk <- characterizedCohort
      } else {
        characterizedCohortk <- characterizedCohortk %>%
          dplyr::union_all(characterizedCohort)
      }
    }
    characterizedTables <- characterizedCohortk %>%
      dplyr::mutate(obscured = dplyr::if_else(
        .data$n < .env$minimumCellCount, TRUE, FALSE
      )) %>%
      dplyr::mutate(n = dplyr::if_else(
        .data$obscured == TRUE, as.numeric(NA), .data$n
      )) %>%
      dplyr::relocate("cohort_definition_id", .before = "concept_id")
  }

  result <- list()
  result$characterization <- characterizedTables
  result$temporalWindows <- temporalWindows
  result$tablesToCharacterize <- tablesToCharacterize
  result$overlap <- overlap

  return(result)
}

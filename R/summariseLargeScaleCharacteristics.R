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
                                               minimumCellCount = 5,
                                               bigMark = ",") {
  checkInputs(
    cohort = cohort, cdm = cdm, window = window,
    tablesToCharacterize = tablesToCharacterize, overlap = overlap,
    minimumCellCount = minimumCellCount, bigMark = bigMark
  )

  # correct overlap
  if (length(overlap) == 1) {
    overlap <- rep(overlap, length(tablesToCharacterize))
  }

  window <- lapply(window, function(x){
    dplyr::tibble(
      lower_bound = x[1], upper_bound = x[2], window_name = tolower(
        paste0(x[1], " to ", x[2])
      )
    )
  }) %>%
    dplyr::bind_rows()

  # compute denominator
  den <- cohort %>%
    dplyr::select("cohort_definition_id", "subject_id", "cohort_start_date") %>%
    dplyr::distinct() %>%
    dplyr::inner_join(
      cdm[["observation_period"]] %>%
        dplyr::select(
          "subject_id" = "person_id",
          "obs_start" = "observation_period_start_date",
          "obs_end" = "observation_period_end_date"
        ),
      by = "subject_id"
    ) %>%
    CDMConnector::computeQuery()

  # add counts
  result <- NULL
  for (k in seq_along(tablesToCharacterize)) {
    resultK <- den %>%
      dplyr::inner_join(
        cdm[[tablesToCharacterize[k]]] %>%
          dplyr::select(
            "subject_id" = "person_id",
            "start_date" = PatientProfiles::getStartName(
              tablesToCharacterize[k]
            ), "end_date" = ifelse(
              overlap[k],
              PatientProfiles::getEndName(
                tablesToCharacterize[k]
              ),
              PatientProfiles::getStartName(
                tablesToCharacterize[k]
              )
            ), "concept_id" = PatientProfiles::getConceptName(
              tablesToCharacterize[k]
            )
          ),
        by = "subject_id"
      ) %>%
      dplyr::mutate("end_date" = dplyr::if_else(
        is.na(.data$end_date), .data$start_date, .data$end_date
      )) %>%
      dplyr::mutate(
        start_date = pmin(.data$start_date, .data$obs_end, na.rm = T),
        end_date = pmax(.data$end_date, .data$obs_start, na.rm = T)
      ) %>%
      dplyr::filter(.data$start_date <= .data$end_date) %>%
      dplyr::mutate(
        start_dif = !!CDMConnector::datediff("cohort_start_date", "start_date"),
        end_dif = !!CDMConnector::datediff("cohort_start_date", "end_date")
      ) %>%
      dplyr::select(
        "cohort_definition_id", "subject_id", "cohort_start_date", "concept_id",
        "start_dif", "end_dif"
      ) %>%
      CDMConnector::computeQuery()
    for (i in 1:nrow(window)) {
      resultKI <- resultK
      if (!is.infinite(window$upper_bound[i])) {
        resultKI <- resultKI %>%
          dplyr::filter(.data$start_dif <= !!window$upper_bound[i])
      }
      if (!is.infinite(window$lower_bound[i])) {
        resultKI <- resultKI %>%
          dplyr::filter(.data$end_dif >= !!window$lower_bound[i])
      }
      result <- result %>%
        dplyr::union_all(
          resultKI %>%
            dplyr::select(
              "cohort_definition_id", "subject_id", "cohort_start_date",
              "concept_id"
            ) %>%
            dplyr::distinct() %>%
            dplyr::group_by(.data$cohort_definition_id, .data$concept_id) %>%
            dplyr::summarise(count = dplyr::n(), .groups = "drop") %>%
            dplyr::inner_join(
              cdm[["concept"]] %>%
                dplyr::select("concept_id", "concept_name"),
              by = "concept_id"
            ) %>%
            dplyr::collect() %>%
            dplyr::mutate(
              table_name = tablesToCharacterize[k],
              window_name = window$window_name[i]
            )
        )
    }
  }

  # add denominator_count
  denominatorCount <- NULL
  den <- den %>%
    dplyr::mutate(
      start_dif = !!CDMConnector::datediff("cohort_start_date", "obs_start"),
      end_dif = !!CDMConnector::datediff("cohort_start_date", "obs_end")
    ) %>%
    dplyr::select(
      "cohort_definition_id", "subject_id", "cohort_start_date", "start_dif",
      "end_dif"
    ) %>%
    CDMConnector::computeQuery()
  for (i in 1:nrow(window)) {
    denI <- den
    if (!is.infinite(window$upper_bound[i])) {
      denI <- denI %>%
        dplyr::filter(.data$start_dif <= !!window$upper_bound[i])
    }
    if (!is.infinite(window$lower_bound[i])) {
      denI <- denI %>%
        dplyr::filter(.data$end_dif >= !!window$lower_bound[i])
    }
    denominatorCount <- denominatorCount %>%
      dplyr::union_all(
        denI %>%
          dplyr::select(
            "cohort_definition_id", "subject_id", "cohort_start_date"
          ) %>%
          dplyr::distinct() %>%
          dplyr::group_by(.data$cohort_definition_id) %>%
          dplyr::summarise(denominator_count = dplyr::n(), .groups = "drop") %>%
          dplyr::collect() %>%
          dplyr::mutate(window_name = window$window_name[i], window_id = i)
      )
  }

  # join all together
  result <- result %>%
    dplyr::inner_join(
      denominatorCount,
      by = c("cohort_definition_id", "window_name")
    ) %>%
    dplyr::mutate(
      "%" = 100 * .data$count / .data$denominator_count,
      "count" = dplyr::if_else(
        .data$count < minimumCellCount,
        paste0("<", minimumCellCount),
        base::format(.data$count, big.mark = bigMark)
      ),
      "denominator_count" = dplyr::if_else(
        .data$denominator_count < minimumCellCount,
        paste0("<", minimumCellCount),
        base::format(.data$denominator_count, big.mark = bigMark)
      )
    ) %>%
    dplyr::inner_join(
      CDMConnector::cohortSet(cohort), by = "cohort_definition_id"
    ) %>%
    dplyr::arrange(
      .data$cohort_name, .data$table_name, .data$window_id, .data$concept_id
    ) %>%
    dplyr::select(
      "cohort_name", "table_name", "window_name", "concept_id",
      "concept_name", "count", "denominator_count", "%"
    )

  return(result)
}


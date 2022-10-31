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

#' Get indication for a target cohort
#'
#' @param cdm cdm
#' @param targetCohortName targetCohortName
#' @param targetCohortDefinitionIds targetCohortDefinitionIds
#' @param indicationCohortName indicationCohortName
#' @param indicationCohortDefinitionIds indicationCohortDefinitionIds
#' @param indicationDefinitionSet indicationDefinitionSet
#' @param indicationGap indicationGap
#' @param unknownIndication unknownIndication
#' @param unknownIndicarionTables unknownIndicarionTables
#' @param indicationTableName indicationTableName
#' @param verbose verbose
#'
#' @return
#' @export
#'
#' @examples
getIndication <- function(cdm,
                          targetCohortName,
                          targetCohortDefinitionIds = NULL,
                          indicationCohortName,
                          indicationCohortDefinitionIds = NULL,
                          indicationDefinitionSet,
                          indicationGap = 0,
                          unknownIndication = TRUE,
                          unknownIndicarionTables = c("condition_occurrence", "observation"),
                          indicationTableName = "indication_table",
                          verbose = FALSE) {
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
    "specimen" = "specimen_date"
  )

  target_db <- cdm[[targetCohortName]]
  indication_db <- cdm[[indicationCohortName]]
  if (!is.null(targetCohortDefinitionIds)) {
    target_db <- target_db %>%
      dplyr::filter(
        .data$cohort_definition_id %in% .env$targetCohortDefinitionIds
      )
  }
  if (!is.null(indicationCohortDefinitionIds)) {
    indication_db <- indication_db %>%
      dplyr::filter(
        .data$cohort_definition_id %in% .env$indication_cohort_definition_ids
      )
  }

  # get indications
  if (indication_gap == 0) {
    target_db <- target_db %>%
      dplyr::left_join(indication_db %>%
        dplyr::rename("indication_id" = "cohort_definition_id") %>%
        dplyr::select("indication_id", "subject_id", "cohort_start_date"),
      by = c("subject_id", "cohort_start_date")
      ) %>%
      dplyr::distinct() %>%
      dplyr::compute()
  } else {
    target_db <- target_db %>%
      dplyr::left_join(target_db %>%
        dplyr::inner_join(indication_db %>%
          dplyr::rename("indication_id" = "cohort_definition_id") %>%
          dplyr::rename("indication_start_date" = "cohort_start_date") %>%
          dplyr::select("indication_id", "subject_id", "indication_start_date"),
        by = c("subject_id")
        ) %>%
        dplyr::mutate(dif_time_indication = as.numeric(difftime(
          .data$cohort_start_date,
          .data$indication_start_date,
          units = "days"
        ))) %>%
        dplyr::filter(.data$dif_time_indication <= .env$indication_gap) %>%
        dplyr::filter(.data$dif_time_indication >= 0) %>%
        dplyr::select(-"dif_time_indication", -"indication_start_date") %>%
        dplyr::distinct(),
      by = c()
      ) %>%
      dplyr::compute()
  }

  # unkown indication
  if (unknownIndication == TRUE) {
    for (table in unknownIndicarionTables) {
      target_db_unkown <- target_db %>%
        dplyr::filter(is.na(.data$indication_id))
      if (target_db_unkown %>% dplyr::tally() %>% dplyr::pull() > 0) {
        table_indication <- cdm[[table]] %>%
          dplyr::rename("unkown_indication_start_date" = get_start_date[[table]]) %>%
          dplyr::select("person_id", "start_date") %>%
          dplyr::distinc()
        if (indicationGap == 0) {
          target_db_unkown <- target_db_unkown %>%
            dplyr::inner_join(
              table_indication,
              by = c("subject_id" = "person_id", "cohort_start_date" = "unkown_indication_start_date")
            ) %>%
            dplyr::compute()
        } else {
          target_db_unkown <- target_db_unkown %>%
            dplyr::left_join(
              table_indication,
              by = c("subject_id" = "person_id")
            ) %>%
            dplyr::mutate(dif_time_unkown_indication = as.numeric(difftime(
              .data$cohort_start_date,
              .data$unkown_indication_start_date,
              units = "days"
            ))) %>%
            dplyr::filter(
              .data$dif_time_unkown_indication <= .env$indication_gap
            ) %>%
            dplyr::filter(.data$dif_time_unkown_indication >= 0) %>%
            dplyr::select(
              -"dif_time_unkown_indication",
              -"unkown_indication_start_date"
            ) %>%
            dplyr::distinct() %>%
            dplyr::compute()
        }
        target_db <- target_db %>%
          dplyr::anti_join(
            target_db_unkown,
            by = c("subject_id", "cohort_start_date")
          ) %>%
          dplyr::union(
            target_db_unkown %>%
              dplyr::mutate(indication_id = 0)
          ) %>%
          dplyr::compute()
      }
    }
  }

  target_db <- target_db %>%
    dplyr::inner_join(
      indicationDefinitionSet %>%
        dplyr::select("indication_id", "indication_name") %>%
        rbind(dplyr::tibble(
          indication_id = c(0, NA),
          indication_name = c("Unkown indication", "No indication")
        )) %>%
        dplyr::distinct(),
      by = "indication_id",
      copy = TRUE
    )

  cdm[[indicationTableName]] <- target_db

  return(cdm)
}

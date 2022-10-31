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
#' @param cdm cdm
#' @param specifications specifications
#' @return
#' @export
#'
#' @examples
computeAggregation <- function(cdm,
                               personSummaryName,
                               genderAggregation,
                               ageGroupsAgregation,
                               ageGroups,
                               yearAggregation,
                               yearMonthAggregation,
                               initialDoseAggregation,
                               meanDoseAggregation,
                               indicationAggregation,
                               indicationTableName,
                               aggregationTableName,
                               permanentAggregationTable,
                               verbose) {
  aggregationTable <- cdm[[personSummaryName]] %>%
    dplyr::mutate(aggregation = "All") %>%
    dplyr::mutate(value = as.character(NA)) %>%
    dplyr::select("subject_id", "aggregation", "value") %>%
    dplyr::compute()

  if (genderAggregation == TRUE) {
    aggregationTable <- aggregationTable %>%
      dplyr::union(
        cdm[[personSummaryName]] %>%
          dplyr::mutate(aggregation = "Gender") %>%
          dplyr::mutate(value = .data$gender) %>%
          dplyr::select("subject_id", "aggregation", "value")
      ) %>%
      dplyr::compute()
  }

  if (ageGroupsAgregation == TRUE){
    aggregationTableAge <- lapply(ageGroups, function(x){
      groupName <- paste0(x[1], ";", x[2])
      return(cdm[[personSummaryName]] %>%
        dplyr::filter(.data$age >= .env$x[1]) %>%
        dplyr::filter(.data$age <= .env$x[2]) %>%
        dplyr::mutate(agregation = "Age groups") %>%
        dplyr::mutate(value = groupName) %>%
        dplyr::compute())
    })
    aggregationTable <- aggregationTable %>%
      dplyr::union(dplyr::bind_rows(aggregationTableAge)) %>%
      dplyr::compute()
  }

  if (yearAggregation == TRUE){
    aggregationTable <- aggregationTable %>%
      dplyr::union(
        cdm[[personSummaryName]] %>%
          dplyr::mutate(aggregation = "Index year") %>%
          dplyr::mutate(value = as.character(lubridate::year(
            .data$cohort_start_date))) %>%
          dplyr::select("subject_id", "aggregation", "value")
      ) %>%
      dplyr::compute()
  }

  if (yearMonthAggregation == TRUE){
    aggregationTable <- aggregationTable %>%
      dplyr::union(
        cdm[[personSummaryName]] %>%
          dplyr::mutate(aggregation = "Index month-year") %>%
          dplyr::mutate(value = as.character(paste0(
            lubridate::month(.data$cohort_start_date),
            "_",
            lubridate::year(.data$cohort_start_date)))) %>%
          dplyr::select("subject_id", "aggregation", "value")
      ) %>%
      dplyr::compute()
  }

  if (initialDoseAggregation == TRUE){
    aggregationTable <- aggregationTable %>%
      dplyr::union(
        cdm[[personSummaryName]] %>%
          dplyr::mutate(aggregation = "Initial dose") %>%
          dplyr::mutate(value = as.character(.data$initial_dose)) %>%
          dplyr::select("subject_id", "aggregation", "value")
      ) %>%
      dplyr::compute()
  }

  if (meanDoseAggregation == TRUE){
    aggregationTable <- aggregationTable %>%
      dplyr::union(
        cdm[[personSummaryName]] %>%
          dplyr::mutate(aggregation = "Mean dose") %>%
          dplyr::mutate(value = as.character(round(
            .data$cumulative_dose/.data$exposed_days))) %>%
          dplyr::select("subject_id", "aggregation", "value")
      ) %>%
      dplyr::compute()
  }

  if (indicationAggregation == TRUE){
    aggregationTable <- aggregationTable %>%
      dplyr::union(
        cdm[[indicationTableName]] %>%
          dplyr::mutate(aggregation = "Indication") %>%
          dplyr::rename("value" = "indication_name") %>%
          dplyr::select("subject_id", "aggregation", "value")
      ) %>%
      dplyr::compute()
  }
  #filter minimum counts
  cdm[[aggregationTableName]] <- aggregationTable

  return(cdm)
}

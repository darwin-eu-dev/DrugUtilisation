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
#' @param cdm cdm
#' @param tableToCharacterizeName tableToCharacterizeName
#' @param characterizationTableName characterizationTableName
#' @param tableResultsName tableResultsName
#' @param tableAggregationName tableAggregationName
#' @param minimumCounts minimumCounts
#' @param verbose verbose
#' @return
#' @export
#'
#' @examples
generateCharacterizationTable <- function(cdm,
                                          tableToCharacterizeName,
                                          characterizationTableName,
                                          tableResultsName,
                                          tableAggregationName,
                                          minimumCounts,
                                          verbose) {
  tableCharacteristics <- cdm[[tableToCharacterizeName]] %>%
    dplyr::inner_join(
      cdm[[tableAggregationName]],
      by = "person_id"
    ) %>%
    dplyr::group_by(
      .data$aggregation,
      .data$value,
      .data$covariate,
      .data$concept_id,
      .data$window_name
    ) %>%
    dplyr::summarise(number_counts = dplyr::n()) %>%
    dplyr::mutate(
      number_counts = dplyr::if_else(
        .data$number_counts < .env$minimumCounts
      ),
      NA,
      .data$number_counts
    ) %>%
    dplyr::collect() %>%
    tidyr::pivot_wider(
      names_from = "window_name",
      values_from = "number_counts",
      values_fill = 0
    ) %>%
    dplyr::mutate(
      covariate = paste0(.data$covariate, " (", .data$concept_id, ")")
    )

  return(NULL)
}

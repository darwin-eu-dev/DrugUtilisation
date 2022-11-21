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
#' @param tableToSummarizeName tableToSummarizeName
#' @param variables variables
#' @param estimates estimates
#' @param tableResultsName tableResultsName
#' @param tableAggregationName tableAggregationName
#' @param verbose verbose
#'
#' @return
#' @export
#'
#' @examples
generateSummarizedTable <- function(cdm,
                                    tableToSummarizeName,
                                    variables,
                                    estimates,
                                    tableResultsName,
                                    tableAggregationName,
                                    verbose) {

  # estimates can not be names aggregation or value

  estimates <- c("mean", "q25")
  estimates_func <- list(
    min = min,
    max = max,
    mean = mean,
    median = stats::median(),
    iqr = stats::IQR(),
    range = range,
    q5 = function(x) {
      stats::quantile(x, 0.05)
    },
    q10 = function(x) {
      stats::quantile(x, 0.10)
    },
    q15 = function(x) {
      stats::quantile(x, 0.15)
    },
    q20 = function(x) {
      stats::quantile(x, 0.20)
    },
    q25 = function(x) {
      stats::quantile(x, 0.25)
    },
    q30 = function(x) {
      stats::quantile(x, 0.30)
    },
    q35 = function(x) {
      stats::quantile(x, 0.35)
    },
    q40 = function(x) {
      stats::quantile(x, 0.40)
    },
    q45 = function(x) {
      stats::quantile(x, 0.45)
    },
    q50 = function(x) {
      stats::quantile(x, 0.50)
    },
    q55 = function(x) {
      stats::quantile(x, 0.55)
    },
    q60 = function(x) {
      stats::quantile(x, 0.60)
    },
    q65 = function(x) {
      stats::quantile(x, 0.65)
    },
    q70 = function(x) {
      stats::quantile(x, 0.70)
    },
    q75 = function(x) {
      stats::quantile(x, 0.75)
    },
    q80 = function(x) {
      stats::quantile(x, 0.80)
    },
    q85 = function(x) {
      stats::quantile(x, 0.85)
    },
    q90 = function(x) {
      stats::quantile(x, 0.90)
    },
    q95 = function(x) {
      stats::quantile(x, 0.95)
    }
  )

  estimates_func <- estimates_func[estimates]

  cdm[[tableResultsName]] <- cdm[[tableAggregationName]] %>%
    dplyr::select("aggregation", "value") %>%
    dplyr::inner_join(
      cdm[[tableToSummarizeName]] %>%
        dplyr::select("subject_id", dplyr::all_of(variables)),
      by = "subject_id"
    ) %>%
    dplyr::select(-"subject_id") %>%
    dplyr::group_by(.data$aggregation, .data$value) %>%
    dplyr::summarise_at(
      dplyr::vars({{ .env$variables }}),
      estimates_func
    ) %>%
    dplyr::compute()

}

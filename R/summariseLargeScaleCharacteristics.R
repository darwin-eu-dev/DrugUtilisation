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

#' @importFrom PatientProfiles summariseLargeScaleCharacteristics
#' @export
PatientProfiles::summariseLargeScaleCharacteristics

#' Summarise a cohort from multipl codelist and windows
#'
#' @param cohort Cohort to summarise
#' @param cdm cdm_reference
#' @param conceptSet A list of concept sets
#' @param strata Stratification list
#' @param window Windows to characterize
#' @param overlap Whether we consider episodes (overlap = TRUE) or incident
#' (overlap = FALSE)
#' @param minCellCount Minimum cell counts
#'
#' @return A SummarisedResults object that contains the characterization
#'
#' @export
#'
summariseCharacteristicsFromCodelist <- function(cohort,
                                                 cdm = lifecycle::deprecated(),
                                                 conceptSet,
                                                 strata = list(),
                                                 window = list(
                                                   c(-Inf, -366), c(-365, -31),
                                                   c(-30, -1), c(0, 0),
                                                   c(1, 30), c(31, 365),
                                                   c(366, Inf)
                                                 ),
                                                 overlap = TRUE,
                                                 minCellCount = lifecycle::deprecated()) {
  lifecycle::deprecate_warn(
    when = "0.5.0",
    what = "summariseCharacteristicsFromCodelist()",
    with = "PatientProfiles::summariseCharacteristics(conceptIntersect)"
  )

  if (lifecycle::is_present(minCellCount)) {
    lifecycle::deprecate_soft(
      when = "0.5.0", what = "summariseCharacteristicsFromCodelist(minCellCount = )"
    )
  }

  if (lifecycle::is_present(cdm)) {
    lifecycle::deprecate_soft(
      when = "0.5.0", what = "summariseCharacteristicsFromCodelist(cdm = )"
    )
  }

  if (overlap) {
    targetEndDate <- "event_end_date"
  } else {
    targetEndDate <- NULL
  }

  result <- PatientProfiles::summariseCharacteristics(
    cohort = cohort,
    demographics = FALSE,
    strata = strata,
    tableIntersect = list(),
    cohortIntersect = list(),
    conceptIntersect = list(list(
      conceptSet = conceptSet, window = window, targetEndDate = targetEndDate,
      value = "flag"
    ))
  ) |>
    dplyr::mutate(
      "package_name" = "DrugUtilisation",
      "package_version" = as.character(utils::packageVersion("DrugUtilisation"))
    )

  return(result)
}

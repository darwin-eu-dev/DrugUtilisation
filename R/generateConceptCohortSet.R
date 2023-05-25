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

#' Generates a cohort for a certain list of concepts.
#'
#' @param cdm 'cdm' object created with CDMConnector::cdm_from_con().
#' @param name Name of the GeneratedCohortSet.
#' @param conceptSetList Named list of concept sets.
#' @param daysPriorHistory Minimum number of days of prior history required for
#' the incident events. If NULL, it is not required that the veent is within the
#' observation period.
#' @param gap Number of days between two events to be joined.
#' @param washout Prior days of washout without a previous event.
#' @param offset Number of days of offset after the cohort_end_date.
#' @param cohortDateRange Range for cohort_start_date and cohort_end_date
#'
#' @return The function returns the 'cdm' object with the created cohort.
#' @export
#'
#' @examples
generateConceptCohortSet <- function(cdm,
                                     name,
                                     conceptSetList,
                                     daysPriorHistory = 0,
                                     gap = 0,
                                     washout = 0,
                                     offset = 0,
                                     cohortDateRange = as.Date(c(NA, NA))) {
  # check input
  checkInputs(
    cdm = cdm, name = name, conceptSetList = conceptSetList,
    daysPriorHistory = daysPriorHistory, gapEra = gap,
    priorUseWashout = washout, offset = offset,
    cohortDateRange = cohortDateRange
  )

  # create conceptSet
  conceptSet <- conceptSetFromConceptSetList(conceptSetList)

  # create cohortSet
  cohortSet <- attr(conceptSet, "cohort_set") %>%
    dplyr::mutate(
      days_prior_history = dplyr::if_else(
        is.null(.env$daysPriorHistory), NA, .env$daysPriorHistory
      ),
      gap = .env$gap,
      washout = .env$washout,
      offset = .env$offset,
      cohort_start_date_range = .env$cohortDateRange[1],
      cohort_end_date_range = .env$cohortDateRange[2]
    )

  # subset tables
  cohortRef <- subsetTables(cdm, conceptSet)
  cohortAttritionRef <- computeCohortAttrition(cohortRef, cdm)

  # check daysPriorHistory
  cohortRef <- requireDaysPriorHistory(cohortRef, cdm, daysPriorHistory)
  cohortAttritionRef <- computeCohortAttrition(
    cohortRef, cdm, cohortAttritionRef, "Satisfy daysPriorHistory"
  )


  # union overlap
  cohortRef <- unionCohort(cohortRef, gap, cdm)
  cohortAttritionRef <- computeCohortAttrition(
    cohortRef, cdm, cohortAttritionRef, "Join records within gap distance"
  )

  # apply washout
  cohortRef <- applyWashout(cohortRef, washout)
  cohortAttritionRef <- computeCohortAttrition(
    cohortRef, cohortAttritionRef, "Washout applied"
  )

  # trim start date
  cohort <- trimStartDate(cohort, cdm, cohortDateRange[1])
  attrition <- computeCohortAttrition(
    cohort, cdm, attrition, "cohort_start_date >= cohort_dates_range_start"
  )

  # offset
  cohortRef <- cohortRef %>%
    dplyr::mutate(
      cohort_end_date = !!CDMConnector::dateadd("cohort_end_date", offset)
    ) %>%
    computeTable(cdm)

  # trim end date
  cohort <- trimEndDate(cohort, cdm, cohortDateRange[2])
  attrition <- computeCohortAttrition(
    cohort, cdm, attrition, "cohort_end_date <= cohort_dates_range_end"
  )

  # get counts
  cohortCountRef <- computeCohortCount(cohortRef, cdm)

  # insert set
  cohortSetRef <- cohortSet %>%
    insertTable(cdm, paste0(name, "_set"), FALSE)

  # insert attrition
  cohortAttritionRef <- cohortAttritionRef %>%
    computeTable(cdm)

  # validate cohort
  cdm[[name]] <- CDMConnector::newGeneratedCohortSet(
    cohortRef, cohortSetRef, cohortAttritionRef, cohortCountRef
  )
  return(cdm)
}


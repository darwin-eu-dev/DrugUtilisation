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
#'
#' @export
#'
#' @examples
#' \donttest{
#' library(DrugUtilisation)
#' library(CDMConnector)
#'
#' cdm <- mockDrugUtilisation()
#'
#' conditions <- list("headache" = 378253, "asthma" = 317009)
#'
#' cdm <- generateConceptCohortSet(
#'   cdm = cdm,
#'   name = "covariates",
#'   conceptSetList = conditions,
#'   daysPriorHistory = 365
#' )
#'
#' cdm$covariates
#'
#' cohortSet(cdm$covariates)
#'
#' cohortCount(cdm$covariates)
#'
#' cohortAttrition(cdm$covariates)
#' }
#'
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
    daysPriorHistory = daysPriorHistory, gap = gap,
    priorUseWashout = washout, offset = offset,
    cohortDateRange = cohortDateRange
  )

  # create conceptSet
  conceptSet <- conceptSetFromConceptSetList(conceptSetList)

  # create cohortSet
  cohortSet <- attr(conceptSet, "cohort_set") %>%
    dplyr::mutate(
      days_prior_history = as.character(dplyr::coalesce(.env$daysPriorHistory, as.numeric(NA))),
      gap = as.character(.env$gap),
      washout = as.character(.env$washout),
      offset = as.character(.env$offset),
      cohort_date_range_start = as.character(.env$cohortDateRange[1]),
      cohort_date_range_end = as.character(.env$cohortDateRange[2])
    )

  # subset tables
  cohortRef <- subsetTables(cdm, conceptSet)
  cohortAttritionRef <- computeCohortAttrition(cohortRef, cdm)

  if (cohortRef %>% dplyr::tally() %>% dplyr::pull("n") > 0) {

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
    cohortRef <- requirePriorUseWashout(cohortRef, cdm, washout)
    cohortAttritionRef <- computeCohortAttrition(
      cohortRef, cdm, cohortAttritionRef, "Washout applied"
    )

    # trim start date
    cohortRef <- trimStartDate(cohortRef, cdm, cohortDateRange[1])
    cohortAttritionRef <- computeCohortAttrition(
      cohortRef, cdm, cohortAttritionRef, "cohort_start_date >= cohort_dates_range_start"
    )

    # offset
    cohortRef <- cohortRef %>%
      dplyr::mutate(
        cohort_end_date = !!CDMConnector::dateadd("cohort_end_date", offset)
      ) %>%
      computeTable(cdm)

    # trim end date
    cohortRef <- trimEndDate(cohortRef, cdm, cohortDateRange[2])
    cohortAttritionRef <- computeCohortAttrition(
      cohortRef, cdm, cohortAttritionRef, "cohort_end_date <= cohort_dates_range_end"
    )

  }

  # create the cohort references
  cohortRef <- cohortRef %>%
    dplyr::select(
      "cohort_definition_id", "subject_id", "cohort_start_date",
      "cohort_end_date"
    ) %>%
    CDMConnector::computeQuery(
      name = paste0(attr(cdm, "write_prefix"), name),
      FALSE, attr(cdm, "write_schema"), TRUE
    )
  cohortSetRef <- cohortSet %>%
    insertTable(cdm, paste0(name, "_set"))
  cohortAttritionRef <- cohortAttritionRef %>%
    CDMConnector::computeQuery(
      name = paste0(attr(cdm, "write_prefix"), name, "_attrition"),
      FALSE, attr(cdm, "write_schema"), TRUE
    )
  cohortCountRef <- computeCohortCount(cohortRef, cdm) %>%
    CDMConnector::computeQuery(
      name = paste0(attr(cdm, "write_prefix"), name, "_count"),
      FALSE, attr(cdm, "write_schema"), TRUE
    )

  # create the resultant GeneratedCohortSet
  cdm[[name]] <- CDMConnector::newGeneratedCohortSet(
    cohortRef = cohortRef,
    cohortSetRef = cohortSetRef,
    cohortAttritionRef = cohortAttritionRef,
    cohortCountRef = cohortCountRef
  )

  return(cdm)
}


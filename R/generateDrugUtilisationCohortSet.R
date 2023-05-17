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

#' Generates a cohort of the drug use of a certain list of concepts.
#'
#' @param cdm A cdm_reference object.
#' @param name Name of the GeneratedCohortSet
#' @param conceptSetList Named list of concept sets.
#' @param summariseMode Choice on how to summarize the exposures. There are
#' three options:
#' "FixedTime" each individual is followed the exact same number of days
#' specified in 'fixedTime' argument.
#' "AllEras" we summarize the output will be a summary of the exposed eras of
#' each individual. Each individual can contribute multiple times.
#' "FirstEra" we only consider the first observable era of each individual. In
#' this case each individual can not contribute with multiple rows.
#' By default: "AllEras".
#' @param fixedTime Time period after first exposure where we summarize the
#' ingredient of interest. Argument only considered if 'summariseMode' =
#' "FixedTime". No default value is provided.
#' @param daysPriorHistory Minimum number of days of prior history
#' (observation time) required for the incident eras to be considered. By
#' default: 0, meaning it has to be in observation_period table.
#' When Null, we do not check if in observation_period table.
#' @param gapEra Number of days between two continuous exposures to be
#' considered in the same era. By default: 180.
#' @param priorUseWashout Prior days without exposure. By default: NULL.
#' @param cohortDateRange Range for cohort_start_date and cohort_end_date
#' @param imputeDuration Whether/how the duration should be imputed
#' "eliminate", "median", "mean", "quantile25", "quantile75".
#' @param durationRange Range between the duration must be comprised. It should
#' be a numeric vector of length two, with no NAs and the first value should be
#' equal or smaller than the second one. It is only required if imputeDuration
#' = TRUE. If NULL no restrictions are applied. By default: NULL.
#'
#' @return The function returns the 'cdm' object with the created tables as
#' references of the object.
#' @export
#'
#' @examples
generateDrugUtilisationCohortSet <- function(cdm,
                                             name,
                                             conceptSetList,
                                             summariseMode = "AllEras",
                                             fixedTime = 365,
                                             daysPriorHistory = 0,
                                             gapEra = 30,
                                             priorUseWashout = 0,
                                             cohortDateRange = as.Date(c(NA, NA)),
                                             imputeDuration = "eliminate",
                                             durationRange = c(1, Inf)) {
  checkInputs(
    cdm = cdm,  name = name, conceptSetList = conceptSetList,
    summariseMode = summariseMode, fixedTime = fixedTime,
    daysPriorHistory = daysPriorHistory, gapEra = gapEra,
    priorUseWashout = priorUseWashout, cohortDateRange = cohortDateRange,
    imputeDuration = imputeDuration, durationRange = durationRange
  )

  # tables to be deleted
  firstTempTable <- getOption("dbplyr_table_name", 0) + 1

  # get conceptSet
  conceptSet <- conceptSetFromConceptSetList(conceptSetList)

  # generate cohort set
  cohortSet <- attr(conceptSet, "cohort_set") %>%
    dplyr::mutate(
      summarise_mode = .env$summariseMode,
      fixed_time = .env$fixedTime,
      days_prior_history = .env$daysPriorHistory,
      gap_era = .env$gapEra,
      prior_use_washout = .env$priorUseWashout,
      cohort_dates_range_start = .env$cohortDateRange[1],
      cohort_dates_range_end = .env$cohortDateRange[2],
      impute_duration = .env$imputeDuration,
      duration_range_min = .env$durationRange[1],
      duration_range_max = .env$durationRange[2]
    )

  # subset drug_exposure and only get the drug concept ids that we are
  # interested in.
  cohort <- subsetTables(cdm, conceptSet, "Drug")
  if (cohort %>% dplyr::tally() %>% dplyr::pull("n") == 0) {
    cli::cli_abort("No record found with the current specifications in
    drug_exposure table")
  }
  attrition <- computeCohortAttrition(cohort, cdm)

  # correct duration
  cohort <- correctDuration(cohort, durationRange, cdm)
  reason <- paste(
    "Duration imputation; affected rows:", attr(cohort, "numberImputations")
  )
  attrition <- computeCohortAttrition(cohort, cdm, attrition, reason)

  # eliminate overlap
  cohort <- unionCohort(cohort, gapEra)
  attrition <- computeCohortAttrition(cohort, cdm, attrition, "Join eras")

  # require daysPriorHistory
  cohort <- requireDaysPriorHistory(cohort, cdm, daysPriorHistory)
  attrition <- computeCohortAttrition(cohort, cdm, attrition, "daysPriorHistory applied")

  # require priorUseWashout
  cohort <- requirePriorUseWashout(cohort, cdm, priorUseWashout)
  attrition <- computeCohortAttrition(cohort, cdm, attrition, "priorUseWashout applied")

  # require cohortDateRange
  cohort <- trimCohortDateRange(cohort, cdm, cohortDateRange)
  attrition <- computeCohortAttrition(cohort, cdm, attrition, "cohortDateRange applied")

  # apply summariseMode
  cohort <- applySummariseMode(cohort, cdm, summariseMode, fixedTime)
  attrition <- computeCohortAttrition(
    cohort, cdm, attrition,
    paste("summariseMode:", summariseMode, "applied")
  )

  # create the cohort references
  cohortRef <- cohort %>%
    CDMConnector::computeQuery(
      name = paste0(attr(cdm, "write_prefix"), name),
      FALSE, attr(cdm, "write_schema"), TRUE
    )
  cohortSetRef <- cohortSet %>%
    insertTable(cdm, paste0(name, "_set"), FALSE)
  cohortAttritionRef <- attrition %>%
    CDMConnector::computeQuery(
      name = paste0(attr(cdm, "write_prefix"), name, "_attrition"),
      FALSE, attr(cdm, "write_schema"), TRUE
    )
  cohortCountRef <- computeCohortCount(cohort, cdm) %>%
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

  # drop intermediary tables that were created in the process
  lastTempTable <- getOption("dbplyr_table_name", 0)
  if (!is.null(attr(cdm, "write_prefix")) & firstTempTable <= lastTempTable) {
    CDMConnector::dropTable(
      cdm, sprintf("dbplyr_%03i", firstTempTable:lastTempTable)
    )
  }

  return(cdm)
}
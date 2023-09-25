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
#' @param daysPriorObservation Minimum number of days of prior observation
#' required for the incident eras to be considered. If NULL its is not required
#' to be within observation_period.
#' @param gapEra Number of days between two continuous exposures to be
#' considered in the same era. By default: 0.
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
#'
#' @export
#'
#' @examples
#' \donttest{
#' library(CodelistGenerator)
#' library(CDMConnector)
#' library(DrugUtilisation)
#'
#' cdm <- mockDrugUtilisation()
#'
#' druglist <- getDrugIngredientCodes(cdm, c("acetaminophen", "metformin"))
#'
#' cdm <- generateDrugUtilisationCohortSet(
#'   cdm = cdm,
#'   name = "drug_cohorts",
#'   conceptSetList = druglist,
#'   daysPriorObservation = 365
#' )
#'
#' cdm$drug_cohorts
#'
#' cohortSet(cdm$drug_cohorts)
#'
#' cohortCount(cdm$drug_cohorts)
#'
#' cohortAttrition(cdm$drug_cohorts)
#' }
#'
generateDrugUtilisationCohortSet <- function(cdm,
                                             name,
                                             conceptSetList,
                                             summariseMode = "AllEras",
                                             fixedTime = NULL,
                                             daysPriorObservation = 0,
                                             gapEra = 0,
                                             priorUseWashout = 0,
                                             cohortDateRange = as.Date(c(NA, NA)),
                                             imputeDuration = "eliminate",
                                             durationRange = c(1, Inf)) {
  checkInputs(
    cdm = cdm,  name = name, conceptSetList = conceptSetList,
    summariseMode = summariseMode, fixedTime = fixedTime,
    daysPriorObservation = daysPriorObservation, gapEra = gapEra,
    priorUseWashout = priorUseWashout, cohortDateRange = cohortDateRange,
    imputeDuration = imputeDuration, durationRange = durationRange
  )

  # get conceptSet
  conceptSet <- conceptSetFromConceptSetList(conceptSetList)

  # generate cohort set
  cohortSetRef <- attr(conceptSet, "cohort_set") %>%
    dplyr::mutate(
      summarise_mode = .env$summariseMode,
      fixed_time = as.character(dplyr::coalesce(.env$fixedTime, as.numeric(NA))),
      days_prior_observation = as.character(dplyr::coalesce(
        .env$daysPriorObservation, as.numeric(NA)
      )),
      gap_era = as.character(.env$gapEra),
      prior_use_washout = as.character(.env$priorUseWashout),
      cohort_date_range_start = as.character(.env$cohortDateRange[1]),
      cohort_date_range_end = as.character(.env$cohortDateRange[2]),
      impute_duration = as.character(.env$imputeDuration),
      duration_range_min = as.character(.env$durationRange[1]),
      duration_range_max = as.character(.env$durationRange[2])
    ) %>%
    insertTable(cdm, paste0(name, "_set"))

  # subset drug_exposure and only get the drug concept ids that we are
  # interested in.
  cohort <- subsetTables(cdm, conceptSet, "Drug")
  attrition <- computeCohortAttrition(cohort, cdm, cohortSet = cohortSetRef)

  if (cohort %>% dplyr::tally() %>% dplyr::pull("n") > 0) {

    # correct duration
    cohort <- correctDuration(cohort, imputeDuration, durationRange, cdm)
    reason <- paste(
      "Duration imputation; affected rows:", attr(cohort, "numberImputations")
    )
    attrition <- computeCohortAttrition(
      cohort, cdm, attrition, reason, cohortSet = cohortSetRef
    )

    # eliminate overlap
    cohort <- unionCohort(cohort, gapEra, cdm)
    attrition <- computeCohortAttrition(
      cohort, cdm, attrition, "Join eras", cohortSet = cohortSetRef
    )

    # require priorUseWashout
    cohort <- requirePriorUseWashout(cohort, cdm, priorUseWashout)
    attrition <- computeCohortAttrition(
      cohort, cdm, attrition, paste0(
        "prior use wahout of ", priorUseWashout, " days"
      ), cohortSet = cohortSetRef
    )

    # require daysPriorObservation
    if (!is.null(daysPriorObservation)) {
      cohort <- requireDaysPriorObservation(cohort, cdm, daysPriorObservation)
      attrition <- computeCohortAttrition(
        cohort, cdm, attrition, paste0(
          "at least ", daysPriorObservation, " prior observation"
        ), cohortSet = cohortSetRef
      )
    }

    # trim start date
    cohort <- trimStartDate(cohort, cdm, cohortDateRange[1])
    attrition <- computeCohortAttrition(
      cohort, cdm, attrition, paste0(
        "cohort_start_date >= ", cohortDateRange[1]
      ), cohortSet = cohortSetRef
    )

    # trim end date
    cohort <- trimEndDate(cohort, cdm, cohortDateRange[2])
    attrition <- computeCohortAttrition(
      cohort, cdm, attrition, paste0(
        "cohort_end_date <= ", cohortDateRange[2]
      ), cohortSet = cohortSetRef
    )

    # apply summariseMode
    cohort <- applySummariseMode(cohort, cdm, summariseMode, fixedTime)
    attrition <- computeCohortAttrition(
      cohort, cdm, attrition, paste("summariseMode:", summariseMode, "applied"),
      cohortSet = cohortSetRef
    )

  }

  # create the cohort references
  cohortRef <- cohort %>%
    dplyr::select(
      "cohort_definition_id", "subject_id", "cohort_start_date",
      "cohort_end_date"
    ) %>%
    CDMConnector::computeQuery(
      name = paste0(attr(cdm, "write_prefix"), name),
      FALSE, attr(cdm, "write_schema"), TRUE
    )
  cohortAttritionRef <- attrition %>%
    CDMConnector::computeQuery(
      name = paste0(attr(cdm, "write_prefix"), name, "_attrition"),
      FALSE, attr(cdm, "write_schema"), TRUE
    )
  cohortCountRef <- computeCohortCount(
    cohort, cdm, cohortSet = cohortSetRef
  ) %>%
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

  #add cdm_reference as attribute
  attr(cdm[[name]], "cdm_reference") <- cdm

  return(cdm)
}

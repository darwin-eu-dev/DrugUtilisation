#' Generates a cohort for a certain list of concepts.
#'
#' @param cdm 'cdm' object created with CDMConnector::cdm_from_con().
#' @param conceptSetList Named list of concept sets.
#' @param name Name of the created cohort.
#' @param temporary Wether the cohort is temporal (TRUE) or permanent (FALSE).
#' @param daysPriorHistory Minimum number of days of prior history required for
#' the incident events. If NULL, it is not required that the veent is within the
#' observation period.
#' @param gap Number of days between two events to be joined.
#' @param washout Prior days of washout without a previous event.
#' @param offset Number of days of offset after the cohort_end_date.
#' @param minimumCohortStartDate Minimum cohort start date.
#' @param maximumCohortEndDate Maximum cohort end date.
#'
#' @return The function returns the 'cdm' object with the created cohort.
#' @export
#'
#' @examples
#' \donttest{
#'   library(xxx)
#'   db <- DBI::dbConnect()
#'   cdm <- mockCdm(db, ...)
#'   cdm <- generateConceptCohortSet(
#'     cdm = cdm,
#'     conceptSetList = list(asthma = c(1, 2), covid = c(4, 5)),
#'     name = "respiratory_complications_cohort"
#'   )
#' }
#'
generateConceptCohortSet <- function(cdm,
                                     conceptSetList,
                                     name,
                                     temporary = TRUE,
                                     daysPriorHistory = 0,
                                     gap = 0,
                                     washout = 0,
                                     offset = 0,
                                     minimumCohortStartDate = NULL,
                                     maximumCohortEndDate = NULL) {
  # check input
  # offset must be smaller than gap
  checkInput(
    cdm, coneptSetList, name, temporary, daysPriorHistory, gap, washout, offset,
    cohortStartDateRange, cohortEndDateRange
  )
  # create cohort set
  cohortSetRef <- conceptSetFromConceptSetList(conceptSetList) %>%
    dplyr::mutate(
      days_prior_history = dplyr::if_else(
        is.null(.env$daysPriorHistory), NA, .env$dayPriorHistory
      ),
      gap = .env$gap,
      washout = .env$washout,
      offset = .env$offset,
      cohort_start_date_range = .env$cohortStartDateRange,
      cohort_end_date_range = .env$cohortEndDateRange
    ) %>%
    CDMConnector::computeQuery()
  # subset tables
  cohortRef <- subsetTables(cdm, cohortSetRef)
  cohortAttritionRef <- addAttritionLine(cohortRef)
  # check daysPriorHistory
  cohortRef <- minimumDaysPriorHistory(cohortRef, cdm, daysPriorHistory)
  cohortAttritionRef <- addAttritionLine(
    cohortRef, cohortAttritionRef, "Satisfy daysPriorHistory"
  )
  # union overlap
  cohortRef <- unionCohort(cohortRef, gap)
  cohortAttritionRef <- addAttritionLine(
    cohortRef, cohortAttritionRef, "Join records within gap distance"
  )
  # apply washout
  cohortRef <- applyWashout(cohortRef, washout)
  cohortAttritionRef <- addAttritionLine(
    cohortRef, cohortAttritionRef, "Washout applied"
  )
  # offset
  cohortRef <- cohortRef %>%
    dplyr::mutate(
      cohort_end_date = !!CDMConnector::dateadd("cohort_end_date", offset)
    )
  # minimum cohort start date
  cohortRef <- applyMinimumStartDate(cohortRef, minimumCohortStartDate)
  cohortAttritionRef <- addAttritionLine(
    cohortRef, cohortAttritionRef, "Trimming cohort_start_date"
  )
  # maximum cohort end date
  cohortRef <- applyMaximumEndDate(cohortRef, maximumCohortEndDate)
  cohortAttritionRef <- addAttritionLine(
    cohortRef, cohortAttritionRef, "Trimming cohort_end_date"
  )
  # get counts
  cohortCountRef <- computeCohortCount(cohort)
  # clean tables

  # validate cohort
  cohort <- CDMConnector::newGeneratedCohortSet(
    cohortRef, cohortSetRef, cohortAttritionRef, cohortCountRef
  )
}


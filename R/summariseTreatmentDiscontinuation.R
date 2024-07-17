
#' Summarise treatment discontinuation
#' @param cdm CDM reference
#' @param targetCohortTable targetCohortTable
#' @param targetCohortId targetCohortId
#' @param outcomeWashout Washout time in days for the outcome
#' @param censorOnCohortExit If TRUE, an individual's follow up will be
#' censored at their cohort exit
#' @param censorOnDate if not NULL, an individual's follow up will be censored
#' at the given date
#' @param followUpDays Number of days to follow up individuals (lower bound 1,
#' upper bound Inf)
#' @param strata strata
#' @param eventGap Days between time points for which to report survival
#' events, which are grouped into the specified intervals.
#' @param estimateGap Days between time points for which to report survival
#' estimates. First day will be day zero with risk estimates provided
#' for times up to the end of follow-up, with a gap in days equivalent
#' to eventGap.
#' @param restrictedMeanFollowUp number of days of follow-up to take into account
#' when calculating restricted mean for all cohorts
#' @param minimumSurvivalDays Minimum number of days required for the main cohort
#' to have survived
#' @param minCellCount The minimum number of events to reported, below which
#' results will be obscured. If 0, all results will be reported.
#'
#' @return tibble with discontinuation information for the drug cohort,
#' including: time, people at risk, survival probability, cumulative incidence,
#' 95 CIs, strata and outcome. A tibble with the number of events is
#' outputted as an attribute of the output
#' @export
summariseTreatmentDiscontinuation <- function(cdm,
                                              targetCohortTable,
                                              targetCohortId = NULL,
                                              outcomeWashout = Inf,
                                              censorOnDate = NULL,
                                              followUpDays = Inf,
                                              strata = NULL,
                                              eventGap = 30,
                                              estimateGap = 1,
                                              restrictedMeanFollowUp = NULL,
                                              minimumSurvivalDays = 1,
                                              minCellCount = 5){


  CohortSurvival::estimateSingleEventSurvival(cdm = cdm,
                                              targetCohortTable = targetCohortTable,
                                              targetCohortId = targetCohortId,
                                              outcomeCohortTable = targetCohortTable,
                                              outcomeCohortId = targetCohortId,
                                              outcomeDateVariable = "cohort_end_date",
                                              outcomeWashout = outcomeWashout,
                                              censorOnCohortExit = FALSE,
                                              censorOnDate = censorOnDate,
                                              followUpDays = followUpDays,
                                              strata = strata,
                                              eventGap = eventGap,
                                              estimateGap = estimateGap,
                                              restrictedMeanFollowUp = restrictedMeanFollowUp,
                                              minimumSurvivalDays = minimumSurvivalDays,
                                              minCellCount = minCellCount,
                                              returnParticipants = FALSE)

}

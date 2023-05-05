#' Generate a cohort using the observation period
#'
#' @param cdm A cdm reference.
#' @param name Name of the cohort to be generated.
#' @param temporary Wether the generated is a temporal or a permanent table
#'
#' @return The cdm object with the instantiated cohort.
#'
#' @export
#'
#' @examples
#' \donttest{
#'   library(xxx)
#'   db <- DBI::dbConnect()
#'   cdm <- mockCdm(db, ...)
#'   cdm <- generateObservationPeriodCohortSet(cdm)
#'   DBI::dbDisconnect(db)
#' }
#'
generateObservationPeriodCohortSet <- function(cdm,
                                               name,
                                               temporary = TRUE,
                                               daysPriorHistory = 0) {
  cohortRef <- cdm$observation_period %>%
    dplyr::rename(
      "subject_id" = "person_id",
      "cohort_start_date" = "observation_period_start_date",
      "cohort_end_date" = "observation_period_end_date"
    ) %>%
    dplyr::mutate(cohort_definition_id = 1) %>%
    dplyr::select(
      "cohort_definition_id", "subject_id", "cohort_start_date",
      "cohort_end_date"
    ) %>%
    CDMConnector::computeQuery()
  cohortSetRef <- dplyr::tibble(
    cohort_definition_id = 1, cohort_name = "observation_period_cohort"
  )
  cohortAttritionRef <- addAttritionLine(cohort)
  cohortCountRef <- computeCohortCount(cohort)
  cdm[[name]] <- CDMConnector::newGeneratedCohortSet(
    cohortRef, cohortSetRef, cohortAttritionRef, cohortCountRef
  )
  return(cdm)
}

#' Generates a cohort for a certain list age groups.
#'
#' @param cdm 'cdm' object created with CDMConnector::cdm_from_con().
#' @param ageGroup Named list of ageGroups.
#' @param name Name of the created cohort.
#' @param temporary Wether the cohort is temporal (TRUE) or permanent (FALSE).
#' @param daysPriorHistory Minimum number of days of prior history required for
#' the incident events. If NULL, it is not required that the event is within the
#' observation period.
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
#'   cdm <- generateAgeCohortSet(
#'     cdm = cdm,
#'     conceptSetList = list(asthma = c(1, 2), covid = c(4, 5)),
#'     name = "respiratory_complications_cohort"
#'   )
#' }
generateAgeCohortSet <- function(cdm,
                                 ageGroup = list("any_age" = c(0, 150)),
                                 name,
                                 teporary = FALSE,
                                 daysPriorHistory = 0,
                                 minimumCohortStartDate = NULL,
                                 maximumCohortEndDate = NULL) {
  # check inputs
  inputCheck(
    cdm, ageGroup, name, temporary, daysPriorHistory, minimumCohortStartDate,
    maximumCohortEndDate
  )
  # create age cohorts
  cohort <- ageCohort(cdm, ageGroup)
  cohortSet <- attr(cohort, "cohortSet")
  attrition <- addAttritionLine(cohort)

}

#' @noRd
ageCohort <- function(cdm, ageGroups) {
  cdm <- generateObservationPeriodCohortSet(cdm, "observation_cohort")
  cdm$observation_cohort <- addDateOfBirth(cdm$observation_cohort, cdm)
  cohortSetRef <- dplyr::tibble(
    cohort_definition_id = seq_along(ageGroups),
    cohort_name = names(ageGroups),
    start_contributing_age = lapply(ageGroups, function(x){x[1]}) %>% unlist(),
    end_contributing_age = lapply(ageGroups, function(x){x[2]}) %>% unlist()
  ) %>%
    insertDatabase(cdm)
  cohortRef <- NULL
  for (k in seq_along(ageGroups)) {
    ageGroup <- ageGroups[[k]]
    cohortRef <- cohortRef %>%
      dplyr::union_all(
        cdm$observation_cohort %>%
          dplyr::mutate(cohort_start_date2 = !!CDMConnector::dateadd(
            "birth_date", !!ageGroup[1], "year"
          )) %>%
          dplyr::mutate(cohort_end_date2 = !!CDMConnector::dateadd(
            "birth_date", !!ageGroup[2], "year"
          )) %>%
          dplyr::mutate(cohort_start_date )
      )

    cdm$person %>%
      addBirthDate(cdm, impose, name = "birth_date")
    dplyr::mutate(cohort_start_date)
  }
  cohortRef <- cohortRef %>% CDMConnector::computeQuery()
  cohortAttritionRef <- addAttritionLine(cohort)
  cohortCountRef <- computeCohortCount(cohort)

}

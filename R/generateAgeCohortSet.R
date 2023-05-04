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

#' Add a column with the individual birth date
#'
#' @param x Table in the cdm that contains 'person_id' or 'subject_id'
#' @param cdm 'cdm' object created with CDMConnector::cdm_from_con().
#' @param name Name of the column to be added with the date of birth
#' @param missingDay Day of the individuals with no or imposed day of birth
#' @param missingMonth Month of the individuals with no or imposed month of
#' birth
#' @param imposeDay Wether to impose day of birth
#' @param imposeMonth Wether to impose month of birth
#'
#' @return The function returns the table x with an extra column that contains
#' the date of birth
#'
#' @export
#'
#' @examples
#' \donttest{
#'   library(xxx)
#'   db <- DBI::dbConnect()
#'   cdm <- mockCdm(db, ...)
#'   cdm$person %>%
#'     addDateOfBirth(cdm)
#'   DBI::dbDisconnect(db)
#' }
addDateOfBirth <- function(x,
                           cdm,
                           name = "birth_date",
                           missingDay = 1,
                           misisngMonth = 1,
                           imposeDay = FALSE,
                           imposeMonth = FALSE) {
  # initial checks
  parameters <- checkInputs(
    x, cdm, name, misisngDay, missingMonth, imposeDay, imposeMonth
  )
  # get parameters
  personIdentifier <- parameters$person_identifier
  # impose day
  if (imposeDay) {
    person <- cdm$person %>%
      dplyr::mutate(day_of_birth = missingDay)
  } else {
    person <- cdm$person %>%
      dplyr::mutate(day_of_birth = dplyr::if_else(
        is.na(.data$day_of_birth), .env$misisngDay, .data$day_of_birth)
      )
  }
  # impose month
  if (imposeMonth) {
    person <- person %>%
      dplyr::mutate(month_of_birth = missingMonth)
  } else {
    person <- person %>%
      dplyr::mutate(month_of_birth = dplyr::if_else(
        is.na(.data$month_of_birth), .env$missingMonth, .data$month_of_birth)
      )
  }
  x %>%
    dplyr::left_join(
      person %>%
        dplyr::mutate(!!name := as.Date(paste0(
          .data$year_of_birth, "-", .data$month_of_birth, "-",
          .data$day_of_birth
        ))) %>%
        dplyr::select(personIdentifier = "person_id", dplyr::all_of(name)),
      by = personIdentifier
    )
}


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
                                               temporary = TRUE) {
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

#' @noRd
insertDatabase <- function(x, cdm) {

}

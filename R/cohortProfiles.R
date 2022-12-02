
#' @noRd
getGender <- function(cdm,
                      cohortTable,
                      cohortIds = NULL) {
  if (isTRUE(is.na(cohortIds))) {
    cohortIds <- NULL
  }

  if (!is.null(cohortIds)) {
    cohortDb <- cdm[[cohortTable]] %>%
      dplyr::filter(.data$cohort_definition_id %in% .env$cohortIds)
  } else {
    cohortDb <- cdm[[cohortTable]]
  }

  person <- cdm[["person"]] %>%
    dplyr::rename("subject_id" = "person_id") %>%
    dplyr::semi_join(cohortDb, by = c("subject_id"))

  cohortDb <- cohortDb %>%
    dplyr::left_join(
      person %>%
        dplyr::mutate(gender = dplyr::case_when(
          .data$gender_concept_id == 8507 ~ "Male",
          .data$gender_concept_id == 8532 ~ "Female",
          TRUE ~ as.character(NA)
        )) %>%
        dplyr::select("subject_id", "gender"),
      by = "subject_id"
    )

  return(cohortDb)
}

#' @noRd
getAge <- function(cdm,
                   cohortTable,
                   ageAt = "cohort_start_date",
                   cohortIds = NULL,
                   dafultMonth = 1,
                   defaultDay = 1,
                   imposeMonth = TRUE,
                   imposeDay = TRUE) {
  if (isTRUE(is.na(cohortIds))) {
    cohortIds <- NULL
  }

  if (!is.null(cohortIds)) {
    cohortDb <- cdm[[cohortTable]] %>%
      dplyr::filter(.data$cohort_definition_id %in% .env$cohortIds)
  } else {
    cohortDb <- cdm[[cohortTable]]
  }

  person <- cdm[["person"]] %>%
    dplyr::rename("subject_id" = "person_id") %>%
    dplyr::inner_join(
      cohortDb %>%
        dplyr::select("subject_id", "date_age" = !!ageAt),
      by = "subject_id"
    )

  if (imposeMonth == TRUE) {
    person <- person %>%
      dplyr::mutate(month_of_birth = .env$dafultMonth)
  } else {
    person <- person %>%
      dplyr::mutate(month_of_birth = dplyr::if_else(
        is.na(.data$month_of_birth),
        .env$dafultMonth,
        .data$month_of_birth
      ))
  }

  if (imposeDay == TRUE) {
    person <- person %>%
      dplyr::mutate(day_of_birth = .env$defaultDay)
  } else {
    person <- person %>%
      dplyr::mutate(day_of_birth = dplyr::if_else(
        is.na(.data$day_of_birth),
        .env$dafultDay,
        .data$day_of_birth
      ))
  }

  cohortDb <- cohortDb %>%
    dplyr::left_join(
      person %>%
        dplyr::mutate(birth_date = as.Date(paste0(
          .data$year_of_birth,
          "-",
          .data$month_of_birth,
          "-",
          .data$day_of_birth
        ))) %>%
        dplyr::mutate(age = dbplyr::sql(sqlGetAge(
          dialect = CDMConnector::dbms(cdm),
          dob = "birth_date",
          dateOfInterest = "date_age"
        ))) %>%
        dplyr::select("subject_id", "age"),
      by = "subject_id"
    )

  return(cohortDb)
}

#' @noRd
getPriorHistory <- function(cdm,
                            cohortTable,
                            cohortIds = NULL,
                            priorHistoryAt = "cohort_start_date") {
  if (isTRUE(is.na(cohortIds))) {
    cohortIds <- NULL
  }

  if (!is.null(cohortIds)) {
    cohortDb <- cdm[[cohortTable]] %>%
      dplyr::filter(.data$cohort_definition_id %in% .env$cohortIds)
  } else {
    cohortDb <- cdm[[cohortTable]]
  }

  observationPeriod <- cdm[["observation_period"]] %>%
    dplyr::rename("subject_id" = "person_id") %>%
    dplyr::inner_join(
      cohortDb %>%
        dplyr::select("subject_id", "date_prior_history" = !!priorHistoryAt),
      by = "subject_id"
    )

  if (imposeMonth == TRUE) {
    person <- person %>%
      dplyr::mutate(month_of_birth = .env$dafultMonth)
  } else {
    person <- person %>%
      dplyr::mutate(month_of_birth = dplyr::if_else(
        is.na(.data$month_of_birth),
        .env$dafultMonth,
        .data$month_of_birth
      ))
  }

  if (imposeDay == TRUE) {
    person <- person %>%
      dplyr::mutate(day_of_birth = .env$dafultDay)
  } else {
    person <- person %>%
      dplyr::mutate(day_of_birth = dplyr::if_else(
        is.na(.data$day_of_birth),
        .env$dafultDay,
        .data$day_of_birth
      ))
  }

  cohortDb <- cohortDb %>%
    dplyr::left_join(
      person %>%
        dplyr::mutate(birth_date = as.Date(paste0(
          .data$year_of_birth,
          "-",
          .data$month_of_birth,
          "-",
          .data$day_of_birth
        ))) %>%
        dplyr::mutate(prior_history = dbplyr::sql(CDMConnector::datediff(
          start = "observation_period_start_date",
          end = "date_prior_history",
          interval = "day"
        ))) %>%
        dplyr::select("subject_id", "prior_history"),
      by = "subject_id"
    )

  return(cohortDb)
}

#' @noRd
sqlGetAge <- function(dialect,
                      dob,
                      dateOfInterest) {
  SqlRender::translate(
    SqlRender::render("((YEAR(@date_of_interest) * 10000 + MONTH(@date_of_interest) * 100 +
                      DAY(@date_of_interest)-(YEAR(@dob)* 10000 + MONTH(@dob) * 100 + DAY(@dob))) / 10000)",
      dob = dob,
      date_of_interest = dateOfInterest
    ),
    targetDialect = dialect
  )
}

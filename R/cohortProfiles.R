#' @noRd
inObservation <- function(x, cdm) {
  x %>%
    dplyr::left_join(
      cdm$observation_period %>%
        dplyr::select(
          "subject_id" = "person_id",
          "observation_period_start_date",
          "observation_period_end_date"
        ),
      by = "subject_id"
    ) %>%
    dplyr::mutate(
      in_observation = dplyr::if_else(
        .data$cohort_start_date >= .data$observation_period_start_date &
          .data$cohort_start_date <= .data$observation_period_end_date,
        TRUE,
        FALSE
      )
    ) %>%
    dplyr::select(
      -"observation_period_start_date", - "observation_period_end_date"
    ) %>%
    dplyr::compute()
}

#' @noRd
getSex <- function(cdm,
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

  return(addSex(cohortDb = cohortDb, cdm = cdm))
}

#' @noRd
getAge <- function(cdm,
                   cohortTable,
                   cohortIds = NULL,
                   ageAt = "cohort_start_date",
                   defaultMonth = 1,
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

  return(addAge(
    cohortDb = cohortDb,
    cdm = cdm,
    ageAt = ageAt,
    defaultMonth = defaultMonth,
    defaultDay = defaultDay,
    imposeMonth = imposeMonth,
    imposeDay = imposeDay
  ))
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

  return(addPriorHistory(
    cohortDb = cohortDb,
    cdm = cdm,
    priorHistoryAt = priorHistoryAt
  ))
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

#' @noRd
dateadd <- function(date, number, interval = "day") {
  rlang::check_installed("SqlRender")
  checkmate::assertCharacter(interval, len = 1)
  checkmate::assertSubset(interval, choices = c("day", "year"))
  checkmate::assertCharacter(date, len = 1)
  # checkmate::assertIntegerish(number)
  dot <- get(".", envir = parent.frame())
  targetDialect <- CDMConnector::dbms(dot$src$con)
  sql <- glue::glue("DATEADD({interval}, {number}, {date})")
  sql <- SqlRender::translate(sql = as.character(sql), targetDialect = targetDialect)
  dbplyr::sql(sql)
}

#' @noRd
addSex <- function(cohortDb, cdm) {
  cdm[["person"]] %>%
    dplyr::rename("subject_id" = "person_id") %>%
    dplyr::inner_join(
      cohortDb %>% dplyr::select("subject_id") %>% dplyr::distinct(),
      by = c("subject_id")
    ) %>%
    dplyr::mutate(sex = dplyr::case_when(
      .data$gender_concept_id == 8507 ~ "Male",
      .data$gender_concept_id == 8532 ~ "Female",
      TRUE ~ as.character(NA)
    )) %>%
    dplyr::select("subject_id", "sex") %>%
    dplyr::right_join(cohortDb, by = "subject_id") %>%
    dplyr::select(dplyr::all_of(colnames(cohortDb)), "sex")
}

#' @noRd
addAge <- function(cohortDb,
                   cdm,
                   ageAt = "cohort_start_date",
                   cohortIds = NULL,
                   defaultMonth = 1,
                   defaultDay = 1,
                   imposeMonth = TRUE,
                   imposeDay = TRUE) {
  defaultMonth <- as.integer(defaultMonth)
  defaultDay <- as.integer(defaultDay)

  person <- cdm[["person"]] %>%
    dplyr::rename("subject_id" = "person_id") %>%
    dplyr::inner_join(
      cohortDb %>%
        dplyr::select("subject_id", dplyr::all_of(ageAt)) %>%
        dplyr::distinct(),
      by = "subject_id"
    )

  if (imposeMonth == TRUE) {
    person <- person %>%
      dplyr::mutate(month_of_birth = .env$defaultMonth)
  } else {
    person <- person %>%
      dplyr::mutate(month_of_birth = dplyr::if_else(
        is.na(.data$month_of_birth),
        .env$defaultMonth,
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
        .env$defaultDay,
        .data$day_of_birth
      ))
  }

  person <- person %>%
    dplyr::filter(!is.na(.data$year_of_birth)) %>%
    dplyr::mutate(year_of_birth1 = as.character(as.integer(.data$year_of_birth))) %>%
    dplyr::mutate(month_of_birth1 = as.character(as.integer(.data$month_of_birth))) %>%
    dplyr::mutate(day_of_birth1 = as.character(as.integer(.data$day_of_birth))) %>%
    dplyr::mutate(birth_date = as.Date(paste0(.data$year_of_birth1, "-",
                                     .data$month_of_birth1, "-",
                                     .data$day_of_birth1)))  %>%
    dplyr::mutate(age = floor(dbplyr::sql(sqlGetAge(
      dialect = CDMConnector::dbms(cdm),
      dob = "birth_date",
      dateOfInterest = ageAt
    )))) %>%
    dplyr::select("subject_id", dplyr::all_of(ageAt), "age") %>%
    dplyr::right_join(cohortDb, by = c("subject_id", ageAt)) %>%
    dplyr::select(dplyr::all_of(colnames(cohortDb)), "age")

  return(person)
}

#' @noRd
addPriorHistory <- function(cohortDb,
                            cdm,
                            priorHistoryAt = "cohort_start_date") {
  cdm[["observation_period"]] %>%
    dplyr::select(
      "subject_id" = "person_id", "observation_period_start_date"
    ) %>%
    dplyr::inner_join(
      cohortDb %>%
        dplyr::select("subject_id", dplyr::all_of(priorHistoryAt)) %>%
        dplyr::distinct(),
      by = "subject_id"
    ) %>%
    dplyr::mutate(prior_history = CDMConnector::datediff(
      start = "observation_period_start_date",
      end = !!priorHistoryAt
    )) %>%
    dplyr::right_join(
      cohortDb,
      by = c("subject_id", priorHistoryAt)
    ) %>%
    dplyr::select(dplyr::all_of(colnames(cohortDb)), "prior_history")
}

#' @noRd
addVisit <- function(cohortDb,
                            cdm, window = c(-365, 0)) {
  cdm[["visit_occurrence"]] %>%
    dplyr::select(
      "subject_id" = "person_id", "visit_concept_id", "visit_start_date"
    ) %>%
    dplyr::inner_join(
      cohortDb %>%
        dplyr::select("subject_id", "cohort_start_date", "cohort_end_date") %>%
        dplyr::distinct(),
      by = "subject_id"
    ) %>%
    dplyr::filter(
      CDMConnector::dateadd("cohort_start_date", window[1]) <=
        .data$visit_start_date
    ) %>%
    dplyr::filter(
      CDMConnector::dateadd("cohort_start_date", window[2]) >=
        .data$visit_start_date
    ) %>%
    dplyr::group_by(
      .data$subject_id, .data$cohort_start_date, .data$cohort_end_date
    ) %>%
    dplyr::summarise(number_visits = dplyr::n()) %>%
    dplyr::ungroup() %>%
    dplyr::right_join(
      cohortDb,
      by = c("subject_id", "cohort_start_date", "cohort_end_date")
    ) %>%
    dplyr::select(dplyr::all_of(colnames(cohortDb)), "number_visits")
}

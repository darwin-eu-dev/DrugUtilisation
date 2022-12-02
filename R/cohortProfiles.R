
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
                   defaultMonth = 1,
                   defaultDay = 1,
                   imposeMonth = TRUE,
                   imposeDay = TRUE) {
  defaultMonth <- as.integer(defaultMonth)
  defaultDay <- as.integer(defaultDay)
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
        dplyr::select("subject_id", dplyr::all_of(ageAt)),
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
      dateOfInterest = ageAt
    ))) %>%
    dplyr::select("subject_id", dplyr::all_of(ageAt), "age") %>%


  cohortDb <- cohortDb %>%
    dplyr::left_join(
      person,
      by = toJoin
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

  variables <- colnames(cohortDb)

  cohortDb  <- cdm[["observation_period"]] %>%
    dplyr::select(
      "subject_id" = "person_id", "observation_period_start_date"
    ) %>%
    dplyr::inner_join(
      cohortDb %>%
        dplyr::select("subject_id", dplyr::all_of(priorHistoryAt)),
      by = "subject_id"
    ) %>%
    dplyr::mutate(prior_history = CDMConnector::datediff(
      start = "observation_period_start_date",
      end = !!priorHistoryAt
    )) %>%
    dplyr::select(
      "subject_id", dplyr::all_of(priorHistoryAt), "prior_history"
    ) %>%
    dplyr::right_join(
      cohortDb,
      by = c("subject_id", priorHistoryAt)
    ) %>%
    dplyr::select(dplyr::all_of(variables), "prior_history")

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

#' @noRd
dateadd <- function(date, number, interval = "day") {
  rlang::check_installed("SqlRender")
  checkmate::assertCharacter(interval, len = 1)
  checkmate::assertSubset(interval, choices = c("day", "year"))
  checkmate::assertCharacter(date, len = 1)
  #checkmate::assertIntegerish(number)
  dot <- get(".", envir = parent.frame())
  targetDialect <- CDMConnector::dbms(dot$src$con)
  sql <- glue::glue("DATEADD({interval}, {number}, {date})")
  sql <- SqlRender::translate(sql = as.character(sql), targetDialect = targetDialect)
  dbplyr::sql(sql)
}

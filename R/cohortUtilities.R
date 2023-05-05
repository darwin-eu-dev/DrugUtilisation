#' Add a line in the attrition table. If the table does not exist it is created
#'
#' @param x A table in the cdm with at lest: 'cohort_definition_id' and
#' subject_id'
#' @param attrition An attrition table. If NULL a new attrition table is created.
#' @param reason A character with the name of the reason.
#'
#' @return The function returns the 'cdm' object with the created tables as
#' references of the object.
#' @export
#'
#' @examples
computeCohortAttrition <- function(x,
                                   attrition = NULL,
                                   reason = "Qualifying initial events") {
  checkInputs(x, attrition, reason)
  attrition <- addAttritionLine(x, attrition, reason)
  return(attrition)
}

#' @noRd
addAttritionLine <- function(cohort, attrition, reason) {
  if (is.null(attrition)) {
    attrition <- countAttrition(cohort, reason, 1)
  } else {
    id <- attrition %>% dplyr::select("reason_id") %>% dplyr::pull() %>% max()
    attrition <- attrition %>%
      dplyr::union_all(countAttrition(cohort, reason, id + 1)) %>%
      addExcludedCounts()
  }
  attrition <- computeTable(attrition, cdm)
  return(attrition)
}

#' @noRd
countAttrition <- function(cohort, reason, id) {
  if (id == 1) {
    num <- 0
  } else {
    num <- as.numeric(NA)
  }
  attrition <- cohort %>%
    dplyr::group_by(.data$cohort_definition_id) %>%
    dplyr::summarise(
      number_records = dplyr::n(),
      number_subjects = dplyr::n_distinct(.data$subject_id),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      reason_id = .env$id, reason = .env$reason, excluded_records = .env$num,
      excluded_subjects = .env$num
    )
  return(attrition)
}

#' @noRd
addExcludedCounts <- function(attrition) {
  attrition %>%
    dplyr::group_by(.data$cohort_definition_id) %>%
    dbplyr::window_order(.data$reason_id) %>%
    dplyr::mutate(
      excluded_records = dplyr::if_else(
        is.na(.data$excluded_records),
        .data$number_records - dplyr::lag(.data$number_records),
        .data$excluded_records
      ),
      excluded_subjects = dplyr::if_else(
        is.na(.data$excluded_subjects),
        .data$number_subjects - dplyr::lag(.data$number_subjects),
        .data$excluded_subjects
      )
    )
}

#' Computes the cohortCount attribute for a certain table
#'
#' @param x A table in the cdm with at lest: 'cohort_definition_id' and
#' subject_id'
#'
#' @return A reference to a table in the database with the cohortCount
#'
#' @export
#'
#' @examples
computeCohortCount <- function(x) {
  x %>%
    dplyr::group_by(.data$cohort_definition_id) %>%
    dplyr::summarise(
      number_records = dplyr::n(),
      number_subjects = dplyr::n_distinct(.data$subject_id),
      .groups = "drop"
    ) %>%
    CDMConnector::computeQuery()
}

#' @noRd
conceptSetFromConceptSetList <- function(conceptSetList) {
  cohortSet <- dplyr::tibble(cohort_name = names(conceptSetList)) %>%
    dplyr::mutate(cohort_definition_id = dplyr::row_number())
  conceptSet <- purrr::map(conceptSetList, dplyr::as_tibble) %>%
    dplyr::bind_rows(.id = "cohort_name") %>%
    dplyr::rename("concept_id" =  "value") %>%
    dplyr::inner_join(cohortSet, by = "cohort_name") %>%
    dplyr::select(-"cohort_name")
  attr(conceptSet, "cohortSet") <- cohortSet
  return(conceptSet)
}

#' @noRd
subsetTables <- function(cdm, conceptSet, domains = NULL) {
  conceptSet <- cdm$concept %>%
    dplyr::select("concept_id", "domain_id") %>%
    dplyr::right_join(
      conceptSet %>%
        dplyr::select("cohort_definition_id", "concept_id"),
      by = "concept_id",
      copy = TRUE
    ) %>%
    CDMConnector::computeQuery()
  if (is.null(domains)) {
    domains <- conceptSet %>%
      dplyr::select("domain_id") %>%
      dplyr::distinct() %>%
      dplyr::pull()
  }
  cohort <- emptyCohort(cdm)
  if (!any(domains %in% domainInformation$domain_id)) {
    cli::cli_alert_warning(paste0(
      "All concepts domain_id (",
      paste(domains, collapse = ", "),
      ") not supported, generated cohort is empty. The supported domain_id are: ",
      paste(domainInformation$domain_id, collapse = ", "),
      "."
    ))
    return(cohort)
  }
  if (length(domains[!(domains %in% domainInformation$domain_id)]) > 0) {
    cli::cli_alert_warning(paste(
      "concepts with domain_id:",
      paste(
        domains[!(domains %in% domainInformation$domain_id)], collapse = ", "
      ),
      "are not going to be instantiated. The supported domain_id are: ",
      paste(domainInformation$domain_id, collapse = ", "),
      "."
    ))
  }
  for (domain in domains) {
    concepts <- conceptSet %>%
      dplyr::filter(.data$domain_id == .env$domain) %>%
      dplyr::select(-"domain_id")
    cohort <- cohort %>%
      dplyr::union_all(
        concepts %>%
          dplyr::inner_join(
            cdm[[getTableName(domain)]] %>%
              dplyr::select(
                "concept_id" = !!getConceptName(domain),
                "subject_id" = "person_id",
                "cohort_start_date" = !!getStartName(domain),
                "cohort_end_date" = !!getEndName(domain)
              ),
            by = "concept_id"
          ) %>%
          dplyr::select(
            "cohort_definition_id", "subject_id", "cohort_start_date",
            "cohort_end_date"
          )
      ) %>%
      CDMConnector::computeQuery()
  }
  return(cohort)
}

#' @noRd
getConceptName <- function(domain) {
  domainInformation$concept_id_name[domainInformation$domain_id == domain]
}

#' @noRd
getTableName <- function(domain) {
  domainInformation$table_name[domainInformation$domain_id == domain]
}

#' @noRd
getStartName <- function(domain) {
  domainInformation$start_name[domainInformation$domain_id == domain]
}

#' @noRd
getEndName <- function(domain) {
  domainInformation$end_name[domainInformation$domain_id == domain]
}

#' @noRd
emptyCohort <- function(cdm, name = CDMConnector:::uniqueTableName()) {
  writePrefix <- attr(cdm, "write_prefix")
  writeSchema <- attr(cdm, "write_schema")
  con <- attr(cdm, "dbcon")
  if (!is.null(writePrefix)) {
    name <- CDMConnector:::inSchema(
      writeSchema, paste0(writePrefix, name), CDMConnector::dbms(con)
    )
    temporary <- FALSE
  } else {
    temporary <- TRUE
  }
  DBI::dbCreateTable(
    con,
    name,
    fields = c(
      cohort_definition_id = "INT",
      subject_id = "BIGINT",
      cohort_start_date = "DATE",
      cohort_end_date = "DATE"
    ),
    temporary = temporary
  )
  dplyr::tbl(con, name)
}

#' @noRd
applyWashout <- function(cohort, washout) {
  cohort <- cohort %>%
    dplyr::group_by(.data$cohort_definition_id, .data$subject_id) %>%
    dbplyr::window_order(.data$cohort_start_date) %>%
    dplyr::mutate(id = dplyr::row_number()) %>%
    CDMConnector::computeQuery()
  cohort <- cohort %>%
    dplyr::left_join(
      cohort %>%
        dplyr::mutate(id = .data$id + 1) %>%
        dplyr::select(
          "cohort_definition_id", "subject_id", "id",
          "prior_date" = "cohort_end_date"
        ),
      by = c("cohort_definition_id", "subject_id", "id")
    ) %>%
    dplyr::mutate(
      prior_time = !!CDMConnector::datediff("prior_date", "cohort_start_date")
    ) %>%
    dplyr::filter(
      is.na(.data$prior_date) | .data$prior_time >= .env$washout
    ) %>%
    dplyr::select(-c("id", "prior_date", "prior_time")) %>%
    CDMConnector::computeQuery()
  return(cohort)
}

#' @noRd
applyMinimumStartDate <- function(cohort, minimumStartDate) {
  if (!is.null(minimumStartDate)) {
    cohort <- cohort %>%
      dplyr::mutate(cohort_start_date = max(
        .data$cohort_start_date, .env$minimumStartDate
      )) %>%
      dplyr::filter(.data$cohort_start_date <= .data$cohort_end_date) %>%
      CDMConnector::computeQuery()
  }
  return(cohort)
}

#' @noRd
applyMaximumEndDate <- function(cohort, maximumEndDate) {
  if (!is.null(maximumEndDate)) {
    cohort <- cohort %>%
      dplyr::mutate(cohort_end_date = min(
        .data$cohort_end_date, .env$maximumEndDate
      )) %>%
      dplyr::filter(.data$cohort_start_date <= .data$cohort_end_date) %>%
      CDMConnector::computeQuery()
  }
  return(cohort)
}

#' @noRd
insertTable <- function(x, cdm, name = CDMConnector:::uniqueTableName()) {
  writePrefix <- attr(cdm, "write_prefix")
  writeSchema <- attr(cdm, "write_schema")
  con <- attr(cdm, "dbcon")
  if (!is.null(writePrefix)) {
    name <- CDMConnector:::inSchema(
      writeSchema, paste0(writePrefix, name), CDMConnector::dbms(con)
    )
    temporary <- FALSE
  } else {
    temporary <- TRUE
  }
  DBI::dbWriteTable(con, name, x, temporary = temporary)
  dplyr::tbl(con, name)
}

#' @noRd
computeTable <- function(x, cdm, name = CDMConnector:::uniqueTableName()) {
  writePrefix <- attr(cdm, "write_prefix")
  writeSchema <- attr(cdm, "write_schema")
  con <- attr(cdm, "dbcon")
  if (!is.null(writePrefix)) {
    name <- paste0(writePrefix, name)
    temporary <- FALSE
  } else {
    temporary <- TRUE
  }
  CDMConnector::computeQuery(x, name, temporary, writeSchema, TRUE)
}

#' @noRd
minimumDaysPriorHistory <- function(x, cdm, priorHistory) {
  if (!is.null(priorHistory)) {
    x <- x %>%
      PatientProfiles::addPriorHistory(cdm) %>%
      dplyr::filter(.data$prior_history >= .env$priorHistory) %>%
      dplyr::select(-"prior_history") %>%
      CDMConnector::computeQuery()
  }
  return(x)
}

#' @noRd
unionCohort <- function(x, gap) {
  x %>%
    dplyr::select(
      "cohort_definition_id",
      "subject_id",
      "date_event" = "cohort_start_date"
    ) %>%
    dplyr::mutate(date_id = -1) %>%
    dplyr::union_all(
      cohort %>%
        dplyr::mutate(
          date_event = as.Date(!!CDMConnector::dateadd(
            date = "cohort_end_date",
            number = gap
          )),
          date_id = 1
        ) %>%
        dplyr::select(
          "cohort_definition_id", "subject_id", "date_event", "date_id"
        )
    ) %>%
    dplyr::group_by(.data$cohort_definition_id, .data$subject_id) %>%
    dbplyr::window_order(.data$date_event, .data$date_id) %>%
    dplyr::mutate(cum_id = cumsum(.data$date_id)) %>%
    dplyr::filter(
      .data$cum_id == 0 || (.data$cum_id == -1 && .data$date_id == -1)
    ) %>%
    dplyr::mutate(
      name = dplyr::if_else(
        .data$date_id == -1, "cohort_start_date", "cohort_end_date"
      ),
      era_id = dplyr::if_else(
        .data$date_id == -1, 1, 0
      )
    ) %>%
    dplyr::mutate(era_id = cumsum(as.numeric(.data$era_id))) %>%
    dplyr::ungroup() %>%
    dbplyr::window_order() %>%
    dplyr::select(
      "cohort_definition_id", "subject_id", "era_id", "name", "date_event"
    ) %>%
    tidyr::pivot_wider(names_from = "name", values_from = "date_event") %>%
    dplyr::mutate(cohort_end_date = as.Date(!!CDMConnector::dateadd(
      date = "cohort_end_date",
      number = -gap
    ))) %>%
    dplyr::select(-"era") %>%
    computeTable(cdm)
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
#' @noRd
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


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
addAttritionLine <- function(x,
                             attrition = NULL,
                             reason = "Qualifying initial events") {
  if (!is.null(attrition)) {
    checkmate::assertTibble(attrition)
    checkmate::assertTRUE(all(
      c(
        "cohort_definition_id", "number_records", "number_subjects", "reason_id",
        "reason", "excluded_records", "excluded_subjects"
      ) %in% colnames(attrition)
    ))
  }
  checkmate::assertClass(x, "tbl")
  checkmate::assertTRUE(
    all(c("cohort_definition_id", "subject_id") %in% colnames(x))
  )
  checkmate::assertCharacter(reason, len = 1)
  attrition <- attritionLine(x, attrition, reason)
  return(attrition)
}

#' @noRd
attritionLine <- function(x, atrition, reason) {
  if (is.null(attrition)) {
    attrition <- countAttrition(x, reason, 1)
  } else {
    id <- max(attrition$reason_id)
    attrition <- attrition %>%
      dplyr::bind_rows(countAttrition(x, reason, id)) %>%
      addExcludedCounts(id)
  }
  return(attrition)
}

#' @noRd
countAttrition <- function(x, reason, id) {
  if (id == 1) {
    num <- 0
  } else {
    num <- as.numeric(NA)
  }
  attrition <- x %>%
    dplyr::group_by(.data$cohort_definition_id) %>%
    dplyr::summarise(
      number_records = dplyr::n(),
      number_subjects = dplyr::n_distinct(.data$subject_id),
      .groups = "drop"
    ) %>%
    dplyr::collect() %>%
    dplyr::mutate(
      reason_id = id, reason = .env$reason, excluded_records = num, excluded_subjects = num
    )
  return(attrition)
}

#' @noRd
addExcludedCounts <- function(x, id) {
  attrition %>%
    dplyr::group_by(.data$cohort_definition_id) %>%
    dplyr::mutate(
      excluded_records = dplyr::if_else(
        is.na(.data$excluded_records),
        .data$number_records[.env$id] - .data$number_records[.env$id - 1],
        .data$excluded_records
      ),
      excluded_subjects = dplyr::if_else(
        is.na(.data$excluded_subjects),
        .data$number_subjects[.env$id] - .data$number_subjects[.env$id - 1],
        .data$excluded_subjects
      )
    )
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

subsetTables <- function(cdm, conceptSet) {
  conceptSet <- cdm$concept %>%
    dplyr::select("concept_id", "domain_id") %>%
    dplyr::right_join(
      conceptSet %>%
        dplyr::select("cohort_definition_id", "concept_id"),
      by = "concept_id",
      copy = TRUE
    ) %>%
    CDMConnector::computeQuery()
  domains <- conceptSet %>%
    dplyr::select("domain_id") %>%
    dplyr::distinct() %>%
    dplyr::pull()
  cohort <- emptyCohort()
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
      dplyr::select(-"domain")
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

subsetTable <- function(cdm, conceptSet, table) {
  # create cohortSet
  cohortSet <- dplyr::tibble(cohort_name = names(conceptSetList)) %>%
    dplyr::mutate(cohort_definition_id = dplyr::row_number())
  # get names
  conceptId <- "drug_concept_id"
  startDate <- "drug_exposure_start_date"
  endDate <- "drug_exposure_end_date"
  # create concept list
  cdm$concept %>%
    dplyr::select("concept_id", "domain_id") %>%
    right_join(
      purrr::map(conceptSetList, dplyr::as_tibble) %>%
        dplyr::bind_rows(.id = "cohort_name") %>%
        dplyr::rename("concept_id" =  "value"),
      copy = TRUE,
      by = "concept_id"
    )
}

getConceptName <- function(domain) {
  domainInformation$concept_id_name[domainInformation$domain_id == domain]
}

getTableName <- function(domain) {
  domainInformation$table_name[domainInformation$domain_id == domain]
}

getStartName <- function(domain) {
  domainInformation$start_name[domainInformation$domain_id == domain]
}

getEndName <- function(domain) {
  domainInformation$end_name[domainInformation$domain_id == domain]
}

emptyCohort <- function() {
  dplyr::tibble(
    cohort_definition_id = NULL, subject_id = NULL, cohort_start_date = NULL,
    cohort_end_date = NULL
  ) %>%
    insertDatabase()
}

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

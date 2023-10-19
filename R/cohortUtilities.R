# Copyright 2022 DARWIN EU (C)
#
# This file is part of DrugUtilisation
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

#' Add a line in the attrition table. If the table does not exist it is created
#'
#' @param x A table in the cdm with at lest: 'cohort_definition_id' and
#' subject_id'
#' @param cdm A cdm reference created using CDMConnector
#' @param attrition An attrition table. If NULL a new attrition table is created.
#' @param reason A character with the name of the reason.
#'
#' @return Reference to a table with the cohort attrition
#'
#' @noRd
#'
computeCohortAttrition <- function(x,
                                   cdm,
                                   attrition = NULL,
                                   reason = "Qualifying initial records",
                                   cohortSet) {
  checkInputs(
    x = x, cdm = cdm, attrition = attrition, reason = reason
  )
  attrition <- addAttritionLine(x, cdm, attrition, reason, cohortSet) %>%
    computeTable(cdm)
  return(attrition)
}

#' @noRd
addAttritionLine <- function(cohort, cdm, attrition, reason, cohortSet) {
  if (is.null(attrition)) {
    attrition <- countAttrition(cohort, reason, 1, cohortSet)
  } else {
    id <- attrition %>%
      dplyr::pull("reason_id") %>%
      max()
    attrition <- attrition %>%
      dplyr::union_all(countAttrition(cohort, reason, id + 1, cohortSet)) %>%
      addExcludedCounts()
  }
  return(attrition)
}

#' @noRd
countAttrition <- function(cohort, reason, id, cohortSet) {
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
    dplyr::right_join(
      cohortSet %>%
        dplyr::select("cohort_definition_id"),
      by = "cohort_definition_id"
    ) %>%
    dplyr::mutate(
      number_records = dplyr::if_else(
        is.na(.data$number_records), 0, .data$number_records
      ),
      number_subjects = dplyr::if_else(
        is.na(.data$number_subjects), 0, .data$number_subjects
      ),
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
        dplyr::lag(.data$number_records) - .data$number_records,
        .data$excluded_records
      ),
      excluded_subjects = dplyr::if_else(
        is.na(.data$excluded_subjects),
        dplyr::lag(.data$number_subjects) - .data$number_subjects,
        .data$excluded_subjects
      )
    ) %>%
    dbplyr::window_order() %>%
    dplyr::ungroup()
}

#' Computes the cohortCount attribute for a certain table
#'
#' @param x A table in the cdm with at lest: 'cohort_definition_id' and
#' subject_id'
#' @param cdm A cdm_reference object
#'
#' @return A reference to a table in the database with the cohortCount
#'
#' @noRd
#'
computeCohortCount <- function(x,
                               cdm,
                               cohortSet) {
  checkInputs(x = x, cdm = cdm)
  return(
    x %>%
      dplyr::group_by(.data$cohort_definition_id) %>%
      dplyr::summarise(
        number_records = dplyr::n(),
        number_subjects = dplyr::n_distinct(.data$subject_id),
        .groups = "drop"
      ) %>%
      dplyr::right_join(
        cohortSet %>%
          dplyr::select("cohort_definition_id"),
        by = "cohort_definition_id"
      ) %>%
      dplyr::mutate(
        number_records = dplyr::if_else(
          is.na(.data$number_records), 0, .data$number_records
        ),
        number_subjects = dplyr::if_else(
          is.na(.data$number_subjects), 0, .data$number_subjects
        )
      ) %>%
      computeTable(cdm)
  )
}

#' @noRd
conceptSetFromConceptSetList <- function(conceptSetList) {
  cohortSet <- dplyr::tibble(cohort_name = names(conceptSetList)) %>%
    dplyr::mutate(cohort_definition_id = dplyr::row_number()) %>%
    dplyr::select("cohort_definition_id", "cohort_name")
  conceptSet <- purrr::map(conceptSetList, dplyr::as_tibble) %>%
    dplyr::bind_rows(.id = "cohort_name") %>%
    dplyr::rename("concept_id" = "value") %>%
    dplyr::inner_join(cohortSet, by = "cohort_name") %>%
    dplyr::select(-"cohort_name")
  attr(conceptSet, "cohort_set") <- cohortSet
  return(conceptSet)
}

#' @noRd
subsetTables <- function(cdm, conceptSet, domains = NULL) {
  conceptSet <- cdm[["concept"]] %>%
    dplyr::select("concept_id", "domain_id") %>%
    dplyr::right_join(
      conceptSet %>%
        dplyr::select("cohort_definition_id", "concept_id"),
      by = "concept_id",
      copy = TRUE
    ) %>%
    computeTable(cdm)
  if (is.null(domains)) {
    domains <- conceptSet %>%
      dplyr::select("domain_id") %>%
      dplyr::distinct() %>%
      dplyr::pull()
  }
  cohort <- emptyCohort(cdm)
  if (!any(domains %in% domainInformation$domain_id)) {
    cli::cli_warn(paste0(
      "All concepts domain_id (",
      paste(domains, collapse = ", "),
      ") not supported, generated cohort is empty. The supported domain_id are: ",
      paste(domainInformation$domain_id, collapse = ", "),
      "."
    ))
    return(cohort)
  }
  if (length(domains[!(domains %in% domainInformation$domain_id)]) > 0) {
    cli::cli_warn(paste(
      "concepts with domain_id:",
      paste(
        domains[!(domains %in% domainInformation$domain_id)],
        collapse = ", "
      ),
      "are not going to be instantiated. The supported domain_id are: ",
      paste(domainInformation$domain_id, collapse = ", "),
      "."
    ))
  }
  domains <- domains[domains %in% domainInformation$domain_id]
  for (domain in domains) {
    if (getTableName(domain) %in% names(cdm)) {
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
        computeTable(cdm)
    } else {
      cli::cli_warn("{getTableName(domain)} not found in the cdm object")
    }
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
emptyCohort <- function(cdm) {
  name <- CDMConnector::uniqueTableName()
  DBI::dbCreateTable(
    attr(cdm, "dbcon"),
    name,
    fields = c(
      cohort_definition_id = "INT",
      subject_id = "BIGINT",
      cohort_start_date = "DATE",
      cohort_end_date = "DATE"
    ),
    temporary = TRUE
  )
  ref <- dplyr::tbl(attr(cdm, "dbcon"), name)
  attr(ref, "cdm_reference") <- cdm
  return(ref)
}

#' @noRd
requirePriorUseWashout <- function(cohort, cdm, washout) {
  cohort <- cohort %>%
    dplyr::group_by(.data$cohort_definition_id, .data$subject_id) %>%
    dbplyr::window_order(.data$cohort_start_date) %>%
    dplyr::mutate(id = dplyr::row_number()) %>%
    computeTable(cdm)
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
    )
  if (is.infinite(washout)) {
    cohort <- cohort %>%
      dplyr::filter(is.na(.data$prior_date))
  } else {
    cohort <- cohort %>%
      dplyr::filter(
        is.na(.data$prior_date) | .data$prior_time >= .env$washout
      )
  }
  cohort <- cohort %>%
    dplyr::select(-c("id", "prior_date", "prior_time")) %>%
    dbplyr::window_order() %>%
    dplyr::ungroup() %>%
    computeTable(cdm)
  return(cohort)
}

#' @noRd
trimStartDate <- function(cohort, cdm, startDate) {
  if (!is.na(startDate)) {
    cohort <- cohort %>%
      dplyr::mutate(cohort_start_date = dplyr::if_else(
        .data$cohort_start_date <= !!startDate,
        as.Date(!!startDate), as.Date(.data$cohort_start_date)
      )) %>%
      dplyr::filter(.data$cohort_start_date <= .data$cohort_end_date) %>%
      computeTable(cdm)
  }
  return(cohort)
}

#' @noRd
trimEndDate <- function(cohort, cdm, endDate) {
  if (!is.na(endDate)) {
    cohort <- cohort %>%
      dplyr::mutate(cohort_end_date = dplyr::if_else(
        .data$cohort_end_date >= !!endDate,
        as.Date(!!endDate), as.Date(.data$cohort_end_date)
      )) %>%
      dplyr::filter(.data$cohort_start_date <= .data$cohort_end_date) %>%
      computeTable(cdm)
  }
  return(cohort)
}

#' @noRd
insertTable <- function(x,
                        cdm,
                        name = CDMConnector::uniqueTableName()) {
  con <- attr(cdm, "dbcon")
  name <- CDMConnector::inSchema(
    attr(cdm, "write_schema"),
    paste0(attr(cdm, "write_prefix"), name),
    CDMConnector::dbms(con)
  )
  DBI::dbWriteTable(con, name, as.data.frame(x), overwrite = TRUE)
  dplyr::tbl(con, name)
}

#' @noRd
computeTable <- function(x,
                         cdm) {
  x %>%
    CDMConnector::computeQuery(
      name = paste0(attr(cdm, "write_prefix"), CDMConnector::uniqueTableName()),
      temporary = is.null(attr(cdm, "write_prefix")),
      schema = attr(cdm, "write_schema"),
      overwrite = TRUE
    )
}

#' @noRd
requirePriorObservation <- function(x, cdm, priorObservation) {
  if (!is.null(priorObservation)) {
    xNew <- x %>%
      PatientProfiles::addDemographics(cdm = cdm, age = FALSE, sex = FALSE) %>%
      dplyr::filter(.data$prior_observation >= .env$priorObservation) %>%
      dplyr::select(-"prior_observation") %>%
      dplyr::mutate(duration = !!CDMConnector::datediff(
        "cohort_start_date", "cohort_end_date"
      )) %>%
      dplyr::mutate(cohort_end_date = dplyr::if_else(
        .data$future_observation < .data$duration,
        !!CDMConnector::dateadd("cohort_start_date", "future_observation"),
        .data$cohort_end_date
      )) %>%
      dplyr::select(-"future_observation", -"duration") %>%
      computeTable(cdm)
    xNew <- PatientProfiles::addAttributes(xNew, x)
    return(xNew)
  } else {
    return(x)
  }
}

#' @noRd
unionCohort <- function(x, gap, cdm) {
  xNew <- x %>%
    dplyr::select(
      "cohort_definition_id",
      "subject_id",
      "date_event" = "cohort_start_date"
    ) %>%
    dplyr::mutate(date_id = -1) %>%
    dplyr::union_all(
      x %>%
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
    dplyr::select(-"era_id") %>%
    computeTable(cdm)
  xNew <- PatientProfiles::addAttributes(xNew, x)
  return(xNew)
}

#' @noRd
applyLimit <- function(cohort, cdm, limit) {
  limit <- tolower(limit)
  if (limit == "first") {
    cohort <- cohort %>%
      dplyr::group_by(.data$cohort_definition_id, .data$subject_id) %>%
      dplyr::filter(
        .data$cohort_start_date == min(.data$cohort_start_date, na.rm = TRUE)
      ) %>%
      dplyr::ungroup() %>%
      computeTable(cdm)
  }
  return(cohort)
}

correctDuration <- function(x,
                            imputeDuration,
                            durationRange,
                            cdm,
                            start = "cohort_start_date",
                            end = "cohort_end_date") {

  x <- x %>%
    dplyr::mutate(
      duration = !!CDMConnector::datediff(start = start, end = end) + 1
    ) %>%
    rowsToImpute("duration", durationRange)
  impute <- x %>%
    dplyr::summarise(
      count = sum(.data$impute, na.rm = TRUE),
      exposures = dplyr::n()
    ) %>%
    dplyr::collect()
  impute <- c(impute$count, 100*impute$count/impute$exposures)
  x <- x %>%
    solveImputation("duration", imputeDuration, TRUE) %>%
    dplyr::mutate(days_to_add = as.integer(.data$duration - 1)) %>%
    dplyr::mutate(
      !!end := !!CDMConnector::dateadd(date = start, number = "days_to_add")
    ) %>%
    dplyr::select(-c("duration", "days_to_add")) %>%
    computeTable(cdm)
  attr(x, "impute") <- impute
  return(x)
}

#' Impute or eliminate values under a certain conditions
#' @noRd
rowsToImpute <- function(x, column, range) {
  # identify NA
  x <- x %>%
    dplyr::mutate(impute = dplyr::if_else(
      is.na(.data[[column]]), 1, 0
    ))

  # identify < range[1]
  if (!is.infinite(range[1])) {
    x <- x %>%
      dplyr::mutate(impute = dplyr::if_else(
        .data$impute == 0, dplyr::if_else(.data[[column]] < !!range[1], 1, 0), 1
      ))
  }
  # identify > range[2]
  if (!is.infinite(range[2])) {
    x <- x %>%
      dplyr::mutate(impute = dplyr::if_else(
        .data$impute == 0, dplyr::if_else(.data[[column]] > !!range[2], 1, 0), 1
      ))
  }

  return(x)
}

solveImputation <- function(x, column, method, toRound = FALSE) {
  if (method == "none") {
    x <- x %>%
      dplyr::filter(.data$impute == 0)
  } else {
    imp <- x %>%
      dplyr::filter(.data$impute == 0)
    if (method == "median") {
      imp <- imp %>%
        dplyr::summarise("x" = stats::median(.data[[column]], na.rm = TRUE)) %>%
        dplyr::pull("x")
    } else if (method == "mean") {
      imp <- imp %>%
        dplyr::summarise("x" = mean(.data[[column]], na.rm = TRUE)) %>%
        dplyr::pull("x")
    } else if (method == "mode") {
      imp <- imp %>%
        dplyr::group_by(.data[[column]]) %>%
        dplyr::summarise(count = as.numeric(dplyr::n()), .groups = "drop") %>%
        dplyr::filter(.data$count == max(.data$count, na.rm = TRUE)) %>%
        dplyr::select("x" = dplyr::all_of(column)) %>%
        dplyr::pull() %>%
        mean()
    } else {
      imp <- as.numeric(method)
    }
    if (toRound) imp <- round(imp)
    x <- x %>%
      dplyr::mutate(!!column := dplyr::if_else(
        .data$impute == 1, .env$imp, .data[[column]]
      ))
  }
  x <- x %>% dplyr::select(-"impute")
  return(x)
}

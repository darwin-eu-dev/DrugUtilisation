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

#' @noRd
conceptSetFromConceptSetList <- function(conceptSetList, cohortSet) {
  purrr::map(conceptSetList, dplyr::as_tibble) %>%
    dplyr::bind_rows(.id = "cohort_name") %>%
    dplyr::rename("drug_concept_id" = "value") %>%
    dplyr::inner_join(
      cohortSet |> dplyr::select("cohort_definition_id", "cohort_name"),
      by = "cohort_name"
    ) %>%
    dplyr::select(-"cohort_name")
}

#' @noRd
subsetTables <- function(cdm, conceptSet) {
  nm <- uniqueTmpName()
  cdm <- omopgenerics::insertTable(
    cdm = cdm, name = nm, table = conceptSet, overwrite = TRUE
  )
  cohort <- cdm$drug_exposure |>
    dplyr::select(
      "drug_concept_id",
      "subject_id" = "person_id",
      "cohort_start_date" = "drug_exposure_start_date",
      "cohort_end_date" = "drug_exposure_end_date"
    ) |>
    dplyr::inner_join(cdm[[nm]], by = "drug_concept_id") |>
    dplyr::compute(
      temporary = FALSE, name = uniqueTmpName(), overwrite = TRUE
    )
  return(cohort)
}

#' @noRd
requirePriorUseWashout <- function(cohort, cdm, washout) {
  cohort <- cohort %>%
    dplyr::group_by(.data$cohort_definition_id, .data$subject_id) %>%
    dplyr::arrange(.data$cohort_start_date) %>%
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
      dplyr::mutate(cohort_end_date = as.Date(dplyr::if_else(
        .data$future_observation < .data$duration,
        !!CDMConnector::dateadd("cohort_start_date", "future_observation"),
        .data$cohort_end_date
      ))) %>%
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
      !!end := as.Date(!!CDMConnector::dateadd(date = start, number = "days_to_add"))
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
        dplyr::summarise(dplyr::across(dplyr::all_of(column),
                                       ~ stats::median(., na.rm = TRUE),
                                       .names = "x")) %>%
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

uniqueTmpName <- function() {
  i <- getOption("tmp_table_name", 0) + 1
  options(tmp_table_name = i)
  sprintf("tmp_%03i", i)
}
dropTmpTables <- function(cdm) {
  omopgenerics::dropTable(cdm = cdm, name = dplyr::starts_with("tmp_"))
}

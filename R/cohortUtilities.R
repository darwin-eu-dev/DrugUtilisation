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
subsetTables <- function(cdm, conceptSet, imputeDuration, durationRange, name) {
  # insert concepts
  nm <- uniqueTmpName()
  cdm <- omopgenerics::insertTable(
    cdm = cdm, name = nm, table = conceptSet, overwrite = TRUE
  )
  cdm[[nm]] <- cdm[[nm]] |> dplyr::compute()

  imputation <- imputeDuration != "none" || !all(durationRange == c(1, Inf))

  # subset table
  cohort <- cdm$drug_exposure |>
    dplyr::select(
      "drug_concept_id",
      "subject_id" = "person_id",
      "cohort_start_date" = "drug_exposure_start_date",
      "cohort_end_date" = "drug_exposure_end_date"
    ) |>
    dplyr::inner_join(cdm[[nm]], by = "drug_concept_id") |>
    dplyr::inner_join(
      cdm$observation_period |>
        dplyr::select(
          "subject_id" = "person_id",
          "observation_period_start_date",
          "observation_period_end_date"
        ),
      by = "subject_id"
    ) |>
    dplyr::mutate(
      "cohort_start_date" = dplyr::if_else(
        .data$cohort_start_date < .data$observation_period_start_date,
        .data$observation_period_start_date,
        .data$cohort_start_date
      ),
      "cohort_end_date" = dplyr::if_else(
        .data$cohort_end_date > .data$observation_period_end_date,
        .data$observation_period_end_date,
        .data$cohort_end_date
      )
    )
  if (!imputation) {
    cohort <- cohort |>
      dplyr::filter(.data$cohort_start_date <= .data$cohort_end_date)
  }
  cohort <- cohort |>
    dplyr::select(
      "cohort_definition_id", "subject_id", "cohort_start_date",
      "cohort_end_date"
    ) |>
    dplyr::compute(temporary = FALSE, name = name, overwrite = TRUE)

  # impute
  if (imputation) {
    cohort <- correctDuration(cohort, imputeDuration, durationRange)
  }

  # erafy
  if (cohort |> dplyr::tally() |> dplyr::pull() > 0) {
    cohort <- cohort |> erafy(gap = 0)
  }

  return(cohort)
}

sumcounts <- function(cohort) {
  cohortCount(cohort) |> dplyr::pull("number_records") |> sum()
}

#' @noRd
erafyCohort <- function(cohort, gap) {
  if (sumcounts(cohort) > 0 & gap > 0) {
    cohort <- cohort |>
      erafy(gap) |>
      omopgenerics::recordCohortAttrition(paste(
        "join exposures separated by", gap, "or less days"
      ))
  }
  return(cohort)
}

#' @noRd
requirePriorUseWashout <- function(cohort, washout) {
  if (sumcounts(cohort) > 0 & washout > 0) {
    name <- attr(cohort, "tbl_name")
    cohort <- cohort %>%
      dplyr::group_by(.data$cohort_definition_id, .data$subject_id) %>%
      dplyr::arrange(.data$cohort_start_date) %>%
      dplyr::mutate(id = dplyr::row_number()) %>%
      dplyr::compute(name = uniqueTmpName(), temporary = FALSE, overwrite = TRUE)
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
      dplyr::arrange() %>%
      dplyr::ungroup() %>%
      dplyr::compute(name = name, temporary = FALSE, overwrite = TRUE)
    res <- paste("require prior use washout of", washout, "days")
    cohort <- cohort |>
      omopgenerics::recordCohortAttrition(reason = res)
  }
  return(cohort)
}

#' @noRd
trimStartDate <- function(cohort, startDate) {
  if (sumcounts(cohort) > 0 & !is.na(startDate)) {
    cohort <- cohort %>%
      dplyr::mutate(cohort_start_date = dplyr::if_else(
        .data$cohort_start_date <= !!startDate,
        as.Date(!!startDate), as.Date(.data$cohort_start_date)
      )) |>
      dplyr::filter(.data$cohort_start_date <= .data$cohort_end_date) |>
      dplyr::compute(
        name = attr(cohort, "tbl_name"), temporary = FALSE, overwrite = TRUE
      ) |>
      omopgenerics::recordCohortAttrition(
        paste("restrict cohort_start_date on or after", startDate)
      )
  }
  return(cohort)
}

#' @noRd
trimEndDate <- function(cohort, endDate) {
  if (sumcounts(cohort) > 0 & !is.na(endDate)) {
    cohort <- cohort %>%
      dplyr::mutate(cohort_end_date = dplyr::if_else(
        .data$cohort_end_date >= !!endDate,
        as.Date(!!endDate), as.Date(.data$cohort_end_date)
      )) %>%
      dplyr::filter(.data$cohort_start_date <= .data$cohort_end_date) |>
      dplyr::compute(
        name = attr(cohort, "tbl_name"), temporary = FALSE, overwrite = TRUE
      ) |>
      omopgenerics::recordCohortAttrition(
        paste("restrict cohort_end_date on or before", endDate)
      )
  }
  return(cohort)
}

#' @noRd
requirePriorObservation <- function(cohort, priorObservation) {
  if (sumcounts(cohort) > 0 & priorObservation > 0) {
    res <- paste("require at least", priorObservation, "prior observation")
    cohort <- cohort |>
      PatientProfiles::addPriorObservation() %>%
      dplyr::filter(.data$prior_observation >= .env$priorObservation) %>%
      dplyr::select(-"prior_observation") %>%
      dplyr::compute(
        name = attr(cohort, "tbl_name"), temporary = FALSE, overwrite = TRUE
      ) |>
      omopgenerics::recordCohortAttrition(reason = res)
  }
  return(cohort)
}

#' @noRd
erafy <- function(x,
                  gap = 0,
                  start = "cohort_start_date",
                  end = "cohort_end_date",
                  group = c("cohort_definition_id", "subject_id")) {
  xstart <- x |>
    dplyr::select(dplyr::all_of(c(group, "date_event" = start))) |>
    dplyr::mutate(date_id = -1)
  xend <- x |>
    dplyr::select(dplyr::all_of(c(group, "date_event" = end))) |>
    dplyr::mutate(date_id = 1)
  if (gap > 0) {
    xend <- xend %>%
      dplyr::mutate("date_event" = as.Date(!!CDMConnector::dateadd(
        date = "date_event", number = gap, interval = "day"
      )))
  }
  x <- xstart |>
    dplyr::union_all(xend) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group))) %>%
    dplyr::arrange(.data$date_event, .data$date_id) %>%
    dplyr::mutate(cum_id = cumsum(.data$date_id)) %>%
    dplyr::filter(
      .data$cum_id == 0 || (.data$cum_id == -1 && .data$date_id == -1)
    ) %>%
    dplyr::mutate(
      name = dplyr::if_else(.data$date_id == -1, .env$start, .env$end),
      era_id = dplyr::if_else(.data$date_id == -1, 1, 0)
    ) %>%
    dplyr::mutate(era_id = cumsum(as.numeric(.data$era_id))) %>%
    dplyr::ungroup() %>%
    dplyr::arrange() %>%
    dplyr::select(
      "cohort_definition_id", "subject_id", "era_id", "name", "date_event"
    ) %>%
    tidyr::pivot_wider(names_from = "name", values_from = "date_event") %>%
    dplyr::mutate(!!end := as.Date(!!CDMConnector::dateadd(
      date = end, number = -gap, interval = "day"
    ))) %>%
    dplyr::select(-"era_id") |>
    dplyr::compute(
      temporary = FALSE, overwrite = TRUE, name = attr(x, "tbl_name")
    )
  return(x)
}

#' @noRd
applyLimit <- function(cohort, limit) {
  limit <- tolower(limit)
  if (sumcounts(cohort) > 0 & limit == "first") {
    cohort <- cohort %>%
      dplyr::group_by(.data$cohort_definition_id, .data$subject_id) %>%
      dplyr::filter(
        .data$cohort_start_date == min(.data$cohort_start_date, na.rm = TRUE)
      ) %>%
      dplyr::ungroup() %>%
      dplyr::compute(
        name = attr(cohort, "tbl_name"), temporary = FALSE, overwrite = TRUE
      ) |>
      omopgenerics::recordCohortAttrition(reason = "restric to first record")
  }
  return(cohort)
}

correctDuration <- function(x,
                            imputeDuration,
                            durationRange,
                            start = "cohort_start_date",
                            end = "cohort_end_date") {
  name <- attr(x, "tbl_name")
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
    dplyr::collect() |>
    dplyr::mutate("count" = dplyr::if_else(
      .data$exposures == 0, 0, .data$count
    ))
  impute <- c(impute$count, 100*impute$count/impute$exposures)
  x <- x %>%
    solveImputation("duration", imputeDuration, TRUE) %>%
    dplyr::mutate(days_to_add = as.integer(.data$duration - 1)) %>%
    dplyr::mutate(
      !!end := as.Date(!!CDMConnector::dateadd(date = start, number = "days_to_add"))
    ) %>%
    dplyr::select(-c("duration", "days_to_add")) %>%
    dplyr::compute(
      temporary = FALSE, name = name, overwirte = TRUE
    )
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
  con <- attr(attr(cdm, "cdm_source"), "dbcon")
  schema <- attr(attr(cdm, "cdm_source"), "write_schema")
  initialTables <- CDMConnector::listTables(con = con, schema = schema)
  droptables <- initialTables[startsWith(initialTables, "tmp_")]
  omopgenerics::dropTable(cdm = cdm, name = droptables)
}

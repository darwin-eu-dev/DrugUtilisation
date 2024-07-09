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

#' Format a summarised_indication object into a visual table.
#'
#' `r lifecycle::badge("experimental")`
#'
#' @param result A summarised_result object with results from
#' summariseIndication().
#' @param header A vector containing which elements should go into the header
#' in order. Allowed are: `cdm_name`, `group`, `strata`, `variable`.
#' @param splitStrata If TRUE strata columns will be splitted.
#' @param cohortName If TRUE cohort names will be displayed.
#' @param cdmName If TRUE database names will be displayed.
#' @param groupColumn Column to use as group labels.
#' @param type Type of desired formatted table, possibilities: "gt",
#' "flextable", "tibble".
#' @param .options Named list with additional formatting options.
#' DrugUtilisation::optionsTableIndication() shows allowed
#' arguments and their default values.
#'
#' @examples
#' \donttest{
#'
#' library(DrugUtilisation)
#'
#' cdm <- mockDrugUtilisation()
#'
#' # result <- cdm$cohort1 |>
#' #  addIndication(
#' #    indicationCohortName = "cohort2", indicationGap = c(0, 7, 30, Inf),
#' #    unknownIndicationTable = "condition_occurrence"
#' #  ) |>
#' #  summariseIndication()
#'
#' # tableIndication(result)
#'
#'
#' CDMConnector::cdmDisconnect(cdm = cdm)
#' }
#'
#' @return A table with a formatted version of summariseIndication() results.
#'
#' @export
#'
tableIndication <- function(result,
                            header = c("group", "strata"),
                            splitStrata = TRUE,
                            cohortName = TRUE,
                            cdmName = TRUE,
                            groupColumn = "variable_name",
                            type = "gt",
                            .options = list()) {
  # check input and filter result
  if (!cohortName & "cohort_name" %in% groupColumn) {
    cli::cli_abort("If `cohortName = FALSE`, `cohort_name` cannot be used in `groupColumn`.")
  }
  if (!cdmName & "cdm_name" %in% groupColumn) {
    cli::cli_abort("If `cdmName = FALSE`, `cdm_name` cannot be used in `groupColumn`.")
  }
  if (!is.null(header)) {
    if (any(! header %in% c("cdm_name", "group", "strata", "variable"))) {
      cli::cli_abort("`header` should be a character vector restricted to the following values: `cdm_name`, `group`, `strata`, `variable`")
    }
    if (!is.null(groupColumn)) {
      if (grepl(paste0(header, collapse = "|"), groupColumn)) {
        cli::cli_abort("Columns to use as header cannot be in `groupColumn`.")
      }
    }
  }
  result <- result |>
    omopgenerics::newSummarisedResult() |>
    visOmopResults::filterSettings(.data$result_type == "summarised_indication") |>
    dplyr::filter(!grepl("number", .data$variable_name))
  if (nrow(result) == 0) {
    cli::cli_abort("There are no results with `result_type = summarised_indication`")
  }
  checkmate::assertLogical(cohortName, any.missing = FALSE)
  checkmate::assertLogical(cdmName, any.missing = FALSE)
  checkmate::assertLogical(splitStrata, any.missing = FALSE)
  checkmate::assertCharacter(header, any.missing = FALSE, null.ok = TRUE)

  # .options
  .options = defaultTableOptions(.options)

  # Split
  split <- c("additional")

  # Exclude columns, rename, and split
  excludeColumns <- c("result_id", "estimate_type", "estimate_name")
  if (!cohortName) {
    if ("group" %in% header) {
      cli::cli_warn(c("!" = "Dropping group from header as `cohortName = FALSE`."))
      header <- header[!"group" %in% header]
    }
    excludeColumns <- c(excludeColumns, "group_name", "group_level")
  } else {
    split <- c(split, "group")
  }

  if (!cdmName) {
    if ("cdm_name" %in% header) {
      cli::cli_warn(c("!" = "Dropping cdm_name from header as `cdmName = FALSE`."))
      header <- header[!"cdm_name" %in% header]
    }
    excludeColumns <- c(excludeColumns, "cdm_name")
    renameColumns <- character()
  } else {
    renameColumns <- c("Database name" = "cdm_name")
  }
  if (!"variable_name" %in% groupColumn) {
    renameColumns <- c(renameColumns, "Indication window" = "variable_name")
  }
  if (!"variable_level" %in% groupColumn) {
    renameColumns <- c(renameColumns, "Indication" = "variable_level")
  }

  # split
  if (splitStrata) {
    split <- c(split, "strata")
  }

  # table
  visOmopResults::visOmopTable(
    result = result,
    formatEstimateName = c("N (%)" = "<count> (<percentage> %)"),
    header = header,
    split = split,
    groupColumn = groupColumn,
    renameColumns = renameColumns,
    type = type,
    excludeColumns = excludeColumns,
    .options = .options
  )
}


#' Format a dose_coverage object into a visual table.
#'
#' `r lifecycle::badge("experimental")`
#'
#' @param result A summarised_result object with results from
#' summariseDoseCoverage().
#' @param header A vector containing which elements should go into the header
#' in order. Allowed are: `cdm_name`, `group`, `strata`, `variable`, and
#' `estimate`.
#' @param splitStrata If TRUE strata columns will be splitted.
#' @param ingridientName If TRUE cohort names will be displayed.
#' @param cdmName If TRUE database names will be displayed.
#' @param groupColumn Column to use as group labels.
#' @param type Type of desired formatted table, possibilities: "gt",
#' "flextable", "tibble".
#' @param formatEstimateName Named list of estimate name's to join, sorted by
#' computation order. Indicate estimate_name's between <...>.
#' @param .options Named list with additional formatting options.
#' DrugUtilisation::optionstableDoseCoverage() shows allowed
#' arguments and their default values.
#'
#' @examples
#' \donttest{
#' library(DrugUtilisation)
#'
#' cdm <- mockDrugUtilisation()
#'
#' result <- summariseDoseCoverage(cdm, 1125315)
#'
#' tableDoseCoverage(result)
#'
#'
#' CDMConnector::cdmDisconnect(cdm = cdm)
#' }
#'
#' @return A table with a formatted version of summariseDrugCoverage() results.
#'
#' @export
#'
tableDoseCoverage <- function(result,
                              header = c("variable", "estimate"),
                              splitStrata = TRUE,
                              ingridientName = TRUE,
                              cdmName = TRUE,
                              groupColumn = NULL,
                              type = "gt",
                              formatEstimateName = c(
                                "N (%)" = "<count_missing> (<percentage_missing> %)",
                                "N" = "<count>",
                                "Mean (SD)" = "<mean> (<sd>)",
                                "Median (Q25 - Q75)" = "<median> (<q25> - <q75>)"
                              ),
                              .options = list()) {
  # check input and filter result
  if (!ingridientName & "ingredient_name" %in% groupColumn) {
    cli::cli_abort("If `ingridientName = FALSE`, `ingredient_name` cannot be used in `groupColumn`.")
  }
  if (!cdmName & "cdm_name" %in% groupColumn) {
    cli::cli_abort("If `cdmName = FALSE`, `cdm_name` cannot be used in `groupColumn`.")
  }
  if (!is.null(header)) {
    if (any(!header %in% c("cdm_name", "group", "strata", "variable", "estimate"))) {
      cli::cli_abort("`header` should be a character vector restricted to the following values: `cdm_name`, `group`, `strata`, `variable`, `estimate`")
    }
    if (!is.null(groupColumn)) {
      if (grepl(paste0(header, collapse = "|"), groupColumn)) {
        cli::cli_abort("Columns to use as header cannot be in `groupColumn`.")
      }
    }
  }
  result <- result |>
    omopgenerics::newSummarisedResult() |>
    visOmopResults::filterSettings(.data$result_type == "dose_coverage")
  if (nrow(result) == 0) {
    cli::cli_abort("There are no results with `result_type = dose_coverage`")
  }
  checkmate::assertLogical(ingridientName, any.missing = FALSE)
  checkmate::assertLogical(cdmName, any.missing = FALSE)
  checkmate::assertLogical(splitStrata, any.missing = FALSE)
  checkmate::assertCharacter(header, any.missing = FALSE, null.ok = TRUE)

  # .options
  .options = defaultTableOptions(.options)

  # Split
  split <- c("additional")

  # Exclude columns, rename, and split
  excludeColumns <- c("result_id", "estimate_type", "variable_level")
  if (!ingridientName) {
    if ("group" %in% header) {
      cli::cli_warn(c("!" = "Dropping group from header as `ingridientName = FALSE`."))
      header <- header[!"group" %in% header]
    }
    excludeColumns <- c(excludeColumns, "group_name", "group_level")
  } else {
    split <- c(split, "group")
  }

  if (!cdmName) {
    if ("cdm_name" %in% header) {
      cli::cli_warn(c("!" = "Dropping cdm_name from header as `cdmName = FALSE`."))
      header <- header[!"cdm_name" %in% header]
    }
    excludeColumns <- c(excludeColumns, "cdm_name")
    renameColumns <- character()
  } else {
    renameColumns <- c("Database name" = "cdm_name")
  }
  if (!"variable_name" %in% groupColumn) {
    renameColumns <- c(renameColumns, "Variable" = "variable_name")
  }

  # split
  if (splitStrata) {
    split <- c(split, "strata")
  }

  # table
  suppressMessages(
    visOmopResults::visOmopTable(
      result = result,
      formatEstimateName = formatEstimateName,
      header = header,
      split = split,
      groupColumn = groupColumn,
      renameColumns = renameColumns,
      type = type,
      excludeColumns = excludeColumns,
      .options = .options
    )
  )
}



defaultTableOptions <- function(.options) {

  defaults <- visOmopResults::optionsVisOmopTable()

  for (opt in names(.options)) {
    defaults[[opt]] <- .options[[opt]]
  }

  return(defaults)
}

#' Additional arguments for the functions tableIndication
#'
#' @description
#' It provides a list of allowed inputs for .option argument in tableIndication,
#' and their given default values.
#'
#'
#' @return The default .options named list.
#'
#' @export
#'
#' @examples
#' {
#' optionsTableIndication()
#' }
#'
optionsTableIndication <- function() {
  defaultTableOptions(NULL)
}

#' Additional arguments for the functions tableDoseCoverage
#'
#' @description
#' It provides a list of allowed inputs for .option argument in tableDoseCoverage,
#' and their given default values.
#'
#'
#' @return The default .options named list.
#'
#' @export
#'
#' @examples
#' {
#' optionsTableDoseCoverage()
#' }
#'
optionsTableDoseCoverage <- function() {
  defaultTableOptions(NULL)
}

#' Format a summarised_treatment object into a visual table.
#'
#' `r lifecycle::badge("experimental")`
#'
#' @param result A summarised_result object with results from
#' summariseTreatmentFromCohort() or summariseTreatmentFromConceptSet().
#' @param header A vector containing which elements should go into the header
#' in order. Allowed are: `cdm_name`, `cohort_name`, `strata`, `variable`,
#' `estimate` and `window_name`.
#' @param splitStrata If TRUE strata columns will be splitted.
#' @param cdmName If TRUE database names will be displayed.
#' @param groupColumn Column to use as group labels.
#' @param type Type of desired formatted table, possibilities: "gt",
#' "flextable", "tibble".
#' @param formatEstimateName Named list of estimate name's to join, sorted by
#' computation order. Indicate estimate_name's between <...>.
#'
#' @examples
#' \donttest{
#' library(DrugUtilisation)
#'
#' cdm <- mockDrugUtilisation()
#'
#' result <- summariseDoseCoverage(cdm, 1125315)
#'
#' tableDoseCoverage(result)
#'
#'
#' CDMConnector::cdmDisconnect(cdm = cdm)
#' }
#'
#' @return A table with a formatted version of summariseDrugCoverage() results.
#'
#' @export
#'
tableTreatment <- function(result,
                           header = c("window_name"),
                           splitStrata = TRUE,
                           cdmName = TRUE,
                           groupColumn = c("cdm_name", "cohort_name"),
                           type = "gt",
                           formatEstimateName = c(
                             "N (%)" = "<count> (<percentage> %)"),
                           .options = list()) {
  opts <- c(
    "cdm_name", "cohort_name", "strata", "variable", "estimate", "window_name")
  assertChoice(header, choices = opts, null = TRUE)
  assertChoice(groupColumn, choices = opts, null = TRUE)
  assertChoice(type, choices = c("gt", "flextable", "tibble"), length = 1)

  if (!is.null(header)) {
    if (any(!header %in% c("cdm_name", "group", "strata", "variable", "estimate"))) {
      cli::cli_abort("`header` should be a character vector restricted to the following values: `cdm_name`, `group`, `strata`, `variable`, `estimate`")
    }
    if (!is.null(groupColumn)) {
      if (grepl(paste0(header, collapse = "|"), groupColumn)) {
        cli::cli_abort("Columns to use as header cannot be in `groupColumn`.")
      }
    }
  }
  result <- result |>
    omopgenerics::newSummarisedResult() |>
    visOmopResults::filterSettings(.data$result_type == "dose_coverage")
  if (nrow(result) == 0) {
    cli::cli_abort("There are no results with `result_type = dose_coverage`")
  }
  checkmate::assertLogical(ingridientName, any.missing = FALSE)
  checkmate::assertLogical(cdmName, any.missing = FALSE)
  checkmate::assertLogical(splitStrata, any.missing = FALSE)
  checkmate::assertCharacter(header, any.missing = FALSE, null.ok = TRUE)

  # .options
  .options = defaultTableOptions(.options)

  # Split
  split <- c("additional")

  # Exclude columns, rename, and split
  excludeColumns <- c("result_id", "estimate_type", "variable_level")
  if (!ingridientName) {
    if ("group" %in% header) {
      cli::cli_warn(c("!" = "Dropping group from header as `ingridientName = FALSE`."))
      header <- header[!"group" %in% header]
    }
    excludeColumns <- c(excludeColumns, "group_name", "group_level")
  } else {
    split <- c(split, "group")
  }

  if (!cdmName) {
    if ("cdm_name" %in% header) {
      cli::cli_warn(c("!" = "Dropping cdm_name from header as `cdmName = FALSE`."))
      header <- header[!"cdm_name" %in% header]
    }
    excludeColumns <- c(excludeColumns, "cdm_name")
    renameColumns <- character()
  } else {
    renameColumns <- c("Database name" = "cdm_name")
  }
  if (!"variable_name" %in% groupColumn) {
    renameColumns <- c(renameColumns, "Variable" = "variable_name")
  }

  # split
  if (splitStrata) {
    split <- c(split, "strata")
  }

  # table
  suppressMessages(
    visOmopResults::visOmopTable(
      result = result,
      formatEstimateName = formatEstimateName,
      header = header,
      split = split,
      groupColumn = groupColumn,
      renameColumns = renameColumns,
      type = type,
      excludeColumns = excludeColumns,
      .options = .options
    )
  )
}

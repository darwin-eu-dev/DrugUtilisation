# Copyright 2024 DARWIN EU (C)
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

#' Create a table showing indication results
#'
#' `r lifecycle::badge("experimental")`
#'
#' @param result A summarised_result created by summariseIndication().
#' @param header A vector containing which elements should go into the header
#' in order. Allowed are: `cdm_name`, `group`, `strata`, `variable`.
#' @param splitStrata If TRUE strata columns will be split.
#' @param cohortName If TRUE cohort names will be displayed.
#' @param cdmName If TRUE database names will be displayed.
#' @param groupColumn Column to use as group labels.
#' @param type Type of desired formatted table, possibilities: "gt",
#' "flextable", "tibble".
#' @param .options Named list with additional formatting options.
#' DrugUtilisation::defaultTableOptions() shows allowed
#' arguments and their default values.
#'
#' @examples
#' \donttest{
#'
#' library(DrugUtilisation)
#'
#' cdm <- mockDrugUtilisation()
#'
#' result <- cdm$cohort1 |>
#'   summariseIndication(
#'     indicationCohortName = "cohort2",
#'     indicationWindow = list(c(-30, 0)),
#'     unknownIndicationTable = "condition_occurrence"
#'   )
#'
#' tableIndication(result, type = "tibble")
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
  if (!cohortName & "cohort_name" %in% unlist(groupColumn)) {
    cli::cli_abort("If `cohortName = FALSE`, `cohort_name` cannot be used in `groupColumn`.")
  }
  if (!cdmName & "cdm_name" %in% unlist(groupColumn)) {
    cli::cli_abort("If `cdmName = FALSE`, `cdm_name` cannot be used in `groupColumn`.")
  }
  if (!is.null(header)) {
    if (any(!header %in% c("cdm_name", "group", "strata", "variable"))) {
      cli::cli_abort("`header` should be a character vector restricted to the following values: `cdm_name`, `group`, `strata`, `variable`")
    }
    if (!is.null(groupColumn)) {
      if (any(grepl(paste0(header, collapse = "|"), unlist(groupColumn)))) {
        cli::cli_abort("Columns to use as header cannot be in `groupColumn`.")
      }
    }
  }
  result <- result |>
    omopgenerics::newSummarisedResult() |>
    visOmopResults::filterSettings(.data$result_type == "summarise_indication") |>
    dplyr::filter(!grepl("number", .data$variable_name))
  if (nrow(result) == 0) {
    cli::cli_abort("There are no results with `result_type = summarise_indication`")
  }
  checkmate::assertLogical(cohortName, any.missing = FALSE)
  checkmate::assertLogical(cdmName, any.missing = FALSE)
  checkmate::assertLogical(splitStrata, any.missing = FALSE)
  checkmate::assertCharacter(header, any.missing = FALSE, null.ok = TRUE)

  # .options
  .options <- defaultTableOptionsInternal(.options)

  # Split
  split <- c("additional")

  # Exclude columns, rename, and split
  excludeColumns <- c("result_id", "estimate_type", "estimate_name")
  if (!cohortName) {
    if ("group" %in% header) {
      cli::cli_warn(c("!" = "Dropping group from header as `cohortName = FALSE`."))
      header <- header[!header %in% "group"]
    }
    excludeColumns <- c(excludeColumns, "group_name", "group_level")
  } else {
    split <- c(split, "group")
  }

  if (!cdmName) {
    if ("cdm_name" %in% header) {
      cli::cli_warn(c("!" = "Dropping cdm_name from header as `cdmName = FALSE`."))
      header <- header[!header %in% "cdm_name"]
    }
    excludeColumns <- c(excludeColumns, "cdm_name")
    renameColumns <- character()
  } else {
    renameColumns <- c("Database name" = "cdm_name")
  }
  if (!"variable_name" %in% unlist(groupColumn)) {
    renameColumns <- c(renameColumns, "Indication window" = "variable_name")
  }
  if (!"variable_level" %in% unlist(groupColumn)) {
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
#' @param splitStrata If TRUE strata columns will be split.
#' @param ingridientName If TRUE cohort names will be displayed.
#' @param cdmName If TRUE database names will be displayed.
#' @param groupColumn Column to use as group labels.
#' @param type Type of desired formatted table, possibilities: "gt",
#' "flextable", "tibble".
#' @param formatEstimateName Named list of estimate name's to join, sorted by
#' computation order. Indicate estimate_name's between <...>.
#' @param .options Named list with additional formatting options.
#' DrugUtilisation::defaultTableOptions() shows allowed
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
  if (!ingridientName & "ingredient_name" %in% unlist(groupColumn)) {
    cli::cli_abort("If `ingridientName = FALSE`, `ingredient_name` cannot be used in `groupColumn`.")
  }
  if (!cdmName & "cdm_name" %in% unlist(groupColumn)) {
    cli::cli_abort("If `cdmName = FALSE`, `cdm_name` cannot be used in `groupColumn`.")
  }
  if (!is.null(header)) {
    if (any(!header %in% c("cdm_name", "group", "strata", "variable", "estimate"))) {
      cli::cli_abort("`header` should be a character vector restricted to the following values: `cdm_name`, `group`, `strata`, `variable`, `estimate`")
    }
    if (!is.null(groupColumn)) {
      if (any(grepl(paste0(header, collapse = "|"), unlist(groupColumn)))) {
        cli::cli_abort("Columns to use as header cannot be in `groupColumn`.")
      }
    }
  }
  result <- result |>
    omopgenerics::newSummarisedResult() |>
    visOmopResults::filterSettings(.data$result_type == "summarise_dose_coverage")
  if (nrow(result) == 0) {
    cli::cli_abort("There are no results with `result_type = summarise_dose_coverage`")
  }
  checkmate::assertLogical(ingridientName, any.missing = FALSE)
  checkmate::assertLogical(cdmName, any.missing = FALSE)
  checkmate::assertLogical(splitStrata, any.missing = FALSE)
  checkmate::assertCharacter(header, any.missing = FALSE, null.ok = TRUE)

  # .options
  .options <- defaultTableOptionsInternal(.options)

  # Split
  split <- c("additional")

  # Exclude columns, rename, and split
  excludeColumns <- c("result_id", "estimate_type", "variable_level")
  if (!ingridientName) {
    if ("group" %in% header) {
      cli::cli_warn(c("!" = "Dropping group from header as `ingridientName = FALSE`."))
      header <- header[!header %in% "group"]
    }
    excludeColumns <- c(excludeColumns, "group_name", "group_level")
  } else {
    split <- c(split, "group")
  }

  if (!cdmName) {
    if ("cdm_name" %in% header) {
      cli::cli_warn(c("!" = "Dropping cdm_name from header as `cdmName = FALSE`."))
      header <- header[!header %in% "cdm_name"]
    }
    excludeColumns <- c(excludeColumns, "cdm_name")
    renameColumns <- character()
  } else {
    renameColumns <- c("Database name" = "cdm_name")
  }
  if (!"variable_name" %in% unlist(groupColumn)) {
    renameColumns <- c(renameColumns, "Variable" = "variable_name")
  }

  # split
  if (splitStrata) {
    split <- c(split, "strata")
  }

  # table
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
}

#' Format a drug_utilisation object into a visual table.
#'
#' `r lifecycle::badge("experimental")`
#'
#' @param result A summarised_result object with results from
#' summariseDrugUtilisation().
#' @param header A vector containing which elements should go into the header
#' in order. Allowed are: `cdm_name`, `group`, `strata`, `variable`, `estimate`.
#' @param splitStrata If TRUE strata columns will be split.
#' @param cohortName If TRUE cohort names will be displayed.
#' @param cdmName If TRUE database names will be displayed.
#' @param conceptSet If TRUE concept sets name will be displayed.
#' @param ingredient If TRUE ingredients names will be displayed for dose
#' calculation.
#' @param groupColumn Column to use as group labels, these can be:
#' "cdm_name", "cohort_name", "concept_set", "variable_name", and/or
#' "ingredient". If strata is split, any of the levels can be used, otherwise
#' "strata_name" and "strata_level" can be used for table group format.
#' @param type Type of desired formatted table, possibilities: "gt",
#' "flextable", "tibble".
#' @param formatEstimateName Named list of estimate name's to join, sorted by
#' computation order. Indicate estimate_name's between <...>.
#' @param .options Named list with additional formatting options.
#' DrugUtilisation::defaultTableOptions() shows allowed
#' arguments and their default values.
#'
#' @examples
#' \donttest{
#'
#' library(DrugUtilisation)
#' library(CodelistGenerator)
#'
#' cdm <- mockDrugUtilisation()
#' codelist <- CodelistGenerator::getDrugIngredientCodes(cdm, "acetaminophen")
#' cdm <- generateDrugUtilisationCohortSet(cdm, "dus_cohort", codelist)
#' cdm[["dus_cohort"]] %>%
#'   summariseDrugUtilisation(ingredientConceptId = 1125315) |>
#'   tableDrugUtilisation()
#' }
#'
#' @return A table with a formatted version of summariseIndication() results.
#'
#' @export
#'
tableDrugUtilisation <- function(result,
                                 header = c("group", "strata"),
                                 splitStrata = TRUE,
                                 cohortName = TRUE,
                                 cdmName = TRUE,
                                 conceptSet = TRUE,
                                 ingredient = TRUE,
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
  result <- result |>
    omopgenerics::newSummarisedResult() |>
    visOmopResults::filterSettings(.data$result_type == "summarise_drug_utilisation")
  if (nrow(result) == 0) {
    cli::cli_abort("There are no results with `result_type = summarise_drug_utilisation`")
  }
  if (!cohortName & "cohort_name" %in% unlist(groupColumn)) {
    cli::cli_abort("If `cohortName = FALSE`, `cohort_name` cannot be used in `groupColumn`.")
  }
  if (!conceptSet & "concept_set" %in% unlist(groupColumn)) {
    cli::cli_abort("If `conceptSet = FALSE`, `concept_set` cannot be used in `groupColumn`.")
  }
  if (!ingredient & "ingredient" %in% unlist(groupColumn)) {
    cli::cli_abort("If `ingredient = FALSE`, `ingredient` cannot be used in `groupColumn`.")
  }
  if (!cdmName & "cdm_name" %in% unlist(groupColumn)) {
    cli::cli_abort("If `cdmName = FALSE`, `cdm_name` cannot be used in `groupColumn`.")
  }
  if (!is.null(header)) {
    if (any(!header %in% c("cdm_name", "group", "strata", "variable", "estimate"))) {
      cli::cli_abort("`header` should be a character vector restricted to the following values: `cdm_name`, `group`, `strata`, `variable`, `estimate`")
    }
    if (!is.null(groupColumn)) {
      if (any(grepl(paste0(header, collapse = "|"), unlist(groupColumn)))) {
        cli::cli_abort("Columns to use as header cannot be in `groupColumn`.")
      }
    }
  }
  if (!is.null(groupColumn)) {
    options <- c("cdm_name", "cohort_name", "concept_set", "ingredient", "variable_name")
    if (splitStrata) {
      options <- c(options, visOmopResults::strataColumns(result))
    } else {
      options <- c(options, "strata_name", "strata_level")
    }
    if (any(!unlist(groupColumn) %in% options)) {
      cli::cli_abort("`groupColumn` can only be one of: {paste0(options, collapse = ', ')}")
    }
  }

  checkmate::assertLogical(cohortName, any.missing = FALSE)
  checkmate::assertLogical(conceptSet, any.missing = FALSE)
  checkmate::assertLogical(ingredient, any.missing = FALSE)
  checkmate::assertLogical(cdmName, any.missing = FALSE)
  checkmate::assertLogical(splitStrata, any.missing = FALSE)
  checkmate::assertCharacter(header, any.missing = FALSE, null.ok = TRUE)

  # .options
  .options <- defaultTableOptionsInternal(.options)

  # Exclude columns, rename, and split
  excludeColumns <- c("result_id", "estimate_type")
  if (!cohortName) {
    if ("group" %in% header) {
      cli::cli_warn(c("!" = "Dropping group from header as `cohortName = FALSE`."))
      header <- header[!"group" %in% header]
    }
    excludeColumns <- c(excludeColumns, "group_name", "group_level")
    split <- character()
  } else {
    split <- c("group")
  }

  if (!cdmName) {
    if ("cdm_name" %in% header) {
      cli::cli_warn(c("!" = "Dropping cdm_name from header as `cdmName = FALSE`."))
      header <- header[!header %in% "cdm_name"]
    }
    excludeColumns <- c(excludeColumns, "cdm_name")
    renameColumns <- character()
  } else {
    renameColumns <- c("Database name" = "cdm_name")
  }
  if (!"variable_name" %in% unlist(groupColumn)) {
    renameColumns <- c(renameColumns, "Variable" = "variable_name")
  }
  if (!all(is.na(result$variable_level))) {
    renameColumns <- c(renameColumns, "Unit" = "variable_level")
  } else if (all(is.na(result$variable_level)) & !"variable" %in% header) {
    excludeColumns <- c(excludeColumns, "variable_level")
  }
  if (!ingredient & !conceptSet) {
    splitAd <- result |>
      visOmopResults::splitAdditional()
    concepts <- unique(splitAd$concept_set)
    concepts <- concepts[!concepts %in% "overall"]
    if (length(concepts) > 1) {
      cli::cli_abort("conceptSet cannot be FALSE if there is more than one.")
    }
    excludeColumns <- c(excludeColumns, "additional_name", "additional_level")
  } else {
    split <- c(split, "additional")
    if (!ingredient & conceptSet) {
      result <- result |>
        visOmopResults::splitAdditional()
      ingr <- unique(result$ingredient)
      ingr <- ingr[!ingr %in% "overall"]
      if (length(ingr) > 2) {
        cli::cli_abort("ingredient cannot be FALSE if there is more than one in the results.")
      }
      result <- result |>
        dplyr::select(!"ingredient") |>
        visOmopResults::uniteAdditional(cols = "concept_set")
    }
    if (ingredient & !conceptSet) {
      result <- result |>
        visOmopResults::splitAdditional()
      concepts <- unique(result$concept_set)
      concepts <- concepts[!concepts %in% "overall"]
      if (length(concepts) > 1) {
        cli::cli_abort("conceptSet cannot be FALSE if there is more than one in the results.")
      }
      if ("ingredient" %in% colnames(result)) {
        result <- result |>
          dplyr::select(!"concept_set") |>
          visOmopResults::uniteAdditional(cols = "ingredient")
      } else {
        result <- result |>
          dplyr::select(!"concept_set") |>
          visOmopResults::uniteAdditional()
      }
    }
  }

  # split
  if (splitStrata) {
    split <- c(split, "strata")
  }

  # table
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
}


#' Format a summarised_treatment result into a visual table.
#'
#' `r lifecycle::badge("experimental")`
#'
#' @param result A summarised_result object with results from
#' summariseTreatmentFromCohort() or summariseTreatmentFromConceptSet().
#' @param header A vector containing which elements should go into the header
#' in order. Allowed values: `cdm_name`, `cohort_name`, `strata`, `variable`,
#' `estimate` and `window_name`.
#' @param splitStrata If TRUE strata columns will be split.
#' @param cdmName If TRUE database names will be displayed.
#' @param groupColumn Column to use as group labels. Allowed values: `cdm_name`,
#' `cohort_name`, `strata`, `variable`, `estimate` and `window_name`.
#' @param type Type of desired formatted table, possibilities: "gt",
#' "flextable", "tibble".
#' @param formatEstimateName Named list of estimate name's to join, sorted by
#' computation order. Indicate estimate_name's between <...>.
#' @param .options Named list with additional formatting options.
#' DrugUtilisation::defaultTableOptions() shows allowed arguments and their
#' default values.
#'
#' @examples
#' \donttest{
#' library(DrugUtilisation)
#'
#' cdm <- mockDrugUtilisation()
#' result <- cdm$cohort1 |>
#'   summariseTreatment(
#'     treatmentCohortName = "cohort2",
#'     window = list(c(0, 30), c(31, 365))
#'   )
#'
#' tableTreatment(result)
#' }
#'
#' @return A table with a formatted version of summariseTreatment() results.
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
                             "N (%)" = "<count> (<percentage> %)"
                           ),
                           .options = list()) {
  opts <- c(
    "cdm_name", "cohort_name", "strata", "variable_name", "estimate", "window_name"
  )
  assertChoice(header, choices = opts, null = TRUE, unique = TRUE)
  assertChoice(unlist(groupColumn), choices = opts, null = TRUE, unique = TRUE)
  assertChoice(type, choices = c("gt", "flextable", "tibble"), length = 1)
  assertClass(result, class = "summarised_result")
  assertLogical(splitStrata, length = 1)
  assertLogical(cdmName, length = 1)

  commonArguments <- intersect(header, unlist(groupColumn))
  if (length(commonArguments) > 0) {
    cli::cli_abort("header and groupColumn must be mutually exclusive: {commonArguments} {?is/are} present in both.")
  }

  result <- result |>
    omopgenerics::newSummarisedResult() |>
    visOmopResults::filterSettings(.data$result_type == "summarise_treatment")
  if (nrow(result) == 0) {
    cli::cli_abort("There are no results with `result_type = summarise_treatment`")
  }

  # .options
  .options <- defaultTableOptionsInternal(.options)

  # Split
  split <- c("group", "additional")

  # Exclude columns, rename, and split
  excludeColumns <- c("result_id", "estimate_type", "variable_level")

  if (!cdmName) {
    if ("cdm_name" %in% header) {
      cli::cli_warn(c("!" = "Dropping cdm_name from header as `cdmName = FALSE`."))
      header <- header[!header %in% "cdm_name"]
    }
    excludeColumns <- c(excludeColumns, "cdm_name")
    renameColumns <- character()
  } else {
    renameColumns <- c("Database name" = "cdm_name")
  }
  if (!"variable_name" %in% unlist(groupColumn)) {
    renameColumns <- c(renameColumns, "Treatment" = "variable_name")
  }

  # split
  if (splitStrata) {
    split <- c(split, "strata")
  }

  header[header == "window_name"] <- "additional"
  header[header == "cohort_name"] <- "group"
  header[header == "variable_name"] <- "variable"

  # table
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
}

#' Format a drug_restart object into a visual table.
#'
#' `r lifecycle::badge("experimental")`
#'
#' @param result A summarised_result object with results from
#' summariseDrugRestart().
#' @param header A vector containing which elements should go into the header
#' in order. Allowed values: `cdm_name`, `cohort_name`, `strata`, `variable`,
#' `estimate`.
#' @param splitStrata If TRUE strata columns will be split.
#' @param cohortName If TRUE cohort names will be displayed.
#' @param cdmName If TRUE database names will be displayed.
#' @param groupColumn Column to use as group labels. Allowed values: `cdm_name`,
#' `cohort_name`, `strata`, `variable_name`, `variable_level`, `estimate_name`.
#' @param type Type of desired formatted table, possibilities: "gt",
#' "flextable", "tibble".
#' @param formatEstimateName Named list of estimate name's to join, sorted by
#' computation order. Indicate estimate_name's between <...>.
#' @param .options Named list with additional formatting options.
#' DrugUtilisation::defaultTableOptions() shows allowed arguments and their
#' default values.
#'
#' @examples
#' \donttest{
#' library(DrugUtilisation)
#'
#' cdm <- mockDrugUtilisation()
#'
#' conceptlist <- list("a" = 1125360, "b" = c(1503297, 1503327))
#' cdm <- generateDrugUtilisationCohortSet(
#'   cdm = cdm,
#'   name = "switch_cohort",
#'   conceptSet = conceptlist
#' )
#'
#' result <- cdm$cohort1 |>
#'   summariseDrugRestart(switchCohortTable = "switch_cohort")
#'
#' tableDrugRestart(result)
#'
#' CDMConnector::cdmDisconnect(cdm = cdm)
#' }
#'
#' @return A table with a formatted version of summariseDrugRestart() results.
#'
#' @export
#'
tableDrugRestart <- function(result,
                             header = c("strata"),
                             splitStrata = TRUE,
                             cohortName = TRUE,
                             cdmName = TRUE,
                             groupColumn = c("cdm_name", "cohort_name"),
                             type = "gt",
                             formatEstimateName = c(
                               "N (%)" = "<count> (<percentage> %)"
                             ),
                             .options = list()) {
  # checks
  assertChoice(header,
    choices = c("cdm_name", "cohort_name", "strata", "variable", "estimate"),
    null = TRUE, unique = TRUE
  )
  assertChoice(unlist(groupColumn),
    choices = c("cdm_name", "cohort_name", "strata", "variable_name", "variable_level", "estimate_name"),
    null = TRUE, unique = TRUE
  )
  assertChoice(type, choices = c("gt", "flextable", "tibble"), length = 1)
  assertClass(result, class = "summarised_result")
  assertLogical(splitStrata, length = 1)
  assertLogical(cdmName, length = 1)

  commonArguments <- intersect(header, unlist(groupColumn))
  if (length(commonArguments) > 0) {
    cli::cli_abort("header and groupColumn must be mutually exclusive: {commonArguments} {?is/are} present in both.")
  }

  result <- result |>
    omopgenerics::newSummarisedResult() |>
    visOmopResults::filterSettings(.data$result_type == "summarise_drug_restart")
  if (nrow(result) == 0) {
    cli::cli_abort("There are no results with `result_type = summarise_drug_restart`")
  }

  # .options
  .options <- defaultTableOptionsInternal(.options)

  # Split
  split <- c("additional")

  # Exclude columns, rename, and split
  excludeColumns <- c("result_id", "estimate_type")

  if (!cdmName) {
    if ("cdm_name" %in% header) {
      cli::cli_warn(c("!" = "Dropping cdm_name from header as `cdmName = FALSE`."))
      header <- header[!header %in% "cdm_name"]
    }
    excludeColumns <- c(excludeColumns, "cdm_name")
    renameColumns <- character()
  } else {
    renameColumns <- c("Database name" = "cdm_name")
  }
  if (!cohortName) {
    if ("cohort_name" %in% header) {
      cli::cli_warn(c("!" = "Dropping cohort_name from header as `cohortName = FALSE`."))
      header <- header[!header %in% "cohort_name"]
    }
    excludeColumns <- c(excludeColumns, "group_name", "group_level")
  } else {
    split <- c(split, "group")
  }
  if (!"variable" %in% groupColumn) {
    renameColumns <- c(renameColumns, "Follow-up" = "variable_name", "Event" = "variable_level")
  }

  # split
  if (splitStrata) {
    split <- c(split, "strata")
  }

  header[header == "cohort_name"] <- "group"

  # table
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
}



#' Create a table with proportion of patients covered results
#'
#' `r lifecycle::badge("experimental")`
#'
#' @param result A summarised_result object with results from
#' summariseProportionOfPatientsCovered().
#' @param times Days to include in the table. If NULL all days will be
#' included.
#' @param header A vector containing which elements should go into the header
#' in order. Allowed are: `cdm_name`, `group`, `strata`, `variable`.
#' @param splitStrata If TRUE strata columns will be split.
#' @param cohortName If TRUE cohort names will be displayed.
#' @param cdmName If TRUE database names will be displayed.
#' @param groupColumn Column to use as group labels.
#' @param type Type of desired formatted table, possibilities: "gt",
#' "flextable", "tibble".
#' @param .options Named list with additional formatting options.
#' DrugUtilisation::defaultTableOptions() shows allowed
#' arguments and their default values.
#'
#' @return A table with a formatted version of summariseProportionOfPatientsCovered() results.
#' @export
#'
tableProportionOfPatientsCovered <- function(result,
                                             times = NULL,
                                             header = c("group", "strata"),
                                             splitStrata = TRUE,
                                             cohortName = TRUE,
                                             cdmName = TRUE,
                                             groupColumn = "variable_name",
                                             type = "gt",
                                             .options = list()) {
  # check input and filter result
  if (!cohortName & "cohort_name" %in% unlist(groupColumn)) {
    cli::cli_abort("If `cohortName = FALSE`, `cohort_name` cannot be used in `groupColumn`.")
  }
  if (!cdmName & "cdm_name" %in% unlist(groupColumn)) {
    cli::cli_abort("If `cdmName = FALSE`, `cdm_name` cannot be used in `groupColumn`.")
  }
  if (!is.null(header)) {
    if (any(!header %in% c("cdm_name", "group", "strata", "variable"))) {
      cli::cli_abort("`header` should be a character vector restricted to the following values: `cdm_name`, `group`, `strata`, `variable`")
    }
    if (!is.null(groupColumn)) {
      if (any(grepl(paste0(header, collapse = "|"), unlist(groupColumn)))) {
        cli::cli_abort("Columns to use as header cannot be in `groupColumn`.")
      }
    }
  }
  result <- result |>
    omopgenerics::newSummarisedResult() |>
    visOmopResults::filterSettings(.data$result_type == "summarise_proportion_of_patients_covered") |>
    dplyr::filter(!grepl("number", .data$variable_name))
  if (nrow(result) == 0) {
    cli::cli_abort("There are no results with `result_type = summarise_proportion_of_patients_covered`")
  }
  checkmate::assertLogical(cohortName, any.missing = FALSE)
  checkmate::assertLogical(cdmName, any.missing = FALSE)
  checkmate::assertLogical(splitStrata, any.missing = FALSE)
  checkmate::assertCharacter(header, any.missing = FALSE, null.ok = TRUE)

  # .options
  .options <- defaultTableOptionsInternal(.options)

  # Split
  split <- c("additional")

  if (!is.null(times)) {
    # filter to specified times
    result <- result |>
      dplyr::mutate(time = as.numeric(.data$additional_level)) |>
      dplyr::filter(.data$time %in% .env$times) |>
      dplyr::select(!"time")
  }

  # Exclude columns, rename, and split
  excludeColumns <- c("result_id", "estimate_type", "estimate_name")
  if (!cohortName) {
    if ("group" %in% header) {
      cli::cli_warn(c("!" = "Dropping group from header as `cohortName = FALSE`."))
      header <- header[!header %in% "group"]
    }
    excludeColumns <- c(excludeColumns, "group_name", "group_level")
  } else {
    split <- c(split, "group")
  }

  if (!cdmName) {
    if ("cdm_name" %in% header) {
      cli::cli_warn(c("!" = "Dropping cdm_name from header as `cdmName = FALSE`."))
      header <- header[!header %in% "cdm_name"]
    }
    excludeColumns <- c(excludeColumns, "cdm_name")
    renameColumns <- character()
  } else {
    renameColumns <- c("Database name" = "cdm_name")
  }


  # split
  if (splitStrata) {
    split <- c(split, "strata")
  }

  result <- result |>
    dplyr::filter(.data$estimate_name == "ppc") |>
    dplyr::mutate(additional_name = "Days since first drug start")

  # table
  visOmopResults::visOmopTable(
    result = result,
    formatEstimateName = c("PPC" = "<ppc>%"),
    header = c(header),
    renameColumns = renameColumns,
    split = split,
    groupColumn = c(groupColumn)
  )
}




defaultTableOptionsInternal <- function(.options = NULL) {
  defaults <- visOmopResults::optionsVisOmopTable()

  for (opt in names(.options)) {
    defaults[[opt]] <- .options[[opt]]
  }

  return(defaults)
}

#' Additional arguments for the table functions.
#'
#' @description
#' It provides a list of allowed inputs for .option argument in table functions,
#' and their given default values.
#'
#'
#' @return The default .options named list.
#'
#' @export
#'
#' @examples
#' {
#'   defaultTableOptions()
#' }
#'
defaultTableOptions <- function() {
  defaultTableOptionsInternal(NULL)
}

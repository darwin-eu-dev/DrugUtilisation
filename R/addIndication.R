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

#' Add a variable indicating individuals indications
#'
#' @description
#' Add a variable to a drug cohort indicating their presence in an indication
#' cohort in a specified time window. If an individual is not in one of the
#' indication cohorts, they will be considered to have an unknown indication if
#' they are present in one of the specified OMOP CDM clinical tables. If they
#' are neither in an indication cohort or a clinical table they will be
#' considered as having no observed indication.
#'
#' @param cohort A cohort table in the cdm.
#' @param indicationCohortName Name of indication cohort table
#' @param indicationCohortId target cohort Id to add indication
#' @param indicationWindow time window of interests
#' @param unknownIndicationTable Tables to search unknown indications
#' @param indexDate Date respect to indication will be calculated.
#' @param censorDate After that day no indication will be considered.
#' @param name name of permanant table
#'
#' @return The original table with a variable added that summarises the
#' individual´s indications.
#'
#' @export
#'
#' @examples
#' \donttest{
#' library(DrugUtilisation)
#' library(CDMConnector)
#' library(dplyr)
#'
#' cdm <- mockDrugUtilisation()
#'
#' indications <- list("headache" = 378253, "asthma" = 317009)
#' cdm <- generateConceptCohortSet(
#'   cdm = cdm, conceptSet = indications, name = "indication_cohorts"
#' )
#'
#' cdm <- generateIngredientCohortSet(
#'   cdm = cdm, name = "drug_cohort",
#'   ingredient = "acetaminophen"
#' )
#'
#' cdm$drug_cohort |>
#'   addIndication("indication_cohorts", indicationWindow = list(c(0, 0))) |>
#'   glimpse()
#' }
#'
addIndication <- function(cohort,
                          indicationCohortName,
                          indicationCohortId = NULL,
                          indicationWindow = list(c(0, 0)),
                          unknownIndicationTable = NULL,
                          indexDate = "cohort_start_date",
                          censorDate = NULL,
                          name = NULL) {
  cdm <- omopgenerics::cdmReference(cohort)

  comp <- newTable(name)

  # check inputs
  checkInputs(
    x = cohort, cdm = cdm, indicationCohortName = indicationCohortName,
    indicationDate = indexDate, window = indicationWindow,
    unknownIndicationTable = unknownIndicationTable, name = name
  )

  if (!(indicationCohortName %in% names(cdm))) {
    cli::cli_abort("indicationCohortName is not in the cdm reference")
  }

  if (!all(unknownIndicationTable %in% names(cdm))) {
    cli::cli_abort("unknownIndicationTable is not in the cdm reference")
  }

  assertNumeric(indicationCohortId, null = TRUE)

  # indicationWindow as list
  if (!inherits(indicationWindow, "list")) {
    indicationWindow <- list(indicationWindow)
  }

  tmpName <- omopgenerics::uniqueTableName()

  windowNames <- getWindowNames(indicationWindow) |> unlist()
  names(indicationWindow) <- paste0("win", seq_along(indicationWindow))

  # select to interest individuals
  ind <- cohort |>
    dplyr::select(dplyr::all_of(c("subject_id", censorDate, indexDate))) |>
    dplyr::distinct() |>
    PatientProfiles::addCohortIntersectFlag(
      targetCohortTable = indicationCohortName,
      indexDate = indexDate,
      censorDate = censorDate,
      targetStartDate = "cohort_start_date",
      targetEndDate = NULL,
      window = indicationWindow,
      targetCohortId = indicationCohortId,
      nameStyle = "i_{window_name}_{cohort_name}",
      name = tmpName
    ) |>
    addUnknownIndication(
      indexDate = indexDate, censorDate = censorDate,
      window = indicationWindow, table = unknownIndicationTable,
      name = tmpName
    ) |>
    dplyr::select(-dplyr::any_of(censorDate)) |>
    collapseIndication(
      window = indicationWindow,
      name = tmpName,
      unknown = length(unknownIndicationTable) > 0
    ) |>
    renameWindows(windowNames)

  newCols <- colnames(ind)
  newCols <- newCols[!newCols %in% c("subject_id", indexDate)]
  toDrop <- intersect(newCols, colnames(cohort))
  if (length(toDrop) > 0) {
    cli::cli_warn("Overwriting existing variables: {toDrop}")
    cohort <- cohort |>
      dplyr::select(!dplyr::all_of(toDrop))
  }

  # add the indication columns to the original table
  result <- cohort |>
    dplyr::left_join(ind, by = c("subject_id", indexDate)) |>
    dplyr::compute(name = comp$name, temporary = comp$temporary)

  omopgenerics::dropTable(cdm = cdm, name = tmpName)

  return(result)
}

addUnknownIndication <- function(x, indexDate, censorDate, window, table, name) {
  if (length(table) == 0) {
    return(x)
  }

  cdm <- omopgenerics::cdmReference(x)
  q <- paste0("dplyr::if_all(dplyr::starts_with('", names(window), "'), ~ . == 0)", collapse = " | ") |>
    rlang::parse_exprs()

  tablePrefix <- omopgenerics::tmpPrefix()

  xx <- x |>
    dplyr::filter(!!!q) |>
    dplyr::select(dplyr::any_of(c("subject_id", indexDate, censorDate))) |>
    dplyr::compute(
      name = omopgenerics::uniqueTableName(tablePrefix), temporary = FALSE
    )

  for (tab in table) {
    xx <- xx |>
      PatientProfiles::addTableIntersectFlag(
        indexDate = indexDate,
        censorDate = censorDate,
        tableName = tab,
        targetEndDate = NULL,
        window = window,
        nameStyle = "unknown_{window_name}_{table_name}",
        name = omopgenerics::uniqueTableName(tablePrefix)
      )
  }

  qq <- paste0("dplyr::if_else(dplyr::if_any(dplyr::starts_with('unknown_", names(window), "')), 1L, 0L)") |>
    rlang::parse_exprs() |>
    rlang::set_names(paste0("i_", names(window), "_unknown"))

  x <- x |>
    dplyr::left_join(
      xx |>
        dplyr::mutate(!!!qq) |>
        dplyr::select(!dplyr::starts_with("unknown_")),
      by = c("subject_id", indexDate)
    ) |>
    dplyr::compute(name = name, temporary = FALSE)

  omopgenerics::dropTable(
    cdm = cdm, name = dplyr::starts_with(tablePrefix)
  )

  return(x)
}
collapseIndication <- function(x, window, name, unknown) {
  indications <- colnames(x)
  indications <- indications[startsWith(indications, "i_win1_")]
  indications <- substr(indications, 8, nchar(indications))
  indications <- indications[indications != "unknown"]
  indications <- sort(indications)

  combs <- rep(list(c(1, 0)), length(indications))
  names(combs) <- paste0("i_x1x_", indications)
  combs <- tidyr::expand_grid(!!!combs)
  xx <- character()
  for (k in seq_len(nrow(combs) - 1)) {
    cols <- combs[k, ] |>
      as.list() |>
      unlist()
    nms <- names(cols)[cols == 1]
    vals <- paste0(".data[['", nms, "']] == 1", collapse = " & ")
    nms <- substr(nms, 7, nchar(nms)) |>
      tolower() |>
      paste0(collapse = " and ")
    val <- paste0(vals, " ~ '", nms, "'")
    xx <- c(xx, val)
  }
  if (unknown) xx <- c(xx, ".data[['i_x1x_unknown']] == 1 ~ 'unknown'")
  xx <- paste0(xx, collapse = ", ")

  q <- character()
  nms <- character()
  for (win in seq_along(window)) {
    q <- c(q, paste0(
      "dplyr::case_when(",
      gsub("_x1x_", paste0("_win", win, "_"), xx),
      ", .default = 'none')"
    ))
    nms <- c(nms, paste0("indication_win", win))
  }
  q <- q |>
    rlang::parse_exprs() |>
    rlang::set_names(nms)

  x <- x |>
    dplyr::mutate(!!!q) |>
    dplyr::select(!dplyr::starts_with("i_")) |>
    dplyr::compute(name = name, temporary = FALSE)

  return(x)
}
renameWindows <- function(x, windowNames) {
  cols <- paste0("indication_win", seq_along(windowNames))
  names(cols) <- paste0("indication_", windowNames)
  x <- x |>
    dplyr::rename(!!cols)
}
getWindowNames <- function(window) {
  getname <- function(element) {
    element <- tolower(as.character(element))
    element <- gsub("-", "m", element)
    invisible(paste0(element[1], "_to_", element[2]))
  }
  windowNames <- names(window)
  if (is.null(windowNames)) {
    windowNames <- lapply(window, getname)
  } else {
    id <- windowNames == ""
    windowNames[id] <- lapply(window[id], getname)
  }
  invisible(windowNames)
}
newTable <- function(name, call = parent.frame()) {
  assertCharacter(name, length = 1, null = TRUE, na = TRUE, call = call)
  if (is.null(name) || is.na(name)) {
    x <- list(name = omopgenerics::uniqueTableName(), temporary = TRUE)
  } else {
    x <- list(name = name, temporary = FALSE)
  }
  return(x)
}

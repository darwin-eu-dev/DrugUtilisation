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

#' Get indication for a target cohort
#'
#' @param x Table in the cdm
#' @param indicationCohortName Name of indication cohort table
#' @param indicationCohortId target cohort Id to add indication
#' @param indicationWindow time window of interests
#' @param unknownIndicationTable Tables to search unknown indications
#' @param indexDate Date of the indication
#' @param name name of permenant table
#'
#' @return Same cohort adding the indications
#'
#' @export
#'
#' @examples
#' \donttest{
#' library(DrugUtilisation)
#' library(CodelistGenerator)
#' library(CDMConnector)
#'
#' cdm <- mockDrugUtilisation()
#'
#' indications <- list("headache" = 378253, "asthma" = 317009)
#' cdm <- generateConceptCohortSet(
#'   cdm, indications, "indication_cohorts"
#' )
#'
#' acetaminophen <- getDrugIngredientCodes(cdm, "acetaminophen")
#' cdm <- generateDrugUtilisationCohortSet(cdm, "drug_cohort", acetaminophen)
#'
#' cdm[["drug_cohort"]] |>
#'   addIndication("indication_cohorts", indicationWindow = list(c(0,0)))
#' }
#'
addIndication <- function(x,
                          indicationCohortName,
                          indicationCohortId = NULL,
                          indicationWindow = list(c(0,0)),
                          unknownIndicationTable = NULL,
                          indexDate = "cohort_start_date",
                          name = NULL) {

  cdm <- omopgenerics::cdmReference(x)

  comp <- newTable(name)

  # check inputs
  checkInputs(
    x = x, cdm = cdm, indicationCohortName = indicationCohortName, indicationDate = indexDate,
    window = indicationWindow,unknownIndicationTable = unknownIndicationTable, name = name
  )

  if (!(indicationCohortName %in% names(cdm))) {
    cli::cli_abort("indicationCohortName is not in the cdm reference")
  }

  if (!all(unknownIndicationTable %in% names(cdm))) {
    cli::cli_abort("unknownIndicationTable is not in the cdm reference")
  }

  assertNumeric(indicationCohortId,null = TRUE)

  # indicationWindow as list
  if (!inherits(indicationWindow,"list")){
    indicationWindow = list(indicationWindow)
  }

  tablePrefix <- omopgenerics::tmpPrefix()

  # nameStyle
  nameformat = "indication_{window_name}_{cohort_name}"

  # select to interest individuals
  ind <- x |>
    dplyr::select(
      "subject_id", "cohort_start_date" = dplyr::all_of(indexDate)
    ) |>
    dplyr::distinct()

  # add indications that are cohorts
  ind <- addCohortIndication(ind, indicationCohortName, indicationWindow,nameformat,unknownIndicationTable,indicationCohortId)

  # add the indication columns to the original table
  result <- x |>
    dplyr::left_join(
      ind |> dplyr::rename(!!indexDate := "cohort_start_date"),
      by = c("subject_id", indexDate)
    ) |> indicationToStrata() |>
    dplyr::compute(
      name = omopgenerics::uniqueTableName(tablePrefix),
      temporary = FALSE,
      overwrite = TRUE
    )


  result <- result |> dplyr::compute(name = comp$name, temporary = comp$temporary)

  omopgenerics::dropTable(
    cdm = cdm, name = dplyr::starts_with(tablePrefix)
  )

  return(result)
}

#' get cohort names
#' @noRd
getCohortName <- function (name) {
  name <- substr(name, 1, utils::tail(unlist(gregexpr('_', name)), n = 3) - 1)
}

#' get indication name from gap and termination
#' @noRd
indicationName <- function(window, termination = "") {
  paste0("indication_", tolower(as.character(window[1])),"_to_",
         tolower(as.character(window[2])), termination)
}

#' Add cohort indications
#' @noRd
addCohortIndication <- function(ind, cohortName, window, name, unknown, id) {
  for (w in seq_len(length(window))) {

    win <- window[[w]]

    ind <- ind |> PatientProfiles::addCohortIntersectFlag(
      cohortName,
      targetEndDate = NULL,
      window = win,
      targetCohortId = id,
      nameStyle = name
    ) |>
      addNoneIndication(win) |>
      dplyr::compute(
        name = omopgenerics::uniqueTableName(tablePrefix),
        temporary = FALSE,
        overwrite = TRUE
      )

    if(!is.null(unknown)){
      ind <- ind |> addUnknownIndication(window = win,table = unknown)
    }

  }
  return(ind)
}


#' Add unknown indications
#' @noRd
addUnknownIndication <- function(x, window, table) {

  for (tab in table){

  columns <- colnames(x)
  windowName <- gsub("-","m",window)
  columnsNone <- columns[grepl(paste0(indicationName(windowName),"_none"), columns)]


  x <-
    x |> PatientProfiles::addTableIntersectFlag(tab, window = window)
  name <- utils::tail(colnames(x), n = 1)

  x <-
    x |> dplyr::mutate(!!rlang::sym(name) := ifelse(.data[[columnsNone]] == 0, 0, .data[[name]]))

  }

  name <- utils::tail(colnames(x), n = length(table))
  unknown <- indicationName(windowName,"_unknown")
  columns[grepl(".*\\s.*", name)] <- paste0("`", columns[grepl(".*\\s.*", name)],"`")
  columns <- paste0(".data$", name, collapse = " + ")
  columns <- paste0("dplyr::if_else(", columns, " > 0, 1, 0)")

  x <- x |>
    dplyr::mutate(!!rlang::sym(unknown) := !!rlang::parse_expr(columns)) |>
    dplyr::mutate(!!rlang::sym(columnsNone) := ifelse(.data[[unknown]] == 1,0, .data[[columnsNone]])) |>
    dplyr::select(-name) |>
    dplyr::compute(
      name = omopgenerics::uniqueTableName(tablePrefix),
      temporary = FALSE,
      overwrite = TRUE
    )

  return(x)

}


#' Sum columns
#' @noRd
addNoneIndication <- function(x, window) {
  window <- gsub("-","m",window)
  columns <- colnames(x)
  columns <- columns[grepl(indicationName(window), columns)]
  columns[grepl(".*\\s.*", columns)] <- paste0("`", columns[grepl(".*\\s.*", columns)],"`")
  columns <- paste0(".data$", columns, collapse = " + ")
  columns <- paste0("dplyr::if_else(", columns, " > 0, 0, 1)")
  x |>
    dplyr::mutate(!!indicationName(window, "_none") := !!rlang::parse_expr(columns))
}

#' Create new variables summarising the data of indication that can be used as
#' stratification columns
#'
#' @noRd
indicationToStrata <- function(cohort,
                               indicationVariables = indicationColumns2(cohort),
                               keep = FALSE){


  # original cohort to keep attributes
  originalCohort <- cohort

  # organize indications
  indications <- groupIndications(indicationVariables)

  # combine each gap
  for (k in seq_along(indications)) {
    cohort <- cohort |>
      addBinaryFromCategorical(
        binaryColumns = indications[[k]]$names,
        newColumn = indications[[k]]$new_name, label = indications[[k]]$label
      )
  }

  # keep variables if asked to
  if (!keep) {
    cohort <- cohort |>
      dplyr::select(-dplyr::all_of(indicationVariables))
  }

  return(cohort)
}

#' @noRd
groupIndications <- function(indicationVariables) {
  window <- gsub("indication_", "", indicationVariables) |>
    lapply(\(x) stringr::str_split(x, "_([^_]*)$", simplify = TRUE)[,1]) |>
    lapply(\(x) x[1]) |>
    unlist() |>
    unique()
  indications <- list()
  for (k in seq_along(window)) {
    indications[[k]] <- list()
    name <- paste0("indication_", window[k])
    indications[[k]]$names <- indicationVariables[
      grep(name, indicationVariables)
    ]
    indications[[k]]$new_name <- name
    lab <- gsub(
      paste0(name, "_"), "", indications[[k]]$names
    )
    indications[[k]]$label <- paste0(
      toupper(substr(lab, 1, 1)), substr(lab, 2, nchar(lab))
    ) |>
      gsub(pattern = "_", replacement = " ")
  }
  return(indications)
}

#' Get indication for a target cohort
#'
#' @param x Table in the cdm
#' @param binaryColumns Binary columns to unite
#' @param newColumn New column to be created from the binaryColumns
#' @param label Label of each binary column
#'
#' @return Table x with a new column summarising the data from binaryColumns
#'
#' @noRd
#'
addBinaryFromCategorical <- function(x, binaryColumns, newColumn, label = binaryColumns) {
  # initial checks
  checkInputs(
    x = x, binaryColumns = binaryColumns, newColumn = newColumn, label = label
  )

  toAdd <- x |>
    dplyr::select(dplyr::all_of(binaryColumns)) |>
    dplyr::distinct() |>
    dplyr::mutate(!!newColumn := "")

  for (k in seq_along(binaryColumns)) {
    toAdd <- toAdd |>
      dplyr::mutate(!!newColumn := dplyr::if_else(
        .data[[binaryColumns[k]]] == 1,
        dplyr::if_else(
          .data[[newColumn]] == "", !!label[k],
          paste0(.data[[newColumn]], " and ", !!label[k])
        ),
        .data[[newColumn]]
      ))
  }

  # mantain the number of columns
  x <- x |>
    dplyr::left_join(toAdd, by = binaryColumns) |>
    dplyr::compute(
      name = omopgenerics::uniqueTableName(tablePrefix),
      temporary = FALSE,
      overwrite = TRUE
    )

  return(x)
}

#' Obtain automatically the indication columns
#'
#' @param x Tibble
#'
#' @return Name of the indication columns
#'
#' @noRd
#'
indicationColumns2 <- function(x) {
  names <- colnames(x)[substr(colnames(x), 1, 11) == "indication_"]
  return(names)
}


#' @noRd
#'
newTable <- function(name, call = parent.frame()) {
  assertCharacter(name, length = 1, null = TRUE, na = TRUE, call = call)
  if (is.null(name) || is.na(name)) {
    x <- list(name = omopgenerics::uniqueTableName(), temporary = TRUE)
  } else {
    x <- list(name = name, temporary = FALSE)
  }
  return(x)
}

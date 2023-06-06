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
#' @param cdm A cdm reference created using CDMConnector
#' @param indicationCohortName Name of indication cohort table
#' @param indicationGap Gap between the event and the indication
#' @param unknownIndicationTable Tables to search unknown indications
#' @param indicationDate Date of the indication
#'
#' @return
#' @export
#'
#' @examples
#' \donttest{
#' library(DrugUtilisation)
#' library(dplyr)
#'
#' cdm <- mockDrugUtilisation()
#' cdm$cohort1 %>%
#'   addIndication(
#'     cdm = cdm, indicationCohortName = "cohort2", indicationGap = c(0, 30)
#'   ) %>%
#'   collect()
#' }
#'
addIndication <- function(x,
                          cdm,
                          indicationCohortName,
                          indicationGap = 0,
                          unknownIndicationTable = c("condition_occurrence", "observation"),
                          indicationDate = "cohort_start_date") {
  # check inputs
  checkInputs(
    x = x, cdm = cdm, indicationCohortName = indicationCohortName,
    indicationGap = indicationGap, indicationDate = indicationDate,
    unknownIndicationTable = unknownIndicationTable
  )

  # sort indicationGap
  indicationGap <- sort(unique(indicationGap))

  # select to interest individuals
  ind <- x %>%
    dplyr::select(
      "subject_id", "cohort_start_date" = dplyr::all_of(indicationDate)
    ) %>%
    dplyr::distinct()

  # add indications that are cohorts
  ind <- addCohortIndication(ind, cdm, indicationCohortName, indicationGap)

  # add unknown indications
  ind <- addUnknownIndication(ind, cdm, unknownIndicationTable, indicationGap)

  # add the indication columns to the original table
  result <- x %>%
    dplyr::left_join(
      ind %>% dplyr::rename(!!indicationDate := "cohort_start_date"),
      by = c("subject_id", indicationDate)
    ) %>%
    computeTable(cdm)

  result <- PatientProfiles::addAttributes(result, x)

  return(result)
}

#' get cohort names
#' @noRd
getCohortName <- function (name) {
  name <- substr(name, 1, utils::tail(unlist(gregexpr('_', name)), n = 3) - 1)
}

#' get indication name from gap
#' @noRd
indicationName <- function(gap) {tolower(paste0("indication_gap_", gap))}

#' get unknown name from gap
#' @noRd
unknownName <- function(gap) {tolower(paste0("unknown_gap_", gap))}

#' Add cohort indications
#' @noRd
addCohortIndication <- function(ind, cdm, cohortName, gaps) {
  for (gap in gaps) {
    columnName <- indicationName(gap)
    xx <- PatientProfiles::addCohortIntersectFlag(
      ind, cdm, cohortName, targetEndDate = NULL, window = c(-gap, 0)
    )
    newnames <- colnames(xx)[!(colnames(xx) %in% colnames(ind))]
    xx <- dplyr::mutate(xx, !!columnName := as.character(NA))
    for (nam in newnames) {
      xx <- xx %>%
        dplyr::mutate(!!columnName := dplyr::if_else(
          .data[[nam]] == 1,
          dplyr::if_else(
            is.na(.data[[columnName]]),
            !!getCohortName(nam),
            paste0(.data[[columnName]], "&&", !!getCohortName(nam))
          ),
          .data[[columnName]]
        ))
    }
    ind <- xx %>%
      dplyr::select(dplyr::all_of(c(colnames(ind), columnName))) %>%
      computeTable(cdm)
  }
  return(ind)
}

#' Add unknown indications
#' @noRd
addUnknownIndication <- function(ind, cdm, unknownTables, gaps) {
  if (!is.null(unknownTables)) {
    individualsUnknown <- ind %>%
      dplyr::filter(is.na(
        .data[[tolower(paste0("indication_gap_", min(gaps)))]]
      )) %>%
      dplyr::select("subject_id", "cohort_start_date") %>%
      dplyr::distinct() %>%
      computeTable(cdm)
    if (individualsUnknown %>% dplyr::tally() %>% dplyr::pull() > 0) {
      for (ut in seq_along(unknownTables)) {
        unknownDate <- PatientProfiles::getStartName(unknownTables[ut])
        x <- cdm[[unknownTables[ut]]] %>%
          dplyr::select(
            "subject_id" = "person_id", "unknown_date" = !!unknownDate
          ) %>%
          dplyr::inner_join(individualsUnknown, by = "subject_id") %>%
          dplyr::filter(.data$unknown_date <= .data$cohort_start_date)
        if (ut == 1) {
          xx <- x
        } else {
          xx <- dplyr::union_all(xx, x)
        }
      }
      xx <- xx %>%
        dplyr::group_by(.data$subject_id, .data$cohort_start_date) %>%
        dplyr::summarise(
          unknown_date = max(.data$unknown_date, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        dplyr::mutate(diff_date = !!CDMConnector::datediff(
          "unknown_date", "cohort_start_date"
        ))
      for (gap in gaps) {
        if (is.infinite(gap)) {
          xx <- xx %>%
            dplyr::mutate(!!unknownName(gap) := 1)
        } else {
          xx <- xx %>%
            dplyr::mutate(!!unknownName(gap) := dplyr::if_else(
              .data$diff_date <= .env$gap, 1, 0
            ))
        }
      }
      xx <- xx %>%
        dplyr::select(-"diff_date", -"unknown_date") %>%
        computeTable(cdm)
      ind <- ind %>%
        dplyr::left_join(xx, by = c("subject_id", "cohort_start_date"))
      for (gap in gaps) {
        ind <- ind %>%
          dplyr::mutate(
            !!indicationName(gap) := dplyr::if_else(
              is.na(.data[[indicationName(gap)]]),
              dplyr::if_else(
                .data[[unknownName(gap)]] == 1,
                "unknown indication",
                "no indication"
              ),
              .data[[indicationName(gap)]]
            )
          )
      }
      ind <- ind %>%
        dplyr::select(
          "subject_id", "cohort_start_date", dplyr::starts_with("indication")
        ) %>%
        computeTable(cdm)
    }
  }
  for (gap in gaps) {
    ind <- mutateNoIndication(ind, tolower(paste0("indication_gap_", gap)))
  }
  ind <- ind %>% computeTable(cdm)
  return(ind)
}

#' add no indication
#' @noRd
mutateNoIndication <- function (x, column) {
  x %>%
    dplyr::mutate(!!column := dplyr::if_else(
      is.na(.data[[column]]), "no indication", .data[[column]]
    ))
}


#' Create new variables summarising the data of indication that can be used as
#' stratification columns
#'
#' @param cohort A cohort in the cdm
#' @param indicationVariables Indication variables that we want to join
#' @param remove Whether to remove the prior indication variables
#'
#' @result The cohort with the new variable
#'
#' @export
#'
#' @examples
#' \donttest{
#' library(DrugUtilisation)
#'
#' cdm <- mockDrugUtilisation()
#' cdm$cohort1 <- cdm$cohort1 %>%
#'   addIndication(cdm, "cohort2") %>%
#'   indicationToStrata()
#' }
#'
indicationToStrata <- function(cohort,
                               indicationVariables = indicationColumns(cohort),
                               remove = TRUE){
  # check inputs
  checkInputs(
    cohort = cohort, indicationVariables = indicationVariables, remove = remove
  )

  # organize indications
  indications <- groupIndications(indicationVariables)

  # combine each gap
  for (k in seq_len(indications)) {
    cohort <- cohort %>%
      combineBinary(
        binaryColumns = indications[[k]]$names,
        newColumn = indications[[k]]$new_name, label = indications[[k]]$labels
      )
  }

  # remove variables if needed
  if (remove) {
    cohort <- cohort %>%
      dplyr::select(-dplyr::all_of(indicationVariables))
  }

  return(cohort)
}

#' @noRd
groupIndications <- function(indicationVariables) {
  gaps <- gsub("indication_gap_", "", indicationVariables) %>%
    strsplit("_") %>%
    lapply(function(x){x[1]}) %>%
    unlist() %>%
    unique()
  indications <- list()
  for (k in seq_len(gaps)) {
    indications[[k]] <- list()
    name <- paste0("indication_gap_", gap[k])
    indications[[k]]$names <- indicationVariables[
      grep(name, indicationVariables)
    ]
    indications[[k]]$new_name <- name
    indications[[k]]$label <- gsub(
      paste0(name, "_"), indications[[k]]$names
    ) %>%
      paste0(toupper(substr(., 1, 1)), substr(., 2, nchar(.))) %>%
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
#' @noRd
#'
combineBinary <- function(x, binaryColumns, newColumn, label = binaryColumns) {
  # initial checks
  checkInputs(
    x = x, binaryColumns = binaryColumns, newColumn = newColumn, label = label
  )

  toAdd <- x %>%
    dplyr::select(dplyr::all_of(binaryColumns)) %>%
    dplyr::distinct() %>%
    dplyr::mutate(
      !!newColumn := "",
      flag = 0,
      flag_total = dplyr::select(., dplyr::all_of(binaryColumns)) %>%
        rowSums(na.rm = TRUE)
    )

  for (k in seq_len(binaryColumns)) {
    toAdd <- toAdd %>%
      dplyr::mutate(
        flag = .data$flag + .data[[binaryColumns[k]]]
      ) %>%
      dplyr::mutate(
        !!newColumn := dplyr::case_when(
          .data[[binaryColumns[k]]] == 1 & .data$flag == 0 ~
            label[k],
          .data[[binaryColumns[k]]] == 1 & .data$flag> 0  &
            .data$flag == .data$flag_total ~
            paste(.data[[newColumn]], "and", label[k]),
          .data[[binaryColumns[k]]] == 1 & .data$flag> 0  &
            .data$flag != .data$flag_total ~
            paste0(.data[[newColumn]], ", ", label[k]),
          .default = .data[[newColumn]]
        )
      )
  }

  # mantain the number of columns
  x <- x %>%
    dplyr::left_join(
      toAdd %>% dplyr::select(-"flag", -"flag_total"), by = binaryColumns
    ) %>%
    CDMConnector::computeQuery()

  return(x)
}

# x
# cohort
# table

# indicationToStrata


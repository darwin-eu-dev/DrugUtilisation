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
#'
#' @return
#' @export
#'
#' @examples
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

  # function to get names
  getCohortName <- function (name) {
    name <- substr(name, 1, tail(unlist(gregexpr('_', name)), n = 3) - 1)
  }
  indicationName <- function(gap) {tolower(paste0("indication_gap_", gap))}
  unknownName <- function(gap) {tolower(paste0("unknown_gap_", gap))}

  # select to interest individuals
  ind <- x %>%
    dplyr::select("subject_id", "cohort_start_date" = indicationDate) %>%
    dplyr::distinct()

  # add indications that are cohorts
  ind <- addCohortIndication(ind, cdm, indicationCohortName, indicationGap)

  # add unknown indications
  ind <- addUnknownIndication(ind, cdm, unknownIndicationTable, indicationGap)

  # add the indication columns to the original table
  x <- x %>%
    dplyr::left_join(
      ind %>% dplyr::rename(!!indicationDate = "cohort_start_date"),
      by = c("subject_id", indicationDate)
    ) %>%
    computeTable(cdm)

  return(x)
}

#' Add cohort indications
addCohortIndication <- function(x, cdm, cohortName, gaps) {
  for (gap in gaps) {
    columnName <- indicationName(gap)
    xx <- x %>%
      PatientProfiles::addCohortIntersectFlag(
        cdm, cohortName, window = c(-gap, 0)
      ) %>%
      dplyr::mutate(!!columnName := as.character(NA))
    newnames <- colnames(xx)[!(colnames(xx) %in% colnames(x))]
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
    x <- xx %>%
      dplyr::select(dplyr::all_of(c(colnames(x), columnName))) %>%
      computeTable(cdm)
  }
  return(x)
}

#' Add unknown indications
addUnknownIndication <- function(x, cdm, unknownTables, gaps) {
  if (!is.null(unknownTables)) {
    individualsUnknown <- x %>%
      dplyr::filter(is.na(
        .data[[tolower(paste0("indication_gap_", min(indicationGap)))]]
      )) %>%
      dplyr::select("subject_id", "cohort_start_date") %>%
      dplyr::distinct() %>%
      computeTable(cdm)
    if (individualsUnknown %>% dplyr::tally() %>% dplyr::pull() > 0) {
      xx <- NULL
      for (unknownTable in unknownTables) {
        unknownDate <- namesTable$start_date_name[
          namesTable$table_name == unknownTable
        ]
        xx <- xx %>%
          dplyr::union_all(
            cdm[[unknownTable]] %>%
              dplyr::select(
                "subject_id" = "person_id", "unknown_date" = !!unknownDate
              ) %>%
              dplyr::inner_join(individualsUnknown, by = "subject_id") %>%
              dplyr::filter(.data$unknown_date <= .data$cohort_start_date)
          )
      }
      xx <- xx %>%
        dplyr::group_by(.data$subject_id, .data$cohort_start_date) %>%
        dplyr::summarise(unknown_date = max(.data$unknown_date, na.rm = T)) %>%
        dplyr::mutate(
          diff_date = !!CDMConnector::datediff("unknown_date", indicationDate)
        )
      for (gap in gaps) {
        xx <- xx %>%
          dplyr::mutate(!!unknownName(gap) := dplyr::if_else(
            .data$diff_date < .env$gap, 1, 0
          ))
      }
      xx <- xx %>%
        dplyr::select(-"diff_date", -"unknown_date") %>%
        computeTable(cdm)
      x <- x %>%
        dplyr::left_join(xx, by = c("subject_id", "cohort_start_date"))
      for (gap in gaps) {
        xx <- xx %>%
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
      x <- x %>%
        dplyr::select(
          "subject_id", "cohort_start_date", dplyr::starts_with("indication")
        ) %>%
        computeTable(cdm)
    } else {
      for (gap in gaps) {
        columnName <- tolower(paste0("indication_gap_", gap))
        x <- x %>%
          dplyr::mutate(!!columnName := dplyr::if_else(
            is.na(.data[[columnName]]), "no indication", .data[[columnName]]
          ))
      }
      x <- x %>% computeTable(cdm)
    }
    return(x)

}
  return(x)
}

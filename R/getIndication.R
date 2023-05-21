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
  indicationGap <- sort(indicationGap)

  # original names of columns
  originalColumns <- colnames(x)

  # function to get the cohort name
  getCohortName <- function (name) {
    name <- substr(name, 1, tail(unlist(gregexpr('_', name)), n = 3) - 1)
  }

  # add indications that are cohorts
  x <- addCohortIndication()

  # add unknown indications
  x <- addUnknownIndication(x, unknownIndicationTable, indicationGap)

  return(x)
}

#' Add cohort indications
addCohortIndication <- function(x, cdm, indicationCohortName, indicationGap, indicationDate) {
  for (gap in indicationGap) {
    columnName <- tolower(paste0("indication_gap_", gap))
    xx <- x %>%
      PatientProfiles::addCohortIntersectFlag(
        cdm, indicationCohortName, indexDate = indicationDate,
        window = c(-gap, 0)
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
            paste0(.data[[columnName]], "&&", getCohortName(nam))
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
addUnknownIndication <- function(x, unknownIndicationTable, indicationGap) {
  if (!is.null(unknownIndicationTable)) {
    individualsUnknown <- x %>%
      dplyr::filter(is.na(
        .data[[tolower(paste0("indication_gap_", min(indicationGap)))]]
      )) %>%
      dplyr::select(
        "person_id" = "subject_id", dplyr::all_of(indicationDate)
      ) %>%
      dplyr::distinct() %>%
      computeTable(cdm)
    if (individualsUnknown %>% dplyr::tally() %>% dplyr::pull() > 0) {
      for (unknownTable in unknownIndicationTable) {
        unknownDate <- namesTable$start_date_name[
          namesTable$table_name == unknownTable
        ]
        xx <- cdm[[unknownTable]] %>%
          dplyr::inner_join(individualsUnknown, by = "person_id") %>%
          dplyr::filter(.data[[unknownDate]] <= .data[[indicationDate]]) %>%
          dplyr::group_by(.data$person_id, .data[[indicationDate]]) %>%
          dplyr::summarise(diff_date = max(.data[[unknownDate]])) %>%
          dplyr::mutate(
            diff_date = !!CDMConnector::datediff("diff_date", indicationDate)
          )
        for (gap in indicationGap) {
          xx <- xx %>%
            dplyr::mutate(
              !!tolower(paste0("unknown_gap_", .env$gap)) := dplyr::if_else(
                .data$diff_date < .env$gap, 1, 0
              )
            )
        }
        xx <- xx %>%
          dplyr::select(-"diff_date") %>%
          compute(cdm)
      }
    }

}
  return(x)
}

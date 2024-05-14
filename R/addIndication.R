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
#' @return Same cohort adding the indications
#'
#' @export
#'
#' @examples
#' \donttest{
#' library(DrugUtilisation)
#' library(CodelistGenerator)
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
#' cdm[["drug_cohort"]] %>%
#'   addIndication(
#'     indicationCohortName = "indication_cohorts",
#'     indicationGap = c(0, 30, 365)
#'   )
#' }
#'
addIndication <- function(x,
                          cdm = lifecycle::deprecated(),
                          indicationCohortName,
                          indicationGap = 0,
                          unknownIndicationTable = NULL,
                          indicationDate = "cohort_start_date") {
  if (lifecycle::is_present(cdm)) {
    lifecycle::deprecate_soft(
      when = "0.5.0", what = "addIndication(cdm = )"
    )
  }

  cdm <- omopgenerics::cdmReference(x)

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
  ind <- addCohortIndication(ind, indicationCohortName, indicationGap)

  # add unknown indications
  ind <- addUnknownIndication(ind, unknownIndicationTable, indicationGap) %>%
    dplyr::select(
      "subject_id", "cohort_start_date", dplyr::starts_with(
        paste0("indication_gap_", tolower(as.character(indicationGap)))
      )
    )

  # add the indication columns to the original table
  result <- x %>%
    dplyr::left_join(
      ind %>% dplyr::rename(!!indicationDate := "cohort_start_date"),
      by = c("subject_id", indicationDate)
    ) %>%
    dplyr::compute()

  dropTmpTables(cdm = cdm)

  return(result)
}

#' get cohort names
#' @noRd
getCohortName <- function (name) {
  name <- substr(name, 1, utils::tail(unlist(gregexpr('_', name)), n = 3) - 1)
}

#' get indication name from gap and termination
#' @noRd
indicationName <- function(gap, termination = "") {
  paste0("indication_gap_", tolower(as.character(gap)), "_", termination)
}

#' Add cohort indications
#' @noRd
addCohortIndication <- function(ind, cohortName, gaps) {
  for (gap in gaps) {
    ind <- ind |>
      PatientProfiles::addCohortIntersectFlag(
        targetCohortTable = cohortName, targetEndDate = NULL,
        window = c(-gap, 0), nameStyle = indicationName(gap, "{cohort_name}")
      ) %>%
      addNoneIndication(gap) %>%
      dplyr::compute(
        temporary = FALSE, overwrite = TRUE, name = uniqueTmpName()
      )
  }
  return(ind)
}

#' Add unknown indications
#' @noRd
addUnknownIndication <- function(ind, unknownTables, gaps) {
  if (!is.null(unknownTables)) {
    individualsUnknown <- ind %>%
      dplyr::filter(.data[[indicationName(min(gaps), "none")]] == 1) %>%
      dplyr::select("subject_id", "cohort_start_date") %>%
      dplyr::distinct() %>%
      dplyr::compute(
        temporary = FALSE, overwrite = TRUE, name = uniqueTmpName()
      )
    if (individualsUnknown %>% dplyr::tally() %>% dplyr::pull() > 0) {
      cdm <- omopgenerics::cdmReference(ind)
      for (ut in seq_along(unknownTables)) {
        unknownDate <- PatientProfiles::startDateColumn(unknownTables[ut])
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
            dplyr::mutate(indication_gap_inf_unknown = 1)
        } else {
          xx <- xx %>%
            dplyr::mutate(!!indicationName(gap, "unknown") := dplyr::if_else(
              .data$diff_date <= .env$gap, 1, 0
            ))
        }
      }
      xx <- xx %>%
        dplyr::select(-"diff_date", -"unknown_date") %>%
        dplyr::compute(
          temporary = FALSE, overwrite = TRUE, name = uniqueTmpName()
        )
      ind <- ind %>%
        dplyr::left_join(xx, by = c("subject_id", "cohort_start_date")) %>%
        dplyr::mutate(dplyr::across(
          dplyr::starts_with("indication_gap_"),
          ~ dplyr::if_else(is.na(.), 0, .)
        ))
      for (gap in gaps) {
        ind <- ind %>%
          dplyr::mutate(!!indicationName(gap, "unknown") := dplyr::if_else(
            .data[[indicationName(gap, "none")]] == 1 &
              .data[[indicationName(gap, "unknown")]] == 1, 1, 0
          )) %>%
          dplyr::mutate(!!indicationName(gap, "none") := dplyr::if_else(
            .data[[indicationName(gap, "none")]] == 1 &
              .data[[indicationName(gap, "unknown")]] == 0, 1, 0
          ))
      }
      ind <- ind %>%
        dplyr::select(
          "subject_id", "cohort_start_date", dplyr::starts_with("indication")
        ) %>%
        dplyr::compute(
          temporary = FALSE, overwrite = TRUE, name = uniqueTmpName()
        )
    }
  }
  return(ind)
}

#' Sum columns
#' @noRd
addNoneIndication <- function(x, gap) {
  columns <- colnames(x)
  columns <- columns[grepl(indicationName(gap), columns)]
  columns[grepl(".*\\s.*", columns)] <- paste0("`", columns[grepl(".*\\s.*", columns)],"`")
  columns <- paste0(".data$", columns, collapse = " + ")
  columns <- paste0("dplyr::if_else(", columns, " > 0, 0, 1)")
  x %>%
    dplyr::mutate(!!indicationName(gap, "none") := !!rlang::parse_expr(columns))
}

#' Create new variables summarising the data of indication that can be used as
#' stratification columns
#'
#' @param cohort A cohort in the cdm
#' @param indicationVariables Indication variables that we want to join
#' @param keep Whether to keep the prior indication variables or not
#'
#' @return description The cohort with the new variable
#'
#' @export
#'
#' @examples
#' \donttest{
#' library(DrugUtilisation)
#'
#' cdm <- mockDrugUtilisation()
#' cdm[["cohort1"]] <- cdm[["cohort1"]] %>%
#'   addIndication(indicationCohortName = "cohort2") %>%
#'   indicationToStrata()
#' }
#'
indicationToStrata <- function(cohort,
                               indicationVariables = indicationColumns(cohort),
                               keep = FALSE){
  # check inputs
  checkInputs(
    cohort = cohort, indicationVariables = indicationVariables, keep = keep
  )

  # original cohort to keep attributes
  originalCohort <- cohort

  # organize indications
  indications <- groupIndications(indicationVariables)

  # combine each gap
  for (k in seq_along(indications)) {
    cohort <- cohort %>%
      addBinaryFromCategorical(
        binaryColumns = indications[[k]]$names,
        newColumn = indications[[k]]$new_name, label = indications[[k]]$label
      )
  }

  # keep variables if asked to
  if (!keep) {
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
  for (k in seq_along(gaps)) {
    indications[[k]] <- list()
    name <- paste0("indication_gap_", gaps[k])
    indications[[k]]$names <- indicationVariables[
      grep(name, indicationVariables)
    ]
    indications[[k]]$new_name <- name
    lab <- gsub(
      paste0(name, "_"), "", indications[[k]]$names
    )
    indications[[k]]$label <- paste0(
      toupper(substr(lab, 1, 1)), substr(lab, 2, nchar(lab))
    ) %>%
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

  toAdd <- x %>%
    dplyr::select(dplyr::all_of(binaryColumns)) %>%
    dplyr::distinct() %>%
    dplyr::mutate(!!newColumn := "")

  for (k in seq_along(binaryColumns)) {
    toAdd <- toAdd %>%
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
  x <- x %>%
    dplyr::left_join(toAdd, by = binaryColumns) %>%
    dplyr::compute()

  return(x)
}

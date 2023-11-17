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

#' This function is used to summarise the lasrge scale characteristics of a
#' cohort table
#'
#' @importFrom PatientProfiles summariseLargeScaleCharacteristics
#' @export
PatientProfiles::summariseLargeScaleCharacteristics

#' Summarise a cohort from multipl codelist and windows
#'
#' @param cohort Cohort to summarise
#' @param cdm cdm_reference
#' @param conceptSet A list of concept sets
#' @param strata Stratification list
#' @param window Windows to characterize
#' @param overlap Whether we consider episodes (overlap = TRUE) or incident
#' (overlap = FALSE)
#' @param minCellCount Minimum cell counts
#'
#' @return A SummarisedResults object that contains the characterization
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
#' conceptSet <- list(
#'   "acetaminophen" = c(1125315, 1125360, 2905077, 43135274),
#'   "group A" = c(
#'     3665501, 378253, 317009, 761948, 1539403, 1503327, 1516980, 4141052,
#'     4313306
#'   )
#' )
#' summariseCharacteristicsFromCodelist(
#'   cdm[["cohort1"]], cdm, conceptSet,
#'   window = list(c(-365, -1), c(0, 0), c(1, 365))
#' )
#' }
#'
summariseCharacteristicsFromCodelist <- function(cohort,
                                                 cdm,
                                                 conceptSet,
                                                 strata = list(),
                                                 window = list(
                                                   c(-Inf, -366), c(-365, -31),
                                                   c(-30, -1), c(0, 0),
                                                   c(1, 30), c(31, 365),
                                                   c(366, Inf)
                                                 ),
                                                 overlap = TRUE,
                                                 minCellCount = 5) {
  # check initial inputs
  checkInputs(
    cohort = cohort, cdm = cdm, conceptSet = conceptSet,
    strata = strata, window = window, overlap = overlap,
    minCellCount = minCellCount
  )

  # add windoName
  window <- windowName(window)

  # save cohortSet
  cohortSet <- CDMConnector::cohortSet(cohort)

  # create codelist
  codelist <- codelistFromConceptSetList(conceptSet)

  # identify domains
  codelist <- addDomain(codelist, cdm)

  # create subset
  cohort <- conceptSubset(cohort, cdm, codelist, overlap)

  # count for each window
  result <- countOccurrences(cohort, window, strata) %>%
    dplyr::inner_join(cohortSet, by = "cohort_definition_id")

  # format output
  result <- result %>%
    dplyr::rename(
      "variable" = "concept_set_name", "group_level" = "cohort_name"
    ) %>%
    dplyr::mutate(
      variable_type = "binary", variable_level = as.character(NA),
      estimate_type = "count", group_name = "Cohort name",
      cdm_name = dplyr::coalesce(CDMConnector::cdmName(cdm), as.character(NA)),
      result_type = "Summary characteristics from codelist"
    ) %>%
    dplyr::mutate(
      "estimate" = dplyr::if_else(
        .data$estimate_type %in% c("count","denominator_count") &
          .data$estimate < minCellCount,
        paste0("<", minCellCount),
        as.character(.data$estimate)
      )
    ) %>%
    dplyr::select(
      "group_name", "group_level", "strata_name", "strata_level", "window_name",
      "variable", "variable_type", "variable_level", "estimate_type",
      "estimate", "cdm_name", "result_type"
    )

  return(result)
}

codelistFromConceptSetList <- function(conceptSet) {
  conceptSet %>%
    lapply(dplyr::as_tibble) %>%
    dplyr::bind_rows(.id = "concept_set_name") %>%
    dplyr::rename("concept_id" = "value")
}

addDomain <- function(codelist, cdm) {
  cdm[["concept"]] %>%
    dplyr::select("concept_id", "domain_id") %>%
    dplyr::inner_join(
      codelist %>% dplyr::select("concept_set_name", "concept_id"),
      by = "concept_id", copy = TRUE
    ) %>%
    CDMConnector::computeQuery()
}

conceptSubset <- function(cohort, cdm, codelist, overlap) {
  domains <- unique(codelist %>% dplyr::pull("domain_id"))
  notSupported <- domains[!(domains %in% domainInformation$domain_id)]
  if (length(notSupported) > 0) {
    cli::cli_alert(paste0(
      "Not supported dommains: ", paste0(notSupported, collapse = ", ")
    ))
  }
  domains <- domains[domains %in% domainInformation$domain_id]
  subsetResult <- NULL
  for (k in seq_along(domains)) {
    domain <- domains[k]
    tableName <- getDomainInfo(domain, "table_name")
    if (!(tableName %in% names(cdm))) {
      cli::cli_alert(paste0(tableName, " table not present in the cdm"))
    } else {
      conceptName <- getDomainInfo(domain, "concept_id_name")
      startName <- getDomainInfo(domain, "start_name")
      endName <- getDomainInfo(domain, "end_name")
      codelistK <- codelist %>%
        dplyr::filter(.data$domain_id == .env$domain) %>%
        dplyr::select("concept_set_name", !!conceptName := "concept_id")
      x <- cdm[[tableName]] %>%
        dplyr::select(
          "subject_id" = "person_id",
          "start_date" = dplyr::all_of(startName),
          "end_date" = dplyr::all_of(ifelse(overlap, endName, startName)),
          dplyr::all_of(conceptName)
        ) %>%
        dplyr::inner_join(cohort, by = "subject_id") %>%
        dplyr::inner_join(codelistK, by = conceptName) %>%
        dplyr::select(-dplyr::all_of(conceptName))
      if (is.null(subsetResult)) {
        subsetResult <- x
      } else {
        subsetResult <- dplyr::union_all(subsetResult, x)
      }
    }
  }
  subsetResult <- subsetResult %>%
    dplyr::union_all(
      cdm[["observation_period"]] %>%
        dplyr::select(
          "subject_id" = "person_id",
          "start_date" = "observation_period_start_date",
          "end_date" = "observation_period_end_date"
        ) %>%
        dplyr::mutate(concept_set_name = "denominator") %>%
        dplyr::inner_join(cohort, by = "subject_id")
    ) %>%
    dplyr::mutate(end_date = dplyr::if_else(
      is.na(.data$end_date), .data$start_date, .data$end_date
    )) %>%
    CDMConnector::computeQuery()
  return(subsetResult)
}

getDomainInfo <- function(domain, column) {
  domainInformation[[column]][domainInformation$domain_id == domain]
}

countOccurrences <- function(cohort, window, strata) {
  for (k in seq_along(window)) {
    windowStart <- window[[k]][1]
    windowEnd <- window[[k]][2]
    windowName <- names(window)[k]
    cohortK <- cohort
    if (!is.infinite(windowStart)) {
      cohortK <- cohortK %>%
        dplyr::filter(
          .data$end_date >=
            CDMConnector::dateadd("cohort_start_date", windowStart)
        )
    }
    if (!is.infinite(windowEnd)) {
      cohortK <- cohortK %>%
        dplyr::filter(
          .data$start_date <=
            CDMConnector::dateadd("cohort_start_date", windowEnd)
        )
    }
    cohortK <- cohortK %>%
      dplyr::select(
        "cohort_definition_id", "subject_id", "cohort_start_date",
        "concept_set_name", dplyr::all_of(unique(unlist(strata)))
      ) %>%
      dplyr::distinct() %>%
      CDMConnector::computeQuery()
    resultK <- cohortK %>%
      dplyr::group_by(.data$cohort_definition_id, .data$concept_set_name) %>%
      dplyr::summarise(estimate = dplyr::n(), .groups = "drop") %>%
      dplyr::collect() %>%
      dplyr::mutate(strata_name = "Overall", strata_level = "Overall")
    for (i in seq_along(strata)) {
      resultK <- resultK %>%
        dplyr::union_all(
          cohortK %>%
            dplyr::mutate(
              strata_level = !!rlang::parse_expr(sqlunite(strata[[i]]))
            ) %>%
            dplyr::group_by(
              .data$cohort_definition_id, .data$concept_set_name,
              .data$strata_level
            ) %>%
            dplyr::summarise(estimate = dplyr::n()) %>%
            dplyr::collect() %>%
            dplyr::mutate(strata_name = names(strata)[i])
        )
    }
    resultK <- resultK %>%
      dplyr::mutate(window_name = !!windowName)
    if (k == 1) {
      result <- resultK
    } else {
      result <- dplyr::union_all(result, resultK)
    }
  }
  return(result)
}

windowName <- function(window) {
  if (is.null(names(window))) {
    nam <- rep("", length(window))
  } else {
    nam <- names(window)
  }
  winName <- function(win) {
    paste(win[1], "to", win[2])
  }
  for (k in seq_along(window)) {
    if (nam[k] == "") {
      nam[k] <- winName(window[[k]])
    }
  }
  names(window) <- nam
  return(window)
}

sqlunite <- function(colnames, sep = " and ") {
  if (length(colnames) == 1) {
    out <- paste0("as.character(.data[['", colnames[1], "']])")
  } else {
    out <- paste0("paste0(as.character(.data[['", colnames[1], "']])")
    for (k in 2:length(colnames)) {
      out <- paste0(
        out, ", ' and ', as.character(.data[['", colnames[k], "']])"
      )
    }
    out <- paste0(out, ")")
  }
  return(out)
}

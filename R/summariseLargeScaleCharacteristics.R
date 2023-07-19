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
#' @param cohort cohort
#' @param cdm 'cdm' object created with CDMConnector::cdm_from_con(). It must
#' must contain the 'targetCohort' table and all the tables that we want to
#' characterize. It is a compulsory input, no default value is provided.
#' @param strata Stratification list
#' @param window Temporal windows that we want to characterize. It must
#' be a list of numeric vectors of length two. The tables will be characterized
#' between the first element and the second element respect to the
#' cohort_start_date of each individual. To refer to any time prior set NA the
#' first element of the vector. To refer to any time after the index date set NA
#' the second element of the vector. By default: list(c(NA, -366), c(-365, -90),
#' c(-365, -31), c(-90, -1), c(-30, -1), c(0, 0), c(1, 30), c(1, 90),
#' c(31, 365), c(91, 365), c(366, NA)).
#' @param tablesToCharacterize Name of the tables in the cdm that we want to
#' summarize. The available tables to characterize are: "visit_occurrence",
#' "condition_occurrence", "drug_exposure", "procedure_occurrence",
#' "device_exposure", "measurement", "observation", "drug_era", "condition_era"
#' and "specimen". By default: c("condition_occurrence", "drug_era",
#' "procedure_occurrence", "measurement").
#' @param overlap Whether you want to consider overlapping events (overlap =
#' TRUE) or only incident ones (overlap = FALSE).
#' @param minCellCount All counts lower than minimumCellCount will be
#' obscured changing its value by NA. 'obscured' column of characterization
#' tibble is TRUE when a count has been obscured. Otherwise it is FALSE.
#'
#' @return The output of this function is a 3 elements list. First
#' ("Characterization") is a reference to a temporal table in the database. It
#' contains the characterization of the desired cohorts of interest. The cohorts
#' of interest are specified using 'targetCohortId' and 'targetCohortName'. The
#' characterized tables are the ones specified in 'tablesToChacaterize'. Second
#' ("temporalWindow") contains the windows used to do the characaterization.
#' Finally "overlap" is also included in the list.
#'
#' @export
#'
#' @examples
#' \donttest{
#' library(DrugUtilisation)
#'
#' cdm <- mockDrugUtilisation()
#'
#' summariseLargeScaleCharacteristics(
#'   cohort = cdm$cohort1,
#'   tablesToCharacterize= c("drug_exposure", "condition_occurrence")
#' )
#' }
#'
summariseLargeScaleCharacteristics <- function(cohort,
                                               cdm = attr(cohort, "cdm_reference"),
                                               strata = list(),
                                               window = list(
                                                 c(-Inf, -366), c(-365, -91),
                                                 c(-365, -31), c(-90, -1),
                                                 c(-30, -1), c(0, 0), c(1, 30),
                                                 c(1, 90), c(31, 365),
                                                 c(91, 365), c(366, Inf)
                                               ),
                                               tablesToCharacterize = c(
                                                 "condition_occurrence",
                                                 "drug_era",
                                                 "procedure_occurrence",
                                                 "measurement"
                                               ),
                                               overlap = TRUE,
                                               minCellCount = 5) {
  checkInputs(
    cohort = cohort, cdm = cdm, strata = strata, window = window,
    tablesToCharacterize = tablesToCharacterize, overlap = overlap,
    minCellCount = minCellCount
  )

  # correct overlap
  if (length(overlap) == 1) {
    overlap <- rep(overlap, length(tablesToCharacterize))
  }

  window <- lapply(window, function(x){
    dplyr::tibble(
      lower_bound = x[1], upper_bound = x[2], window_name = tolower(
        paste0(x[1], " to ", x[2])
      )
    )
  }) %>%
    dplyr::bind_rows()

  # compute denominator
  den <- cohort %>%
    dplyr::select(
      "cohort_definition_id", "subject_id", "cohort_start_date",
      dplyr::all_of(unique(unlist(strata)))
    ) %>%
    dplyr::distinct() %>%
    dplyr::inner_join(
      cdm[["observation_period"]] %>%
        dplyr::select(
          "subject_id" = "person_id",
          "obs_start" = "observation_period_start_date",
          "obs_end" = "observation_period_end_date"
        ),
      by = "subject_id"
    ) %>%
    CDMConnector::computeQuery()

  # add counts
  result <- NULL
  for (k in seq_along(tablesToCharacterize)) {
    resultK <- den %>%
      dplyr::inner_join(
        cdm[[tablesToCharacterize[k]]] %>%
          dplyr::select(
            "subject_id" = "person_id",
            "start_date" = PatientProfiles::getStartName(
              tablesToCharacterize[k]
            ), "end_date" = ifelse(
              overlap[k],
              PatientProfiles::getEndName(
                tablesToCharacterize[k]
              ),
              PatientProfiles::getStartName(
                tablesToCharacterize[k]
              )
            ), "concept_id" = PatientProfiles::getConceptName(
              tablesToCharacterize[k]
            )
          ),
        by = "subject_id"
      ) %>%
      dplyr::mutate("end_date" = dplyr::if_else(
        is.na(.data$end_date), .data$start_date, .data$end_date
      )) %>%
      dplyr::mutate(
        start_date = dplyr::if_else(
          .data$start_date >= .data$obs_start, .data$start_date, .data$obs_start
        ),
        end_date = dplyr::if_else(
          .data$end_date <= .data$obs_end, .data$end_date, .data$obs_end
        )
      ) %>%
      dplyr::filter(.data$start_date <= .data$end_date) %>%
      dplyr::mutate(
        start_dif = !!CDMConnector::datediff("cohort_start_date", "start_date"),
        end_dif = !!CDMConnector::datediff("cohort_start_date", "end_date")
      ) %>%
      dplyr::select(
        "cohort_definition_id", "subject_id", "cohort_start_date", "concept_id",
        "start_dif", "end_dif", dplyr::all_of(unique(unlist(strata)))
      ) %>%
      CDMConnector::computeQuery()
    for (i in 1:nrow(window)) {
      resultKI <- resultK
      if (!is.infinite(window$upper_bound[i])) {
        resultKI <- resultKI %>%
          dplyr::filter(.data$start_dif <= !!window$upper_bound[i])
      }
      if (!is.infinite(window$lower_bound[i])) {
        resultKI <- resultKI %>%
          dplyr::filter(.data$end_dif >= !!window$lower_bound[i])
      }
      resultKI <- resultKI %>%
        dplyr::select(
          "cohort_definition_id", "subject_id", "cohort_start_date",
          "concept_id", dplyr::all_of(unique(unlist(strata)))
        ) %>%
        dplyr::distinct() %>%
        CDMConnector::computeQuery()
      result <- result %>%
        dplyr::union_all(
          resultKI %>%
            dplyr::group_by(.data$cohort_definition_id, .data$concept_id) %>%
            dplyr::summarise(count = dplyr::n(), .groups = "drop") %>%
            dplyr::inner_join(
              cdm[["concept"]] %>%
                dplyr::select("concept_id", "concept_name"),
              by = "concept_id"
            ) %>%
            dplyr::collect() %>%
            dplyr::mutate(
              strata_name = "Overall", strata_level = "Overall",
              table_name = tablesToCharacterize[k],
              window_name = window$window_name[i]
            )
        )
      for (j in seq_along(strata)) {
        result <- result %>%
          dplyr::union_all(
            resultKI %>%
              dplyr::mutate(
                strata_level = !!rlang::parse_expr(sqlunite(strata[[j]]))
              ) %>%
              dplyr::group_by(
                .data$cohort_definition_id, .data$concept_id, .data$strata_level
              ) %>%
              dplyr::summarise(count = dplyr::n(), .groups = "drop") %>%
              dplyr::inner_join(
                cdm[["concept"]] %>%
                  dplyr::select("concept_id", "concept_name"),
                by = "concept_id"
              ) %>%
              dplyr::collect() %>%
              dplyr::mutate(
                strata_name = names(strata)[j],
                table_name = tablesToCharacterize[k],
                window_name = window$window_name[i]
              )
          )
      }
    }
  }

  # add denominator_count
  denominatorCount <- NULL
  den <- den %>%
    dplyr::mutate(
      start_dif = !!CDMConnector::datediff("cohort_start_date", "obs_start"),
      end_dif = !!CDMConnector::datediff("cohort_start_date", "obs_end")
    ) %>%
    dplyr::select(
      "cohort_definition_id", "subject_id", "cohort_start_date", "start_dif",
      "end_dif", dplyr::all_of(unique(unlist(strata)))
    ) %>%
    CDMConnector::computeQuery()
  for (i in 1:nrow(window)) {
    denI <- den
    if (!is.infinite(window$upper_bound[i])) {
      denI <- denI %>%
        dplyr::filter(.data$start_dif <= !!window$upper_bound[i])
    }
    if (!is.infinite(window$lower_bound[i])) {
      denI <- denI %>%
        dplyr::filter(.data$end_dif >= !!window$lower_bound[i])
    }
    denominatorCount <- denominatorCount %>%
      dplyr::union_all(
        denI %>%
          dplyr::select(
            "cohort_definition_id", "subject_id", "cohort_start_date"
          ) %>%
          dplyr::distinct() %>%
          dplyr::group_by(.data$cohort_definition_id) %>%
          dplyr::summarise(denominator_count = dplyr::n(), .groups = "drop") %>%
          dplyr::collect() %>%
          dplyr::mutate(
            strata_level = "Overall", strata_name = "Overall",
            window_name = window$window_name[i], window_id = i
          )
      )
    for (j in seq_along(strata)) {
      denominatorCount <- denominatorCount %>%
        dplyr::union_all(
          denI %>%
            dplyr::mutate(
              strata_level = !!rlang::parse_expr(sqlunite(strata[[j]]))
            ) %>%
            dplyr::group_by(
              .data$cohort_definition_id, .data$strata_level
            ) %>%
            dplyr::summarise(denominator_count = dplyr::n(), .groups = "drop") %>%
            dplyr::collect() %>%
            dplyr::mutate(
              strata_name = names(strata)[j],
              window_name = window$window_name[i], window_id = i
            )
        )
    }
  }

  # join all together
  result <- result %>%
    dplyr::inner_join(
      denominatorCount,
      by = c("cohort_definition_id", "window_name", "strata_level", "strata_name")
    ) %>%
    dplyr::mutate(
      "%" = 100 * .data$count / .data$denominator_count,
      "count" = dplyr::if_else(
        .data$count < minCellCount,
        paste0("<", minCellCount),
        as.character(.data$count)
      ),
      "denominator_count" = dplyr::if_else(
        .data$denominator_count < minCellCount,
        paste0("<", minCellCount),
        as.character(.data$denominator_count)
      )
    ) %>%
    dplyr::inner_join(
      CDMConnector::cohortSet(cohort), by = "cohort_definition_id"
    ) %>%
    dplyr::arrange(
      .data$cohort_name, .data$table_name, .data$window_id, .data$concept_id
    ) %>%
    dplyr::mutate(
      cdm_name = dplyr::coalesce(CDMConnector::cdmName(cdm), as.character(NA)),
      result_type = "Summary large scale characteristics"
    ) %>%
    dplyr::select(
      "cohort_name", "strata_name", "strata_level", "table_name", "window_name",
      "concept_id", "concept_name", "count", "denominator_count", "%",
      "cdm_name", "result_type"
    )


  return(result)
}

#' Summarise a cohort from multipl codelist and windows
#'
#' @param cohort Cohort to summarise
#' @param cdm cdm_reference
#' @param conceptSetList A list of concept sets
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
#' conceptSetList <- list(
#'   "acetaminophen" = c(1125315, 1125360, 2905077, 43135274),
#'   "group A" = c(
#'     3665501, 378253, 317009, 761948, 1539403, 1503327, 1516980, 4141052,
#'     4313306
#'   )
#' )
#' summariseCharacteristicsFromCodelist(
#'   cdm$cohort1, cdm, conceptSetList,
#'   window = list(c(-365, -1), c(0, 0), c(1, 365))
#' )
#' }
#'
summariseCharacteristicsFromCodelist <- function(cohort,
                                                 cdm,
                                                 conceptSetList,
                                                 strata = list(),
                                                 window = list(
                                                   c(-Inf, -366), c(-365, -91),
                                                   c(-365, -31), c(-90, -1),
                                                   c(-30, -1), c(0, 0),
                                                   c(1, 30), c(1, 90),
                                                   c(31, 365), c(91, 365),
                                                   c(366, Inf)
                                                 ),
                                                 overlap = TRUE,
                                                 minCellCount = 5) {
  # check initial inputs
  checkInputs(
    cohort = cohort, cdm = cdm, conceptSetList = conceptSetList,
    strata = strata, window = window, overlap = overlap,
    minCellCount = minCellCount
  )

  # add windoName
  window <- windowName(window)

  # save cohortSet
  cohortSet <- CDMConnector::cohortSet(cohort)

  # create codelist
  codelist <- codelistFromConceptSetList(conceptSetList)

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

codelistFromConceptSetList <- function(conceptSetList) {
  conceptSetList %>%
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

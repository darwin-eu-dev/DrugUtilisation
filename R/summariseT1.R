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

#' It instantiates the cohorts and their supplementary information
#' (cohorts_info) for the DUS study
#'
#' @param cdm 'cdm' object created with CDMConnector::cdm_from_con(). It must
#' must contain at least 'drug_exposure', 'drug_strength' and
#' observation_period' tables. The 'cdm' object must contain the
#' 'write_schema' as attribute and  the user should have permission to write on
#' it. It is a compulsory input, no default value is provided.
#' @param targetCohortName Name of the target cohort
#' @param targetCohortId id or vector of ids of the cohort definitions in
#' targetCohort that we are interested in. If NULL all cohort_definitions from
#' targetCohort are considered By default: NULL.
#' @param covariateTableName covariateTableName
#' @param covariatesSet covariatesSet
#' @param covariatesWindow covariatesWindow
#' @param ... add as many covariates groups as you want
#'
#' @return The function returns the 'cdm' object with the created tables as
#' references of the object.
#' @export
#'
#' @examples
summariseT1 <- function(cdm,
                        targetCohortName,
                        targetCohortId = NULL,
                        covariatesTableName = NULL,
                        covariatesSet = NULL,
                        covariatesWindow = NULL,
                        ...) {
  listTables <- list(...)
  if (!is.null(covariatesTableName) &
      !is.null(covariatesWindow) &
      !is.null(covariatesSet)){
    listTables <- c(listTables, list(
      "covariatesTableName" = covariatesTableName,
      "covariatesSet" = covariatesSet,
      "covariatesWindow" = covariatesWindow
      ))
  } else if (is.null(covariatesTableName) &
             is.null(covariatesWindow) &
             is.null(covariatesSet) &
             is.null(listTables)) {
    stop("not enough input arguments")
  }
  checkmate::assertTRUE(length(listTables) == length(unique(names(listTables))))
  namesTables <- names(listTables)
  namesTables <- lapply(
    stringr::str_split(namesTables, "[[:upper:]]"),
    function(x){x[1]}
  ) %>%
    unlist() %>%
    unique()
  for (k in 1:length(namesTables)) {
    errorMessage <- checkmate::makeAssertCollection()
    name <- namesTables[k]
    tableName <- listTables[[paste0(name, "TableName")]]
    set <- listTables[[paste0(name, "Set")]]
    lookbackWindow <- listTables[[paste0(name, "Window")]]
    checkmate::assertTibble(set, add = errorMessage)
    checkmate::assertTRUE(
      all(c("cohortId", "cohortName") %in% colnames(set)), add = errorMessage
    )
    checkmate::assertIntegerish(set$cohortId, add = errorMessage)
    checkmate::assertCharacter(
      set$cohortName, any.missing = FALSE, add = errorMessage
    )
    checkmate::assertIntegerish(
      lookbackWindow,
      min.len = 1,
      max.len = 2,
      null.ok = FALSE,
      add = errorMessage
    )
    checkmate::assertTRUE(tableName %in% names(cdm), add = errorMessage)
    checkmate::assertTRUE(
      all(colnames(cdm[[tableName]]) %in% c(
        "cohort_definition_id", "subject_id", "cohort_start_date",
        "cohort_end_date"
      )),
      add = errorMessage
    )
    if (!errorMessage$isEmpty()) {
      errorMessage$push(paste0("- In ", name))
    }
    checkmate::reportAssertions(collection = errorMessage)
  }

  if (is.null(targetCohortId)) {
    targetCohortId <- cdm[[targetCohortName]] %>%
      dplyr::select("cohort_definition_id") %>%
      dplyr::distinct() %>%
      dplyr::pull()
  }

  for (k in 1:length(namesTables)){
    name <- namesTables[k]
    tableName <- listTables[[paste0(name, "TableName")]]
    lookbackWindow <- listTables[[paste0(name, "Window")]]
    if (length(lookbackWindow) == 1){
      lookbackWindow = c(lookbackWindow, lookbackWindow)
    }
    set <- listTables[[paste0(name, "Set")]]
    setRename <- set %>%
      dplyr::mutate(
        covariate_name = paste0(
          "overlap_", .env$tableName, "_", .data$cohortId
        ),
        covariate = paste0(
          .env$name, "_", .data$cohortName, "_",
          ifelse(is.na(.env$lookbackWindow[1]), "-Any", .env$lookbackWindow[1]),
          ";",
          ifelse(is.na(.env$lookbackWindow[2]), "Any", .env$lookbackWindow[2])
        )
      ) %>%
      dplyr::select("covariate_name", "covariate")
    result.k <- getOverlappingCohortSubjects(
      cdm = cdm,
      targetCohortName = targetCohortName,
      targetCohortId = targetCohortId,
      overlapCohortName = tableName,
      overlapCohortId = set$cohortId,
      lookbackWindow = lookbackWindow
    ) %>%
      dplyr::group_by(.data$cohort_definition_id) %>%
      dplyr::summarise(dplyr::across(
        dplyr::starts_with("overlap"), ~ sum(.x, na.rm = TRUE)
      )) %>%
      dplyr::collect() %>%
      tidyr::pivot_longer(
        dplyr::starts_with("overlap"),
        names_to = "covariate_name",
        values_to = "value"
      ) %>%
      dplyr::mutate(estimate = "counts") %>%
      dplyr::inner_join(setRename, by = "covariate_name") %>%
      dplyr::select("cohort_definition_id", "covariate", "estimate", "value")
    if (k == 1) {
      result <- result.k
    } else {
      result <- rbind(result, result.k)
    }
  }

  return(result)
}

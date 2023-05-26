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

#' This function is used to summarise the dose and/or indication over multiple
#' cohorts.
#'
#' @param cohort A cohort in the cdm
#' @param cdm A cdm_reference created by CDMConnector
#' @param ageGroup A list of age groups.
#' @param windowVisitOcurrence Window to count visit occurrences.
#' @param covariates Named list of windows to check covariates. The name must
#' point to a cohortTableName in the cdm.
#' @param minimumCellCount minimum counts due to obscure
#'
#' @return A summary of the characteristics of the individuals
#'
#' @export
#'
#' @examples
#' \donttest{
#' cdm <- mockDrugUtilisation(numberIndividuals = 1000)
#' summariseTableOne(
#'   cdm$cohort1,
#'   cdm,
#'   ageGroup = list(c(0, 19), c(20, 39), c(40, 59), c(60, 79), c(80, 150)),
#'   windowVisitOcurrence = c(-180, 0)
#' )
#' }
summariseTableOne <- function(cohort,
                              cdm,
                              ageGroup = NULL,
                              windowVisitOcurrence = NULL,
                              covariates = list(),
                              minimumCellCount = 5) {


  listTables <- list(...)
  #check listTables
  checkListTable(listTables)

  if (!is.null(covariatesTableName) &
      !is.null(covariatesWindow) &
      !is.null(covariatesSet)) {
    listTables <- c(listTables, list(
      "covariatesTableName" = covariatesTableName,
      "covariatesSet" = covariatesSet,
      "covariatesWindow" = covariatesWindow
    ))
  }

  # Checks
  errorMessage <- checkmate::makeAssertCollection()
  # check cdm
  checkCdm(cdm)
  # person table in cdm
  checkmate::assertTRUE("person" %in% names(cdm))
  # check targetCohortName and targetCohortId
  checkmate::assertCharacter(
    targetCohortName,
    len = 1,
    null.ok = FALSE,
    any.missing = FALSE,
    add = errorMessage
  )

  checkmate::assertTRUE(targetCohortName %in% names(cdm))

  checkmate::assertInteger(
    targetCohortId,
    null.ok = TRUE,
    add = errorMessage
  )
  # check ageGroups
  checkAgeGroup(ageGroups)




  #define targetCohort
  targetCohort <- cdm[[targetCohortName]]

  if (is.null(targetCohortId)) {
    targetCohortId <- targetCohort %>%
      dplyr::select("cohort_definition_id") %>%
      dplyr::distinct() %>%
      dplyr::pull()
  } else {
    targetCohort <- targetCohort %>%
      dplyr::filter(.data$cohort_definition_id %in% .env$targetCohortId)
  }


  #add age, sex, priorHistory
  subjects <- targetCohort %>%
    dplyr::select("subject_id", "cohort_start_date", "cohort_end_date") %>%
    dplyr::distinct() %>%
    PatientProfiles::addDemographics(cdm,
                                     ageGroup = ageGroups,
                                     futureObservation = FALSE) %>%
    dplyr::compute()

  result <- targetCohort %>%
    dplyr::left_join(
      subjects,
      by = c("subject_id", "cohort_start_date", "cohort_end_date")
    ) %>%
    dplyr::group_by(.data$cohort_definition_id) %>%
    dplyr::summarise(
      number_obervations.count = as.character(dplyr::n()),
      number_subjects.count = as.character(dplyr::n_distinct(.data$subject_id)),
      sex_female.count = as.character(dplyr::count(.data$sex[.data$sex == "Female"])),
      sex_male.count = as.character(dplyr::count(.data$sex[.data$sex == "Male"])),
      age.mean = as.character(mean(.data$age, na.rm = TRUE)),
      age.std = as.character(sd(.data$age, na.rm = TRUE)),
      prior_history.mean = as.character(mean(.data$prior_history, na.rm = TRUE)),
      prior_history.std = as.character(sd(.data$prior_history, na.rm = TRUE)),
      number_observations.count = as.character(dplyr::n()),
      cohort_start_date.min = as.character(min(
        .data$cohort_start_date,
        na.rm = TRUE
      )),
      cohort_start_date.max = as.character(max(
        .data$cohort_start_date,
        na.rm = TRUE
      )),
      cohort_end_date.min = as.character(min(
        .data$cohort_end_date,
        na.rm = TRUE
      )),
      cohort_end_date.max = as.character(max(
        .data$cohort_end_date,
        na.rm = TRUE
      )),
      .groups = "drop"
    ) %>%
    dplyr::collect() %>%
    dplyr::left_join(
      targetCohort %>%
        dplyr::left_join(
          subjects,
          by = c("subject_id", "cohort_start_date", "cohort_end_date")
        ) %>%
        dplyr::group_by(.data$cohort_definition_id) %>%
        dplyr::summarise(
          age.median = as.character(median(.data$age, na.rm = TRUE)),
          age.quantile25 = as.character(quantile(.data$age, 0.25, na.rm = TRUE)),
          age.quantile75 = as.character(quantile(.data$age, 0.75, na.rm = TRUE)),
          .groups = "drop"
        ) %>%
        dplyr::collect(),
      by = "cohort_definition_id"
    ) %>%
    dplyr::left_join(
      targetCohort %>%
        dplyr::left_join(
          subjects,
          by = c("subject_id", "cohort_start_date", "cohort_end_date")
        ) %>%
        dplyr::group_by(.data$cohort_definition_id) %>%
        dplyr::summarise(
          prior_history.median = as.character(median(.data$prior_history, na.rm = TRUE)),
          prior_history.quantile25 = as.character(quantile(.data$prior_history, 0.25, na.rm = TRUE)),
          prior_history.quantile75 = as.character(quantile(.data$prior_history, 0.75, na.rm = TRUE)),
          .groups = "drop"
        ) %>%
        dplyr::collect(),
      by = "cohort_definition_id"
    )

  result <- result %>%
    tidyr::pivot_longer(
      cols = !"cohort_definition_id",
      names_to = c("variable", "estimate"),
      names_sep = "\\."
    )

  if (!is.null(windowVisitOcurrence)) {
    result.visit_occurrence <- targetCohort %>%
      dplyr::left_join(
        subjects %>% addVisit(cdm = cdm, window = windowVisitOcurrence),
        by = c("subject_id", "cohort_start_date", "cohort_end_date")
      ) %>%
      dplyr::group_by(.data$cohort_definition_id) %>%
      dplyr::summarise(
        visit_occurrence.mean = as.character(mean(.data$number_visits, na.rm = TRUE)),
        visit_occurrence.std = as.character(sd(.data$number_visits, na.rm = TRUE)),
        visit_occurrence.median = as.character(median(.data$number_visits, na.rm = TRUE)),
        visit_occurrence.quantile25 = as.character(quantile(.data$number_visits, 0.25, na.rm = TRUE)),
        visit_occurrence.quantile75 = as.character(quantile(.data$number_visits, 0.75, na.rm = TRUE))
      ) %>%
      dplyr::collect()
    result.visit_occurrence <- result.visit_occurrence %>%
      tidyr::pivot_longer(
        cols = colnames(result.visit_occurrence)[-1],
        names_to = c("variable", "estimate"),
        names_sep = "\\."
      ) %>%
      dplyr::select(
        "cohort_definition_id", "variable", "estimate", "value"
      )
  } else {
    result.visit_occurrence <- NULL
  }

  if (!is.null(ageGroups)) {



    result.age <- targetCohort %>%
      dplyr::left_join(
        subjects,
        by = c("subject_id", "cohort_start_date", "cohort_end_date")
      ) %>%
      dplyr::group_by(.data$cohort_definition_id, .data$age_group) %>%
      dplyr::summarise(n = as.integer(dplyr::n()), .groups = "drop") %>%
      dplyr::collect() %>%
      dplyr::mutate(
        estimate = "count",
        variable = paste0("age_group_", .data$age_group)
      ) %>%
      dplyr::select(
        "cohort_definition_id", "variable", "estimate",
        "value" = "n"
      )
  } else {
    result.age <- NULL
  }



  #adding listTables
  namesTables <- names(listTables)

  if (length(namesTables) > 0) {
    for (k in 1:length(namesTables)) {
      name <- namesTables[k]
      tableName <- listTables[[paste0(name, "TableName")]]
      lookbackWindow <- listTables[[paste0(name, "Window")]]
      if (length(lookbackWindow) == 1) {
        lookbackWindow <- c(lookbackWindow, lookbackWindow)
      }
      set <- listTables[[paste0(name, "Set")]]
      setRename <- set %>%
        dplyr::mutate(
          variable_name = paste0(
            "overlap_", .env$tableName, "_", .data$cohortId
          ),
          variable = paste0(
            .env$name, "_", .data$cohortName, "_",
            ifelse(is.na(.env$lookbackWindow[1]), "-Any", .env$lookbackWindow[1]),
            ";",
            ifelse(is.na(.env$lookbackWindow[2]), "Any", .env$lookbackWindow[2])
          )
        ) %>%
        dplyr::select("variable_name", "variable")
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
          names_to = "variable_name",
          values_to = "value"
        ) %>%
        dplyr::mutate(estimate = "count") %>%
        dplyr::inner_join(setRename, by = "variable_name") %>%
        dplyr::select("cohort_definition_id", "variable", "estimate", "value")
      if (k == 1) {
        result.covariates <- result.k
      } else {
        result.covariates <- rbind(result.covariates, result.k)
      }
    }
  } else {
    result.covariates <- NULL
  }

  output <- rbind(
    result, result.age, result.visit_occurrence, result.covariates
  ) %>% obscureSummary(minimumCellCount = minimumCellCount)

  return(output)
}

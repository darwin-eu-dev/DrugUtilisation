# Copyright 2022 DARWIN EU (C)
#
# This file is part of DrugUtilizationCharacteristics
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
#' @param cdm 'cdm' object created with CDMConnector::cdm_from_con(). It must
#' must contain the 'doseTableName' table and  also 'strataCohortName' and
#' 'indicationList' if specified. It is a compulsory input, no default
#' value is provided.
#' @param targetCohortName target cohort which table one is created for, character
#' @param targetCohortId target cohort definition ids aiming to include, can be a list of integers or NULL. When Null, use all ids
#' @param ageGroups A list of age groups we are interested in adding count. Each group should contain a vector with min age and max
#' age. e.g. `list(c(0,10),c(20,30))` Can be NULL. If NULL, do not consider age groups
#' @param windowVisitOcurrence A vector of window, using which visit occurrence count will be checked.
#' @param covariatesTableName covariatesTableName
#' @param covariatesSet covariatesSet
#' @param covariatesWindow covariatesWindow
#' @param ... you can add as many covariates tables that you want following the
#' pattern: xxxTableName, xxxSet, xxxWindow, where xxxTableName would be the
#' cohort table name in the cdm, xxxSet the cohortSet and xxxWindow the window
#' to asses the covariates. xxx will be the name of
#' @return
#'
#' @export
#'
#' @examples
getTableOne <- function(cdm,
                        targetCohortName,
                        targetCohortId = NULL,
                        ageGroups = NULL,
                        windowVisitOcurrence = NULL,
                        covariatesTableName = NULL,
                        covariatesSet = NULL,
                        covariatesWindow = NULL,
                        ...) {
  listTables <- list(...)
  if (!is.null(covariatesTableName) &
    !is.null(covariatesWindow) &
    !is.null(covariatesSet)) {
    listTables <- c(listTables, list(
      "covariatesTableName" = covariatesTableName,
      "covariatesSet" = covariatesSet,
      "covariatesWindow" = covariatesWindow
    ))
  }
  # first round of assertions CLASS
  # start checks
  errorMessage <- checkmate::makeAssertCollection()
  # check cdm
  checkmate::assertClass(
    cdm,
    "cdm_reference",
    add = errorMessage
  )
  # check targetCohortName
  checkmate::assertCharacter(
    targetCohortName,
    len = 1,
    null.ok = FALSE,
    any.missing = FALSE,
    add = errorMessage
  )

  # check strataCohort
  checkmate::assertTRUE(targetCohortName %in% names(cdm))
  checkmate::assertTRUE("person" %in% names(cdm))

  targetCohort <- cdm[[targetCohortName]]
  # check targetCohortId
  checkmate::assertInteger(
    targetCohortId,
    null.ok = TRUE,
    add = errorMessage
  )
  # check ageGroups
  checkmate::assert_list(ageGroups,
    add = errorMessage
  )
  if (!is.null(ageGroups)) {
    for (i in seq_along(ageGroups)) {
      checkmate::assertTRUE(length(ageGroups[[i]]) == 2)
      checkmate::assert_numeric(ageGroups[[i]][1],
        add = errorMessage
      )
      checkmate::assert_numeric(ageGroups[[i]][2],
        add = errorMessage
      )
      ageCheck <- ageGroups[[i]][1] <=
        ageGroups[[i]][2]
      checkmate::assertTRUE(ageCheck,
        add = errorMessage
      )
      if (!isTRUE(ageCheck)) {
        errorMessage$push(
          "- upper age value must be equal or higher than lower age value"
        )
      }
      checkmate::assertTRUE(ageGroups[[i]][1] >= 0,
        add = errorMessage
      )
      checkmate::assertTRUE(ageGroups[[i]][2] >= 0,
        add = errorMessage
      )
    }
  }

  checkmate::assertTRUE(length(listTables) == length(unique(names(listTables))))
  namesTables <- names(listTables)
  namesTables <- lapply(
    stringr::str_split(namesTables, "[[:upper:]]"),
    function(x) {
      x[1]
    }
  ) %>%
    unlist() %>%
    unique()
  if (length(namesTables) > 0) {
    for (k in 1:length(namesTables)) {
      errorMessage <- checkmate::makeAssertCollection()
      name <- namesTables[k]
      tableName <- listTables[[paste0(name, "TableName")]]
      set <- listTables[[paste0(name, "Set")]]
      lookbackWindow <- listTables[[paste0(name, "Window")]]
      checkmate::assertTibble(set, add = errorMessage)
      checkmate::assertTRUE(
        all(c("cohortId", "cohortName") %in% colnames(set)),
        add = errorMessage
      )
      checkmate::assertIntegerish(set$cohortId, add = errorMessage)
      checkmate::assertCharacter(
        set$cohortName,
        any.missing = FALSE, add = errorMessage
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
  }

  if (is.null(targetCohortId)) {
    targetCohortId <- targetCohort %>%
      dplyr::select("cohort_definition_id") %>%
      dplyr::distinct() %>%
      dplyr::pull()
  }

  targetCohort <- targetCohort %>%
    addPriorHistory(cdm = cdm) %>%
    addSex(cdm = cdm) %>%
    addAge(cdm = cdm)

  if (!is.null(windowVisitOcurrence)) {
    targetCohort <- targetCohort %>%
      addVisit(cdm = cdm, window = windowVisitOcurrence)
    for (k in 1:length(targetCohortId)) {
      result.k.visit_occurrence <- targetCohort %>%
        dplyr::summarize(
          visit_occurrence.mean = as.character(mean(.data$number_visits, na.rm = TRUE)),
          visit_occurrence.std = as.character(sd(.data$number_visits, na.rm = TRUE)),
          visit_occurrence.median = as.character(median(.data$number_visits, na.rm = TRUE)),
          visit_occurrence.quantile25 = as.character(quantile(.data$number_visits, 0.25, na.rm = TRUE)),
          visit_occurrence.quantile25 = as.character(quantile(.data$number_visits, 0.75, na.rm = TRUE))
        ) %>%
        dplyr::collect() %>%
        tidyr::pivot_longer(
          cols = dplyr::everything(),
          names_to = c("covariate", "estimate"),
          names_sep = "\\."
        ) %>%
        dplyr::mutate(cohort_definition_id = !!targetCohortId[k]) %>%
        dplyr::select(
          "cohort_definition_id", "covariate", "estimate", "value"
        )

      if (k == 1) {
        result.visit_occurrence <- result.k.visit_occurrence
      } else {
        result.visit_occurrence <- rbind(result.visit_occurrence, result.k.visit_occurrence)
      }
    }
  } else {
    result.visit_occurrence <- NULL
  }




  for (k in 1:length(targetCohortId)) {
    result.k <- targetCohort %>%
      dplyr::filter(.data$cohort_definition_id == !!targetCohortId[k]) %>%
      dplyr::summarise(
        sex_female.count = as.character(count(.data$sex[.data$sex == "Female"])),
        sex_male.count = as.character(count(.data$sex[.data$sex == "Male"])),
        age.mean = as.character(mean(.data$age, na.rm = TRUE)),
        age.std = as.character(sd(.data$age, na.rm = TRUE)),
        age.median = as.character(median(.data$age, na.rm = TRUE)),
        age.quantile25 = as.character(quantile(.data$age, 0.25, na.rm = TRUE)),
        age.quantile25 = as.character(quantile(.data$age, 0.75, na.rm = TRUE)),
        prior_history.mean = as.character(mean(.data$prior_history, na.rm = TRUE)),
        prior_history.std = as.character(sd(.data$prior_history, na.rm = TRUE)),
        prior_history.median = as.character(median(.data$prior_history, na.rm = TRUE)),
        prior_history.quantile25 = as.character(quantile(.data$prior_history, 0.25, na.rm = TRUE)),
        prior_history.quantile25 = as.character(quantile(.data$prior_history, 0.75, na.rm = TRUE)),
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
        ))
      ) %>%
      dplyr::collect() %>%
      tidyr::pivot_longer(
        cols = dplyr::everything(),
        names_to = c("covariate", "estimate"),
        names_sep = "\\."
      ) %>%
      dplyr::mutate(cohort_definition_id = !!targetCohortId[k]) %>%
      dplyr::select(
        "cohort_definition_id", "covariate", "estimate", "value"
      )

    if (k == 1) {
      result <- result.k
    } else {
      result <- rbind(result, result.k)
    }
  }




  if (!is.null(ageGroups)) {
    for (k in 1:length(targetCohortId)) {
      ageGrDf <- data.frame(do.call(rbind, ageGroups)) %>%
        dplyr::mutate(ageGroup = paste0(.data$X1, ";", .data$X2))

      for (i in 1:dim(ageGrDf)[1]) {
        max_age <- ageGrDf$X2[i]
        min_age <- ageGrDf$X1[i]
        targetCohort <- targetCohort %>% dplyr::mutate(
          max_age = .env$max_age,
          min_age = .env$min_age,
          !!paste0("ageGroup_", ageGrDf$ageGroup[i]) :=
            dplyr::if_else(age > min_age && age < max_age, 1, 0)
        )
      }



      result.k.age <- targetCohort %>%
        dplyr::summarize_at(dplyr::vars(starts_with("ageGroup")), sum, na.rm = TRUE) %>%
        dplyr::rename_at(dplyr::vars(starts_with("ageGroup")), list(~ paste0(., ".count"))) %>%
        dplyr::collect() %>%
        tidyr::pivot_longer(
          cols = dplyr::everything(),
          names_to = c("covariate", "estimate"),
          names_sep = "\\."
        ) %>%
        dplyr::mutate(cohort_definition_id = !!targetCohortId[k]) %>%
        dplyr::select(
          "cohort_definition_id", "covariate", "estimate", "value"
        )

      if (k == 1) {
        result.age <- result.k.age
      } else {
        result.age <- rbind(result.age, result.k.age)
      }
    }
  } else {
    result.age <- NULL
  }

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
        dplyr::mutate(estimate = "count") %>%
        dplyr::inner_join(setRename, by = "covariate_name") %>%
        dplyr::select("cohort_definition_id", "covariate", "estimate", "value")
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
  )

  return(output)
}

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

#' Explain function
#'
#' @param cdm A cdm object created with CDMConnector. No default.
#' @param targetCohortName A taget cohort instantiated in the database.
#' @param targetCohortId The cohort definition id of the cohort of interest. By
#' default NULL. NULL only valid when targetCohort contains only one cohort.
#' @param sex The sex stratification. By default = NULL.
#' @param ageGroup The age groups stratification. By default = NULL.
#' @param indexYearGroup The index Years stratification. By default = NULL.
#' @param indicationTable The indication table that contains the different
#' indications.
#' @param oneStrata Weather we want to stratify one strata (TRUE) each or
#' combine in multiple stratas using expand_grid (FALSE). By deafault = FALSE.
#'
#' @return Multiple cohorts as a temporal table in the database. The
#' stratification for each cohort can be see as attribute ("strata").
#' @export
#'
#' @examples
getStratification <- function(cdm,
                              targetCohortName,
                              targetCohortId = NULL,
                              sex = NULL,
                              ageGroup = NULL,
                              indexYearGroup = NULL,
                              indicationTable = NULL,
                              oneStrata = FALSE) {
  # initial checks
  errorMessage <- checkmate::makeAssertCollection()
  checkmate::assertClass(cdm, "cdm_reference", add = errorMessage)
  checkmate::assertCharacter(
    targetCohortName,
    any.missing = FALSE,
    len = 1,
    add = errorMessage
  )
  checkmate::assertCount(targetCohortId, null.ok = TRUE, add = errorMessage)
  checkmate::assertCharacter(
    sex,
    any.missing = FALSE, unique = TRUE, null.ok = TRUE, add = errorMessage
  )
  checkmate::assertList(
    ageGroup,
    c("numeric", "logical"),
    min.len = 1,
    any.missing = FALSE,
    null.ok = TRUE,
    add = errorMessage
  )
  checkmate::assertList(
    indexYearGroup,
    "numeric",
    min.len = 1,
    any.missing = FALSE,
    null.ok = TRUE,
    add = errorMessage
  )
  checkmate::assertList(
    indicationTable,
    min.len = 1,
    null.ok = TRUE,
    any.missing = FALSE,
    add = errorMessage
  )
  checkmate::reportAssertions(collection = errorMessage)

  # second round of checks
  checkmate::assertFALSE(
    is.null(sex) && is.null(ageGroup) && is.null(indexYearGroup) &&
      is.null(indicationTable),
    add = errorMessage
  )
  checkmate::assertTRUE(targetCohortName %in% names(cdm), add = errorMessage)
  if (!is.null(sex)) {
    checkmate::assertTRUE(
      all(sex %in% c("Both", "Male", "Female")),
      add = errorMessage
    )
  }
  if (!is.null(ageGroup)) {
    if (!all(unlist(lapply(ageGroup, length)) == 2)) {
      errorMessage$push("Length of all elements in ageGroup should be 2.")
    }
    if (!all(unlist(lapply(ageGroup, function(x) {
      all(is.na(x) | x >= 0)
    })))) {
      errorMessage$push(
        "Length of all elements in ageGroup should be NA or >= 0."
      )
    }
    if (!all(unlist(lapply(ageGroup, function(x) {
      is.na(x[1]) | is.na(x[2]) | x[1] <= x[2]
    })))) {
      errorMessage$push(paste0(
        "The first value of each element in ageGroup shoudl be smaller or equal ",
        "than the second one."
      ))
    }
  }
  if (!is.null(indexYearGroup)) {
    checkmate::assertNumeric(
      unlist(indexYearGroup),
      any.missing = FALSE,
      add = errorMessage
    )
    for (k in 1:length(indexYearGroup)) {
      if (length(indexYearGroup[[k]]) > 2) {
        errorMessage$push(paste0(
          "Length of indexYear groups should be 1 ",
          "(only one year) or 2 (a period: provide min and max)"
        ))
      }
    }
  }
  if (!is.null(indicationTable)) {
    if (!all(unlist(lapply(indicationTable, function(x) {
      c(
        "cohort_definition_id", "indication_id", "subject_id",
        "cohort_start_date", "cohort_end_date"
      ) %in% colnames(x)
    })))) {
      errorMessage$push(paste0(
        "All indicationTable elements must be temp tables",
        " with columns: 'indication_id', 'subject_id', 'cohort_start_date', ",
        "'cohort_end_date'."
      ))
      errorMessage$push("Use getIndication function to obtain indicationTable.")
    }
  }
  checkmate::reportAssertions(collection = errorMessage)

  targetCohort <- cdm[[targetCohortName]]
  checkmate::assertTRUE(all(c(
    "cohort_definition_id", "subject_id", "cohort_start_date", "cohort_end_date"
  ) %in% colnames(targetCohort)),
  add = errorMessage
  )
  checkmate::assertTRUE(length(colnames(targetCohort)) == 4, add = errorMessage)
  if (is.null(targetCohortId)) {
    targetCohortId <- targetCohort %>%
      dplyr::select("cohort_definition_id") %>%
      dplyr::distinct() %>%
      dplyr::pull()
    if (length(targetCohortId) > 1) {
      errorMessage$push(paste0(
        "targetCohortId should be provided when ",
        "targetCohort contains more than one cohort_definition_id."
      ))
    }
  }
  checkmate::reportAssertions(collection = errorMessage)
  if (targetCohort %>%
    dplyr::filter(.data$cohort_definition_id == .env$targetCohortId) %>%
    dplyr::tally() %>%
    dplyr::pull("n") == 0) {
    errorMessage$push("No counts in the targetCohortId")
  }
  checkmate::reportAssertions(collection = errorMessage)

  # set default options
  if (is.null(sex)) {
    sex <- "Both"
  }
  if (is.null(ageGroup)) {
    ageGroup <- list(c(0, 150))
  }
  if (is.null(indexYearGroup)) {
    indexYearGroup <- list(c(
      targetCohort %>%
        dplyr::pull("cohort_start_date") %>%
        lubridate::year() %>%
        base::min(),
      targetCohort %>%
        dplyr::pull("cohort_start_date") %>%
        lubridate::year() %>%
        base::max()
    ))
  }

  # create tibbles to add the groups
  # sex
  sexGroup <- dplyr::tibble(sex_group = sex) %>%
    dplyr::left_join(
      dplyr::tibble(
        sex_group = c("Both", "Both", "Male", "Female"),
        sex = c("Male", "Female", "Male", "Female")
      ),
      by = "sex_group"
    )
  # age group
  ageGroup <- lapply(ageGroup, function(x) {
    if (is.na(x[1])) {
      x[1] <- 0
    }
    if (is.na(x[2])) {
      x[2] <- 150
    }
    return(dplyr::tibble(
      age_group = paste0(x[1], ";", x[2]),
      age_min = x[1],
      age_max = x[2]
    ))
  }) %>%
    dplyr::bind_rows() %>%
    dplyr::mutate(to_join = 1) %>%
    dplyr::inner_join(
      dplyr::tibble(to_join = 1, age = 0:150),
      by = "to_join"
    ) %>%
    dplyr::filter(.data$age >= .data$age_min) %>%
    dplyr::filter(.data$age <= .data$age_max) %>%
    dplyr::select("age_group", "age")
  # index year
  indexYearGroup <- lapply(indexYearGroup, function(x) {
    if (length(x) == 1) {
      return(dplyr::tibble(index_year_group = as.character(x), index_year = x))
    } else {
      return(dplyr::tibble(
        index_year_group = paste0(x[1], "-", x[2]),
        index_year = x[1]:x[2]
      ))
    }
  }) %>%
    dplyr::bind_rows()
  # indication
  if (!is.null(indicationTable)) {
    gaps <- names(indicationTable)
    indicationDefinitionSet <- attr(indicationTable, "indicationDefinitionSet")
    indicationNames <- indicationDefinitionSet$indication_name
    indicationGroup <- dplyr::tibble(indication_group = "Any") %>%
      dplyr::union_all(tidyr::expand_grid(
        gap = gaps,
        indication_name = indicationNames
      ) %>%
        dplyr::mutate(indication_group = paste0(
          "gap:", .data$gap, "; ", .data$indication_name
        )) %>%
        dplyr::select("indication_group"))
  } else {
    indicationGroup <- dplyr::tibble(indication_group = "Any")
  }

  # get the groups
  sexGroups <- unique(sexGroup$sex_group)
  ageGroups <- unique(ageGroup$age_group)
  indexYearGroups <- unique(indexYearGroup$index_year_group)
  indicationGroups <- unique(indicationGroup$indication_group)

  # create the combinations for the different cohorts
  settings <- tidyr::expand_grid(
    sex_group = sexGroups,
    age_group = ageGroups,
    index_year_group = indexYearGroups,
    indication_group = indicationGroups
  )
  if (oneStrata == TRUE) {
    settings <- settings %>%
      dplyr::mutate(
        sex_def = dplyr::if_else(
          .data$sex_group == .env$sexGroups[1],
          1,
          0
        ),
        age_def = dplyr::if_else(
          .data$age_group == .env$ageGroups[1],
          1,
          0
        ),
        idy_def = dplyr::if_else(
          .data$index_year_group == .env$indexYearGroups[1],
          1,
          0
        ),
        ind_def = dplyr::if_else(
          .data$indication_group == .env$indicationGroups[1],
          1,
          0
        )
      ) %>%
      dplyr::mutate(
        sum_def = .data$sex_def + .data$age_def + .data$idy_def + .data$ind_def
      ) %>%
      dplyr::filter(.data$sum_def >= 3) %>%
      dplyr::select(
        "sex_group", "age_group", "index_year_group", "indication_group"
      )
  }

  settings <- settings %>%
    dplyr::mutate(cohort_definition_id = dplyr::row_number()) %>%
    dplyr::relocate("cohort_definition_id", .before = "sex_group")

  # prepare indicationTable
  if (!is.null(indicationTable)) {
    for (k in 1:length(gaps)) {
      indicationTab <- indicationTable[[k]] %>%
        dplyr::select(
          "subject_id", "cohort_start_date", "cohort_end_date", "indication_id"
        ) %>%
        dplyr::distinct() %>%
        dplyr::inner_join(
          indicationDefinitionSet,
          by = "indication_id",
          copy = TRUE
        ) %>%
        dplyr::mutate(indication_group = paste0(
          "gap:", !!gaps[k], "; ", .data$indication_name
        )) %>%
        dplyr::select(
          "subject_id", "cohort_start_date", "cohort_end_date",
          "indication_group"
        )
      if (k == 1) {
        indication <- indicationTab
      } else {
        indication <- indication %>% dplyr::union_all(indicationTab)
      }
    }
    indication <- indication %>%
      dplyr::union_all(
        indicationTab %>%
          dplyr::mutate(indication_group = "Any") %>%
          dplyr::distinct()
      ) %>%
      dplyr::compute()
  }

  targetCohort <- targetCohort %>%
    dplyr::filter(.data$cohort_definition_id == .env$targetCohortId) %>%
    addSex(cdm = cdm) %>%
    addAge(cdm = cdm) %>%
    dplyr::mutate(index_year = lubridate::year(.data$cohort_start_date)) %>%
    dplyr::select(
      "subject_id", "cohort_start_date", "cohort_end_date", "sex", "age",
      "index_year"
    )

  if (!is.null(indicationTable)) {
    targetCohort <- targetCohort %>%
      dplyr::inner_join(
        indication,
        by = c("subject_id", "cohort_start_date", "cohort_end_date")
      )
  } else {
    targetCohort <- targetCohort %>% dplyr::mutate(indication_group = "Any")
  }

  targetCohort <- targetCohort %>%
    dplyr::compute() %>%
    dplyr::inner_join(sexGroup, by = "sex", copy = TRUE) %>%
    dplyr::inner_join(ageGroup, by = "age", copy = TRUE) %>%
    dplyr::inner_join(indexYearGroup, by = "index_year", copy = TRUE) %>%
    dplyr::inner_join(
      settings,
      by = c("sex_group", "age_group", "index_year_group", "indication_group"),
      copy = TRUE
    ) %>%
    dplyr::select(
      "cohort_definition_id", "subject_id", "cohort_start_date",
      "cohort_end_date"
    ) %>%
    dplyr::compute()

  attr(targetCohort, "cohortSet") <- settings

  return(targetCohort)
}

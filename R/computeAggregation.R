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
#' @return
#' @export
#'
#' @examples
computeAggregation <- function(cdm,
                               doseCohortName,
                               sex = NULL,
                               ageGroup = NULL,
                               indexYear = NULL,
                               initialDose = NULL,
                               indicationCohortName = NULL,
                               indicationDefinitionSet = NULL,
                               unknownIndicationTables = NULL,
                               gapIndication = NULL,
                               oneStrata = FALSE) {
  # create initial assertions
  errorMessage <- checkmate::makeAssertCollection()
  # check cdm
  checkmate::assertClass(cdm, "cdm_reference", add = errorMessage)
  # check doseCohortName
  checkmate::assertCharacter(doseCohortName, len = 1, add = errorMessage)
  if (is.character(doseCohortName) && length(doseCohortName) == 1) {
    checkmate::assertTRUE(all(c(
      "cohort_definition_id", "subject_id", "cohort_start_date",
      "cohort_end_date"
    ) %in% colnames(cdm[[doseCohortName]])),
    add = errorMessage
    )
  }
  # check sex
  checkmate::assertTRUE(
    all(sex %in% c("Both", "Male", "Female")),
    add = errorMessage
  )
  checkmate::assertTRUE(identical(sex, unique(sex)), add = errorMessage)
  # check ageGroup
  checkmate::assertList(
    ageGroup,
    types = "numeric",
    min.len = 1,
    null.ok = TRUE,
    unique = TRUE,
    add = errorMessage
  )
  if (!is.null(ageGroup)) {
    tryCatch(expr = {
      if (unique(unlist(lapply(ageGroup, length))) != 2) {
        errorMessage$push("length of all element in 'ageGroup' should be 2.")
      }
    })
    tryCatch(expr = {
      if (all(unlist(lapply(ageGroup, function(x) {
        if (!is.na(x[1]) && !is.na(x[2]) && x[1] > x[2]) {
          return(FALSE)
        } else {
          return(TRUE)
        }
      }))) == FALSE) {
        errorMessage$push(paste0(
          "First value should be smaller or equal to second for all 'ageGorup'",
          " elements."
        ))
      }
    })
  }
  # check indexYear
  checkmate::assertIntegerish(indexYear, null.ok = TRUE, add = errorMessage)
  # check initialDose
  checkmate::assertList(
    initialDose,
    types = "numeric",
    min.len = 1,
    null.ok = TRUE,
    unique = TRUE,
    add = errorMessage
  )
  if (!is.null(initialDose)) {
    tryCatch(expr = {
      if (unique(unlist(lapply(initialDose, length))) != 2) {
        errorMessage$push("length of all element in 'initialDose' should be 2.")
      }
    })
    tryCatch(expr = {
      if (all(unlist(lapply(initialDose, function(x) {
        if (!is.na(x[1]) && !is.na(x[2]) && x[1] > x[2]) {
          return(FALSE)
        } else {
          return(TRUE)
        }
      }))) == FALSE) {
        errorMessage$push(paste0(
          "First value should be smaller or equal to second for all ",
          "'initialDose' elements."
        ))
      }
    })
  }
  # check indicationCohortName
  checkmate::assertCharacter(
    indicationCohortName,
    len = 1, null.ok = TRUE, add = errorMessage
  )
  if (is.null(indicationCohortName)) {
    if (!is.null(indicationDefinitionSet)) {
      errorMessage$push(paste0(
        "if indicationCohortName is NULL indicationDefinitionSet should not ",
        "be provided."
      ))
    }
    if (!is.null(unknownIndicationTables)) {
      errorMessage$push(paste0(
        "if indicationCohortName is NULL unknownIndicationTables should not ",
        "be provided."
      ))
    }
    if (!is.null(gapIndication)) {
      errorMessage$push(paste0(
        "if indicationCohortName is NULL gapIndication should not be provided."
      ))
    }
  } else {
    # check indicationDefinitionSet
    checkmate::assertTibble(
      indicationDefinitionSet,
      min.rows = 1,
      ncols = 2,
      add = errorMessage
    )
    checkmate::assertTRUE(
      all(c("indication_id", "indication_name") %in%
            colnames(indicationDefinitionSet)),
      add = errorMessage
    )
    try(checkmate::assertIntegerish(
      indicationDefinitionSet$indication_id,
      lower = 1,
      unique = TRUE,
      any.missing = FALSE,
      add = errorMessage
    ))
    try(checkmate::assertCharacter(
      indicationDefinitionSet$indication_name,
      unique = TRUE,
      any.missing = FALSE,
      add = errorMessage
    ))
    checkmate::assertCharacter(
      unknownIndicationTables,
      any.missing = FALSE,
      null.ok = TRUE,
      add = errorMessage
    )
    checkmate::assertTRUE(
      all(unknownIndicationTables %in% names(cdm)),
      add = errorMessage
    )
    checkmate::assertCount(gapIndication, add = errorMessage)
  }
  checkmate::assertLogical(
    oneStrata,
    any.missing = FALSE, len = 1, add = errorMessage
  )
  checkmate::assertFALSE(
    is.null(sex) &&
      is.null(ageGroup) &&
      is.null(indexYear) &&
      is.null(initialDose) &&
      is.null(indicationCohortName),
    add = errorMessage
  )

  # report collection of errors
  checkmate::reportAssertions(collection = errorMessage)

  # compute sex strata options
  if (is.null(sex)) {
    sex <- "Both"
  }
  sex <- sort(sex)
  sexStrata <- dplyr::tibble(sex_id = 1:length(sex), sex_name = sex)

  # compute age strata options
  ageGroup <- unique(c(list(c(NA, NA)), ageGroup))
  ageGroupStrata <- lapply(ageGroup, function(x) {
    nam <- paste0(
      ifelse(is.na(x[1]), "Any", x[1]),
      ";",
      ifelse(is.na(x[2]), "Any", x[2])
    )
    x <- dplyr::tibble(
      min_age = x[1], max_age = x[2], age_group_name = nam
    )
    return(x)
  }) %>%
    dplyr::bind_rows() %>%
    dplyr::mutate(age_group_id = dplyr::row_number()) %>%
    dplyr::select(
      "age_group_id", "age_group_name", "age_group_min", "age_group_max"
    )

  indexYear <- unique(c(NA, indexYear))
  indexYearStrata <- dplyr::tibble(
    index_year_id = 1:length(indexYear),
    index_year_name = as.character(indexYear),
    index_year_min = indexYear,
    index_year_max = indexYear
  )
  if (nrow(indexYearStrata) == 1) {
    indexYearStrata$index_year_min[1] <- cdm[[doseCohortName]] %>%
      dplyr::pull("cohort_start_date") %>%
      base::min() %>%
      lubridate::year()
    indexYearStrata$index_year_max[1] <- cdm[[doseCohortName]] %>%
      dplyr::pull("cohort_start_date") %>%
      base::max() %>%
      lubridate::year()
    if (indexYearStrata$index_year_min[1] == indexYearStrata$index_year_max[1]){
      indexYearStrata$index_year_name[1] <- as.character(
        indexYearStrata$index_year_min[1]
      )
    } else {
      indexYearStrata$index_year_name[1] <- paste0(
        indexYearStrata$index_year_min[1],
        ";",
        indexYearStrata$index_year_max[1]
      )
    }
  } else {
    indexYearStrata$index_year_min[1] <- cdm[[doseCohortName]] %>%
      dplyr::pull("cohort_start_date") %>%
      base::min() %>%
      lubridate::year()
  }

  # compute age strata options
  if (is.null(initialDose)) {
    initialDose <- list(c(NA, NA))
  } else {
    initialDose <- unique(c(list(c(NA, NA)), initialDose))
  }
  ageGroupStrata <- lapply(initialDose, function(x) {
    nam <- paste0(
      ifelse(is.na(x[1]), "Any", x[1]),
      ";",
      ifelse(is.na(x[2]), "Any", x[2])
    )
    x <- dplyr::tibble(
      min_initial_dose = x[1], max_initial_dose = x[2], initial_dose_name = nam
    )
    return(x)
  }) %>%
    dplyr::bind_rows() %>%
    dplyr::mutate(initial_dose_id = dplyr::row_number()) %>%
    dplyr::select(
      "initial_dose_id", "initial_dose_name", "initial_dose_min",
      "initial_dose_max"
    )

  return(cdm)
}

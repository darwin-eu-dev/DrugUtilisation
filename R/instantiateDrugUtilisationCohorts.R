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
#' @param cdm cdm
#' @param specifications specifications
#' @param studyTime studyTime
#' @param gapEra gapEra
#' @param eraJoinMode eraJoinMode
#' @param overlapMode overlapMode
#' @param sameIndexMode sameIndexMode
#' @param drugUtilisationCohortName drugUtilisationCohortName
#' @param imputeDuration imputeDuration
#' @param imputeDailyDose imputeDailyDose
#' @param durationLowerBound durationLowerBound
#' @param durationUpperBound durationUpperBound
#' @param dailyDoseLowerBound dailyDoseLowerBound
#' @param dailyDoseUpperBound dailyDoseUpperBound
#' @param verbose verbose
#'
#' @return
#' @export
#'
#' @examples
instantiateDrugUtilisationCohorts <- function(cdm,
                                              specifications,
                                              ingredient_concept_id,
                                              studyTime = 365,
                                              gapEra = 30,
                                              eraJoinMode = "first",
                                              overlapMode = "first",
                                              sameIndexMode = "sum",
                                              drugUtilisationCohortName,
                                              imputeDuration = FALSE,
                                              imputeDailyDose = FALSE,
                                              durationLowerBound = NULL,
                                              durationUpperBound = NULL,
                                              dailyDoseLowerBound = NULL,
                                              dailyDoseUpperBound = NULL,
                                              verbose = FALSE) {
  error_message <- checkmate::makeAssertCollection()
  #check cdm
  cdm_inherits_check <- inherits(cdm, "cdm_reference")
  checkmate::assertTRUE(cdm_inherits_check,
                        add = error_message
  )
  if (!isTRUE(cdm_inherits_check)) {
    error_message$push(
      "- cdm must be a CDMConnector CDM reference object"
    )
  }
  #check ingredient concept id is an integer
  checkmate::assert_int(ingredient_concept_id,
                        add = error_message,
                        null.ok = TRUE)

  #check studyTime is an integer
  checkmate::assert_int(studyTime,
                         add = error_message,
                         null.ok = TRUE)

  #check gapEra is an integer
  checkmate::assert_int(gapEra,
                        add = error_message,
                        null.ok = TRUE)

  #check drugUtilisationCohortName is a character
  checkmate::assert_character(drugUtilisationCohortName,
                              add = messageStore)

  #check imputeDuration is a character
  checkmate::assert_logical(imputeDuration,
                            add = messageStore)

  #check imputeDailyDose is a character
  checkmate::assert_logical(imputeDailyDose,
                            add = messageStore)

  #check verbose is a character
  checkmate::assert_logical(verbose,
                            add = messageStore)

  #check durationLowerBound is an integer
  checkmate::assert_int(durationLowerBound,
                        add = error_message,
                        null.ok = TRUE)

  #check durationUpperBound an integer
  checkmate::assert_int(durationUpperBound,
                        add = error_message,
                        null.ok = TRUE)
  #check dailyDoseLowerBound is numeric
  checkmate::assert_numeric(dailyDoseLowerBound,
                            add = messageStore,
                            null.ok = TRUE)

  #check dailyDoseUpperBound is numeric
  checkmate::assert_numeric(dailyDoseUpperBound,
                            add = messageStore,
                            null.ok = TRUE)

  #if request to impute days supply, must provide default duration
  if(imputeDuration == TRUE){
    default_duration_exists <- any(grepl("default_duration",
                                     colnames(specifications),ignore.case=TRUE))
    checkmate::assertTRUE(default_duration_exists, add = error_message)
    if (!isTRUE(default_duration_exists)) {
      error_message$push(
        "- must provide default_duration if imputeDuration = TRUE"
      )
    }
  }
  #if request to impute daily dose, must provide default daily dose
  if(imputeDailyDose == TRUE){
    default_daily_dose_exists <- any(grepl("default_daily_dose",
                                           colnames(specifications),ignore.case=TRUE))
    checkmate::assertTRUE(default_daily_dose_exists, add = error_message)
    if (!isTRUE(default_daily_dose_exists)) {
      error_message$push(
        "- must provide default_daily_dose if imputeDailyDose = TRUE"
      )
    }
  }

  #check drug exposure table exist
  cdm_drug_exp_exists <- inherits(cdm$drug_exposure, "tbl_dbi")
  checkmate::assertTRUE(cdm_drug_exp_exists, add = error_message)
  if (!isTRUE(cdm_drug_exp_exists)) {
    error_message$push(
      "- table `drug exposure` is not found"
    )
  }

  #check eraJoinMode is correctly specified
  eraJoinModeCheck <- eraJoinMode %in% c("zero", "join", "first", "second")
  checkmate::assertTRUE(eraJoinModeCheck, add = error_message)
  if(!isTRUE(eraJoinModeCheck)){
    error_message$push(
      glue::glue("- eraJoinMode must be one of `zero`, `join`, `first`, `second`"
    ))
  }

  #check overlapMode is correctly specified
  overlapModeCheck <- overlapMode %in% c("max", "sum", "min", "first", "second")
  checkmate::assertTRUE(overlapModeCheck, add = error_message)
  if(!isTRUE(overlapModeCheck)){
    error_message$push(
      glue::glue("- overlapMode must be one of `max`, `sum`, `min`, `first`,
                 `second`"))}

  #check sameIndexMode is correctly specified
  sameIndexModeCheck <- sameIndexMode %in% c("max", "sum", "min", "first", "second")
  checkmate::assertTRUE(sameIndexModeCheck, add = error_message)
  if(!isTRUE(sameIndexModeCheck)){
    error_message$push(
      glue::glue("- sameIndexMode must be one of `max`, `sum`, `min`"))}

  checkmate::reportAssertions(collection = error_message)

  dialect <- CDMConnector::dbms(attr(cdm, "dbcon"))
  drugUtilisationTableDataName <- paste0(drugUtilisationCohortName, "_info")

  if(is.null(specifications)){
    allIngredient <- cdm$drug_strength$ingredient_concept_id
    cdm$drug
  }

  specifications <- specifications %>%
    dplyr::select(
      "drug_concept_id",
      tidyselect::matches("default_duration"),
      tidyselect::matches("default_daily_dose")
    )
  if (isTRUE(is.na(studyTime))) {
    studyTime <- NULL
  }
  drugUtilisationCohort <- cdm[["drug_exposure"]] %>%
    dplyr::select(
      "person_id", "drug_concept_id", "drug_exposure_start_date",
      "drug_exposure_end_date", "quantity", "drug_exposure_id"
    ) %>%
    dplyr::inner_join(
      specifications,
      by = "drug_concept_id",
      copy = TRUE
    ) %>%
    dplyr::compute()

  if (!is.null(studyTime)) {
    drugUtilisationCohort <- drugUtilisationCohort %>%
      dplyr::group_by(.data$person_id) %>%
      dplyr::mutate(
        drug_exposure_end_date_max = min(
          .data$drug_exposure_start_date,
          na.rm = TRUE
        )
      ) %>%
      dplyr::mutate(
        drug_exposure_end_date_max = as.Date(dbplyr::sql(sql_add_days(
          CDMConnector::dbms(attr(cdm, "dbcon")),
          studyTime - 1,
          "drug_exposure_end_date_max"
        )))
      ) %>%
      dplyr::ungroup() %>%
      dplyr::filter(
        .data$drug_exposure_start_date <= .data$drug_exposure_end_date_max
      ) %>%
      dplyr::compute()
  }

  drugUtilisationCohort <- drugUtilisationCohort %>%
    dplyr::mutate(days_exposed = dbplyr::sql(sqlDiffDays(
      CDMConnector::dbms(attr(cdm, "dbcon")),
      "drug_exposure_start_date",
      "drug_exposure_end_date"
    )) + 1)

  # impute duration
  drugUtilisationCohort <- imputeVariable(
    x = drugUtilisationCohort,
    variableName = "days_exposed",
    impute = imputeDuration,
    lowerBound = durationLowerBound,
    upperBound = durationUpperBound,
    imputeValueName = "default_duration",
    allowZero = FALSE
  )

  # compute the daily dose
  drugUtilisationCohort <- computeDailyDose(
    table = drugUtilisationCohort,
    cdm = cdm,
    ingredient_concept_id = ingredient_concept_id,
    verbose = verbose
  )

  if (!is.null(studyTime)) {
    drugUtilisationCohort <- drugUtilisationCohort %>%
      dplyr::mutate(
        days_exposed = as.integer(days_exposed)) %>%
      dplyr::mutate(drug_exposure_end_date = as.Date(dbplyr::sql(sql_add_days(
        CDMConnector::dbms(attr(cdm, "dbcon")),
        "days_exposed",
        "drug_exposure_start_date"
      )))) %>% dplyr::compute() %>%
      dplyr::mutate(
        drug_exposure_end_date = as.Date(dbplyr::sql(sql_add_days(
          CDMConnector::dbms(attr(cdm, "dbcon")),
          -1,
          "drug_exposure_end_date"
        )))
      ) %>%
      dplyr::mutate(
        force_max = dplyr::if_else(
          .data$drug_exposure_end_date > .data$drug_exposure_end_date_max,
          1,
          0
        )
      ) %>%
      dplyr::mutate(
        drug_exposure_end_date = dplyr::if_else(
          .data$force_max == 1,
          .data$drug_exposure_end_date_max,
          .data$drug_exposure_end_date
        )
      ) %>%
      dplyr::mutate(
        days_exposed = dplyr::if_else(
          .data$force_max == 1,
          dbplyr::sql(sqlDiffDays(
            CDMConnector::dbms(attr(cdm, "dbcon")),
            "drug_exposure_start_date",
            "drug_exposure_end_date"
          )) + 1,
          .data$days_exposed
        )
      ) %>%
      dplyr::select(-"force_max", -"drug_exposure_end_date_max") %>%
      dplyr::compute()
  }

  # impute daily_dose
  drugUtilisationCohort <- imputeVariable(
    x = drugUtilisationCohort,
    variableName = "daily_dose",
    impute = imputeDuration,
    lowerBound = dailyDoseLowerBound,
    upperBound = dailyDoseUpperBound,
    imputeValueName = "default_daily_dose",
    allowZero = TRUE
  )

  # compute cumulative dose per person
  cumulativeDoseNoRestrictions <- drugUtilisationCohort %>%
    dplyr::group_by(.data$person_id) %>%
    dplyr::summarise(
      cumulative_dose_no_restrictions = sum(
        .data$daily_dose * .data$days_exposed,
        na.rm = TRUE
      )
    ) %>%
    dplyr::compute()

  # get only the variables that we are interested in
  drugUtilisationCohort <- drugUtilisationCohort %>%
    dplyr::select(
      "person_id", "drug_exposure_id", "drug_exposure_start_date",
      "drug_exposure_end_date", "daily_dose", "days_exposed"
    ) %>%
    dplyr::compute()

  drugUtilisationCohort <- getPeriods(
    x = drugUtilisationCohort,
    dialect = dialect,
    verbose = verbose
  )

  drugUtilisationCohort <- joinExposures(
    x = drugUtilisationCohort,
    gapEra = gapEra,
    eraJoinMode = eraJoinMode,
    sameIndexMode = sameIndexMode,
    dialect = dialect,
    verbose = verbose
  )

  if (is.null(studyTime)) {
    drugUtilisationCohort <- drugUtilisationCohort %>%
      dplyr::filter(.data$era_id == 1) %>%
      dplyr::compute()
  }

  drugUtilisationCohort <- continuousExposures(
    x = drugUtilisationCohort,
    overlapMode = overlapMode,
    sameIndexMode = sameIndexMode,
    dialect = dialect,
    verbose = verbose
  )
}

#' Impute or eliminate values under a certain conditions
#'
#' @param x x
#' @param variableName variableName
#' @param impute impute
#' @param lowerBound lowerBound
#' @param upperBound upperBound
#' @param imputeValueName imputeValueName
#' @param allowZero allowZero
#'
#' @noRd
imputeVariable <- function(x,
                           variableName,
                           impute,
                           lowerBound,
                           upperBound,
                           imputeValueName,
                           allowZero) {
  x <- x %>%
    dplyr::rename("variable" = .env$variableName)
  # impute if allow zero
  if (isTRUE(allowZero)) {
    x <- x %>%
      dplyr::mutate(impute = dplyr::if_else(
        is.na(.data$variable) | .data$variable < 0,
        1,
        0
      ))
  } else {
    x <- x %>%
      dplyr::mutate(impute = dplyr::if_else(
        is.na(.data$variable) | .data$variable <= 0,
        1,
        0
      ))
  }
  # impute lower bound
  if (!is.null(lowerBound)) {
    x <- x %>%
      dplyr::mutate(impute = dplyr::if_else(
        .data$variable < .env$lowerBound,
        1,
        .data$impute
      ))
  }
  if (!is.null(upperBound)) {
    x <- x %>%
      dplyr::mutate(impute = dplyr::if_else(
        .data$variable > .env$upperBound,
        1,
        .data$impute
      ))
  }
  if (isFALSE(impute)) {
    x <- x %>%
      dplyr::filter(.data$impute == 0)
  } else {
    x <- x %>%
      dplyr::rename("imputeValue" = .env$imputeValueName) %>%
      dplyr::mutate(variable = dplyr::if_else(
        .data$impute == 1,
        .data$imputeValue,
        .data$variable
      )) %>%
      dplyr::rename(!!imputeValueName := "imputeValue")
  }
  x <- x %>%
    dplyr::select(-"impute") %>%
    dplyr::rename(!!variableName := "variable") %>%
    dplyr::compute()
  return(x)
}


#' Explain function
#'
#' @param x table
#' @param dialect dialect
#' @param verbose verbose
#'
#' @noRd
getPeriods <- function(x, dialect, verbose) {
  # compute the start of possible overlapping periods
  x_start <- x %>%
    dplyr::select("person_id", "drug_exposure_start_date") %>%
    dplyr::distinct() %>%
    dplyr::rename("start_interval" = "drug_exposure_start_date") %>%
    dplyr::union(
      x %>%
        dplyr::select("person_id", "drug_exposure_end_date") %>%
        dplyr::distinct() %>%
        dplyr::mutate(
          start_interval = as.Date(dbplyr::sql(sql_add_days(
            dialect,
            1,
            "drug_exposure_end_date"
          )))
        ) %>%
        dplyr::select(-"drug_exposure_end_date")
    ) %>%
    dplyr::group_by(.data$person_id) %>%
    dplyr::filter(
      .data$start_interval < max(.data$start_interval, na.rm = TRUE)
    ) %>%
    dbplyr::window_order(.data$start_interval) %>%
    dplyr::mutate(subexposure_id = dplyr::row_number()) %>%
    dplyr::ungroup() %>%
    dplyr::compute()
  # compute the start of possible overlapping periods
  x_end <- x %>%
    dplyr::select("person_id", "drug_exposure_start_date") %>%
    dplyr::distinct() %>%
    dplyr::mutate(
      end_interval = as.Date(dbplyr::sql(sql_add_days(
        dialect,
        -1,
        "drug_exposure_start_date"
      )))
    ) %>%
    dplyr::select(-"drug_exposure_start_date") %>%
    dplyr::union(x %>%
                   dplyr::select("person_id", "drug_exposure_end_date") %>%
                   dplyr::distinct() %>%
                   dplyr::rename("end_interval" = "drug_exposure_end_date")) %>%
    dplyr::group_by(.data$person_id) %>%
    dplyr::filter(
      .data$end_interval > min(.data$end_interval, na.rm = TRUE)
    ) %>%
    dbplyr::window_order(.data$end_interval) %>%
    dplyr::mutate(subexposure_id = dplyr::row_number()) %>%
    dplyr::ungroup() %>%
    dplyr::compute()
  # compute the overlapping periods joining start and end dates
  x_intervals <- x_start %>%
    dplyr::inner_join(
      x_end,
      by = c("person_id", "subexposure_id")
    ) %>%
    dplyr::compute()
  # we join the exposures with the overlapping periods and we only consider the
  # exposures that contribute to each overlapping period
  x_intervals <- x_intervals %>%
    dplyr::inner_join(x, by = "person_id") %>%
    dplyr::filter(.data$drug_exposure_start_date <= .data$start_interval) %>%
    dplyr::filter(.data$drug_exposure_end_date >= .data$end_interval) %>%
    dplyr::compute()
  # the overlapping subgroups are grouped into groups of continuous exposure,
  # see the documentation for a more detailed explanation of the difference
  # between overlapping group and subgroups
  x_intervals_ids <- x_intervals %>%
    dplyr::select("person_id", "subexposure_id") %>%
    dplyr::distinct() %>%
    dplyr::left_join(
      x_intervals %>%
        dplyr::select("person_id", "subexposure_id") %>%
        dplyr::distinct() %>%
        dplyr::mutate(subexposure_id = .data$subexposure_id + 1) %>%
        dplyr::mutate(index_continuous_exposure = 0),
      by = c("person_id", "subexposure_id")
    ) %>%
    dplyr::mutate(index_continuous_exposure = dplyr::if_else(
      is.na(.data$index_continuous_exposure),
      1,
      .data$index_continuous_exposure
    )) %>%
    dplyr::group_by(.data$person_id) %>%
    dbplyr::window_order(.data$subexposure_id) %>%
    dplyr::mutate(
      continuous_exposure_id = cumsum(.data$index_continuous_exposure)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select("person_id", "subexposure_id", "continuous_exposure_id") %>%
    dplyr::compute()
  x_intervals <- x_intervals %>%
    dplyr::left_join(
      x_intervals_ids,
      by = c("person_id", "subexposure_id")
    ) %>%
    dplyr::compute()
  return(x_intervals)
}

#' Explain function
#'
#' @param x table
#' @param gapEra gapEra
#' @param eraJoinMode eraJoinMode
#' @param sameIndexMode sameIndexMode
#' @param dialect dialect
#' @param verbose verbose
#'
#' @noRd
joinExposures <- function(x,
                          gapEra,
                          eraJoinMode,
                          sameIndexMode,
                          dialect,
                          verbose) {
  # get the start of gaps
  gap_start <- x %>%
    dplyr::group_by(.data$person_id) %>%
    dplyr::filter(
      .data$continuous_exposure_id < max(
        .data$continuous_exposure_id,
        na.rm = TRUE
      )
    ) %>%
    dplyr::group_by(.data$person_id, .data$continuous_exposure_id) %>%
    dplyr::summarise(
      start_interval = max(.data$end_interval, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      start_interval = as.Date(dbplyr::sql(sql_add_days(
        dialect,
        1,
        "start_interval"
      )))
    ) %>%
    dplyr::compute()
  # get the end of gaps
  gap_end <- x %>%
    dplyr::group_by(.data$person_id) %>%
    dplyr::filter(
      .data$continuous_exposure_id > min(
        .data$continuous_exposure_id,
        na.rm = TRUE
      )
    ) %>%
    dplyr::group_by(.data$person_id, .data$continuous_exposure_id) %>%
    dplyr::summarise(
      end_interval = min(.data$start_interval, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      end_interval = as.Date(dbplyr::sql(sql_add_days(
        dialect,
        -1,
        "end_interval"
      )))
    ) %>%
    dplyr::mutate(continuous_exposure_id = .data$continuous_exposure_id - 1) %>%
    dplyr::compute()
  # get eras
  gap_period <- gap_start %>%
    dplyr::inner_join(
      gap_end,
      by = c("person_id", "continuous_exposure_id")
    ) %>%
    dplyr::mutate(days_exposed = dbplyr::sql(sqlDiffDays(
      dialect,
      "start_interval",
      "end_interval"
    )) + 1) %>%
    dplyr::filter(.data$days_exposed <= .env$gapEra) %>%
    dplyr::mutate(gap = 1)
  if (eraJoinMode == "zero") {
    gap_period <- gap_period %>%
      dplyr::mutate(daily_dose = 0)
  } else if (eraJoinMode == "join") {
    gap_period <- gap_period %>%
      dplyr::mutate(daily_dose = 0) %>%
      dplyr::mutate(days_exposed = 0)
  }
  subexposure_id <- gap_period %>%
    dplyr::select("person_id", "continuous_exposure_id") %>%
    dplyr::inner_join(
      x %>%
        dplyr::group_by(.data$person_id, .data$continuous_exposure_id) %>%
        dplyr::summarise(
          subexposure_id = max(.data$subexposure_id, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        dplyr::mutate(subexposure_id = .data$subexposure_id + 1),
      by = c("person_id", "continuous_exposure_id")
    ) %>%
    dplyr::compute()
  gap_period <- gap_period %>%
    dplyr::inner_join(
      subexposure_id,
      by = c("person_id", "continuous_exposure_id")
    ) %>%
    dplyr::select(-"continuous_exposure_id") %>%
    dplyr::compute()
  if (eraJoinMode == "first" | eraJoinMode == "second") {
    if (eraJoinMode == "first") {
      daily_dose <- gap_period %>%
        dplyr::select("person_id", "subexposure_id") %>%
        dplyr::inner_join(
          x %>%
            dplyr::select(
              "person_id", "subexposure_id", "drug_exposure_start_date",
              "daily_dose"
            ) %>%
            dplyr::mutate(subexposure_id = .data$subexposure_id + 1),
          by = c("person_id", "subexposure_id")
        ) %>%
        dplyr::group_by(.data$person_id, .data$subexposure_id) %>%
        dplyr::filter(
          drug_exposure_start_date == min(
            .data$drug_exposure_start_date,
            na.rm = TRUE
          )
        ) %>%
        dplyr::mutate(number_in_group = dplyr::n()) %>%
        dplyr::ungroup() %>%
        dplyr::select(-"drug_exposure_start_date") %>%
        dplyr::compute()
    } else {
      daily_dose <- gap_period %>%
        dplyr::select("person_id", "subexposure_id") %>%
        dplyr::inner_join(
          x %>%
            dplyr::select(
              "person_id", "subexposure_id", "daily_dose"
            ) %>%
            dplyr::mutate(subexposure_id = .data$subexposure_id - 1),
          by = c("person_id", "subexposure_id")
        ) %>%
        dplyr::group_by(.data$person_id, .data$subexposure_id) %>%
        dplyr::mutate(number_in_group = dplyr::n()) %>%
        dplyr::ungroup() %>%
        dplyr::compute()
    }
    daily_dose_multiple <- daily_dose %>%
      dplyr::filter(.data$number_in_group > 1) %>%
      dplyr::select(-"number_in_group") %>%
      dplyr::compute()
    if (daily_dose_multiple %>% dplyr::tally() %>% dplyr::pull() > 0) {
      daily_dose_multiple <- daily_dose_multiple %>%
        dplyr::group_by(.data$person_id, .data$subexposure_id)
      if (sameIndexMode == "sum") {
        daily_dose_multiple <- daily_dose_multiple %>%
          dplyr::summarise(
            daily_dose = sum(.data$daily_dose, na.rm = TRUE),
            .groups = "drop"
          )
      } else if (sameIndexMode == "max") {
        daily_dose_multiple <- daily_dose_multiple %>%
          dplyr::summarise(
            daily_dose = max(.data$daily_dose, na.rm = TRUE),
            .groups = "drop"
          )
      } else if (sameIndexMode == "min") {
        daily_dose_multiple <- daily_dose_multiple %>%
          dplyr::summarise(
            daily_dose = min(.data$daily_dose, na.rm = TRUE),
            .groups = "drop"
          )
      }
      daily_dose_multiple <- daily_dose_multiple %>%
        dplyr::compute()
      daily_dose <- daily_dose %>%
        dplyr::select(-"number_in_group") %>%
        dplyr::anti_join(
          daily_dose_multiple,
          by = c("person_id", "subexposure_id")
        ) %>%
        dplyr::union_all(daily_dose_multiple) %>%
        dplyr::compute()
    }
    gap_period <- gap_period %>%
      dplyr::inner_join(
        daily_dose,
        by = c("person_id", "subexposure_id")
      ) %>%
      dplyr::compute()
  }
  # add eras to the exposures
  x <- x %>%
    dplyr::union_all(gap_period) %>%
    dplyr::mutate(gap = dplyr::if_else(is.na(.data$gap), 0, 1)) %>%
    dplyr::mutate(days_exposed =  dbplyr::sql(sqlDiffDays(
      dialect,
      "start_interval",
      "end_interval")) + 1) %>%
    dplyr::compute()
  era_id <- x %>%
    dplyr::select("person_id", "subexposure_id") %>%
    dplyr::distinct() %>%
    dplyr::compute()
  era_id <- era_id %>%
    dplyr::left_join(
      era_id %>%
        dplyr::mutate(subexposure_id = .data$subexposure_id + 1) %>%
        dplyr::mutate(era_id = 0),
      by = c("person_id", "subexposure_id")
    ) %>%
    dplyr::mutate(era_id = dplyr::if_else(is.na(era_id), 1, 0)) %>%
    dplyr::group_by(.data$person_id) %>%
    dbplyr::window_order(.data$subexposure_id) %>%
    dplyr::mutate(era_id = cumsum(.data$era_id)) %>%
    dplyr::ungroup() %>%
    dplyr::compute()
  x <- x %>%
    dplyr::inner_join(
      era_id,
      by = c("person_id", "subexposure_id")
    ) %>%
    dplyr::compute()

  return(x)
}

#' Explain function
#'
#' @param x table
#' @param overlapMode overlapMode
#' @param sameIndexMode sameIndexMode
#' @param verbose verbose
#'
#' @noRd
continuousExposures <- function(x,
                                overlapMode,
                                sameIndexMode,
                                dialect,
                                verbose) {
  x <- x %>%
    dplyr::select(-"drug_exposure_end_date") %>%
    dplyr::group_by(.data$person_id, .data$subexposure_id) %>%
    dplyr::mutate(number_exposures_interval = dplyr::n()) %>%
    dplyr::ungroup() %>%
    dplyr::compute()
  # save number of exposures and number of groups and subgroups
  exposureCounts <- x %>%
    dplyr::group_by(.data$person_id) %>%
    dplyr::summarise(
      number_exposures = dplyr::n_distinct(.data$drug_exposure_id),
      number_subexposures = dplyr::n_distinct(.data$subexposure_id),
      number_subexposures_with_overlap = dplyr::n_distinct(
        .data$subexposure_id[.data$number_exposures_interval > 1]
      ),
      number_continuous_exposures = dplyr::n_distinct(
        .data$continuous_exposure_id[!is.na(.data$continuous_exposure_id)]
      ),
      number_continuous_exposures_with_overlap = dplyr::n_distinct(
        .data$continuous_exposure_id[.data$number_exposures_interval > 1 &
                                       !is.na(.data$continuous_exposure_id)]
      ),
      number_eras = dplyr::n_distinct(.data$era_id),
      number_eras_with_overlap = dplyr::n_distinct(
        .data$era_id[.data$number_exposures_interval > 1]
      ),
      number_non_exposed_periods = dplyr::n_distinct(.data$era_id),
      number_gaps = sum(.data$gap, na.rm = TRUE),
      number_days_gap = sum(.data$gap * .data$days_exposed, na.rm = TRUE),
      cumulative_gap_dose = sum(
        .data$gap * .data$days_exposed * daily_dose,
        na.rm = TRUE
      ),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      number_subexposures_no_overlap =
        .data$number_subexposures - .data$number_subexposures_with_overlap
    ) %>%
    dplyr::mutate(
      number_continuous_exposures_no_overlap =
        .data$number_continuous_exposures -
        .data$number_continuous_exposures_with_overlap
    ) %>%
    dplyr::mutate(
      number_eras_no_overlap =
        .data$number_eras - .data$number_eras_with_overlap
    ) %>%
    dplyr::compute()
  # get the subgroups without overlap
  uniqueExposures <- x %>%
    dplyr::filter(.data$number_exposures_interval == 1) %>%
    dplyr::select(
      "person_id", "subexposure_id", "daily_dose", "days_exposed",
      "start_interval", "end_interval"
    ) %>%
    dplyr::compute()

  # get the overlapped groups
  multipleExposures <- x %>%
    dplyr::filter(.data$number_exposures_interval > 1) %>%
    dplyr::select(
      "person_id", "subexposure_id", "daily_dose", "days_exposed",
      "start_interval", "end_interval", "drug_exposure_start_date"
    ) %>%
    dplyr::compute()

  if (multipleExposures %>% dplyr::tally() %>% dplyr::pull() > 0) {

    # search individuals with same index date in subgroups
    multipleExposuresSameIndex <- multipleExposures %>%
      dplyr::group_by(
        .data$person_id, .data$subexposure_id, .data$drug_exposure_start_date
      ) %>%
      dplyr::mutate(number_same_index = dplyr::n()) %>%
      dplyr::ungroup() %>%
      dplyr::filter(.data$number_same_index > 1) %>%
      dplyr::select(-"number_same_index") %>%
      dplyr::compute()

    # get the overlaps with different index date
    multipleExposures <- multipleExposures %>%
      dplyr::anti_join(
        multipleExposuresSameIndex,
        by = c("person_id", "subexposure_id", "drug_exposure_start_date")
      ) %>%
      dplyr::compute()

    # I AM HERE (ELIMINATE NOT USED VARIABLES AS DRUG_EXPOSSURE_END_DATE)

    # solve the overlap with same index date
    # if criteria is to pick maximum
    if (sameIndexMode == "max") {
      multipleExposuresSameIndex <- multipleExposuresSameIndex %>%
        dplyr::group_by(
          .data$person_id, .data$subexposure_id, .data$drug_exposure_start_date
        ) %>%
        dplyr::filter(daily_dose == max(.data$daily_dose, na.rm = TRUE)) %>%
        dplyr::distinct() %>%
        dplyr::ungroup() %>%
        dplyr::compute()
      # if criteria is to pick the sum
    } else if (sameIndexMode == "sum") {
      multipleExposuresSameIndex <- multipleExposuresSameIndex %>%
        dplyr::group_by(
          .data$person_id, .data$subexposure_id, .data$drug_exposure_start_date
        ) %>%
        dplyr::mutate(daily_dose = sum(.data$daily_dose, na.rm = TRUE)) %>%
        dplyr::distinct() %>%
        dplyr::ungroup() %>%
        dplyr::compute()
      # if criteria is to pick the minimum
    } else if (sameIndexMode == "min") {
      multipleExposuresSameIndex <- multipleExposuresSameIndex %>%
        dplyr::group_by(
          .data$person_id, .data$subexposure_id, .data$drug_exposure_start_date
        ) %>%
        dplyr::filter(daily_dose == min(.data$daily_dose, na.rm = TRUE)) %>%
        dplyr::distinct() %>%
        dplyr::ungroup() %>%
        dplyr::compute()
    }
    # add again the merged same overlap into overlapedGroups
    multipleExposures <- multipleExposures %>%
      dplyr::union_all(multipleExposuresSameIndex) %>%
      dplyr::compute()
    # solve the overlap with different index dates group
    multipleExposures <- multipleExposures %>%
      dplyr::group_by(.data$person_id, .data$subexposure_id)
    # if the overlapMode is first (earliest exposure prevails)
    if (overlapMode == "first") {
      multipleExposures <- multipleExposures %>%
        dplyr::filter(
          .data$drug_exposure_start_date == min(
            .data$drug_exposure_start_date,
            na.rm = TRUE
          )
        )
      # if the overlapMode is second (latest exposure prevails)
    } else if (overlapMode == "second") {
      multipleExposures <- multipleExposures %>%
        dplyr::filter(
          .data$drug_exposure_start_date == max(
            .data$drug_exposure_start_date,
            na.rm = TRUE
          )
        )
      # if the overlapMode is max (exposure with more daily dose prevails)
    } else if (overlapMode == "max") {
      multipleExposures <- multipleExposures %>%
        dplyr::select(-"drug_exposure_start_date") %>%
        dplyr::filter(
          .data$daily_dose == max(.data$daily_dose, na.rm = TRUE)
        ) %>%
        dplyr::distinct()
      # if the overlapMode is sum (all exposure are considered)
    } else if (overlapMode == "sum") {
      multipleExposures <- multipleExposures %>%
        dplyr::select(-"drug_exposure_start_date") %>%
        dplyr::mutate(daily_dose = sum(.data$daily_dose, na.rm = TRUE)) %>%
        dplyr::distinct()
      # if the overlapMode is min (exposure with less daily dose prevails)
    } else if (overlapMode == "min") {
      multipleExposures <- multipleExposures %>%
        dplyr::select(-"drug_exposure_start_date") %>%
        dplyr::filter(
          .data$daily_dose == min(.data$daily_dose, na.rm = TRUE)
        ) %>%
        dplyr::distinct()
    }
    multipleExposures <- multipleExposures %>%
      dplyr::ungroup() %>%
      dplyr::select(
        "person_id", "subexposure_id", "daily_dose", "days_exposed",
        "start_interval", "end_interval"
      ) %>%
      dplyr::compute()

    if (uniqueExposures %>% dplyr::tally() %>% dplyr::pull() > 0) {
      uniqueExposures <- multipleExposures %>%
        dplyr::union(uniqueExposures) %>%
        dplyr::compute()
    } else {
      uniqueExposures <- multipleExposures
    }
  }
  # obtain first, cumulative and era time
  personSummary <- uniqueExposures %>%
    dplyr::group_by(.data$person_id) %>%
    dplyr::summarise(
      cohort_start_date = min(.data$start_interval, na.rm = TRUE),
      cohort_end_date = max(.data$end_interval, na.rm = TRUE),
      exposed_days = sum(.data$days_exposed, na.rm = TRUE),
      cumulative_dose = sum(.data$daily_dose * .data$days_exposed, na.rm = TRUE),
      subexposure_id = min(.data$subexposure_id, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(study_days = dbplyr::sql(sqlDiffDays(
      dialect,
      "cohort_start_date",
      "cohort_end_date"
    )) + 1) %>%
    dplyr::mutate(not_exposed_days = .data$study_days - .data$exposed_days) %>%
    dplyr::inner_join(uniqueExposures %>%
                        dplyr::rename("initial_dose" = "daily_dose") %>%
                        dplyr::select("person_id", "subexposure_id", "initial_dose"),
                      by = c("person_id", "subexposure_id")
    ) %>%
    dplyr::select(-"subexposure_id") %>%
    dplyr::left_join(exposureCounts,
                     by = c("person_id")
    ) %>%
    dplyr::compute()

  return(personSummary)
}

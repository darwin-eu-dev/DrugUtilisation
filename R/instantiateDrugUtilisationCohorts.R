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

#' Explain function
#'
#' @param cdm Connection to a database using DBI::dbConnect()
#' @param targetCohortName Name of the table in the cdm that contains the
#' target cohort
#' @param targetCohortId Indentifier for the analyzed target
#' cohorts
#' @param temporalWindows Temporal windows that we want to characterize
#' @param tablesToCharacterize Name of the tables in the cdm that we want to
#' summarize
#' @param characterizationTableName characterizationTableName
#' @param verbose verbose
#'
#' @return
#' @export
#'
#' @examples
instantiateDrugUtilisationCohorts <- function(cdm,
                                              specifications,
                                              studyTime = NULL,
                                              drugUtilisationCohortName,
                                              instantiateIncidencePrevalenceCohort = TRUE,
                                              imputeDuration = FALSE,
                                              imputeDailyDose = FALSE,
                                              durationLowerBound = NULL,
                                              durationUpperBound = NULL,
                                              dailyDoseLowerBound = NULL,
                                              dailyDoseUpperBound = NULL,
                                              verbose = FALSE) {
  drugUtilisationTableDataName <- paste0(drugUtilisationCohortName, "_info")
  incidencePrevalenceCohortName <- paste0(drugUtilisationCohortName, "_incprev")
  specifications <- specifications %>%
    dplyr::select(
      "drug_concept_id", "ingredient_concept_id",
      tidyselect::matches("default_duration"),
      tidyselect::matches("default_daily_dose"),
      tidyselect::matches("default_quantity")
    )
  if (isTRUE(is.na(studyTime))) {
    studyTime <- NULL
  }
  drugUtilisationCohort <- cdm[["drug_exposure"]] %>%
    dplyr::select(
      "person_id", "drug_concept_id", "drug_exposure_start_date",
      "drug_exposure_end_date", "quantity",
      tidyselect::matches("days_supply")
    ) %>%
    dplyr::inner_join(
      specifications,
      by = "drug_concept_id",
      copy = TRUE
    )
  if (isFALSE("days_supply" %in% colnames(cdm[["drug_exposure"]]))) {
    drugUtilisationCohort <- drugUtilisationCohort %>%
      dplyr::mutate(days_supply = dbplyr::sql(sqlDiffDays(
        CDMConnector::dbms(attr(cdm, "dbcon")),
        "drug_exposure_start_date",
        "drug_exposure_end_date"
      )) + 1)
  }

  # impute duration
  drugUtilisationCohort <- imputeVariable(
    x = drugUtilisationCohort,
    variableName = "days_supply",
    impute = imputeDuration,
    lowerBound = durationLowerBound,
    upperBound = durationUpperBound,
    imputeValueName = "default_duration",
    allowZero = FALSE)

  # compute the daily dose
  drugUtilisationCohort <- computeDailyDose(
    table = drugUtilisationCohort,
    cdm = cdm,
    verbose = verbose
  )

  # impute daily_dose
  drugUtilisationCohort <- imputeVariable(
    x = drugUtilisationCohort,
    variableName = "daily_dose",
    impute = imputeDuration,
    lowerBound = dailyDoseLowerBound,
    upperBound = dailyDoseUpperBound,
    imputeValueName = "default_daily_dose",
    allowZero = TRUE)

  drugUtilisationCohort <- drugUtilisationCohort %>%
    dplyr::rename(
      "cohort_start_date" = "drug_exposure_start_date",
      "cohort_end_date" = "drug_exposure_start_date"
    ) %>%
    dplyr::select(
      "person_id", "daily_dose", "cohort_start_date", "cohort_end_date"
    ) %>%
    dplyr::compute()

  # compute cumulative dose per person
  cumulativeDoseNoRestrictions <- drugUtilisationCohort %>%
    dplyr::group_by(.data$person_id) %>%
    dplyr::summarise(
      cumulative_dose = sum(
        .data$daily_dose * (.data$cohort_end_date - .data$cohort_start_date + 1)
      )
    ) %>%
    dplyr::compute()

  drugUtilisationCohort <- getNonOverlapedExposures(
    interestExposures = drugUtilisationCohort,
    overlapMode = overlapMode,
    sameIndexMode = sameIndexMode,
    instantiateIncidencePrevalenceCohort = instantiateIncidencePrevalenceCohort,
    gapEra = gapEra,
    incidencePrevalenceCohortName = incidencePrevalenceCohortName,
    verbose = verbose
  )
}

#' Get drose cohorts without overlaping.
#'
#' @param interestExposures interestExposures
#' @param overlapMode overlapMode
#' @param sameIndexMode sameIndexMode
#' @param instantiateIncidencePrevalenceCohort
#' instantiateIncidencePrevalenceCohort
#' @param gapEra gapEra
#' @param incidencePrevalenceCohortName incidencePrevalenceCohortName
#' @param verbose verbose
#'
#' @noRd
getNonOverlapedExposures <- function(interestExposures,
                                     overlapMode,
                                     sameIndexMode,
                                     instantiateIncidencePrevalenceCohort,
                                     gapEra,
                                     incidencePrevalenceCohortName,
                                     verbose) {
  # compute the start of possible overlapping periods
  overlap_intervals_start <- interestExposures %>%
    dplyr::select("person_id", "cohort_start_date") %>%
    dplyr::distinct() %>%
    dplyr::rename("start_overlap" = "cohort_start_date") %>%
    dplyr::union(
      interestExposures %>%
        dplyr::select("person_id", "cohort_end_date") %>%
        dplyr::distinct() %>%
        dplyr::mutate(
          start_overlap = .data$cohort_end_date + lubridate::days(1)
        ) %>%
        dplyr::select(-"cohort_end_date")
    ) %>%
    dplyr::group_by(.data$person_id) %>%
    dplyr::filter(
      .data$start_overlap < max(.data$start_overlap, na.rm = TRUE)
    ) %>%
    dplyr::arrange(.data$start_overlap) %>%
    dplyr::mutate(overlap_subgroup = dplyr::row_number()) %>%
    dplyr::ungroup() %>%
    dplyr::compute()
  # compute the start of possible overlapping periods
  overlap_intervals_end <- interestExposures %>%
    dplyr::select("person_id", "cohort_start_date") %>%
    dplyr::distinct() %>%
    dplyr::mutate(
      end_overlap = .data$cohort_start_date - lubridate::days(1)
    ) %>%
    dplyr::select(-"cohort_start_date") %>%
    dplyr::union(interestExposures %>%
      dplyr::select("person_id", "cohort_end_date") %>%
      dplyr::distinct() %>%
      dplyr::rename("end_overlap" = "cohort_end_date")) %>%
    dplyr::group_by(.data$person_id) %>%
    dplyr::filter(
      .data$end_overlap > min(.data$end_overlap, na.rm = TRUE)
    ) %>%
    dplyr::arrange(.data$end_overlap) %>%
    dplyr::mutate(overlap_subgroup = dplyr::row_number()) %>%
    dplyr::ungroup() %>%
    dplyr::compute()
  # compute the overlapping periods joining start and end dates
  overlap_intervals <- overlap_intervals_start %>%
    dplyr::inner_join(
      overlap_intervals_end,
      by = c("person_id", "overlap_subgroup")
    ) %>%
    dplyr::compute()
  # we join the exposures with the overlapping periods and we only consider the
  # exposures that contribute to each overlapping period
  overlap_groups <- overlap_intervals %>%
    dplyr::inner_join(
      interestExposures %>%
        dplyr::mutate(exposure_id = dplyr::row_number()),
      by = "person_id"
    ) %>%
    dplyr::filter(.data$cohort_start_date <= .data$start_overlap) %>%
    dplyr::filter(.data$cohort_end_date >= .data$end_overlap) %>%
    dplyr::group_by(.data$person_id, .data$overlap_subgroup) %>%
    dplyr::mutate(n_ind_subgroup = dplyr::n()) %>%
    dplyr::ungroup() %>%
    dplyr::compute()
  # the overlapping subgroups are grouped into groups of continuous exposure,
  # see the documentation for a more detailed explanation of the difference
  # between overlapping group and subgroups
  overlap_groups <- overlap_groups %>%
    dplyr::left_join(
      overlap_groups %>%
        dplyr::mutate(overlap_subgroup = .data$overlap_subgroup - 1) %>%
        dplyr::rename("next_overlap_start" = "overlap_start") %>%
        dplyr::select("person_id", "overlap_subgroup", "next_overlap_start"),
      by = c("person_id", "overlap_subgroup")
    ) %>%
    dplyr::mutate(index_group = dplyr::if_else(
      is.na(.data$next_overlap_start),
      0,
      dplyr::if_else(
        .data$overlap_end + 1 == .data$next_overlap_start,
        0,
        1
      )
    )) %>%
    dplyr::group_by(.data$person_id) %>%
    dplyr::arrange(.data$overlap_subgroup) %>%
    dplyr::mutate(overlap_group = 1 + cumsum(.data$index_group)) %>%
    dplyr::select(-"index_group") %>%
    dplyr::compute()
  # instantiate the cohorts if we want to compute incidence and prevalence
  if (instantiateIncidencePrevalenceCohort == TRUE) {
    # compute the exposures to instantiate incidence prevalence cohort
    incidencePrevelenceExposures <- overlap_groups %>%
      dplyr::group_by(.data$person_id, .data$overlap_group) %>%
      dplyr::summarise(
        cohort_start_date = min(.data$overlap_start, na.rm = TRUE),
        cohort_end_date = min(.data$overlap_end, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      dplyr::compute()
    cdm <- computeIncidencePrevalenceCohorts(
      cdm = cdm,
      interestExposures = incidencePrevelenceExposures,
      gapEra = gapEra,
      incidencePrevalenceCohortName = incidencePrevalenceCohortName,
      verbose = verbose
    )
  }
  # save number of exposures and number of groups and subgroups
  exposureCounts <- overlap_groups %>%
    dplyr::group_by(.data$person_id) %>%
    dplyr::summarise(
      number_exposures = dplyr::n_distinct(.data$exposure_id),
      number_groups_with_overlap = dplyr::n_distinct(
        .data$overlap_group[.data$number_individuals_subgroup > 1]
      ),
      number_groups_no_overlap = dplyr::n_distinct(.data$overlap_group),
      number_subgroups_with_overlap = dplyr::n_distinct(
        .data$overlap_sub_group[.data$number_individuals_subgroup > 1]
      ),
      number_subgroups_no_overlap = dplyr::n_distinct(.data$overlap_sub_group),
      number_subgroups = dplyr::n_distinct(.data$overlap_subgroup),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      number_groups_no_overlap =
        .data$number_groups_no_overlap - .data$number_group_with_overlap
    ) %>%
    dplyr::mutate(
      number_subgroups_no_overlap =
        .data$number_subgroups_no_overlap - .data$number_subgroup_with_overlap
    ) %>%
    dplyr::compute()
  # get the subgroups without overlap
  notOverlapedGroups <- overlap_groups %>%
    dplyr::filter(.data$number_individuals_subgroup == 1) %>%
    dplyr::select(-"number_individuals_subgroup", -"exposure_id") %>%
    dplyr::mutate(
      exposed_days = as.numeric(
        difftime(.data$end_overlap, .data$start_overlap, units = "days")
      ) + 1
    ) %>%
    dplyr::mutate(cumulative_dose = daily_dose * exposed_days) %>%
    dplyr::select(
      "person_id", "overlap_group", "overlap_subgroup",
      "daily_dose", "exposed_days", "cumulative_dose", "start_overlap",
      "end_overlap"
    ) %>%
    dplyr::compute()

  # get the overlapped groups
  overlapedGroups <- overlap_groups %>%
    dplyr::filter(.data$number_individuals_subgroup > 1) %>%
    dplyr::select(-"number_individuals_subgroup", -"exposure_id") %>%
    dplyr::compute()

  if (overlaped_groups %>% dplyr::tally() %>% dplyr::pull() > 0) {

    # search individuals with same index date in subgroups
    overlapedGroupsSameIndex <- overlapedGroups %>%
      dplyr::group_by(
        .data$person_id, .data$overlap_subgroup, .data$cohort_start_date
      ) %>%
      dplyr::mutate(n_same_index = dplyr::n()) %>%
      dplyr::ungroup() %>%
      dplyr::filter(.data$n_same_index > 1) %>%
      dplyr::select(-"n_same_index") %>%
      dplyr::compute()

    # get the overlaps with different index date
    overlapedGroups <- overlapedGroups %>%
      dplyr::anti_join(overlapedGroupsSameIndex,
        by = c(
          "person_id", "overlap_group", "overlap_subgroup",
          "cohort_start_date"
        )
      ) %>%
      dplyr::compute()

    # solve the overlap with same index date
    # if criteria is to pick maximum
    if (sameIndexMode == "max") {
      overlapedGroupsSameIndex <- overlapedGroupsSameIndex %>%
        dplyr::group_by(
          .data$person_id, .data$overlap_subgroup,
          .data$cohort_start_date
        ) %>%
        dplyr::filter(dose == max(.data$dose, na.rm = TRUE)) %>%
        dplyr::distinct() %>%
        dplyr::ungroup() %>%
        dplyr::compute()
      # if criteria is to pick the sum
    } else if (same_index_mode == "sum") {
      overlaped_groups_same_index <- overlaped_groups_same_index %>%
        dplyr::group_by(
          .data$person_id, .data$overlap_subgroup,
          .data$cohort_start_date
        ) %>%
        dplyr::mutate(dose = sum(.data$dose, na.rm = TRUE)) %>%
        dplyr::distinct() %>%
        dplyr::ungroup() %>%
        dplyr::compute()
      # if criteria is to pick the minimum
    } else if (sameIndexMode == "min") {
      overlapedGroupsSameIndex <- overlapedGroupsSameIndex %>%
        dplyr::group_by(
          .data$person_id, .data$overlap_subgroup,
          .data$cohort_start_date
        ) %>%
        dplyr::filter(dose == min(.data$dose, na.rm = TRUE)) %>%
        dplyr::distinct() %>%
        dplyr::ungroup() %>%
        dplyr::compute()
    }
    # add again the merged same overlap into overlapedGroups
    overlapedGroups <- overlapedGroups %>%
      dplyr::union_all(overlapedGroupsSameIndex) %>%
      dplyr::compute()
    # solve the overlap with different index dates group
    overlapedGroups <- overlapedGroups %>%
      dplyr::group_by(.data$person_id, .data$overlap_subgroup)
    # if the overlapMode is first (earliest exposure prevails)
    if (overlapMode == "first") {
      overlapedGroups <- overlapedGroups %>%
        dplyr::filter(
          .data$cohort_start_date == min(.data$cohort_start_date, na.rm = TRUE)
        )
      # if the overlapMode is second (latest exposure prevails)
    } else if (overlapMode == "second") {
      overlapedGroups <- overlapedGroups %>%
        dplyr::filter(
          .data$cohort_start_date == max(.data$cohort_start_date, na.rm = TRUE)
        )
      # if the overlapMode is max (exposure with more daily dose prevails)
    } else if (overlapMode == "max") {
      overlapedGroups <- overlapedGroups %>%
        dplyr::filter(.data$daily_dose == max(.data$daily_dose, na.rm = TRUE))
      # if the overlapMode is sum (all exposure are considered)
    } else if (overlapMode == "sum") {
      overlapedGroups <- overlapedGroups %>%
        dplyr::mutate(daily_dose = sum(.data$daily_dose, na.rm = TRUE))
      # if the overlapMode is min (exposure with less daily dose prevails)
    } else if (overlapMode == "min") {
      overlapedGroups <- overlapedGroups %>%
        dplyr::filter(.data$daily_dose == min(.data$daily_dose, na.rm = TRUE))
    }
    overlapedGroups <- overlapedGroups %>%
      dplyr::select(
        "person_id", "overlap_group", "overlap_subgroup",
        "start_overlap", "end_overlap", "daily_dose"
      ) %>%
      dplyr::distinct() %>%
      dplyr::ungroup() %>%
      dplyr::mutate(exposed_days = as.numeric(difftime(.data$end_overlap,
        .data$start_overlap,
        units = "days"
      )) + 1) %>%
      dplyr::mutate(cumulative_dose = daily_dose * exposed_days) %>%
      dplyr::compute()

    if (not_overlaped_groups %>% dplyr::tally() %>% dplyr::pull() > 0) {
      overlaped_groups <- overlaped_groups %>%
        dplyr::union(not_overlaped_groups) %>%
        dplyr::compute()
    }
  } else {
    overlaped_groups <- not_overlaped_groups
  }
  # obtain first, cumulative and era time
  overlaped_groups <- overlaped_groups %>%
    dplyr::group_by(.data$person_id, .data$overlap_group) %>%
    dplyr::summarise(
      start_overlap = min(.data$start_overlap),
      end_overlap = max(.data$end_overlap),
      exposed_days = sum(.data$exposed_days),
      cumulative_dose = sum(.data$cumulative_dose),
      overlap_subgroup = min(.data$overlap_subgroup),
      .groups = "drop"
    ) %>%
    dplyr::inner_join(overlaped_groups %>%
      dplyr::rename("initial_dose" = "daily_dose") %>%
      dplyr::select("person_id", "overlap_group", "overlap_subgroup", "initial_dose"),
    by = c("person_id", "overlap_group", "overlap_subgroup")
    ) %>%
    dplyr::left_join(overlap_exposure_counts,
      by = c("person_id", "overlap_group")
    ) %>%
    dplyr::left_join(overlap_period_counts,
      by = c("person_id", "overlap_group")
    ) %>%
    dplyr::mutate(N_overlap_periods = dplyr::if_else(
      is.na(N_overlap_periods), as.integer(0), .data$N_overlap_periods, as.integer(0)
    )) %>%
    dplyr::select(
      "person_id", "overlap_group", "start_overlap", "end_overlap",
      "initial_dose", "exposed_days", "cumulative_dose", "N_exposures",
      "N_overlap_periods"
    ) %>%
    dplyr::inner_join(
      overlap_groups_total_dose,
      by = c("person_id", "start_overlap")
    ) %>%
    dplyr::mutate(
      not_considered_cumulative_dose =
        .data$not_considered_cumulative_dose - .data$cumulative_dose
    ) %>%
    dplyr::compute()

  cdm[[nonOverlapedExposuresName]] <- overlaped_groups

  return(cdm)
}

#' Get the cohorts to compute incidence and prevalence
#'
#' @param interestExposures interestExposures
#' @param gapEra gapEra
#' @param incidencePrevalenceCohortName incidencePrevalenceCohortName
#' @param verbose verbose
#'
#' @noRd
computeIncidencePrevalenceCohorts <- function(cdm,
                                              interestExposures,
                                              gapEra,
                                              incidencePrevalenceCohortName,
                                              verbose) {
  # join the exposures to obtain the era table
  interestExposures <- interestExposures %>%
    dplyr::left_join(
      interestExposures %>%
        dplyr::mutate(overlap_group = .data$overlap_group - 1) %>%
        dplyr::rename("next_exposure" = "cohort_start_date") %>%
        dplyr::select("person_id", "overlap_group", "next_exposure"),
      by = c("person_id", "overlap_group")
    ) %>%
    dplyr::mutate(era_index = dplyr::if_else(
      is.na(.data$next_exposure),
      0,
      dplyr::if_else(
        .data$next_exposure - .data$cohort_end_date - 1 <= .env$gapEra,
        0,
        1
      )
    )) %>%
    dplyr::group_by(.data$person_id) %>%
    dplyr::arrange(.data$overlap_group) %>%
    dplyr::mutate(era_group = cumsum(era_index)) %>%
    dplyr::summarise(
      cohort_start_date = min(.data$cohort_start_date, na.rm = TRUE),
      cohort_end_date = max(.data$cohort_end_date, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::rename("subject_id" = "person_id") %>%
    dplyr::mutate(cohort_definition_id = 1) %>%
    dplyr::select(
      "cohort_definition_id", "subject_id", "cohort_start_date",
      "cohort_end_date"
    ) %>%
    dplyr::compute()
  # get the query to instantiate the table
  sql_query <- paste0(
    "SELECT * INTO",
    attr(cdm, "write_schema"),
    ".",
    incidencePrevalenceCohortName,
    " FROM (",
    dbplyr::sql_render(PASC_cohort_table),
    ") AS from_table"
  )
  # execute the query to instantiate the table
  DBI::dbExecute(db, as.character(sql_query))
  # make the table visible in the current cdm object
  cdm[[incidencePrevalenceCohortName]] <- dplyr::tbl(
    attr(cdm, "dbcon"),
    paste0(
      "SELECT * FROM ",
      attr(cdm, "write_schema"),
      ".",
      incidencePrevalenceCohortName
    )
  )
  # return cdm
  return(cdm)
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
                           allowZero){
  x <- x %>%
    dplyr::rename("variable" = .env$variableName)
  # impute if allow zero
  if (isTRUE(allowZero)){
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

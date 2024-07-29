# Copyright 2024 DARWIN EU (C)
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

#' Add new columns with drug use related information
#'
#' `r lifecycle::badge("deprecated")`
#'
#' @param cohort Cohort in the cdm
#' @param cdm deprecated
#' @param ingredientConceptId Ingredient OMOP concept that we are interested for
#' the study. It is a compulsory input, no default value is provided.
#' @param conceptSet List of concepts to be included. If NULL all the
#' descendants of ingredient concept id will be used.
#' @param duration Whether to add duration related columns.
#' @param quantity Whether to add quantity related columns.
#' @param dose Whether to add dose related columns.
#' @param gapEra Number of days between two continuous exposures to be
#' considered in the same era.
#' @param eraJoinMode How two different continuous exposures are joined in an
#' era. There are four options:
#' "zero" the exposures are joined considering that the period between both
#' continuous exposures the subject is treated with a daily dose of zero. The
#' time between both exposures contributes to the total exposed time.
#' "join" the exposures are joined considering that the period between both
#' continuous exposures the subject is treated with a daily dose of zero. The
#' time between both exposures does not contribute to the total exposed time.
#' "previous" the exposures are joined considering that the period between both
#' continuous exposures the subject is treated with the daily dose of the
#' previous subexposure. The time between both exposures contributes to the
#' total exposed time.
#' "subsequent" the exposures are joined considering that the period between
#' both continuous exposures the subject is treated with the daily dose of the
#' subsequent subexposure. The time between both exposures contributes to the
#' total exposed time.
#' @param overlapMode How the overlapping between two exposures that do not
#' start on the same day is solved inside a subexposure. There are five possible
#'  options:
#' "previous" the considered daily_dose is the one of the earliest exposure.
#' "subsequent" the considered daily_dose is the one of the new exposure that
#' starts in that subexposure.
#' "minimum" the considered daily_dose is the minimum of all of the exposures in
#' the subexposure.
#' "maximum" the considered daily_dose is the maximum of all of the exposures in
#' the subexposure.
#' "sum" the considered daily_dose is the sum of all the exposures present in
#' the subexposure.
#' @param sameIndexMode How the overlapping between two exposures that start on
#' the same day is solved inside a subexposure. There are three possible options:
#' "minimum" the considered daily_dose is the minimum of all of the exposures in
#' the subexposure.
#' "maximum" the considered daily_dose is the maximum of all of the exposures in
#' the subexposure.
#' "sum" the considered daily_dose is the sum of all the exposures present in
#' the subexposure.
#' @param imputeDuration Whether/how the duration should be imputed
#' "none", "median", "mean", "mode" or a number
#' @param imputeDailyDose Whether/how the daily_dose should be imputed
#' "none", "median", "mean", "mode" or a number
#' @param durationRange Range between the duration must be comprised. It should
#' be a numeric vector of length two, with no NAs and the first value should be
#' equal or smaller than the second one. It must not be NULL if imputeDuration
#' is not "none". If NULL no restrictions are applied.
#' @param dailyDoseRange Range between the daily_dose must be comprised. It
#' should be a numeric vector of length two, with no NAs and the first value
#' should be equal or smaller than the second one. It must not be NULL if
#' imputeDailyDose is not "none". If NULL no restrictions are applied.
#'
#' @return The same cohort with the added columns.
#'
#' @export
#'
#' @examples
#' \donttest{
#' library(DrugUtilisation)
#'
#' cdm <- mockDrugUtilisation()
#' codelist <- CodelistGenerator::getDrugIngredientCodes(
#'   cdm,
#'   name = "acetaminophen"
#' )
#'
#' cdm <- generateDrugUtilisationCohortSet(cdm, "dus_cohort", codelist)
#' cdm[["dus_cohort"]] |>
#'   addDrugUse(ingredientConceptId = 1125315)
#' }
#'
addDrugUse <- function(cohort,
                       cdm = lifecycle::deprecated(),
                       ingredientConceptId,
                       conceptSet = NULL,
                       duration = TRUE,
                       quantity = TRUE,
                       dose = TRUE,
                       gapEra = 0,
                       eraJoinMode = "zero",
                       overlapMode = "sum",
                       sameIndexMode = "sum",
                       imputeDuration = "none",
                       imputeDailyDose = "none",
                       durationRange = c(1, Inf),
                       dailyDoseRange = c(0, Inf)) {
  lifecycle::deprecate_soft(
    when = "0.7.0",
    what = "DrugUtilisation::addDrugUse()",
    with = "DrugUtilisation::addDrugUtilisation()"
  )
  if (lifecycle::is_present(cdm)) {
    lifecycle::deprecate_soft("0.5.0", "addDrugUse(cdm = )")
  }
  cdm <- omopgenerics::cdmReference(cohort)

  vars <- c(
    "eraJoinMode", "overlapMode", "sameIndexMode", "imputeDuration",
    "imputeDailyDose"
  )
  for (char in vars) {
    if (is.character(get(char))) {
      assign(char, tolower(get(char)))
    }
  }

  if (length(conceptSet) > 1) {
    cli::cli_abort("Only one concept set should be provided")
  }
  if (is.null(conceptSet)) {
    checkInputs(ingredientConceptId = ingredientConceptId, cdm = cdm)
    conceptSet <- list(
      cdm[["drug_strength"]] |>
        dplyr::filter(.data$ingredient_concept_id == .env$ingredientConceptId) |>
        dplyr::pull("drug_concept_id")
    )
    names(conceptSet) <- cdm[["concept"]] |>
      dplyr::filter(.data$concept_id == .env$ingredientConceptId) |>
      dplyr::pull("concept_name")
  }

  # initial checks
  checkInputs(
    cohort = cohort, cdm = cdm, ingredientConceptId = ingredientConceptId,
    conceptSet = conceptSet, duration = duration, quantity = quantity,
    dose = dose, gapEra = gapEra, eraJoinMode = eraJoinMode,
    overlapMode = overlapMode, sameIndexMode = sameIndexMode,
    imputeDuration = imputeDuration, imputeDailyDose = imputeDailyDose,
    durationRange = durationRange, dailyDoseRange = dailyDoseRange
  )

  if (length(conceptSet) > 1) {
    cli::cli_abort("Only one concept set is allowed")
  }

  # save original reference
  originalCohort <- cohort

  # unique cohort entries
  cohort <- cohort |>
    dplyr::select("subject_id", "cohort_start_date", "cohort_end_date") |>
    dplyr::distinct() |>
    addDuration(duration) |>
    dplyr::compute(
      temporary = FALSE, overwrite = TRUE, name = uniqueTmpName()
    )

  # subset drug_exposure and only get the drug concept ids that we are
  # interested in.
  conceptSet <- conceptSet |>
    unlist() |>
    unname() |>
    dplyr::as_tibble() |>
    dplyr::rename("drug_concept_id" = "value")
  nm <- uniqueTmpName()
  cdm <- omopgenerics::insertTable(
    cdm = cdm, name = nm, table = conceptSet, overwrite = TRUE
  )
  cdm[[nm]] <- cdm[[nm]] |> dplyr::compute()
  cohortInfo <- initialSubset(cdm, cohort, cdm[[nm]])

  cohort <- cohort |>
    addInfo(cohortInfo, quantity, cdm)

  # correct duration
  cohortInfo <- cohortInfo %>%
    dplyr::mutate(duration = !!CDMConnector::datediff(
      start = "drug_exposure_start_date", end = "drug_exposure_end_date"
    ) + 1) |>
    rowsToImpute("duration", durationRange)

  cohort <- cohort |>
    addImpute(cohortInfo, duration, "impute_duration_percentage")

  cohortInfo <- cohortInfo |>
    solveImputation("duration", imputeDuration, TRUE) |>
    dplyr::mutate(days_to_add = as.integer(.data$duration - 1)) %>%
    dplyr::mutate(drug_exposure_end_date = !!CDMConnector::dateadd(
      date = "drug_exposure_start_date", number = "days_to_add"
    )) |>
    dplyr::select(-c("duration", "days_to_add")) |>
    dplyr::compute(
      temporary = FALSE, overwrite = TRUE, name = uniqueTmpName()
    )

  # add number eras
  cohort <- addNumberErasDrugUse(cohort, cohortInfo, gapEra, cdm)

  if (dose) {
    # add daily dose
    cohortInfo <- cohortInfo |>
      .addDailyDose(ingredientConceptId = ingredientConceptId) |>
      dplyr::select(-"quantity") |>
      dplyr::distinct()

    # impute daily dose
    cohortInfo <- cohortInfo |> rowsToImpute("daily_dose", dailyDoseRange)

    cohort <- cohort |>
      addImpute(cohortInfo, TRUE, "impute_daily_dose_percentage")

    cohortInfo <- cohortInfo |>
      solveImputation("daily_dose", imputeDailyDose) |>
      dplyr::compute(
        temporary = FALSE, overwrite = TRUE, name = uniqueTmpName()
      )

    # get distinct units to cover
    units <- cohortInfo |>
      dplyr::select("unit") |>
      dplyr::distinct() |>
      dplyr::filter(!is.na(.data$unit)) |>
      dplyr::pull()

    cohort <- cohort |>
      addInitialDailyDoseDrugUse(cohortInfo, sameIndexMode, units, cdm) |>
      addCumulativeDoseDrugUse(
        cohortInfo, cdm, gapEra, sameIndexMode, overlapMode, eraJoinMode, units
      )
  }

  # add result
  cohort <- originalCohort |>
    dplyr::left_join(
      cohort,
      by = c("subject_id", "cohort_start_date", "cohort_end_date")
    ) |>
    dplyr::compute()

  dropTmpTables(cdm = cdm)

  return(cohort)
}

addDuration <- function(cohort, duration) {
  if (duration) {
    cohort <- cohort %>%
      dplyr::mutate(duration = !!CDMConnector::datediff(
        "cohort_start_date", "cohort_end_date"
      ) + 1)
  }
  return(cohort)
}

addInfo <- function(cohort,
                    cohortInfo,
                    quantity,
                    cdm) {
  cohort <- cohort |>
    dplyr::left_join(
      cohortInfo |>
        dplyr::group_by(
          .data$subject_id, .data$cohort_start_date,
          .data$cohort_end_date
        ) |>
        dplyr::summarise(
          number_exposures = dplyr::n(),
          cumulative_quantity = sum(.data$quantity, na.rm = TRUE),
          .groups = "drop"
        ),
      by = c(
        "subject_id", "cohort_start_date", "cohort_end_date"
      ),
    ) |>
    dplyr::mutate(number_exposures = dplyr::if_else(
      is.na(.data$number_exposures), 0, .data$number_exposures
    )) |>
    dplyr::compute(
      temporary = FALSE, overwrite = TRUE, name = uniqueTmpName()
    )
  if (quantity) {
    cohort <- cohort |>
      dplyr::left_join(
        cohortInfo |>
          dplyr::group_by(
            .data$subject_id, .data$cohort_start_date,
            .data$cohort_end_date
          ) |>
          dplyr::filter(
            .data$drug_exposure_start_date <= .data$cohort_start_date
          ) |>
          dplyr::summarise(
            initial_quantity = sum(.data$quantity, na.rm = TRUE),
            .groups = "drop"
          ),
        by = c(
          "subject_id", "cohort_start_date", "cohort_end_date"
        ),
      ) |>
      dplyr::compute(
        temporary = FALSE, overwrite = TRUE, name = uniqueTmpName()
      )
  } else {
    cohort <- cohort |> dplyr::select(-"cumulative_quantity")
  }
  return(cohort)
}

addInitialDailyDoseDrugUse <- function(cohort,
                                       cohortInfo,
                                       sameIndexMode,
                                       units,
                                       cdm) {
  if (length(units) == 0) {
    cohortInfo <- cohortInfo |>
      dplyr::select("subject_id", "cohort_start_date", "cohort_end_date") |>
      dplyr::mutate("initial_daily_dose" = as.numeric(NA))
  } else {
    cohortInfo <- cohortInfo |>
      dplyr::group_by(
        .data$subject_id, .data$cohort_start_date,
        .data$cohort_end_date, .data$unit
      ) |>
      dplyr::filter(
        .data$drug_exposure_start_date <= .data$cohort_start_date
      )
    if (sameIndexMode == "sum") {
      cohortInfo <- cohortInfo |>
        dplyr::summarise(
          initial_daily_dose = sum(.data$daily_dose, na.rm = TRUE),
          .groups = "drop"
        )
    } else if (sameIndexMode == "min") {
      cohortInfo <- cohortInfo |>
        dplyr::summarise(
          initial_daily_dose = min(.data$daily_dose, na.rm = TRUE),
          .groups = "drop"
        )
    } else {
      cohortInfo <- cohortInfo |>
        dplyr::summarise(
          initial_daily_dose = max(.data$daily_dose, na.rm = TRUE),
          .groups = "drop"
        )
    }
    for (u in units) {
      cohortInfo <- cohortInfo |>
        dplyr::mutate(
          !!paste0("initial_daily_dose_", u) := dplyr::if_else(
            .data$unit == .env$u, .data$initial_daily_dose, NA
          )
        )
    }
    cohortInfo <- cohortInfo |>
      dplyr::select(-"initial_daily_dose", -"unit")
  }
  cohort <- cohort |>
    dplyr::left_join(
      cohortInfo,
      by = c("subject_id", "cohort_start_date", "cohort_end_date")
    ) |>
    dplyr::compute(
      temporary = FALSE, overwrite = TRUE, name = uniqueTmpName()
    )
  return(cohort)
}

addNumberErasDrugUse <- function(cohort, cohortInfo, gapEra, cdm) {
  cohort |>
    dplyr::left_join(
      cohortInfo |>
        dplyr::select(
          "subject_id", "cohort_start_date", "cohort_end_date",
          "date_event" = "drug_exposure_start_date"
        ) |>
        dplyr::mutate(value = -1) |>
        dplyr::union_all(
          cohortInfo |>
            dplyr::select(
              "subject_id", "cohort_start_date", "cohort_end_date",
              "date_event" = "drug_exposure_end_date"
            ) %>%
            dplyr::mutate(
              value = 1,
              date_event = !!CDMConnector::dateadd("date_event", gapEra + 1)
            )
        ) |>
        dplyr::group_by(
          .data$subject_id, .data$cohort_start_date, .data$cohort_end_date
        ) |>
        dbplyr::window_order(.data$date_event, .data$value) |>
        dplyr::filter(cumsum(.data$value) == 0) |>
        dplyr::summarise(
          number_eras = dplyr::n(), .groups = "drop"
        ),
      by = c("subject_id", "cohort_start_date", "cohort_end_date")
    ) |>
    dplyr::mutate(number_eras = dplyr::if_else(
      is.na(.data$number_eras), 0, .data$number_eras
    )) |>
    dplyr::compute(
      temporary = FALSE, overwrite = TRUE, name = uniqueTmpName()
    )
}

addCumulativeDoseDrugUse <- function(cohort,
                                     cohortInfo,
                                     cdm,
                                     gapEra,
                                     sameIndexMode,
                                     overlapMode,
                                     eraJoinMode,
                                     units) {
  if (length(units) == 0) {
    cumDose <- cohortInfo |>
      dplyr::select(
        "subject_id", "cohort_start_date", "cohort_end_date"
      ) |>
      dplyr::distinct() |>
      dplyr::mutate("cumulative_dose" = as.numeric(NA))
  } else {
    # split the exposures in subexposures inside each cohort
    cohortInfo <- splitSubexposures(cohortInfo, cdm)

    # add the overlapping flag
    cohortInfo <- addOverlappingFlag(cohortInfo)

    # add the type of subexposure
    cohortInfo <- addTypeSubexposure(cohortInfo, gapEra)

    # solve same index day overlapping
    cohortInfo <- solveSameIndexOverlap(cohortInfo, cdm, sameIndexMode)

    # solve not same index overlapping
    cohortInfo <- solveOverlap(cohortInfo, cdm, overlapMode)

    # add daily dose to gaps
    cohortInfo <- addGapDailyDose(cohortInfo, cdm, eraJoinMode)

    # add cumulative dose
    cumDose <- cohortInfo |>
      dplyr::mutate(
        exposed_dose = .data$daily_dose * .data$subexposed_days
      ) |>
      dplyr::group_by(
        .data$subject_id, .data$cohort_start_date, .data$cohort_end_date,
        .data$unit
      ) |>
      dplyr::summarise(
        cumulative_dose = sum(
          .data$exposed_dose[.data$considered_subexposure == "yes"],
          na.rm = TRUE
        ),
        .groups = "drop"
      )
    for (u in units) {
      cumDose <- cumDose |>
        dplyr::mutate(
          !!paste0("cumulative_dose_", u) := dplyr::if_else(
            .data$unit == .env$u, .data$cumulative_dose, NA
          )
        )
    }
    cumDose <- cumDose |> dplyr::select(-"cumulative_dose", -"unit")
  }

  cohort |>
    dplyr::left_join(
      cumDose,
      by = c("subject_id", "cohort_start_date", "cohort_end_date")
    ) |>
    dplyr::compute(
      temporary = FALSE, overwrite = TRUE, name = uniqueTmpName()
    )
}

#' @noRd
initialSubset <- function(cdm, dusCohort, conceptSet) {
  cdm[["drug_exposure"]] |>
    dplyr::select(
      "subject_id" = "person_id",
      "drug_concept_id",
      "drug_exposure_id",
      "drug_exposure_start_date",
      "drug_exposure_end_date",
      "quantity"
    ) |>
    dplyr::inner_join(dusCohort, by = "subject_id") |>
    dplyr::inner_join(conceptSet, by = "drug_concept_id") |>
    dplyr::filter(
      (is.na(.data$drug_exposure_end_date) &
        (.data$drug_exposure_start_date <= .data$cohort_end_date)) |
        (!is.na(.data$drug_exposure_end_date) &
          ((.data$drug_exposure_end_date >= .data$cohort_start_date) &
            (.data$drug_exposure_start_date <= .data$cohort_end_date)))
    ) |>
    dplyr::compute(
      temporary = FALSE, overwrite = TRUE, name = uniqueTmpName()
    )
}

#' @noRd
splitSubexposures <- function(x, cdm) {
  x_intervals <- x |>
    dplyr::select(
      "subject_id", "cohort_start_date", "cohort_end_date",
      "date_event" = "drug_exposure_start_date"
    ) |>
    dplyr::distinct() |>
    dplyr::union_all(
      x |>
        dplyr::select(
          "subject_id", "cohort_start_date", "cohort_end_date",
          "date_event" = "drug_exposure_start_date"
        ) |>
        dplyr::distinct() %>%
        dplyr::mutate(date_event = as.Date(!!CDMConnector::dateadd(
          "date_event", -1
        )))
    ) |>
    dplyr::union_all(
      x |>
        dplyr::select(
          "subject_id", "cohort_start_date", "cohort_end_date",
          "date_event" = "drug_exposure_end_date"
        ) |>
        dplyr::distinct()
    ) |>
    dplyr::union_all(
      x |>
        dplyr::select(
          "subject_id", "cohort_start_date", "cohort_end_date",
          "date_event" = "drug_exposure_end_date"
        ) |>
        dplyr::distinct() %>%
        dplyr::mutate(date_event = as.Date(!!CDMConnector::dateadd(
          "date_event", 1
        )))
    ) |>
    dplyr::group_by(
      .data$subject_id, .data$cohort_start_date, .data$cohort_end_date
    ) |>
    dbplyr::window_order(.data$date_event) |>
    dplyr::mutate(id1 = dplyr::row_number()) |>
    dplyr::mutate(
      id2 = floor(1.25 + 0.5 * .data$id1),
      date_type = dplyr::if_else(
        .data$id1 %% 2 == 0,
        "subexposure_start_date",
        "subexposure_end_date"
      )
    ) |>
    dplyr::select(-"id1") |>
    dplyr::filter(.data$date_event >= .data$cohort_start_date) |>
    dplyr::filter(.data$date_event <= .data$cohort_end_date) |>
    dbplyr::window_order() |>
    tidyr::pivot_wider(names_from = "date_type", values_from = "date_event") |>
    dplyr::select(-"id2") |>
    dplyr::ungroup() |>
    dplyr::compute(
      temporary = FALSE, overwrite = TRUE, name = uniqueTmpName()
    )

  x_intervals <- x_intervals |>
    dplyr::filter(is.na(.data$subexposure_start_date)) |>
    dplyr::mutate(subexposure_start_date = .data$cohort_start_date) |>
    dplyr::union_all(
      x_intervals |>
        dplyr::filter(is.na(.data$subexposure_end_date)) |>
        dplyr::mutate(subexposure_end_date = .data$cohort_end_date)
    ) |>
    dplyr::union_all(
      x_intervals |>
        dplyr::filter(
          !is.na(.data$subexposure_start_date) &&
            !is.na(.data$subexposure_end_date)
        ) |>
        dplyr::mutate(
          subexposure_start_date = dplyr::if_else(
            .data$subexposure_start_date < .data$cohort_start_date,
            .data$cohort_start_date,
            .data$subexposure_start_date
          ),
          subexposure_end_date = dplyr::if_else(
            .data$subexposure_end_date > .data$cohort_end_date,
            .data$cohort_end_date,
            .data$subexposure_end_date
          )
        )
    ) |>
    dplyr::group_by(
      .data$subject_id, .data$cohort_start_date, .data$cohort_end_date
    ) |>
    dbplyr::window_order(.data$subexposure_start_date) |>
    dplyr::mutate(subexposure_id = dplyr::row_number()) |>
    dplyr::ungroup() |>
    dbplyr::window_order() %>%
    dplyr::mutate(subexposed_days = !!CDMConnector::datediff(
      "subexposure_start_date", "subexposure_end_date"
    ) + 1) |>
    dplyr::compute(
      temporary = FALSE, overwrite = TRUE, name = uniqueTmpName()
    )

  # we join the exposures with the overlapping periods and we only consider the
  # exposures that contribute to each overlapping period
  x_intervals <- x_intervals |>
    dplyr::left_join(
      x_intervals |>
        dplyr::inner_join(
          x,
          by = c("subject_id", "cohort_start_date", "cohort_end_date")
        ) |>
        dplyr::filter(
          .data$drug_exposure_start_date <= .data$subexposure_start_date
        ) |>
        dplyr::filter(
          .data$drug_exposure_end_date >= .data$subexposure_end_date
        ),
      by = c(
        "subject_id", "cohort_start_date", "cohort_end_date",
        "subexposure_start_date", "subexposure_end_date", "subexposure_id",
        "subexposed_days"
      )
    ) |>
    dplyr::compute(
      temporary = FALSE, overwrite = TRUE, name = uniqueTmpName()
    )

  return(x_intervals)
}

#' @noRd
addOverlappingFlag <- function(x) {
  x <- x |>
    dplyr::group_by(
      .data$subject_id, .data$cohort_start_date, .data$cohort_end_date,
      .data$subexposure_id, .data$unit
    ) |>
    dplyr::mutate(overlapping = dplyr::n()) |>
    dplyr::ungroup()
  return(x)
}

#' @noRd
addTypeSubexposure <- function(x, gapEra) {
  x <- x |>
    dplyr::group_by(
      .data$subject_id, .data$cohort_start_date, .data$cohort_end_date
    ) |>
    dplyr::mutate(
      type_subexposure = dplyr::case_when(
        !is.na(.data$drug_concept_id) ~ "exposed",
        is.na(.data$drug_concept_id) &
          .data$subexposed_days <= .env$gapEra &
          .data$subexposure_id > 1 &
          .data$subexposure_id < max(.data$subexposure_id, na.rm = TRUE) ~
          "gap",
        TRUE ~ "untreated"
      )
    ) |>
    dplyr::filter(.data$type_subexposure != "untreated") |>
    dplyr::ungroup()
  return(x)
}

#' @noRd
solveSameIndexOverlap <- function(x, cdm, sameIndexMode) {
  sameIndexMode <- tolower(sameIndexMode)
  x_same_index <- x |>
    dplyr::group_by(
      .data$subject_id, .data$cohort_start_date, .data$cohort_end_date,
      .data$subexposure_id, .data$drug_exposure_start_date, .data$unit
    ) |>
    dplyr::filter(dplyr::n() > 1)
  if (sameIndexMode == "minimum") {
    x_same_index <- x_same_index |>
      dplyr::left_join(
        x_same_index |>
          dplyr::filter(
            .data$daily_dose == min(.data$daily_dose, na.rm = TRUE)
          ) |>
          dplyr::filter(
            .data$drug_exposure_id == min(.data$drug_exposure_id, na.rm = TRUE)
          ) |>
          dplyr::mutate(considered_subexposure = "yes"),
        by = colnames(x_same_index)
      ) |>
      dplyr::mutate(considered_subexposure = dplyr::if_else(
        is.na(.data$considered_subexposure), "no", "yes"
      ))
  } else if (sameIndexMode == "maximum") {
    x_same_index <- x_same_index |>
      dplyr::left_join(
        x_same_index |>
          dplyr::filter(
            .data$daily_dose == max(.data$daily_dose, na.rm = TRUE)
          ) |>
          dplyr::filter(
            .data$drug_exposure_id == min(.data$drug_exposure_id, na.rm = TRUE)
          ) |>
          dplyr::mutate(considered_subexposure = "yes"),
        by = colnames(x_same_index)
      ) |>
      dplyr::mutate(considered_subexposure = dplyr::if_else(
        is.na(.data$considered_subexposure),
        "no",
        "yes"
      ))
  } else if (sameIndexMode == "sum") {
    x_same_index <- x_same_index |>
      dplyr::mutate(daily_dose = sum(.data$daily_dose, na.rm = TRUE)) |>
      dplyr::filter(
        .data$drug_exposure_id == min(.data$drug_exposure_id, na.rm = TRUE)
      ) |>
      dplyr::mutate(considered_subexposure = "yes")
  }
  x <- x |>
    dplyr::group_by(
      .data$subject_id, .data$cohort_start_date, .data$cohort_end_date,
      .data$subexposure_id, .data$drug_exposure_start_date, .data$unit
    ) |>
    dplyr::filter(dplyr::n() == 1) |>
    dplyr::ungroup() |>
    dplyr::union_all(x_same_index |> dplyr::ungroup()) |>
    dplyr::compute(
      temporary = FALSE, overwrite = TRUE, name = uniqueTmpName()
    )

  return(x)
}

#' @noRd
solveOverlap <- function(x, cdm, overlapMode) {
  overlapMode <- tolower(overlapMode)
  x_overlap <- x |>
    dplyr::group_by(
      .data$subject_id, .data$cohort_start_date, .data$cohort_end_date,
      .data$subexposure_id, .data$unit
    ) |>
    dplyr::filter(
      is.na(.data$considered_subexposure) |
        .data$considered_subexposure == "yes"
    ) |>
    dplyr::filter(dplyr::n() > 1)
  if (
    x_overlap |> dplyr::ungroup() |> dplyr::tally() |> dplyr::pull("n") > 0
  ) {
    if (overlapMode == "minimum") {
      x_overlap <- x_overlap |>
        dplyr::select(-"considered_subexposure")
      x_overlap <- x_overlap |>
        dplyr::left_join(
          x_overlap |>
            dplyr::filter(.data$daily_dose > 0) |>
            dplyr::filter(
              .data$daily_dose == min(.data$daily_dose, na.rm = TRUE)
            ) |>
            dplyr::filter(
              .data$drug_exposure_id == min(.data$drug_exposure_id, na.rm = TRUE)
            ) |>
            dplyr::mutate(considered_subexposure = "yes"),
          by = colnames(x_overlap)
        ) |>
        dplyr::mutate(considered_subexposure = dplyr::if_else(
          is.na(.data$considered_subexposure),
          "no",
          "yes"
        ))
    } else if (overlapMode == "maximum") {
      x_overlap <- x_overlap |>
        dplyr::select(-"considered_subexposure")
      x_overlap <- x_overlap |>
        dplyr::left_join(
          x_overlap |>
            dplyr::filter(
              .data$daily_dose == max(.data$daily_dose, na.rm = TRUE)
            ) |>
            dplyr::filter(
              .data$drug_exposure_id == min(.data$drug_exposure_id, na.rm = TRUE)
            ) |>
            dplyr::mutate(considered_subexposure = "yes"),
          by = colnames(x_overlap)
        ) |>
        dplyr::mutate(considered_subexposure = dplyr::if_else(
          is.na(.data$considered_subexposure),
          "no",
          "yes"
        ))
    } else if (overlapMode == "sum") {
      x_overlap <- x_overlap |>
        dplyr::mutate(considered_subexposure = "yes")
    } else if (overlapMode == "previous") {
      x_overlap <- x_overlap |>
        dplyr::mutate(
          first_drug_exposure_start_date =
            min(.data$drug_exposure_start_date, na.rm = TRUE)
        ) |>
        dplyr::mutate(
          considered_subexposure = dplyr::if_else(
            .data$drug_exposure_start_date ==
              .data$first_drug_exposure_start_date,
            "yes",
            "no"
          )
        ) |>
        dplyr::select(-"first_drug_exposure_start_date")
    } else if (overlapMode == "subsequent") {
      x_overlap <- x_overlap |>
        dplyr::mutate(
          last_drug_exposure_start_date =
            max(.data$drug_exposure_start_date, na.rm = TRUE)
        ) |>
        dplyr::mutate(
          considered_subexposure = dplyr::if_else(
            .data$drug_exposure_start_date ==
              .data$last_drug_exposure_start_date,
            "yes",
            "no"
          )
        ) |>
        dplyr::select(-"last_drug_exposure_start_date")
    }
    x_overlap <- x_overlap |>
      dplyr::ungroup() |>
      dplyr::compute(
        temporary = FALSE, overwrite = TRUE, name = uniqueTmpName()
      )
    x <- x |>
      dplyr::group_by(
        .data$subject_id, .data$cohort_start_date, .data$cohort_end_date,
        .data$subexposure_id, .data$unit
      ) |>
      dplyr::filter(
        is.na(.data$considered_subexposure) |
          .data$considered_subexposure == "yes"
      ) |>
      dplyr::filter(dplyr::n() == 1) |>
      dplyr::mutate(considered_subexposure = dplyr::if_else(
        .data$type_subexposure == "exposed" & is.na(.data$considered_subexposure),
        "yes",
        .data$considered_subexposure
      )) |>
      dplyr::ungroup() |>
      dplyr::union_all(x_overlap) |>
      dplyr::compute(
        temporary = FALSE, overwrite = TRUE, name = uniqueTmpName()
      )
  }
  return(x)
}

#' @noRd
addGapDailyDose <- function(x, cdm, eraJoinMode) {
  eraJoinMode <- tolower(eraJoinMode)
  x_gaps_dose <- x |>
    dplyr::filter(.data$type_subexposure == "gap")
  if (x_gaps_dose |> dplyr::tally() |> dplyr::pull() == 0) {
    return(x)
  }
  if (eraJoinMode == "zero") {
    return(
      x |>
        dplyr::filter(.data$type_subexposure != "gap")
    )
  } else if (eraJoinMode == "previous") {
    x_gaps_dose <- x_gaps_dose |>
      dplyr::select(-"daily_dose", -"unit") |>
      dplyr::inner_join(
        x |>
          dplyr::mutate(subexposure_id = .data$subexposure_id + 1) |>
          dplyr::filter(.data$considered_subexposure == "yes") |>
          dplyr::group_by(
            .data$subject_id, .data$cohort_start_date, .data$subexposure_id,
            .data$unit
          ) |>
          dplyr::mutate(daily_dose = sum(.data$daily_dose, na.rm = TRUE)) |>
          dplyr::ungroup() |>
          dplyr::select(
            "subject_id", "cohort_start_date", "cohort_end_date",
            "subexposure_id", "daily_dose", "unit"
          ) |>
          dplyr::distinct(),
        by = c(
          "subject_id", "cohort_start_date", "cohort_end_date",
          "subexposure_id"
        )
      )
  } else if (eraJoinMode == "subsequent") {
    x_gaps_dose <- x_gaps_dose |>
      dplyr::select(-"daily_dose", -"unit") |>
      dplyr::inner_join(
        x |>
          dplyr::mutate(subexposure_id = .data$subexposure_id - 1) |>
          dplyr::filter(.data$considered_subexposure == "yes") |>
          dplyr::group_by(
            .data$subject_id, .data$cohort_start_date, .data$subexposure_id,
            .data$unit
          ) |>
          dplyr::mutate(daily_dose = sum(.data$daily_dose, na.rm = TRUE)) |>
          dplyr::ungroup() |>
          dplyr::select(
            "subject_id", "cohort_start_date", "cohort_end_date",
            "subexposure_id", "daily_dose", "unit"
          ) |>
          dplyr::distinct(),
        by = c(
          "subject_id", "cohort_start_date", "cohort_end_date",
          "subexposure_id"
        )
      )
  }
  x <- x |>
    dplyr::filter(.data$type_subexposure != "gap") |>
    dplyr::union_all(
      x_gaps_dose |> dplyr::mutate(considered_subexposure = "yes")
    ) |>
    dplyr::compute(
      temporary = FALSE, overwrite = TRUE, name = uniqueTmpName()
    )
  return(x)
}

#' @noRd
getVariables <- function(initialDailyDose,
                         numberExposures,
                         duration,
                         cumulativeDose,
                         numberEras,
                         supplementary) {
  variables <- c(
    "initial_daily_dose", "number_exposures", "duration",
    "cumulative_dose", "number_eras"
  )[c(
    initialDailyDose, numberExposures, duration, cumulativeDose, numberEras
  )]
  if (supplementary) {
    variables <- c(
      variables, "gap_days", "unexposed_days", "not_considered_days",
      "number_subexposures", "number_continuous_exposures", "number_gaps",
      "number_unexposed_periods", "number_subexposures_overlap",
      "number_eras_overlap", "number_continuous_exposure_overlap",
      "cumulative_gap_dose", "cumulative_not_considered_dose",
      "sum_all_exposed_dose", "sum_all_exposed_days", "exposed_days",
      "number_subexposures_no_overlap", "number_eras_no_overlap",
      "number_continuous_exposures_no_overlap", "proportion_gap_dose",
      "proportion_not_considered_dose", "first_era_days"
    )
  }
  variables <- c(
    "subject_id", "cohort_start_date", "cohort_end_date", variables
  )
  return(variables)
}

addImpute <- function(cohort, cohortInfo, imputeCount, label) {
  if (imputeCount) {
    cohort <- cohort |>
      dplyr::left_join(
        cohortInfo |>
          dplyr::group_by(
            .data$subject_id, .data$cohort_start_date, .data$cohort_end_date
          ) |>
          dplyr::summarise(
            !!label := 100 * sum(.data$impute, na.rm = TRUE) / dplyr::n(),
            .groups = "drop"
          ),
        by = c("subject_id", "cohort_start_date", "cohort_end_date")
      )
  }
  return(cohort)
}

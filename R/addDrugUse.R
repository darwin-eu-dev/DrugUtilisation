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

#' Add new columns with dose related information to the cohort
#'
#' @param cohort Cohort in the cdm
#' @param cdm cdm_reference created with CDMConnector::cdmFromCon
#' @param ingredientConceptId Ingredient OMOP concept that we are interested for
#' the study. It is a compulsory input, no default value is provided.
#' @param conceptSetList List of concepts to be included. If NULL all the descendants
#' of ingredient concept id will be used. By default: NULL.
#' @param initialDailyDose Whether initial dose should be displayed
#' @param numberExposures Whether number exposures should be displayed
#' @param duration Whether duration should be displayed
#' @param cumulativeDose Whether cumulative dose should be displayed
#' @param numberEras Whether the number of eras should be displayed
#' @param initialQuantity Whether initial quantity should be displayed
#' @param cumulativeQuantity Whether cumulative quantity should be displayed
#' @param gapEra Number of days between two continuous exposures to be
#' considered in the same era. By default: 180.
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
#' By default: "previous".
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
#' By default: "previous".
#' @param sameIndexMode How the overlapping between two exposures that start on
#' the same day is solved inside a subexposure. There are five possible options:
#' "minimum" the considered daily_dose is the minimum of all of the exposures in
#' the subexposure.
#' "maximum" the considered daily_dose is the maximum of all of the exposures in
#' the subexposure.
#' "sum" the considered daily_dose is the sum of all the exposures present in
#' the subexposure.
#' By default: "sum".
#' @param imputeDuration Whether/how the duration should be imputed
#' "none", "median", "mean", "mode"
#' . By default: "none"
#' @param imputeDailyDose Whether/how the daily_dose should be imputed
#' "eliminate", "median", "mean", "quantile25", "quantile75". By default:
#' "eliminate"
#' @param durationRange Range between the duration must be comprised. It should
#' be a numeric vector of length two, with no NAs and the first value should be
#' equal or smaller than the second one. It is only required if imputeDuration
#' = TRUE. If NULL no restrictions are applied. By default: NULL.
#' @param dailyDoseRange Range between the daily_dose must be comprised. It
#' should be a numeric vector of length two, with no NAs and the first value
#' should be equal or smaller than the second one. It is only required if
#' imputeDailyDose = TRUE. If NULL no restrictions are applied. By default:
#' NULL.
#'
#' @return The function returns the dose information for all the individuals of
#' dusCohortName.
#'
#' @export
#'
#' @examples
#' \donttest{
#' library(DrugUtilisation)
#' library(CodelistGenerator)
#'
#' concept_relationship <- dplyr::tibble(
#'   concept_id_1 = c(1125315, 43135274, 2905077, 1125360),
#'   concept_id_2 = c(19016586, 46275062, 35894935, 19135843),
#'   relationship_id = c(rep("RxNorm has dose form", 4))
#' )
#' cdm <- mockDrugUtilisation(extraTables = list("concept_relationship" = concept_relationship))
#' cdm <- generateDrugUtilisationCohortSet(
#'   cdm, "dus_cohort", getDrugIngredientCodes(cdm, "acetaminophen")
#' )
#' cdm[["dus_cohort"]] %>%
#'   addDrugUse(cdm, 1125315)
#' }
#'
addDrugUse <- function(cohort,
                       cdm = attr(cohort, "cdm_reference"),
                       ingredientConceptId,
                       conceptSetList = NULL,
                       initialDailyDose = TRUE,
                       numberExposures = TRUE,
                       duration = TRUE,
                       cumulativeDose = TRUE,
                       numberEras = TRUE,
                       initialQuantity = TRUE,
                       cumulativeQuantity = TRUE,
                       gapEra = 30,
                       eraJoinMode = "zero",
                       overlapMode = "sum",
                       sameIndexMode = "sum",
                       imputeDuration = "none",
                       imputeDailyDose = "eliminate",
                       durationRange = c(1, Inf),
                       dailyDoseRange = c(0, Inf)) {
  character <- c("eraJoinMode", "overlapMode", "sameIndexMode", "imputeDuration", "imputeDailyDose")
  for (char in character) {
    if (is.character(get(char))) {
      assign(char, tolower(get(char)))
    }
  }

  if (length(conceptSetList) > 1) {
    cli::cli_abort("Only one concept set should be provided")
  }
  if (is.null(conceptSetList)) {
    checkInputs(ingredientConceptId = ingredientConceptId, cdm = cdm)
    conceptSetList <- list(
      cdm[["drug_strength"]] %>%
        dplyr::filter(.data$ingredient_concept_id == .env$ingredientConceptId) %>%
        dplyr::pull("drug_concept_id")
    )
    names(conceptSetList) <- cdm[["concept"]] %>%
      dplyr::filter(.data$concept_id == .env$ingredientConceptId) %>%
      dplyr::pull("concept_name")
  }

  # initial checks
  checkInputs(
    cohort = cohort, cdm = cdm, ingredientConceptId = ingredientConceptId,
    conceptSetList = conceptSetList, initialDailyDose = initialDailyDose,
    numberExposures = numberExposures, duration = duration,
    cumulativeDose = cumulativeDose, numberEras = numberEras,
    gapEra = gapEra, eraJoinMode = eraJoinMode,
    initialQuantity = initialQuantity, cumulativeQuantity = cumulativeQuantity,
    overlapMode = overlapMode, sameIndexMode = sameIndexMode,
    imputeDuration = imputeDuration,
    imputeDailyDose = imputeDailyDose, durationRange = durationRange,
    dailyDoseRange = dailyDoseRange
  )

  # get conceptSet
  conceptSet <- conceptSetFromConceptSetList(conceptSetList)

  # check unit
  conceptSet <- conceptSet %>%
    dplyr::rename("drug_concept_id" = "concept_id") %>%
    dplyr::left_join(
      drugStrengthPattern(
        cdm = cdm, ingredientConceptId = ingredientConceptId, pattern = FALSE,
        patternDetails = FALSE, unit = TRUE, route = FALSE, formula = FALSE,
        ingredient = FALSE
      ) %>%
        dplyr::collect(),
      by = "drug_concept_id"
    ) %>%
    dplyr::filter(!is.na(.data$unit))
  unit <- conceptSet %>%
    dplyr::pull("unit") %>%
    unique()
  if (length(unit) > 1) {
    cli::cli_abort(
      "More than one unit included in the conceptSetList, please stratify by
      unit. You can check the unit of each with:
      tibble(drug_concept_id = unlist(conceptSetList)) %>%
      addPattern(cdm, ingredientConceptId)"
    )
  }
  conceptSet <- conceptSet %>%
    dplyr::select("cohort_definition_id", "concept_id" = "drug_concept_id")

  # consistency with cohortSet
  cs <- CDMConnector::cohortSet(cohort)
  parameters <- checkConsistentCohortSet(
    cs, conceptSetList, gapEra, imputeDuration, durationRange,
    missing(gapEra), missing(imputeDuration), missing(durationRange)
  )
  gapEra <- parameters$gapEra
  imputeDuration <- parameters$imputeDuration
  durationRange <- parameters$durationRange
  if (length(conceptSetList) > 1) {
    cli::cli_abort("Only one concept set is allowed")
  }

  # save original reference
  originalCohort <- cohort

  # unique cohort entries
  cohort <- cohort %>%
    dplyr::select("subject_id", "cohort_start_date", "cohort_end_date") %>%
    dplyr::distinct() %>%
    addDuration(duration) %>%
    CDMConnector::computeQuery()

  if (initialDailyDose | numberExposures | cumulativeDose | numberEras |
    initialQuantity | cumulativeQuantity) {
    # subset drug_exposure and only get the drug concept ids that we are
    # interested in.
    cohortInfo <- initialSubset(cdm, cohort, conceptSet)

    cohort <- cohort %>%
      addInfo(cohortInfo, numberExposures, initialQuantity, cumulativeQuantity)

    if (initialDailyDose | cumulativeDose | numberEras) {
      # correct duration
      cohortInfo <- correctDuration(
        cohortInfo, imputeDuration, durationRange, cdm,
        "drug_exposure_start_date", "drug_exposure_end_date"
      )

      # add daily dose
      cohortInfo <- cohortInfo %>%
        addDailyDose(ingredientConceptId = ingredientConceptId) %>%
        dplyr::select(-"quantity", -"unit", -"route") %>%
        dplyr::distinct()

      # impute daily dose
      cohortInfo <- imputeVariable(
        cohortInfo, "daily_dose",
        impute = imputeDailyDose,
        range = dailyDoseRange,
        start = "drug_exposure_start_date",
        end = "drug_exposure_end_date"
      ) %>%
        computeTable(cdm)

      cohort <- cohort %>%
        addInitialDailyDose(cohortInfo, initialDailyDose, sameIndexMode)

      # split the exposures in subexposures inside each cohort
      cohortInfo <- splitSubexposures(cohortInfo, cdm)

      # add the overlapping flag
      cohortInfo <- addOverlappingFlag(cohortInfo)

      # add the type of subexposure
      cohortInfo <- addTypeSubexposure(cohortInfo, gapEra)

      # add era_id
      cohortInfo <- addEraId(cohortInfo)

      # add number eras
      cohort <- addNumberEras(cohort, cohortInfo, numberEras)

      if (cumulativeDose) {
        # add continuous_exposure_id
        cohortInfo <- addContinuousExposureId(cohortInfo)

        # solve same index day overlapping
        cohortInfo <- solveSameIndexOverlap(cohortInfo, cdm, sameIndexMode)

        # solve not same index overlapping
        cohortInfo <- solveOverlap(cohortInfo, cdm, overlapMode)

        # add daily dose to gaps
        cohortInfo <- addGapDailyDose(cohortInfo, cdm, eraJoinMode)

        # add cumulative dose
        cohort <- addCumulativeDose(cohort, cohortInfo)
      }
    }
  }

  # add result
  cohort <- originalCohort %>%
    dplyr::left_join(
      cohort,
      by = c("subject_id", "cohort_start_date", "cohort_end_date")
    ) %>%
    CDMConnector::computeQuery() %>%
    PatientProfiles::addAttributes(originalCohort)

  # add attributes back to the cohort
  cohort <- PatientProfiles::addAttributes(cohort, originalCohort)

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
                    numberExposures,
                    initialQuantity,
                    cumulativeQuantity) {
  if (numberExposures | cumulativeQuantity) {
    cohort <- cohort %>%
      dplyr::left_join(
        cohortInfo %>%
          dplyr::group_by(
            .data$subject_id, .data$cohort_start_date,
            .data$cohort_end_date
          ) %>%
          dplyr::summarise(
            number_exposures = dplyr::n(),
            cumulative_quantity = sum(.data$quantity, na.rm = TRUE),
            .groups = "drop"
          ),
        by = c(
          "subject_id", "cohort_start_date", "cohort_end_date"
        ),
      ) %>%
      dplyr::mutate(
        number_exposures = dplyr::if_else(
          is.na(.data$number_exposures), 0, .data$number_exposures
        ),
        cumulative_quantity = dplyr::if_else(
          is.na(.data$cumulative_quantity), 0, .data$cumulative_quantity
        )
      ) %>%
      CDMConnector::computeQuery()
  }
  if (initialQuantity) {
    cohort <- cohort %>%
      dplyr::left_join(
        cohortInfo %>%
          dplyr::group_by(
            .data$subject_id, .data$cohort_start_date,
            .data$cohort_end_date
          ) %>%
          dplyr::filter(
            .data$drug_exposure_start_date == min(
              .data$drug_exposure_start_date,
              na.rm = TRUE
            ) &
              .data$drug_exposure_start_date <= .data$cohort_start_date &
              .data$drug_exposure_end_date >= .data$cohort_start_date
          ) %>%
          dplyr::summarise(
            initial_quantity = sum(.data$quantity, na.rm = TRUE),
            .groups = "drop"
          ),
        by = c(
          "subject_id", "cohort_start_date", "cohort_end_date"
        ),
      ) %>%
      dplyr::mutate(initial_quantity = dplyr::if_else(
        is.na(.data$initial_quantity), 0, .data$initial_quantity
      )) %>%
      CDMConnector::computeQuery()
  }
}

addInitialDailyDose <- function(cohort,
                                cohortInfo,
                                initialDailyDose,
                                sameIndexMode) {
  sameIndexMode <- tolower(sameIndexMode)
  if (initialDailyDose) {
    cohortInfo <- cohortInfo %>%
      dplyr::group_by(
        .data$subject_id, .data$cohort_start_date,
        .data$cohort_end_date
      ) %>%
      dplyr::filter(
        .data$drug_exposure_start_date == min(
          .data$drug_exposure_start_date,
          na.rm = TRUE
        ) &
          .data$drug_exposure_start_date <= .data$cohort_start_date &
          .data$drug_exposure_end_date >= .data$cohort_start_date
      )
    if (sameIndexMode == "sum") {
      cohortInfo <- cohortInfo %>%
        dplyr::summarise(
          initial_daily_dose = sum(.data$daily_dose, na.rm = TRUE),
          .groups = "drop"
        )
    } else if (sameIndexMode == "min") {
      cohortInfo <- cohortInfo %>%
        dplyr::summarise(
          initial_daily_dose = min(.data$daily_dose, na.rm = TRUE),
          .groups = "drop"
        )
    } else {
      cohortInfo <- cohortInfo %>%
        dplyr::summarise(
          initial_daily_dose = max(.data$daily_dose, na.rm = TRUE),
          .groups = "drop"
        )
    }
    cohort <- cohort %>%
      dplyr::left_join(
        cohortInfo,
        by = c("subject_id", "cohort_start_date", "cohort_end_date")
      ) %>%
      dplyr::mutate(initial_daily_dose = dplyr::if_else(
        is.na(.data$initial_daily_dose), 0, .data$initial_daily_dose
      )) %>%
      CDMConnector::computeQuery()
  }
}

addNumberEras <- function(cohort, cohortInfo, numberEras) {
  if (numberEras) {
    cohort <- cohort %>%
      dplyr::left_join(
        cohortInfo %>%
          dplyr::group_by(
            .data$subject_id, .data$cohort_start_date, .data$cohort_end_date
          ) %>%
          dplyr::summarise(
            number_eras = max(.data$era_id, na.rm = TRUE),
            .groups = "drop"
          ),
        by = c("subject_id", "cohort_start_date", "cohort_end_date")
      ) %>%
      dplyr::mutate(number_eras = dplyr::if_else(
        is.na(.data$number_eras), 0, .data$number_eras
      )) %>%
      CDMConnector::computeQuery()
  }
}

addCumulativeDose <- function(cohort, cohortInfo) {
  cohort <- cohort %>%
    dplyr::left_join(
      cohortInfo %>%
        dplyr::mutate(
          exposed_dose = .data$daily_dose * .data$subexposed_days
        ) %>%
        dplyr::group_by(
          .data$subject_id, .data$cohort_start_date, .data$cohort_end_date
        ) %>%
        dplyr::summarise(
          cumulative_dose = sum(
            .data$exposed_dose[.data$considered_subexposure == "yes"],
            na.rm = TRUE
          ),
          .groups = "drop"
        ),
      by = c("subject_id", "cohort_start_date", "cohort_end_date")
    ) %>%
    dplyr::mutate(cumulative_dose = dplyr::if_else(
      is.na(.data$cumulative_dose), 0, .data$cumulative_dose
    )) %>%
    CDMConnector::computeQuery()
}

#' @noRd
initialSubset <- function(cdm, dusCohort, conceptSet) {
  cdm[["drug_exposure"]] %>%
    dplyr::select(
      "subject_id" = "person_id",
      "drug_concept_id",
      "drug_exposure_id",
      "drug_exposure_start_date",
      "drug_exposure_end_date",
      "quantity"
    ) %>%
    dplyr::inner_join(dusCohort, by = "subject_id") %>%
    dplyr::inner_join(
      conceptSet %>%
        dplyr::select("drug_concept_id" = "concept_id"),
      by = "drug_concept_id",
      copy = TRUE
    ) %>%
    dplyr::filter(.data$drug_exposure_start_date <= .data$cohort_end_date) %>%
    dplyr::filter(.data$drug_exposure_end_date >= .data$cohort_start_date) %>%
    computeTable(cdm)
}

#' @noRd
splitSubexposures <- function(x, cdm) {
  x_intervals <- x %>%
    dplyr::select(
      "subject_id", "cohort_start_date", "cohort_end_date",
      "date_event" = "drug_exposure_start_date"
    ) %>%
    dplyr::distinct() %>%
    dplyr::union_all(
      x %>%
        dplyr::select(
          "subject_id", "cohort_start_date", "cohort_end_date",
          "date_event" = "drug_exposure_start_date"
        ) %>%
        dplyr::distinct() %>%
        dplyr::mutate(date_event = as.Date(!!CDMConnector::dateadd(
          "date_event", -1
        )))
    ) %>%
    dplyr::union_all(
      x %>%
        dplyr::select(
          "subject_id", "cohort_start_date", "cohort_end_date",
          "date_event" = "drug_exposure_end_date"
        ) %>%
        dplyr::distinct()
    ) %>%
    dplyr::union_all(
      x %>%
        dplyr::select(
          "subject_id", "cohort_start_date", "cohort_end_date",
          "date_event" = "drug_exposure_end_date"
        ) %>%
        dplyr::distinct() %>%
        dplyr::mutate(date_event = as.Date(!!CDMConnector::dateadd(
          "date_event", 1
        )))
    ) %>%
    dplyr::group_by(
      .data$subject_id, .data$cohort_start_date, .data$cohort_end_date
    ) %>%
    dbplyr::window_order(.data$date_event) %>%
    dplyr::mutate(id1 = dplyr::row_number()) %>%
    dplyr::mutate(
      id2 = floor(1.25 + 0.5 * .data$id1),
      date_type = dplyr::if_else(
        .data$id1 %% 2 == 0,
        "subexposure_start_date",
        "subexposure_end_date"
      )
    ) %>%
    dplyr::select(-"id1") %>%
    dplyr::filter(.data$date_event >= .data$cohort_start_date) %>%
    dplyr::filter(.data$date_event <= .data$cohort_end_date) %>%
    dbplyr::window_order() %>%
    tidyr::pivot_wider(names_from = "date_type", values_from = "date_event") %>%
    dplyr::select(-"id2") %>%
    dplyr::ungroup() %>%
    computeTable(cdm)

  x_intervals <- x_intervals %>%
    dplyr::filter(is.na(.data$subexposure_start_date)) %>%
    dplyr::mutate(subexposure_start_date = .data$cohort_start_date) %>%
    dplyr::union_all(
      x_intervals %>%
        dplyr::filter(is.na(.data$subexposure_end_date)) %>%
        dplyr::mutate(subexposure_end_date = .data$cohort_end_date)
    ) %>%
    dplyr::union_all(
      x_intervals %>%
        dplyr::filter(
          !is.na(.data$subexposure_start_date) &&
            !is.na(.data$subexposure_end_date)
        ) %>%
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
    ) %>%
    dplyr::group_by(
      .data$subject_id, .data$cohort_start_date, .data$cohort_end_date
    ) %>%
    dbplyr::window_order(.data$subexposure_start_date) %>%
    dplyr::mutate(subexposure_id = dplyr::row_number()) %>%
    dplyr::ungroup() %>%
    dbplyr::window_order() %>%
    dplyr::mutate(subexposed_days = !!CDMConnector::datediff(
      "subexposure_start_date", "subexposure_end_date"
    ) + 1) %>%
    computeTable(cdm)

  # we join the exposures with the overlapping periods and we only consider the
  # exposures that contribute to each overlapping period
  x_intervals <- x_intervals %>%
    dplyr::left_join(
      x_intervals %>%
        dplyr::inner_join(
          x,
          by = c("subject_id", "cohort_start_date", "cohort_end_date")
        ) %>%
        dplyr::filter(
          .data$drug_exposure_start_date <= .data$subexposure_start_date
        ) %>%
        dplyr::filter(
          .data$drug_exposure_end_date >= .data$subexposure_end_date
        ),
      by = c(
        "subject_id", "cohort_start_date", "cohort_end_date",
        "subexposure_start_date", "subexposure_end_date", "subexposure_id",
        "subexposed_days"
      )
    ) %>%
    computeTable(cdm)

  return(x_intervals)
}

#' @noRd
addOverlappingFlag <- function(x) {
  x <- x %>%
    dplyr::group_by(
      .data$subject_id, .data$cohort_start_date, .data$cohort_end_date,
      .data$subexposure_id
    ) %>%
    dplyr::mutate(overlapping = dplyr::n()) %>%
    dplyr::ungroup()
  return(x)
}

#' @noRd
addTypeSubexposure <- function(x, gapEra) {
  x <- x %>%
    dplyr::group_by(
      .data$subject_id, .data$cohort_start_date, .data$cohort_end_date
    ) %>%
    dplyr::mutate(
      type_subexposure = dplyr::case_when(
        !is.na(.data$drug_concept_id) ~ "exposed",
        is.na(.data$drug_concept_id) &
          .data$subexposed_days <= .env$gapEra &
          .data$subexposure_id > 1 &
          .data$subexposure_id < max(.data$subexposure_id, na.rm = TRUE) ~
          "gap",
        TRUE ~ "unexposed"
      )
    )
  return(x)
}

#' @noRd
addEraId <- function(x) {
  x <- x %>%
    dplyr::mutate(era_id = dplyr::if_else(
      .data$type_subexposure == "unexposed" & .data$subexposure_id > 1,
      1,
      0
    )) %>%
    dbplyr::window_order(.data$subexposure_start_date) %>%
    dplyr::mutate(era_id = cumsum(as.numeric(.data$era_id))) %>%
    dplyr::mutate(era_id = dplyr::if_else(
      .data$type_subexposure == "unexposed",
      as.numeric(NA),
      .data$era_id + 1
    )) %>%
    dbplyr::window_order()
  return(x)
}

#' @noRd
addContinuousExposureId <- function(x) {
  x <- x %>%
    dplyr::mutate(continuous_exposure_id = dplyr::if_else(
      .data$type_subexposure != "exposed" & .data$subexposure_id > 1,
      1,
      0
    )) %>%
    dbplyr::window_order(.data$subexposure_start_date) %>%
    dplyr::mutate(continuous_exposure_id = cumsum(as.numeric(
      .data$continuous_exposure_id
    ))) %>%
    dplyr::mutate(continuous_exposure_id = dplyr::if_else(
      .data$type_subexposure != "exposed",
      as.numeric(NA),
      .data$continuous_exposure_id + 1
    )) %>%
    dplyr::ungroup() %>%
    dbplyr::window_order()
  return(x)
}

#' @noRd
solveSameIndexOverlap <- function(x, cdm, sameIndexMode) {
  sameIndexMode <- tolower(sameIndexMode)
  x_same_index <- x %>%
    dplyr::group_by(
      .data$subject_id, .data$cohort_start_date, .data$subexposure_id,
      .data$drug_exposure_start_date
    ) %>%
    dplyr::filter(dplyr::n() > 1)
  if (sameIndexMode == "minimum") {
    x_same_index <- x_same_index %>%
      dplyr::left_join(
        x_same_index %>%
          dplyr::filter(
            .data$daily_dose == min(.data$daily_dose, na.rm = TRUE)
          ) %>%
          dplyr::filter(
            .data$drug_exposure_id == min(.data$drug_exposure_id, na.rm = TRUE)
          ) %>%
          dplyr::mutate(considered_subexposure = "yes"),
        by = colnames(x_same_index)
      ) %>%
      dplyr::mutate(considered_subexposure = dplyr::if_else(
        is.na(.data$considered_subexposure),
        "no",
        "yes"
      ))
  } else if (sameIndexMode == "maximum") {
    x_same_index <- x_same_index %>%
      dplyr::left_join(
        x_same_index %>%
          dplyr::filter(
            .data$daily_dose == max(.data$daily_dose, na.rm = TRUE)
          ) %>%
          dplyr::filter(
            .data$drug_exposure_id == min(.data$drug_exposure_id, na.rm = TRUE)
          ) %>%
          dplyr::mutate(considered_subexposure = "yes"),
        by = colnames(x_same_index)
      ) %>%
      dplyr::mutate(considered_subexposure = dplyr::if_else(
        is.na(.data$considered_subexposure),
        "no",
        "yes"
      ))
  } else if (sameIndexMode == "sum") {
    x_same_index <- x_same_index %>%
      dplyr::mutate(daily_dose = sum(.data$daily_dose, na.rm = TRUE)) %>%
      dplyr::filter(
        .data$drug_exposure_id == min(.data$drug_exposure_id, na.rm = TRUE)
      ) %>%
      dplyr::union_all(
        x_same_index %>%
          dplyr::filter(
            .data$drug_exposure_id > min(.data$drug_exposure_id, na.rm = TRUE)
          ) %>%
          dplyr::mutate(daily_dose = 0)
      ) %>%
      dplyr::mutate(considered_subexposure = "yes")
  }
  x <- x %>%
    dplyr::anti_join(
      x_same_index,
      by = c(
        "subject_id", "cohort_start_date", "cohort_end_date", "subexposure_id",
        "drug_exposure_start_date"
      )
    ) %>%
    dplyr::union_all(x_same_index %>% dplyr::ungroup()) %>%
    computeTable(cdm)

  return(x)
}

#' @noRd
solveOverlap <- function(x, cdm, overlapMode) {
  overlapMode <- tolower(overlapMode)
  x_overlap <- x %>%
    dplyr::group_by(
      .data$subject_id, .data$cohort_start_date, .data$subexposure_id,
    ) %>%
    dplyr::filter(
      is.na(.data$considered_subexposure) |
        .data$considered_subexposure == "yes"
    ) %>%
    dplyr::filter(dplyr::n() > 1)
  if (
    x_overlap %>% dplyr::ungroup() %>% dplyr::tally() %>% dplyr::pull("n") > 0
  ) {
    if (overlapMode == "minimum") {
      x_overlap <- x_overlap %>%
        dplyr::select(-"considered_subexposure")
      x_overlap <- x_overlap %>%
        dplyr::left_join(
          x_overlap %>%
            dplyr::filter(.data$daily_dose > 0) %>%
            dplyr::filter(
              .data$daily_dose == min(.data$daily_dose, na.rm = TRUE)
            ) %>%
            dplyr::filter(
              .data$drug_exposure_id == min(.data$drug_exposure_id, na.rm = TRUE)
            ) %>%
            dplyr::mutate(considered_subexposure = "yes"),
          by = colnames(x_overlap)
        ) %>%
        dplyr::mutate(considered_subexposure = dplyr::if_else(
          is.na(.data$considered_subexposure),
          "no",
          "yes"
        ))
    } else if (overlapMode == "maximum") {
      x_overlap <- x_overlap %>%
        dplyr::select(-"considered_subexposure")
      x_overlap <- x_overlap %>%
        dplyr::left_join(
          x_overlap %>%
            dplyr::filter(
              .data$daily_dose == max(.data$daily_dose, na.rm = TRUE)
            ) %>%
            dplyr::filter(
              .data$drug_exposure_id == min(.data$drug_exposure_id, na.rm = TRUE)
            ) %>%
            dplyr::mutate(considered_subexposure = "yes"),
          by = colnames(x_overlap)
        ) %>%
        dplyr::mutate(considered_subexposure = dplyr::if_else(
          is.na(.data$considered_subexposure),
          "no",
          "yes"
        ))
    } else if (overlapMode == "sum") {
      x_overlap <- x_overlap %>%
        dplyr::mutate(considered_subexposure = "yes")
    } else if (overlapMode == "previous") {
      x_overlap <- x_overlap %>%
        dplyr::mutate(
          first_drug_exposure_start_date =
            min(.data$drug_exposure_start_date, na.rm = TRUE)
        ) %>%
        dplyr::mutate(
          considered_subexposure = dplyr::if_else(
            .data$drug_exposure_start_date ==
              .data$first_drug_exposure_start_date,
            "yes",
            "no"
          )
        ) %>%
        dplyr::select(-"first_drug_exposure_start_date")
    } else if (overlapMode == "subsequent") {
      x_overlap <- x_overlap %>%
        dplyr::mutate(
          last_drug_exposure_start_date =
            max(.data$drug_exposure_start_date, na.rm = TRUE)
        ) %>%
        dplyr::mutate(
          considered_subexposure = dplyr::if_else(
            .data$drug_exposure_start_date ==
              .data$last_drug_exposure_start_date,
            "yes",
            "no"
          )
        ) %>%
        dplyr::select(-"last_drug_exposure_start_date")
    }
    x_overlap <- x_overlap %>%
      dplyr::ungroup() %>%
      computeTable(cdm)
    x <- x %>%
      dplyr::anti_join(
        x_overlap,
        by = c(
          "subject_id", "cohort_start_date", "subexposure_id", "drug_exposure_id"
        )
      ) %>%
      dplyr::mutate(considered_subexposure = dplyr::if_else(
        .data$type_subexposure == "exposed" & is.na(.data$considered_subexposure),
        "yes",
        .data$considered_subexposure
      )) %>%
      dplyr::union_all(x_overlap) %>%
      computeTable(cdm)
  }
  return(x)
}

#' @noRd
addGapDailyDose <- function(x, cdm, eraJoinMode) {
  eraJoinMode <- tolower(eraJoinMode)
  x_gaps_dose <- x %>%
    dplyr::filter(.data$type_subexposure == "gap")
  if (eraJoinMode == "zero") {
    x_gaps_dose <- x_gaps_dose %>%
      dplyr::mutate(daily_dose = as.numeric(0))
  } else if (eraJoinMode == "previous") {
    x_gaps_dose <- x_gaps_dose %>%
      dplyr::select(-"daily_dose") %>%
      dplyr::inner_join(
        x %>%
          dplyr::mutate(subexposure_id = .data$subexposure_id + 1) %>%
          dplyr::filter(.data$considered_subexposure == "yes") %>%
          dplyr::group_by(
            .data$subject_id, .data$cohort_start_date, .data$subexposure_id
          ) %>%
          dplyr::mutate(daily_dose = sum(.data$daily_dose, na.rm = TRUE)) %>%
          dplyr::ungroup() %>%
          dplyr::select(
            "subject_id", "cohort_start_date", "cohort_end_date",
            "subexposure_id", "daily_dose"
          ) %>%
          dplyr::distinct(),
        by = c(
          "subject_id", "cohort_start_date", "cohort_end_date",
          "subexposure_id"
        )
      )
  } else if (eraJoinMode == "subsequent") {
    x_gaps_dose <- x_gaps_dose %>%
      dplyr::select(-"daily_dose") %>%
      dplyr::inner_join(
        x %>%
          dplyr::mutate(subexposure_id = .data$subexposure_id - 1) %>%
          dplyr::filter(.data$considered_subexposure == "yes") %>%
          dplyr::group_by(
            .data$subject_id, .data$cohort_start_date, .data$subexposure_id
          ) %>%
          dplyr::mutate(daily_dose = sum(.data$daily_dose, na.rm = TRUE)) %>%
          dplyr::ungroup() %>%
          dplyr::select(
            "subject_id", "cohort_start_date", "cohort_end_date",
            "subexposure_id", "daily_dose"
          ) %>%
          dplyr::distinct(),
        by = c(
          "subject_id", "cohort_start_date", "cohort_end_date",
          "subexposure_id"
        )
      )
  }
  x <- x %>%
    dplyr::anti_join(
      x_gaps_dose,
      by = c(
        "subject_id", "cohort_start_date", "cohort_end_date",
        "subexposure_id"
      )
    ) %>%
    dplyr::union_all(
      x_gaps_dose %>% dplyr::mutate(considered_subexposure = "yes")
    ) %>%
    computeTable(cdm)
  return(x)
}

#' @noRd
summariseCohort <- function(x, cdm) {
  x <- x %>%
    dplyr::mutate(exposed_dose = .data$daily_dose * .data$subexposed_days) %>%
    computeTable(cdm) %>%
    dplyr::group_by(
      .data$subject_id, .data$cohort_start_date, .data$cohort_end_date
    ) %>%
    dplyr::summarise(
      gap_days = sum(
        .data$subexposed_days[.data$type_subexposure == "gap"],
        na.rm = TRUE
      ),
      unexposed_days = sum(
        .data$subexposed_days[.data$type_subexposure == "unexposed"],
        na.rm = TRUE
      ),
      not_considered_days = sum(
        .data$subexposed_days[
          .data$type_subexposure == "exposed" &
            .data$considered_subexposure == "no"
        ],
        na.rm = TRUE
      ),
      start_first_era = min(
        .data$subexposure_start_date[.data$era_id == 1],
        na.rm = TRUE
      ),
      end_first_era = max(
        .data$subexposure_end_date[.data$era_id == 1],
        na.rm = TRUE
      ),
      number_exposures = dplyr::n_distinct(.data$drug_exposure_id),
      number_subexposures = max(.data$subexposure_id, na.rm = TRUE),
      number_continuous_exposures = max(
        .data$continuous_exposure_id,
        na.rm = TRUE
      ),
      number_eras = max(.data$era_id, na.rm = TRUE),
      number_gaps = dplyr::n_distinct(
        .data$subexposure_id[.data$type_subexposure == "gap"]
      ),
      number_unexposed_periods = sum(
        dplyr::if_else(.data$type_subexposure == "unexposed", 1, 0),
        na.rm = TRUE
      ),
      number_subexposures_overlap = dplyr::n_distinct(
        .data$subexposure_id[.data$overlapping > 1]
      ),
      number_eras_overlap = dplyr::n_distinct(
        .data$era_id[.data$overlapping > 1]
      ),
      number_continuous_exposure_overlap = dplyr::n_distinct(
        .data$continuous_exposure_id[.data$overlapping > 1]
      ),
      cumulative_dose = sum(
        .data$exposed_dose[.data$considered_subexposure == "yes"],
        na.rm = TRUE
      ),
      initial_daily_dose = sum(
        .data$daily_dose[
          .data$subexposure_id == 1 & .data$considered_subexposure == "yes"
        ],
        na.rm = TRUE
      ),
      cumulative_gap_dose = sum(
        .data$exposed_dose[.data$type_subexposure == "gap"],
        na.rm = TRUE
      ),
      cumulative_not_considered_dose = sum(
        .data$exposed_dose[.data$considered_subexposure == "no"],
        na.rm = TRUE
      ),
      sum_all_exposed_dose = sum(
        .data$exposed_dose[.data$type_subexposure == "exposed"],
        na.rm = TRUE
      ),
      sum_all_exposed_days = sum(
        .data$subexposed_days[.data$type_subexposure == "exposed"],
        na.rm = TRUE
      ),
      .groups = "drop"
    ) %>%
    computeTable(cdm) %>%
    # replace NA
    dplyr::mutate(
      gap_days = dplyr::if_else(
        is.na(.data$gap_days),
        0,
        .data$gap_days
      ),
      unexposed_days = dplyr::if_else(
        is.na(.data$unexposed_days),
        0,
        .data$unexposed_days
      ),
      not_considered_days = dplyr::if_else(
        is.na(.data$not_considered_days),
        0,
        .data$not_considered_days
      ),
      number_exposures = dplyr::if_else(
        is.na(.data$number_exposures),
        0,
        .data$number_exposures
      ),
      number_subexposures = dplyr::if_else(
        is.na(.data$number_subexposures),
        0,
        .data$number_subexposures
      ),
      number_continuous_exposures = dplyr::if_else(
        is.na(.data$number_continuous_exposures),
        0,
        .data$number_continuous_exposures
      ),
      number_eras = dplyr::if_else(
        is.na(.data$number_eras),
        0,
        .data$number_eras
      ),
      number_gaps = dplyr::if_else(
        is.na(.data$number_gaps),
        0,
        .data$number_gaps
      ),
      number_unexposed_periods = dplyr::if_else(
        is.na(.data$number_unexposed_periods),
        0,
        .data$number_unexposed_periods
      ),
      number_subexposures_overlap = dplyr::if_else(
        is.na(.data$number_subexposures_overlap),
        0,
        .data$number_subexposures_overlap
      ),
      number_eras_overlap = dplyr::if_else(
        is.na(.data$number_eras_overlap),
        0,
        .data$number_eras_overlap
      ),
      number_continuous_exposure_overlap = dplyr::if_else(
        is.na(.data$number_continuous_exposure_overlap),
        0,
        .data$number_continuous_exposure_overlap
      ),
      cumulative_dose = dplyr::if_else(
        is.na(.data$cumulative_dose),
        0,
        .data$cumulative_dose
      ),
      initial_daily_dose = dplyr::if_else(
        is.na(.data$initial_daily_dose),
        0,
        .data$initial_daily_dose
      ),
      cumulative_gap_dose = dplyr::if_else(
        is.na(.data$cumulative_gap_dose),
        0,
        .data$cumulative_gap_dose
      ),
      cumulative_not_considered_dose = dplyr::if_else(
        is.na(.data$cumulative_not_considered_dose),
        0,
        .data$cumulative_not_considered_dose
      )
    ) %>%
    # end replace NA
    dplyr::mutate(duration = !!CDMConnector::datediff(
      "cohort_start_date", "cohort_end_date"
    ) + 1) %>%
    dplyr::mutate(
      exposed_days =
        .data$duration - .data$unexposed_days - .data$gap_days
    ) %>%
    dplyr::mutate(
      number_subexposures_no_overlap =
        .data$number_subexposures - .data$number_subexposures_overlap
    ) %>%
    dplyr::mutate(
      number_eras_no_overlap =
        .data$number_eras - .data$number_eras_overlap
    ) %>%
    dplyr::mutate(
      number_continuous_exposures_no_overlap =
        .data$number_continuous_exposures - .data$number_continuous_exposure_overlap
    ) %>%
    dplyr::mutate(proportion_gap_dose = dplyr::if_else(
      .data$cumulative_dose == 0,
      as.numeric(NA),
      .data$cumulative_gap_dose / .data$cumulative_dose
    )) %>%
    dplyr::mutate(proportion_not_considered_dose = dplyr::if_else(
      .data$cumulative_dose == 0,
      as.numeric(NA),
      .data$cumulative_not_considered_dose / .data$cumulative_dose
    )) %>%
    dplyr::mutate(first_era_days = dplyr::if_else(
      is.na(.data$start_first_era),
      0,
      !!CDMConnector::datediff(
        "start_first_era",
        "end_first_era"
      ) + 1
    )) %>%
    dplyr::select(-"start_first_era", -"end_first_era") %>%
    computeTable(cdm)

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

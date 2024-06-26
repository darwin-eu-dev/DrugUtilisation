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

#' Add new columns with drug use related information
#'
#' @param cohort Cohort in the cdm
#' @param indexDate Name of a column that indicates the date to start the
#' analysis.
#' @param censorDate Name of a column that indicates the date to stop the
#' analysis, if NULL end of individuals observation is used.
#' @param ingredientConceptId Ingredient OMOP concept that we are interested for
#' the study. It is a compulsory input, no default value is provided.
#' @param conceptSet List of concepts to be included. If NULL all the
#' descendants of ingredient concept id will be used.
#' ingredientConceptId = NULL,
#' @param restrictIncident Whether to include only incident prescriptions in the
#' analysis. If FALSE all prescriptions that overlap with the study period will
#' be included.
#' @param gapEra Number of days between two continuous exposures to be
#' considered in the same era.
#'
#' @return The same cohort with the added columns.
#'
#' @export
#'
#' @examples
#' \donttest{
#' library(DrugUtilisation)
#' library(CodelistGenerator)
#'
#' cdm <- mockDrugUtilisation()
#' cdm <- generateDrugUtilisationCohortSet(
#'   cdm, "dus_cohort", getDrugIngredientCodes(cdm, name = "acetaminophen")
#' )
#' cdm[["dus_cohort"]] %>%
#'   addDrugUse(ingredientConceptId = 1125315)
#' }
#'
addDrugUse <- function(cohort,
                       indexDate = "cohort_start_date",
                       censorDate = "cohort_end_date",
                       ingredientConceptId = NULL,
                       conceptSet = NULL,
                       restrictIncident = TRUE,
                       gapEra = 0) {
  x <- cohort |>
    addDrugUseInternal(
      indexDate = indexDate,
      censorDate = censorDate,
      conceptSet = conceptSet,
      ingredientConceptId = ingredientConceptId,
      restrictIncident = restrictIncident,
      numberExposures = TRUE,
      numberEras = TRUE,
      exposedTime = TRUE,
      indexQuantity = TRUE,
      initialQuantity = TRUE,
      cumulativeQuantity = TRUE,
      indexDose = TRUE,
      initialDose = TRUE,
      cumulativeDose = TRUE,
      gapEra = gapEra,
      nameStyle = "{value}",
      name = NULL)

  return(x)
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
    dplyr::mutate(number_exposures = dplyr::if_else(
      is.na(.data$number_exposures), 0, .data$number_exposures
    )) %>%
    dplyr::compute(
      temporary = FALSE, overwrite = TRUE, name = uniqueTmpName()
    )
  if (quantity) {
    cohort <- cohort %>%
      dplyr::left_join(
        cohortInfo %>%
          dplyr::group_by(
            .data$subject_id, .data$cohort_start_date,
            .data$cohort_end_date
          ) %>%
          dplyr::filter(
            .data$drug_exposure_start_date <= .data$cohort_start_date
          ) %>%
          dplyr::summarise(
            initial_quantity = sum(.data$quantity, na.rm = TRUE),
            .groups = "drop"
          ),
        by = c(
          "subject_id", "cohort_start_date", "cohort_end_date"
        ),
      ) %>%
      dplyr::compute(
        temporary = FALSE, overwrite = TRUE, name = uniqueTmpName()
      )
  } else {
    cohort <- cohort %>% dplyr::select(-"cumulative_quantity")
  }
  return(cohort)
}

addInitialDailyDose <- function(cohort,
                                cohortInfo,
                                sameIndexMode,
                                units,
                                cdm) {
  if (length(units) == 0) {
    cohortInfo <- cohortInfo %>%
      dplyr::select("subject_id", "cohort_start_date", "cohort_end_date") %>%
      dplyr::mutate("initial_daily_dose" = as.numeric(NA))
  } else {
    cohortInfo <- cohortInfo %>%
      dplyr::group_by(
        .data$subject_id, .data$cohort_start_date,
        .data$cohort_end_date, .data$unit
      ) %>%
      dplyr::filter(
        .data$drug_exposure_start_date <= .data$cohort_start_date
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
    for (u in units) {
      cohortInfo <- cohortInfo %>%
        dplyr::mutate(
          !!paste0("initial_daily_dose_", u) := dplyr::if_else(
            .data$unit == .env$u, .data$initial_daily_dose, NA
          )
        )
    }
    cohortInfo <- cohortInfo %>%
      dplyr::select(-"initial_daily_dose", -"unit")
  }
  cohort <- cohort %>%
    dplyr::left_join(
      cohortInfo,
      by = c("subject_id", "cohort_start_date", "cohort_end_date")
    ) %>%
    dplyr::compute(
      temporary = FALSE, overwrite = TRUE, name = uniqueTmpName()
    )
  return(cohort)
}

addNumberEras <- function(cohort, cohortInfo, gapEra, cdm) {
  cohort %>%
    dplyr::left_join(
      cohortInfo %>%
        dplyr::select(
          "subject_id", "cohort_start_date", "cohort_end_date",
          "date_event" = "drug_exposure_start_date"
        ) %>%
        dplyr::mutate(value = -1) %>%
        dplyr::union_all(
          cohortInfo %>%
            dplyr::select(
              "subject_id", "cohort_start_date", "cohort_end_date",
              "date_event" = "drug_exposure_end_date"
            ) %>%
            dplyr::mutate(
              value = 1,
              date_event = !!CDMConnector::dateadd("date_event", gapEra + 1)
            )
        ) %>%
        dplyr::group_by(
          .data$subject_id, .data$cohort_start_date, .data$cohort_end_date
        ) %>%
        dbplyr::window_order(.data$date_event, .data$value) %>%
        dplyr::filter(cumsum(.data$value) == 0) %>%
        dplyr::summarise(
          number_eras = dplyr::n(), .groups = "drop"
        ),
      by = c("subject_id", "cohort_start_date", "cohort_end_date")
    ) %>%
    dplyr::mutate(number_eras = dplyr::if_else(
      is.na(.data$number_eras), 0, .data$number_eras
    )) %>%
    dplyr::compute(
      temporary = FALSE, overwrite = TRUE, name = uniqueTmpName()
    )
}

addCumulativeDose <- function(cohort,
                              cohortInfo,
                              cdm,
                              gapEra,
                              sameIndexMode,
                              overlapMode,
                              eraJoinMode,
                              units) {
  if (length(units) == 0) {
    cumDose <- cohortInfo %>%
      dplyr::select(
        "subject_id", "cohort_start_date", "cohort_end_date"
      ) %>%
      dplyr::distinct() %>%
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
    cumDose <- cohortInfo %>%
      dplyr::mutate(
        exposed_dose = .data$daily_dose * .data$subexposed_days
      ) %>%
      dplyr::group_by(
        .data$subject_id, .data$cohort_start_date, .data$cohort_end_date,
        .data$unit
      ) %>%
      dplyr::summarise(
        cumulative_dose = sum(
          .data$exposed_dose[.data$considered_subexposure == "yes"],
          na.rm = TRUE
        ),
        .groups = "drop"
      )
    for (u in units) {
      cumDose <- cumDose %>%
        dplyr::mutate(
          !!paste0("cumulative_dose_", u) := dplyr::if_else(
            .data$unit == .env$u, .data$cumulative_dose, NA
          )
        )
    }
    cumDose <- cumDose %>% dplyr::select(-"cumulative_dose", -"unit")
  }

  cohort %>%
    dplyr::left_join(
      cumDose,
      by = c("subject_id", "cohort_start_date", "cohort_end_date")
    ) %>%
    dplyr::compute(
      temporary = FALSE, overwrite = TRUE, name = uniqueTmpName()
    )
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
    dplyr::inner_join(conceptSet, by = "drug_concept_id") %>%
    dplyr::filter(
      (is.na(.data$drug_exposure_end_date) &
         (.data$drug_exposure_start_date <= .data$cohort_end_date)) |
        (!is.na(.data$drug_exposure_end_date) &
           ((.data$drug_exposure_end_date >= .data$cohort_start_date) &
              (.data$drug_exposure_start_date <= .data$cohort_end_date)))
    ) %>%
    dplyr::compute(
      temporary = FALSE, overwrite = TRUE, name = uniqueTmpName()
    )
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
    dplyr::compute(
      temporary = FALSE, overwrite = TRUE, name = uniqueTmpName()
    )

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
    dplyr::compute(
      temporary = FALSE, overwrite = TRUE, name = uniqueTmpName()
    )

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
    dplyr::compute(
      temporary = FALSE, overwrite = TRUE, name = uniqueTmpName()
    )

  return(x_intervals)
}

#' @noRd
addOverlappingFlag <- function(x) {
  x <- x %>%
    dplyr::group_by(
      .data$subject_id, .data$cohort_start_date, .data$cohort_end_date,
      .data$subexposure_id, .data$unit
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
        TRUE ~ "untreated"
      )
    ) %>%
    dplyr::filter(.data$type_subexposure != "untreated") %>%
    dplyr::ungroup()
  return(x)
}

#' @noRd
solveSameIndexOverlap <- function(x, cdm, sameIndexMode) {
  sameIndexMode <- tolower(sameIndexMode)
  x_same_index <- x %>%
    dplyr::group_by(
      .data$subject_id, .data$cohort_start_date, .data$cohort_end_date,
      .data$subexposure_id, .data$drug_exposure_start_date, .data$unit
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
        is.na(.data$considered_subexposure), "no", "yes"
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
      dplyr::mutate(considered_subexposure = "yes")
  }
  x <- x %>%
    dplyr::group_by(
      .data$subject_id, .data$cohort_start_date, .data$cohort_end_date,
      .data$subexposure_id, .data$drug_exposure_start_date, .data$unit
    ) %>%
    dplyr::filter(dplyr::n() == 1) %>%
    dplyr::ungroup() %>%
    dplyr::union_all(x_same_index %>% dplyr::ungroup()) %>%
    dplyr::compute(
      temporary = FALSE, overwrite = TRUE, name = uniqueTmpName()
    )

  return(x)
}

#' @noRd
solveOverlap <- function(x, cdm, overlapMode) {
  overlapMode <- tolower(overlapMode)
  x_overlap <- x %>%
    dplyr::group_by(
      .data$subject_id, .data$cohort_start_date, .data$cohort_end_date,
      .data$subexposure_id, .data$unit
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
      dplyr::compute(
        temporary = FALSE, overwrite = TRUE, name = uniqueTmpName()
      )
    x <- x %>%
      dplyr::group_by(
        .data$subject_id, .data$cohort_start_date, .data$cohort_end_date,
        .data$subexposure_id, .data$unit
      ) %>%
      dplyr::filter(
        is.na(.data$considered_subexposure) |
          .data$considered_subexposure == "yes"
      ) %>%
      dplyr::filter(dplyr::n() == 1) %>%
      dplyr::mutate(considered_subexposure = dplyr::if_else(
        .data$type_subexposure == "exposed" & is.na(.data$considered_subexposure),
        "yes",
        .data$considered_subexposure
      )) %>%
      dplyr::ungroup() %>%
      dplyr::union_all(x_overlap) %>%
      dplyr::compute(
        temporary = FALSE, overwrite = TRUE, name = uniqueTmpName()
      )
  }
  return(x)
}

#' @noRd
addGapDailyDose <- function(x, cdm, eraJoinMode) {
  eraJoinMode <- tolower(eraJoinMode)
  x_gaps_dose <- x %>%
    dplyr::filter(.data$type_subexposure == "gap")
  if (x_gaps_dose %>% dplyr::tally() %>% dplyr::pull() == 0) {
    return(x)
  }
  if (eraJoinMode == "zero") {
    return(
      x %>%
        dplyr::filter(.data$type_subexposure != "gap")
    )
  } else if (eraJoinMode == "previous") {
    x_gaps_dose <- x_gaps_dose %>%
      dplyr::select(-"daily_dose", -"unit") %>%
      dplyr::inner_join(
        x %>%
          dplyr::mutate(subexposure_id = .data$subexposure_id + 1) %>%
          dplyr::filter(.data$considered_subexposure == "yes") %>%
          dplyr::group_by(
            .data$subject_id, .data$cohort_start_date, .data$subexposure_id,
            .data$unit
          ) %>%
          dplyr::mutate(daily_dose = sum(.data$daily_dose, na.rm = TRUE)) %>%
          dplyr::ungroup() %>%
          dplyr::select(
            "subject_id", "cohort_start_date", "cohort_end_date",
            "subexposure_id", "daily_dose", "unit"
          ) %>%
          dplyr::distinct(),
        by = c(
          "subject_id", "cohort_start_date", "cohort_end_date",
          "subexposure_id"
        )
      )
  } else if (eraJoinMode == "subsequent") {
    x_gaps_dose <- x_gaps_dose %>%
      dplyr::select(-"daily_dose", -"unit") %>%
      dplyr::inner_join(
        x %>%
          dplyr::mutate(subexposure_id = .data$subexposure_id - 1) %>%
          dplyr::filter(.data$considered_subexposure == "yes") %>%
          dplyr::group_by(
            .data$subject_id, .data$cohort_start_date, .data$subexposure_id,
            .data$unit
          ) %>%
          dplyr::mutate(daily_dose = sum(.data$daily_dose, na.rm = TRUE)) %>%
          dplyr::ungroup() %>%
          dplyr::select(
            "subject_id", "cohort_start_date", "cohort_end_date",
            "subexposure_id", "daily_dose", "unit"
          ) %>%
          dplyr::distinct(),
        by = c(
          "subject_id", "cohort_start_date", "cohort_end_date",
          "subexposure_id"
        )
      )
  }
  x <- x %>%
    dplyr::filter(.data$type_subexposure != "gap") %>%
    dplyr::union_all(
      x_gaps_dose %>% dplyr::mutate(considered_subexposure = "yes")
    ) %>%
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
    cohort <- cohort %>%
      dplyr::left_join(
        cohortInfo %>%
          dplyr::group_by(
            .data$subject_id, .data$cohort_start_date, .data$cohort_end_date
          ) %>%
          dplyr::summarise(
            !!label := 100 * sum(.data$impute, na.rm = TRUE) / dplyr::n(),
            .groups = "drop"
          ),
        by = c("subject_id", "cohort_start_date", "cohort_end_date")
      )
  }
  return(cohort)
}

addDrugUseInternal <- function(x,
                               indexDate,
                               censorDate,
                               conceptSet,
                               ingredientConceptId,
                               restrictIncident,
                               numberExposures,
                               numberEras,
                               exposedTime,
                               indexQuantity,
                               initialQuantity,
                               cumulativeQuantity,
                               indexDose,
                               initialDose,
                               cumulativeDose,
                               gapEra,
                               nameStyle,
                               name,
                               call = parent.frame()) {
  # initial checks
  x <- validateX(x, call)
  cdm <- omopgenerics::cdmReference(x)
  indexDate <- validateIndexDate(indexDate, x, call)
  ingredientConceptId <- validateIngredientConceptId(ingredientConceptId, cdm, call)
  conceptSet <- validateConceptSet(conceptSet, ingredientConceptId, cdm, call)
  restrictIncident <- validateLogical(restrictIncident, "restrictIncident", call)
  numberExposures <- validateLogical(numberExposures, "numberExposures", call)
  numberEras <- validateLogical(numberEras, "numberEras", call)
  exposedTime <- validateLogical(exposedTime, "exposedTime", call)
  indexQuantity <- validateLogical(indexQuantity, "indexQuantity", call)
  initialQuantity <- validateLogical(initialQuantity, "initialQuantity", call)
  cumulativeQuantity <- validateLogical(cumulativeQuantity, "cumulativeQuantity", call)
  indexDose <- validateLogical(indexDose, "indexDose", call)
  initialDose <- validateLogical(initialDose, "initialDose", call)
  cumulativeDose <- validateLogical(cumulativeDose, "cumulativeDose", call)
  gapEra <- validateGapEra(gapEra, call)
  values <- c(
    numberExposures, numberEras, exposedTime, indexQuantity, initialQuantity,
    cumulativeQuantity, indexDose, initialDose, cumulativeDose
  )
  values <- values[values]
  nameStyle <- validateNameStyle(
    nameStyle, ingredientConceptId, conceptSet, values, call)
  name <- validateName(name, cdm, call)

  if ((indexDose | initialDose | cumulativeDose) & is.null(ingredientConceptId)) {
    "{.strong ingredientConceptId} can not be NULL for dose calculations" |>
      cli::cli_abort(call = call)
  }
  tablePrefix <- omopgenerics::tmpPrefix()

  nm1 <- omopgenerics::uniqueTableName(tablePrefix)
  cdm <- omopgenerics::insertTable(
    cdm = cdm,
    name = nm1,
    table = dplyr::tibble("drug_concept_id" = unlist(conceptSet)),
    temporary = FALSE
  )

  id <- omopgenerics::getPersonIdentifier(x)
  idFuture <- omopgenerics::uniqueId()

  xdates <- x|>
    dplyr::select(dplyr::all_of(c(id, indexDate, censorDate))) |>
    dplyr::distinct() |>
    PatientProfiles::addFutureObservation(
      indexDate = indexDate,
      futureObservationName = idFuture,
      futureObservationType = "date",
      name = omopgenerics::uniqueTableName(tablePrefix)
    )
  if (is.null(censorDate)) {
    cols <- c(id, indexDate)
    censorDate <- idFuture
  } else {
    xdates <- xdates |>
      dplyr::mutate(!!censorDate := dplyr::if_else(
        is.na(.data[[censorDate]]),
        .data[[idFuture]],
        .data[[censorDate]]
      )) |>
      dplyr::select(-dplyr::all_of(idFuture))
    cols <- c(id, indexDate, censorDate)
  }

  drugData <- xdates |>
    dplyr::inner_join(
      cdm$drug_exposure |>
        dplyr::inner_join(cdm[[nm1]], by = "drug_concept_id") |>
        dplyr::select(
          !!id := "person_id", "drug_exposure_start_date",
          "drug_exposure_end_date", "quantity", "drug_concept_id"
        ),
      by = id
    ) |>
    dplyr::mutate("drug_exposure_end_date" = dplyr::if_else(
      is.na(.data$drug_exposure_end_date),
      .data$drug_exposure_start_date,
      .data$drug_exposure_end_date
    )) |>
    dplyr::filter(
      .data$drug_exposure_start_date <= .data$seug_exposure_end_date
    )
  if (restrictIncident) {
    drugData <- drugData |>
      dplyr::filter(
        .data$drug_exposure_start_date >= .data[[indexDate]] &
          .data$drug_exposure_start_date <= .data[[censorDate]]
      )
  } else {
    drugData <- drugData |>
      dplyr::filter(
        .data$drug_exposure_start_date <= .data[[censorDate]] &
          .data$drug_exposure_end_date >= .data[[indexDate]]
      )
  }
  drugData <- drugData |>
    dplyr::compute(
      name = omopgenerics::uniqueTableName(tablePrefix), temporary = FALSE
    )

  if (numberEras | exposedTime) {
    drugDataErafied <- drugData |>
      erafy(
        start = "drug_exposure_start_date",
        end = "drug_exposure_end_date",
        group = cols,
        gap = gapEra
      ) |>
      dplyr::compute(
        name = omopgenerics::uniqueTableName(tablePrefix), temporary = FALSE
      )
  }

  if (numberExposures) {
    nameCol <- getColName("number_exposures", nameStyle)
    x <- x |>
      dplyr::left_join(
        drugData |>
          dplyr::count(dplyr::across(dplyr::all_of(cols)), name = nameCol),
        by = cols
      ) |>
      dplyr::mutate(
        !!nameCol := dplyr::coalesce(as.integer(.data[[nameCol]]), 0L)
      )
  }

  if (numberEras) {
    nameCol <- getColName("number_eras", nameStyle)
    x <- x |>
      dplyr::left_join(
        drugDataErafied |>
          dplyr::count(dplyr::across(dplyr::all_of(cols)), name = nameCol),
        by = cols
      ) |>
      dplyr::mutate(
        !!nameCol := dplyr::coalesce(as.integer(.data[[nameCol]]), 0L)
      )
  }

  if (exposedTime) {
    col <- getColName("exposed_time", nameStyle)
    x <- x |>
      dplyr::left_join(
        drugDataErafied %>%
          dplyr::mutate("exposed_time" = as.integer(!!CDMConnector::datediff(
            start = "drug_exposure_start_date",
            end = "drug_exposure_end_date",
            interval = "day"
          )) + 1L) |>
          dplyr::group_by(dplyr::across(dplyr::all_of(cols))) |>
          dplyr::summarise(
            !!col := sum(.data$exposed_time, na.rm = TRUE),
            .groups = "drop"
          ),
        by = cols
      ) |>
      dplyr::mutate(!!col := dplyr::coalesce(.data$exposed_time, 0L))
  }

  if (indexDose | cumulativeDose | initialDose) {
    drugData <- drugData |>
      addDailyDose(
        ingredientConceptId = ingredientConceptId,
        name = omopgenerics::uniqueTableName(tablePrefix)
      )
    unit <- drugData |>
      dplyr::select("unit") |>
      dplyr::distinct() |>
      dplyr::pull()
  }

  if (indexQuantity | indexDose) {
    qIndex <- c(
      "sum(.data$quantity, na.rm = TRUE)", "sum(.data$daily_dose, na.rm = TRUE)"
    ) |>
      rlang::parse_exprs() |>
      rlang::set_names(c(
        getColName(c("index_quantity", paste0("index_dose_", unit)), nameStyle)
      ))
    qIndex <- qIndex[c(indexQuantity, indexDose)]
    x <- x |>
      dplyr::left_join(
        drugData |>
          dplyr::filter(
            .data[[indexDate]] >= .data$drug_exposure_start_date &
              .data[[indexDate]] <= .data$drug_exposure_end_date
          ) |>
          dplyr::group_by(dplyr::across(dplyr::all_of(cols))) |>
          dplyr::summarise(!!!qIndex, .groups = "drop"),
        by = cols
      )
  }

  if (initialQuantity | initialDose) {
    qInitial <- c(
      "sum(.data$quantity, na.rm = TRUE)", "sum(.data$daily_dose, na.rm = TRUE)"
    ) |>
      rlang::parse_exprs() |>
      rlang::set_names(c(
        getColName(c("initial_quantity", paste0("initial_dose_", unit)), nameStyle)
      ))
    qIndex <- qIndex[c(indexQuantity, indexDose)]
    x <- x |>
      dplyr::left_join(
        drugData |>
          dplyr::group_by(dplyr::across(dplyr::all_of(cols))) |>
          dplyr::filter(
            .data[[indexDate]] == min(.data[[indexDate]], na.rm = TRUE)
          ) |>
          dplyr::summarise(!!!qInitial, .groups = "drop"),
        by = cols
      )
  }

  if (cumulativeDose | cumulativeQuantity) {
    qCumulative <- c(
      'sum(.data$quantity * .data$corrector_factor, na.rm = TRUE)',
      "sum(.data$daily_dose * .data$exposed_days, na.rm = TRUE)"
    ) |>
      rlang::parse_exprs() |>
      rlang::set_names(getColName(
        c("cumulative_quantity", paste0("cumulative_dose_", unit)), nameStyle
      ))
    qCumulative <- qCumulative[c(cumulativeQuantity, cumulativeDose)]
    newVariables <- c(
      "dplyr::if_else(
          .data[['{indexDate}']] <= .data$drug_exposure_start_date,
          as.numeric(.data$drug_exposure_start_date),
          as.numeric(.data[['{indexDate}']])
        )",
      "dplyr::if_else(
          .data[['{censorDate}']] >= .data$drug_exposure_end_date,
          as.numeric(.data$drug_exposure_end_date),
          as.numeric(.data[['{censorDate}']])
        )",
      ".data$end - .data$start + 1",
      ".data$exposed_days / (as.numeric(.data$drug_exposure_end_date) -
            as.numeric(.data$drug_exposure_start_date) + 1)"
    ) |>
      rlang::parse_exprs() |>
      rlang::set_names(c("start", "end", "exposed_days", "corrector_factor"))
    if (!cumulativeQuantity) newVariables <- newVariables[1:3]

    x <- x |>
      dplyr::left_join(
        drugData |>
          dplyr::mutate(!!!newVariables) |>
          dplyr::group_by(dplyr::across(dplyr::all_of(cols))) |>
          dplyr::summarise(!!!qCumulative, .groups = "drop"),
        by = cols
      )
  }

  if (is.null(name)) {
    x <- x |> dplyr::compute()
  } else {
    x <- x |> dplyr::compute(name = name, temporary = FALSE)
  }

  omopgenerics::dropTable(cdm = cdm, name = dplyr::starts_with(tablePrefix))

  return(x)
}
getColName <- function(nm, nameStyle) {
  glue::glue(nameStyle, value = nm) |> as.character()
}

validateX <- function(x, call) {
  assertClass(x, "cdm_table", call = call)
  id <- c("subject_id", "person_id")
  id <- id[id %in% colnames(x)]
  if (length(id) == 0) {
    "person_id or subject_id must be columns in x" |>
      cli::cli_abort(call = call)
  }
  if (length(id) == 2) {
    "person_id and subject_id must not be columns in x" |>
      cli::cli_abort(call = call)
  }
  return(invisible(x))
}
validateConceptSet <- function(conceptSet, ingredientConceptId, cdm, call) {
  if (is.null(conceptSet)) {
    if (is.null(ingredientConceptId)) {
      "Either conceptSet or ingredientConceptId must be provided." |>
        cli::cli_abort(call = call)
    }
    conceptSet <- cdm$concept_ancestor |>
      dplyr::filter(.data$ancestor_concept_id %in% .env$ingredientConceptId) |>
      dplyr::select("ancestor_concept_id", "descendant_concept_id") |>
      dplyr::collect() |>
      dplyr::arrange(.data$ancestor_concept_id) |>
      dplyr::group_by(.data$ancestor_concept_id) |>
      dplyr::group_split() |>
      lapply(dplyr::pull, "descendant_concept_id") |>
      rlang::set_names(paste0("ingredient_", sort(ingredientConceptId), "_descendants"))

  }
  conceptSet <- omopgenerics::newCodelist(conceptSet)
  return(invisible(conceptSet))
}
validateIngredientConceptId <- function(ingredientConceptId, cdm, call) {
  if (is.null(ingredientConceptId)) return(invisible(ingredientConceptId))
  assertNumeric(
    ingredientConceptId, integerish = TRUE, min = 0, unique = TRUE, call = call
  )
  ingredients <- cdm$concept |>
    dplyr::filter(.data$concept_class_id == "Ingredient") |>
    dplyr::filter(.data$concept_id %in% .env$ingredientConceptId) |>
    dplyr::pull("concept_id") |>
    as.integer()
  missingIngredients <- ingredientConceptId[
    !as.integer(ingredientConceptId) %in% ingredients]
  if (length(missingIngredients) > 0) {
    "Ingredients not present in concept table: {missingIngredients}" |>
      cli::cli_abort(call = call)
  }
  return(ingredients)
}
validateIndexDate <- function(indexDate, x, call) {
  msg <- "{.strong indexDate} must point to date column in x"
  assertCharacter(indexDate, length = 1, call = call, msg = msg)
  if (!indexDate %in% colnames(x)) cli::cli_abort(message = msg, call = call)
  type <- x |> utils::head(1) |> dplyr::pull(indexDate) |> dplyr::type_sum()
  if (type != "date") cli::cli_abort(message = msg, call = call)
  return(invisible(indexDate))
}
validateCensorDate <- function(censorDate, x, call) {
  if (is.null(censorDate)) return(invisible(censorDate))
  msg <- "{.strong censorDate} must be NULL or point to date column in x"
  assertCharacter(censorDate, length = 1, call = call, msg = msg)
  if (!censorDate %in% colnames(x)) cli::cli_abort(message = msg, call = call)
  type <- x |> utils::head(1) |> dplyr::pull(censorDate) |> dplyr::type_sum()
  if (type != "date") cli::cli_abort(message = msg, call = call)
  return(invisible(censorDate))
}
validateLogical <- function(x, nm, call) {
  msg <- paste0("{.strong ", nm, "} must be TRUE or FALSE")
  assertLogical(x, length = 1, msg = msg, call = call)
  return(invisible(x))
}
validateGapEra <- function(gapEra, call) {
  assertNumeric(gapEra, integerish = TRUE, min = 0, length = 1, call = call)
  return(invisible(gapEra))
}
validateNameStyle(nameStyle, ingredientConceptId, conceptSet, values, call) {
  assertCharacter(nameStyle, length = 1, call = call)
  msg <- character()
  if (length(ingredientConceptId) > 1 && !grepl("\\{ingredient\\}", nameStyle)) {
    msg <- c(msg, "{{ingredient}} must be part of nameStyle")
  }
  if (length(conceptSet) > 1 && !grepl("\\{concept_name\\}", nameStyle)) {
    msg <- c(msg, "{{concept_name}} must be part of nameStyle")
  }
  if (length(values) > 1 && !grepl("\\{value\\}", nameStyle)) {
    msg <- c(msg, "{{value}} must be part of nameStyle")
  }
  if (length(msg) > 1) {
    cli::cli_abort(message = msg, call = call)
  }
}
validateName <- function(name, cdm, call) {
  assertCharacter(name, length = 1, na = FALSE, null = TRUE, call = call)
  if (!is.null(name) && name %in% names(cdm)) {
    c("!" = "table {.strong {name}} already exist in the cdm and will be overwritten") |>
      cli::cli_inform()
  }
  return(invisible(name))
}

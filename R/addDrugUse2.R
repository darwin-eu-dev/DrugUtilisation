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

# NEW NAME SUGGESTIONS
# addDrugUsage # NMB
# addDrugUtilisation # NMB
# addDrugUse2
# addDrugInformation
# addUseOfDrugs
# addDrugSummary
# addDrugUseSummary

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
#' @param numberExposures Whether to add a column with the number of exposures.
#' @param numberEras Whether to add a column with the number of eras.
#' @param exposedTime Whether to add a column with the number of exposed days.
#' @param timeToExposure Whether to add a column with the number of days between
#' indexDate and start of the first exposure.
#' @param initialQuantity Whether to add a column with the initial quantity.
#' @param cumulativeQuantity Whether to add a column with the cumulative
#' quantity during the whole exposure.
#' @param initialDailyDose Whether to add a column with the initial daily dose.
#' @param cumulativeDose Whether to add a column with the cumulative dose.
#' @param nameStyle Character string to specify the nameStyle of the new columns.
#' @param name Name of the new computed column if NULL a temporary tables is
#' created.
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
#'   addDrugUse2(ingredientConceptId = 1125315)
#' }
#'
addDrugUse2 <- function(cohort,
                       indexDate = "cohort_start_date",
                       censorDate = "cohort_end_date",
                       ingredientConceptId = NULL,
                       conceptSet = NULL,
                       restrictIncident = TRUE,
                       gapEra = 0,
                       numberExposures = TRUE,
                       numberEras = TRUE,
                       exposedTime = TRUE,
                       timeToExposure = TRUE,
                       initialQuantity = TRUE,
                       cumulativeQuantity = TRUE,
                       initialDailyDose = TRUE,
                       cumulativeDose = TRUE,
                       nameStyle = "{value}_{concept_name}_{ingredient}",
                       name = NULL) {
  cohort |>
    addDrugUseInternal(
      indexDate = indexDate,
      censorDate = censorDate,
      conceptSet = conceptSet,
      ingredientConceptId = ingredientConceptId,
      restrictIncident = restrictIncident,
      numberExposures = numberExposures,
      numberEras = numberEras,
      exposedTime = exposedTime,
      timeToExposure = timeToExposure,
      initialQuantity = initialQuantity,
      cumulativeQuantity = cumulativeQuantity,
      initialDailyDose = initialDailyDose,
      cumulativeDose = cumulativeDose,
      gapEra = gapEra,
      nameStyle = nameStyle,
      name = name)
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
                               timeToExposure,
                               initialQuantity,
                               cumulativeQuantity,
                               initialDailyDose,
                               cumulativeDose,
                               gapEra,
                               name,
                               nameStyle,
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
  initialDailyDose <- validateLogical(initialDailyDose, "initialDailyDose", call)
  cumulativeDose <- validateLogical(cumulativeDose, "cumulativeDose", call)
  gapEra <- validateGapEra(gapEra, call)
  values <- c(
    numberExposures, numberEras, exposedTime, indexQuantity, initialQuantity,
    cumulativeQuantity, indexDose, initialDailyDose, cumulativeDose
  )
  values <- values[values]
  nameStyle <- validateNameStyle(nameStyle, ingredientConceptId, conceptSet, values, call)
  name <- validateName(name, cdm, call)
  nameStyle <- gsub("{value}", "{.value}", x = nameStyle, fixed = TRUE)
  nameStyleI <- noIngredientNameStyle(nameStyle)

  if ((indexDose | initialDailyDose | cumulativeDose) & is.null(ingredientConceptId)) {
    "{.strong ingredientConceptId} can not be NULL for dose calculations" |>
      cli::cli_abort(call = call)
  }
  tablePrefix <- omopgenerics::tmpPrefix()

  nm1 <- omopgenerics::uniqueTableName(tablePrefix)
  cdm <- omopgenerics::insertTable(
    cdm = cdm, name = nm1, table = conceptSetTibble(conceptSet), temporary = F
  )

  id <- omopgenerics::getPersonIdentifier(x)
  idFuture <- omopgenerics::uniqueId(exclude = colnames(x))

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
          "drug_exposure_end_date", "quantity", "drug_concept_id",
          "concept_name"
        ),
      by = id
    ) |>
    dplyr::mutate("drug_exposure_end_date" = dplyr::if_else(
      is.na(.data$drug_exposure_end_date),
      .data$drug_exposure_start_date,
      .data$drug_exposure_end_date
    )) |>
    dplyr::filter(
      .data$drug_exposure_start_date <= .data$drug_exposure_end_date
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

  if (cumulativeQuantity | numberExposures | timeToExposure) {
    qs <- c(
      "as.integer(dplyr::n())",
      "min(.data$drug_exposure_start_date, na.rm = TRUE)",
      "as.numeric(sum(.data$quantity, na.rm = TRUE))"
    ) |>
      rlang::parse_exprs() |>
      rlang::set_names(c(
        "number_exposures", "time_to_exposure","cumulative_quantity"
      ))
    qs <- qs[c(numberExposures, timeToExposure, cumulativeQuantity)]
    toJoin <- drugData |>
      dplyr::group_by(dplyr::across(dplyr::all_of(c(cols, "concept_name")))) |>
      dplyr::summarise(!!!qs, .groups = "drop")
    if (timeToExposure) {
      toJoin <- toJoin %>%
        dplyr::mutate("time_to_exposure" = dplyr::if_else(
          .data$time_to_exposure <= .data[[indexDate]],
          0L,
          as.integer(!!CDMConnector::datediff(start = indexDate, end = "time_to_exposure"))
        ))
    }
    x <- x |>
      dplyr::left_join(
        toJoin |>
          tidyr::pivot_wider(
            names_from = "concept_name",
            names_glue = nameStyleI,
            values_from = dplyr::all_of(names(qs))
          ),
        by = cols
      ) |>
      dplyr::mutate(dplyr::across(
        dplyr::starts_with(names(qs)), ~ dplyr::coalesce(.x, 0L)
      )) |>
      compute2(name)
  }

  if (initialQuantity) {
    x <- x |>
      dplyr::left_join(
        drugData |>
          dplyr::group_by(dplyr::across(dplyr::all_of(c(cols, "concept_name")))) |>
          dplyr::filter(.data$drug_exposure_start_date == min(.data$drug_exposure_start_date, na.rm = TRUE)) |>
          dplyr::summarise("initial_quantity" = as.numeric(sum(.data$quantity, na.rm = TRUE)), .groups = "drop") |>
          tidyr::pivot_wider(
            names_from = "concept_name",
            names_glue = nameStyleI,
            values_from = "initial_quantity"
          ),
        by = cols
      ) |>
      dplyr::mutate(dplyr::across(
        dplyr::starts_with("initial_quantity"), ~ dplyr::coalesce(.x, 0L)
      )) |>
      compute2(name)
  }

  if (numberEras | exposedTime) {
    toJoin <- drugData |>
      erafy(
        start = "drug_exposure_start_date",
        end = "drug_exposure_end_date",
        group = c(cols, "concept_name"),
        gap = gapEra
      )
    if (exposedTime) {
      toJoin <- toJoin %>%
        dplyr::mutate("exposed_time" = as.integer(!!CDMConnector::datediff(
          start = "drug_exposure_start_date",
          end = "drug_exposure_end_date",
          interval = "day"
        )) + 1L)
    }
    qs <- c(
      "as.integer(dplyr::n())", "sum(.data$exposed_time, na.rm = TRUE)"
    ) |>
      rlang::parse_exprs() |>
      rlang::set_names(c("number_eras", "exposed_time"))
    qs <- qs[c(numberEras, exposedTime)]
    x <- x |>
      dplyr::left_join(
        toJoin |>
          dplyr::group_by(dplyr::across(dplyr::all_of(c(cols, "concept_name")))) |>
          dplyr::summarise(!!!qs, .groups = "drop") |>
          tidyr::pivot_wider(
            names_from = "concept_name",
            names_glue = nameStyleI,
            values_from = dplyr::all_of(names(qs))
          ),
        by = cols
      ) |>
      dplyr::mutate(dplyr::across(
        dplyr::starts_with(names(qs)), ~ dplyr::coalesce(.x, 0L)
      )) |>
      compute2(name)
  }

  if (initialDailyDose | cumulativeDose) {
    if (!cumulativeDose) {
      doseData <- drugData |>
        dplyr::group_by(dplyr::across(c(cols, "concept_name"))) |>
        dplyr::filter(.data$drug_exposure_start_date == min(
          .data$drug_exposure_start_date, na.rm = TRUE
        )) |>
        dplyr::ungroup()
    } else {
      id <- omopgenerics::uniqueId(n = 2, exclude = colnames(drugData))
      doseData <- drugData %>%
        dplyr::mutate(
          !!id[1] := dplyr::if_else(
            .data[[indexDate]] <= .data$drug_exposure_start_date,
            .data$drug_exposure_start_date,
            .data[[indexDate]]
          ),
          !!id[2] := dplyr::if_else(
            .data[[censorDate]] >= .data$drug_exposure_end_date,
            .data$drug_exposure_end_date,
            .data[[censorDate]]
          ),
          "exposure_duration" = as.integer(!!CDMConnector::datediff(
            start = id[1], end = id[2]))
        ) |>
        dplyr::select(!id)
    }
    doseData <- doseData |>
      dplyr::compute(
        name = omopgenerics::uniqueTableName(tablePrefix), temporary = FALSE
      )

    for (k in seq_along(ingredientConceptId)) {
      nm <- omopgenerics::uniqueTableName(tablePrefix)
      toJoin <- doseData |>
        addDailyDose(ingredientConceptId = ingredientConceptId[k], name = nm)
      if (cumulativeDose) {
        x <- x |>
          dplyr::left_join(
            toJoin |>
              dplyr::group_by(dplyr::across(c(cols, "concept_name", "unit"))) |>
              dplyr::summarise(
                "cumulative_dose" = as.integer(sum(.data$daily_dose * .data$exposed_time, na.rm = TRUE)),
                .groups = "drop"
              ) |>
              tidyr::pivot_wider(
                names_from = c("concept_name", "unit"),
                names_glue = !!ingredientNameStyle(nameStyle, ingredientConceptId[k]),
                values_from = "cumulative_dose"
              ),
            by = cols
          ) |>
          dplyr::mutate(dplyr::across(
            dplyr::starts_with("cumulative"), ~ dplyr::coalesce(.x, 0L)
          )) |>
          compute2(name)
        if (initialDailyDose) {
          toJoin <- toJoin |>
            dplyr::group_by(dplyr::across(c(cols, "concept_name"))) |>
            dplyr::filter(.data$drug_exposure_start_date == min(
              .data$drug_exposure_start_date, na.rm = TRUE
            )) |>
            dplyr::ungroup()
        }
      }
      if (initialDailyDose) {
        x <- x |>
          dplyr::left_join(
            toJoin |>
              dplyr::group_by(dplyr::across(c(cols, "concept_name", "unit"))) |>
              dplyr::summarise(
                "initial_daily_dose" = as.integer(sum(.data$daily_dose, na.rm = TRUE)),
                .groups = "drop"
              ) |>
              tidyr::pivot_wider(
                names_from = c("concept_name", "unit"),
                names_glue = !!ingredientNameStyle(nameStyle, ingredientConceptId[k]),
                values_from = "initial_daily_dose"
              ),
            by = cols
          ) |>
          dplyr::mutate(dplyr::across(
            dplyr::starts_with("initial_daily_dose"), ~ dplyr::coalesce(.x, 0L)
          )) |>
          compute2(name)
      }
    }
  }

  omopgenerics::dropTable(cdm = cdm, name = dplyr::starts_with(tablePrefix))

  return(x)
}
compute2 <- function(x, name) {
  if (is.null(name)) {
    x <- x |> dplyr::compute()
  } else {
    x <- x |> dplyr::compute(name = name, temporary = FALSE)
  }
  return(x)
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
    ingredientConceptId, integerish = TRUE, min = 0, unique = TRUE, call = call,
    length = 1 # to be removed when multiple ingredients are supported
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
validateNameStyle <- function(nameStyle, ingredientConceptId, conceptSet, values, call) {
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
  return(invisible(nameStyle))
}
validateName <- function(name, cdm, call) {
  assertCharacter(name, length = 1, na = FALSE, null = TRUE, call = call)
  if (!is.null(name) && name %in% names(cdm)) {
    c("!" = "table {.strong {name}} already exist in the cdm and will be overwritten") |>
      cli::cli_inform()
  }
  return(invisible(name))
}

noIngredientNameStyle <- function(x) {
  x <- gsub("_{ingredient}", "", x, fixed = TRUE)
  x <- gsub("{ingredient}_", "", x, fixed = TRUE)
  return(x)
}
ingredientNameStyle <- function(x, ing) {
  x <- gsub("{ingredient}", as.character(ing), x, fixed = TRUE)
  x <- gsub("{.value}", "{.value}_{unit}", x, fixed = TRUE)
  return(x)
}
conceptSetTibble <- function(conceptSet) {
  purrr::map(conceptSet, dplyr::as_tibble) |>
    dplyr::bind_rows(.id = "concept_name") |>
    dplyr::rename("drug_concept_id" = "value")
}

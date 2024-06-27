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
addDrugUse2 <- function(cohort,
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
      timeToExposure = TRUE,
      initialQuantity = TRUE,
      cumulativeQuantity = TRUE,
      initialDose = TRUE,
      cumulativeDose = TRUE,
      gapEra = gapEra,
      nameStyle = "{value}",
      name = NULL)

  return(x)
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
    cdm = cdm, name = nm1, table = conceptSetTibble(conceptSet), temporary = F
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
    cols <- c(id, indexDate, "concept_name")
    censorDate <- idFuture
  } else {
    xdates <- xdates |>
      dplyr::mutate(!!censorDate := dplyr::if_else(
        is.na(.data[[censorDate]]),
        .data[[idFuture]],
        .data[[censorDate]]
      )) |>
      dplyr::select(-dplyr::all_of(idFuture))
    cols <- c(id, indexDate, censorDate, "concept_name")
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
}
validateName <- function(name, cdm, call) {
  assertCharacter(name, length = 1, na = FALSE, null = TRUE, call = call)
  if (!is.null(name) && name %in% names(cdm)) {
    c("!" = "table {.strong {name}} already exist in the cdm and will be overwritten") |>
      cli::cli_inform()
  }
  return(invisible(name))
}

conceptSetTibble <- function(conceptSet) {
  purrr::map(conceptSet, dplyr::as_tibble) |>
    dplyr::bind_rows(.id = "concept_name") |>
    dplyr::rename("drug_concept_id" = "value")
}

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

# check functions

checkInputs <- function(...) {
  inputs <- list(...)
  lapply(names(inputs), function(x) {
    funName <- paste0(
      "check", toupper(substr(x, 1, 1)), substr(x, 2, nchar(x))
    )
    varName <- eval(parse(text = paste0("names(formals(", funName, "))")))
    eval(parse(text = paste0(
      funName, "(",
      paste0(paste0("inputs[[\"", varName, "\"]]"), collapse = ", "), ")"
    )))
  })
  invisible(NULL)
}

checkCdm <- function(cdm) {
  if (!("cdm_reference" %in% class(cdm))) {
    cli::cli_abort(
      "Argument cdm is not a valid cdm reference, please use
      CDMConnector::cdmFromCon() to create a valid cdm reference"
    )
  }
}

checkConceptSet <- function(conceptSet) {
  errorMessage <- "conceptSet must be a uniquely named list of integerish,
  no NA are allowed"
  if (!is.list(conceptSet)) {
    cli::cli_abort(errorMessage)
  }
  if (!all(sapply(conceptSet, is.numeric))) {
    cli::cli_abort(errorMessage)
  }
  x <- unlist(conceptSet)
  if (any(is.na(x))) {
    cli::cli_abort(errorMessage)
  }
  if (any(abs(x - round(x)) > sqrt(.Machine$double.eps))) {
    cli::cli_abort(errorMessage)
  }
  if (length(names(x)) != length(x)) {
    cli::cli_abort(errorMessage)
  }
  if (length(names(x)) != length(unique(names(x)))) {
    cli::cli_abort(errorMessage)
  }
}


checkName <- function(name, cdm) {
  checkmate::assertCharacter(name, len = 1, any.missing = FALSE)
  if (name %in% CDMConnector::tbl_group("all")) {
    cli::cli_abort(
      'name can not one of the stadard tables of the cdm. To see standard
      tables: CDMConnector::tbl_group("all")'
    )
  }
  if (name %in% names(cdm)) {
    cli::cli_warn(
      "A cohort with this name already exist in the cdm object. It will be overwritten."
    )
  }
}


checkLimit <- function(limit) {
  errorMessage <- "limit must be a choice between: 'all' and 'first'"
  if (!is.character(limit) | length(limit) > 1) {
    cli::cli_abort(errorMessage)
  }
  if (!(limit %in% c("all", "first"))) {
    cli::cli_abort(errorMessage)
  }
}


checkPriorObservation <- function(priorObservation) {
  checkmate::assertIntegerish(
    priorObservation,
    lower = 0, any.missing = F, len = 1, null.ok = T
  )
}

checkGapEra <- function(gapEra) {
  checkmate::assertIntegerish(gapEra, lower = 0, any.missing = F, len = 1)
}

checkGap <- function(gap) {
  checkmate::assertIntegerish(gap, lower = 0, any.missing = F, len = 1)
}

checkPriorUseWashout <- function(priorUseWashout) {
  if (!(is.numeric(priorUseWashout) & length(priorUseWashout) == 1 & is.infinite(priorUseWashout))) {
    checkmate::assertIntegerish(
      priorUseWashout,
      lower = 0, any.missing = F, len = 1
    )
  }
}

checkCohortDateRange <- function(cohortDateRange) {
  checkmate::assertDate(cohortDateRange, len = 2)
  if (!is.na(cohortDateRange[1]) &
    !is.na(cohortDateRange[2]) &
    cohortDateRange[1] > cohortDateRange[2]) {
    cli::cli_abort(
      "cohortDateRange[1] should be equal or smaller than cohortDateRange[2]"
    )
  }
}

checkImputeDuration <- function(imputeDuration) {
  if (is.character(imputeDuration)) {
    imputeDuration <- tolower(imputeDuration)
    checkmate::assertChoice(
      imputeDuration,
      c("none", "median", "mean", "mode")
    )
  } else {
    checkmate::assertCount(
      imputeDuration,
      positive = TRUE
    )
  }
}

checkImputeDailyDose <- function(imputeDailyDose) {
  if (is.character(imputeDailyDose)) {
    checkmate::assertChoice(
      imputeDailyDose,
      c("none", "median", "mean", "mode")
    )
  } else {
    checkmate::assertCount(
      imputeDailyDose,
      positive = TRUE
    )
  }
}

checkDurationRange <- function(durationRange) {
  errorMessage <- "durationRange has to be numeric of length 2 with no NA and
      durationRange[1] <= durationRange[2]"
  if (!is.numeric(durationRange) |
    length(durationRange) != 2 |
    any(is.na(durationRange))) {
    cli::cli_abort(errorMessage)
  }
  if (durationRange[1] > durationRange[2]) {
    cli::cli_abort(errorMessage)
  }
}

checkDailyDoseRange <- function(dailyDoseRange) {
  errorMessage <- "dailyDoseRange has to be numeric of length 2 with no NA and
      dailyDoseRange[1] <= dailyDoseRange[2]"
  if (!is.numeric(dailyDoseRange) |
    length(dailyDoseRange) != 2 |
    any(is.na(dailyDoseRange))) {
    cli::cli_abort(errorMessage)
  }
  if (dailyDoseRange[1] > dailyDoseRange[2]) {
    cli::cli_abort(errorMessage)
  }
}

checkAttrition <- function(attrition) {
  if (!is.null(attrition)) {
    errorMessage <- "attrition should be a table with at least:
  'cohort_definition_id', 'number_records', 'number_subjects', 'reason_id',
  'reason', 'excluded_records', 'excluded_subjects' as columns."
    if (!("tbl" %in% class(attrition))) {
      cli::cli_abort(errorMessage)
    }
    if (!all(c(
      "cohort_definition_id", "number_records", "number_subjects", "reason_id",
      "reason", "excluded_records", "excluded_subjects"
    ) %in% colnames(attrition))) {
      cli::cli_abort(errorMessage)
    }
  }
}

checkReason <- function(reason) {
  checkmate::assertCharacter(reason, len = 1, min.chars = 1)
}

checkTargetCohortName <- function(targetCohortName, cdm) {
  errorMessage <- "targetCohortName must be a character string of length 1"
  check <- !is.character(targetCohortName) | length(targetCohortName) > 1 |
    any(is.na(targetCohortName)) | any(nchar(targetCohortName) == 0)
  if (check) {
    cli::cli_abort(errorMessage)
  }
  if (!(targetCohortName %in% names(cdm))) {
    cli::cli_abort("targetCohortName is not in the cdm reference")
  }
  numberRows <- cdm[[targetCohortName]] %>%
    dplyr::tally() %>%
    dplyr::pull()
  if (numberRows == 0) {
    cli::cli_abort("targetCohort is empty")
  }
}

checkPath <- function(path) {
  if (typeof(path) != "character" || length(path) != 1) {
    cli::cli_abort("path is not a character of length 1")
  }

  if (!file.exists(path)) {
    cli::cli_abort(glue::glue("Invalid path: {path}"))
  }
}

checkAgeGroup <- function(ageGroup) {
  checkmate::assertList(ageGroup, min.len = 1, null.ok = TRUE)
  if (!is.null(ageGroup)) {
    errorMessage <- "ageGroup should be a list of length 2 integerish elements. first value <= second value."
    if (!all(lengths(ageGroup) == 2) | !is.numeric(unlist(ageGroup))) {
      cli::cli_abort(errorMessage)
    }
    if (!all(unlist(lapply(ageGroup, function(x) {
      x[1] <= x[2]
    })))) {
      cli::cli_abort(errorMessage)
    }
  }
}

checkEraJoinMode <- function(eraJoinMode) {
  errorMessage <- "eraJoinMode must be a choice between: 'previous', 'subsequent', 'zero' and 'join'"
  if (!is.character(eraJoinMode) | length(eraJoinMode) > 1) {
    cli::cli_abort(errorMessage)
  } else{
    eraJoinMode <- tolower(eraJoinMode)
  }
  if (!(eraJoinMode %in% c("previous", "subsequent", "zero", "join"))) {
    cli::cli_abort(errorMessage)
  }
}

checkOverlapMode <- function(overlapMode) {
  errorMessage <- "overlapMode must be a choice between: 'previous', 'subsequent', 'minimum', 'maximum' and 'sum'"
  if (!is.character(overlapMode) | length(overlapMode) > 1) {
    cli::cli_abort(errorMessage)
  } else{
    overlapMode <- tolower(overlapMode)
  }
  if (!(overlapMode %in% c("previous", "subsequent", "minimum", "maximum", "sum"))) {
    cli::cli_abort(errorMessage)
  }
}

checkSameIndexMode <- function(sameIndexMode) {
  errorMessage <- "sameIndexMode must be a choice between: 'minimum', 'maximum' and 'sum'"
  if (!is.character(sameIndexMode) | length(sameIndexMode) > 1) {
    cli::cli_abort(errorMessage)
  }else{
    sameIndexMode <- tolower(sameIndexMode)
  }
  if (!(sameIndexMode %in% c("minimum", "maximum", "sum"))) {
    cli::cli_abort(errorMessage)
  }
}

checkIngredientConceptId <- function(ingredientConceptId, cdm) {
  if (is.null(ingredientConceptId)) {
    cli::cli_abort("ingredientConceptId ca not be NULL")
  }
  if (!isInteger(ingredientConceptId)) {
    cli::cli_abort("ingredientConceptId is not an integer of length 1")
  }
  if (cdm[["concept"]] %>%
    dplyr::filter(.data$concept_id == .env$ingredientConceptId) %>%
    dplyr::pull("concept_class_id") != "Ingredient"
  ) {
    cli::cli_abort("ingredientConceptId is not found in vocabulary")
  }
}

checkSample <- function(sample) {
  if (isInteger(sample)) {
    cli::cli_abort("sample is not an integer of length 1")
  }
}

checkIndicationCohortName <- function(indicationCohortName, cdm) {
  if (!is.character(indicationCohortName) & length(indicationCohortName) == 1) {
    cli::cli_abort("indicationCohortName must be a character of length 1.")
  }
  if (!(indicationCohortName %in% names(cdm))) {
    cli::cli_abort("indicationCohortName is not in the cdm reference")
  }
}

checkIndicationGap <- function(indicationGap) {
  errorMessage <- "indicationGap must be an integer (>= 0) vector"
  if (!is.numeric(indicationGap)) {
    cli::cli_abort(errorMessage)
  }
  if (sum(indicationGap < 0) > 0) {
    cli::cli_abort(errorMessage)
  }
  if (!(lapply(indicationGap, isInteger) %>% unlist() %>% all())) {
    cli::cli_abort(errorMessage)
  }
}

checkUnknownIndicationTable <- function(unknownIndicationTable, cdm) {
  if (!is.null(unknownIndicationTable)) {
    errorMessage <- "unknownIndicationTable must point to a table in the cdm"
    if (!is.character(unknownIndicationTable)) {
      cli::cli_abort(errorMessage)
    }
    if (!all(unknownIndicationTable %in% names(cdm))) {
      cli::cli_abort("unknownIndicationTable is not in the cdm reference")
    }
  }
}

checkIndicationVariables <- function(indicationVariables, cohort) {
  errorMessage <- "indicationVariables must point to character columns in cohort"
  if (!is.character(indicationVariables) | length(indicationVariables) == 0) {
    cli::cli_abort(errorMessage)
  }
  if (!all(indicationVariables %in% colnames(cohort))) {
    cli::cli_abort(errorMessage)
  }
  cohort <- cohort %>%
    dplyr::select(dplyr::all_of(indicationVariables)) %>%
    utils::head(1) %>%
    dplyr::collect()
  variableType <- PatientProfiles::variableTypes(cohort)$variable_type %>%
    unique()
  if (!all(variableType %in% c("binary", "categorical"))) {
    cli::cli_abort(
      "indicationVariables should point to binary or categorical variables"
    )
  }
  if (!all(substr(indicationVariables, 1, 15) == "indication_gap_")) {
    cli::cli_abort("indicationVariables should start with indication_gap_")
  }
}

checkX <- function(x) {
  if (!("tbl_sql" %in% class(x))) {
    cli::cli_abort("x must be a table in the database")
  }
}

checkXx <- function(xx) {
  if (!("tbl" %in% class(xx))) {
    cli::cli_abort("xx must be a tibble")
  }
}

checkIndicationDate <- function(indicationDate, x) {
  if (!is.character(indicationDate) & length(indicationDate) == 1) {
    cli::cli_abort("indicationDate must be a character of length 1.")
  }
  if (!(indicationDate %in% colnames(x))) {
    cli::cli_abort("indicationDate must be a column of x")
  }
}

checkCohort <- function(cohort) {
  errorMessage <- "cohort must be a GeneratedCohortSet"
  if (!("GeneratedCohortSet" %in% class(cohort))) {
    cli::cli_abort(errorMessage)
  }
}

checkStrata <- function(strata, cohort) {
  errorMessage <- "strata must be a list of columns in cohort"
  if (!is.list(strata)) {
    cli::cli_abort(errorMessage)
  }
  if (length(strata) > 0) {
    if (!is.character(unlist(strata))) {
      cli::cli_abort(errorMessage)
    }
    if (!all(unlist(strata) %in% colnames(cohort))) {
      cli::cli_abort(errorMessage)
    }
  }
}

checkMinCellCount <- function(minCellCount) {
  checkmate::assertIntegerish(minCellCount, lower = 0, any.missing = F, len = 1)
}

checkOffset <- function(offset) {
  checkmate::assertIntegerish(offset, lower = 0, any.missing = F, len = 1)
}

checkInitialDailyDose <- function(initialDailyDose, cohort) {
  if (!is.logical(initialDailyDose) | length(initialDailyDose) != 1) {
    cli::cli_warn("initialDailyDose must be TRUE or FALSE")
  }
  if (initialDailyDose & "initial_dose" %in% colnames(cohort)) {
    cli::cli_warn("`initial_dose` column will be overwrite")
  }
}

checkNumberExposures <- function(numberExposures, cohort) {
  if (!is.logical(numberExposures) | length(numberExposures) != 1) {
    cli::cli_warn("numberExposures must be TRUE or FALSE")
  }
  if (numberExposures & "number_exposures" %in% colnames(cohort)) {
    cli::cli_warn("`number_exposures` column will be overwrite")
  }
}

checkDuration <- function(duration, cohort) {
  if (!is.logical(duration) | length(duration) != 1) {
    cli::cli_warn("duration must be TRUE or FALSE")
  }
  invisible(duration)
}

checkDose <- function(dose, cohort) {
  if (!is.logical(dose) | length(dose) != 1) {
    cli::cli_warn("dose must be TRUE or FALSE")
  }
  invisible(dose)
}

checkQuantity <- function(quantity, cohort) {
  if (!is.logical(quantity) | length(quantity) != 1) {
    cli::cli_warn("quantity must be TRUE or FALSE")
  }
  invisible(quantity)
}

checkCumulativeDose <- function(cumulativeDose, cohort) {
  if (!is.logical(cumulativeDose) | length(cumulativeDose) != 1) {
    cli::cli_warn("cumulativeDose must be TRUE or FALSE")
  }
  if (cumulativeDose & "cumulative_dose" %in% colnames(cohort)) {
    cli::cli_warn("`cumulative_dose` column will be overwrite")
  }
}

checkNumberEras <- function(numberEras, cohort) {
  if (!is.logical(numberEras) | length(numberEras) != 1) {
    cli::cli_warn("numberEras must be TRUE or FALSE")
  }
  if (numberEras & "number_eras" %in% colnames(cohort)) {
    cli::cli_warn("`number_eras` column will be overwrite")
  }
}

checkSupplementary <- function(supplementary, cohort) {
  if (!is.logical(supplementary) | length(supplementary) != 1) {
    cli::cli_warn("supplementary must be TRUE or FALSE")
  }
  if (supplementary) {
    variables <- c(
      "exposed_days", "unexposed_days", "not_considered_days", "first_era_days",
      "number_exposures", "number_subexposures", "number_continuous_exposures",
      "number_eras", "number_gaps", "number_unexposed_periods",
      "number_subexposures_overlap", "number_eras_overlap",
      "number_continuous_exposure_overlap", "initial_daily_dose",
      "sum_all_exposed_dose", "sum_all_exposed_days", "follow_up_days", "gap_days",
      "number_subexposures_no_overlap", "number_eras_no_overlap",
      "number_continuous_exposures_no_overlap",
      "cumulative_dose", "cumulative_gap_dose", "cumulative_not_considered_dose"
    )
    variables <- variables[variables %in% colnames(cohort)]
    if (length(variables) > 0) {
      cli::cli_warn(paste0("`", paste0(variables, collapse = "`, `"), "` column will be overwrite"))
    }
  }
}

checkWindowVisitOcurrence <- function(windowVisitOccurrence) {
  checkmate::assertNumeric(
    windowVisitOccurrence,
    any.missing = FALSE, null.ok = TRUE, len = 2
  )
}

checkCovariates <- function(covariates, cdm) {
  if (!is.null(covariates)) {
    errorMessage <- "covariates should be a named list of two numeric values
    (windows). First element of the window must be smaller than the second one.
    Names should point to tables of the cdm."
    if (!is.list(covariates)) {
      cli::cli_abort(errorMessage)
    }
    if (!all(lengths(covariates) == 2)) {
      cli::cli_abort(errorMessage)
    }
    if (!(length(names(covariates)) == length(covariates))) {
      cli::cli_abort(errorMessage)
    }
    if (!all(names(covariates) %in% names(cdm))) {
      cli::cli_abort(errorMessage)
    }
  }
}

checkOverlap <- function(overlap, tablesToCharacterize) {
  if (length(overlap) == 1) {
    checkmate::assertLogical(overlap, any.missing = FALSE)
  } else {
    checkmate::assertLogical(
      overlap,
      any.missing = FALSE, len = length(tablesToCharacterize)
    )
  }
}

checkBigMark <- function(bigMark) {
  checkmate::checkCharacter(bigMark, min.chars = 0, len = 1, any.missing = F)
}

checkWindow <- function(window) {
  errorMessage <- "window should be a list of numeric two vectors intervals"
  if (!is.list(window)) {
    cli::cli_abort(errorMessage)
  }
  if (!is.numeric(unlist(window))) {
    cli::cli_abort(errorMessage)
  }
  if (!all(lengths(window) == 2)) {
    cli::cli_abort(errorMessage)
  }
  if (!all(!is.na(unlist(window)))) {
    cli::cli_abort(errorMessage)
  }
  if (!all(unlist(lapply(window, function(x) {
    x[1] <= x[2]
  })))) {
    cli::cli_abort(errorMessage)
  }
}

checkTablesToCharacterize <- function(tablesToCharacterize, cdm) {
  choices <- c(
    "visit_occurrence", "condition_occurrence", "drug_exposure",
    "procedure_occurrence", "device_exposure", "measurement", "observation",
    "drug_era", "condition_era", "specimen"
  )
  errorMessage <- paste0(
    "tablesToCharacterize must point to tables in the cdm. Choice between: ",
    paste0(choices, collapse = ", ")
  )
  if (!is.character(tablesToCharacterize)) {
    cli::cli_abort(errorMessage)
  }
  if (length(tablesToCharacterize) != length(unique(tablesToCharacterize))) {
    cli::cli_abort(errorMessage)
  }
  if (!all(tablesToCharacterize %in% choices)) {
    cli::cli_abort(errorMessage)
  }
  if (!all(tablesToCharacterize %in% names(cdm))) {
    cli::cli_abort(errorMessage)
  }
}

checkDrugUseEstimates <- function(drugUseEstimates) {
  choices <- PatientProfiles::availableFunctions() %>%
    dplyr::filter(.data$variable_type == "numeric") %>%
    dplyr::pull("format_key")
  errorMessage <- paste0(
    "drugUseEstimates must be a subset of: ", paste0(choices, collapse = ", ")
  )
  if (!is.character(drugUseEstimates)) {
    cli::cli_abort(errorMessage)
  }
  if (length(drugUseEstimates) != length(unique(drugUseEstimates))) {
    cli::cli_abort(errorMessage)
  }
  if (!all(drugUseEstimates %in% choices)) {
    cli::cli_abort(errorMessage)
  }
}

checkDrugExposure <- function(drugExposure) {
  errorMessage <- "drugExposure must be a table in the cdm with
  `drug_concept_id`, `quantity`, `drug_exposure_start_date` and
  `drug_exposure_end_date` as columns"
  if (!("tbl_sql" %in% class(drugExposure))) {
    cli::cli_abort(errorMessage)
  }
  if (!all(c(
    "drug_concept_id", "quantity", "drug_exposure_start_date",
    "drug_exposure_end_date"
  ) %in% colnames(drugExposure))) {
    cli::cli_abort(errorMessage)
  }
}

checkDrugList <- function(drugList) {
  errorMessage <- "drugList must be a table in the cdm with `drug_concept_id`
  as column"
  if (!("tbl" %in% class(drugList))) {
    cli::cli_abort(errorMessage)
  }
  if (!all("drug_concept_id" %in% colnames(drugList))) {
    cli::cli_abort(errorMessage)
  }
}

checkStratifyByConcept <- function(stratifyByConcept) {
  checkmate::assertLogical(stratifyByConcept, any.missing = FALSE, len = 1)
}

checkSeed <- function(seed) {
  checkmate::assertIntegerish(seed, lower = 1, len = 1)
}

checkRecordCount <- function(recordCount) {
  checkmate::assertLogical(recordCount, any.missing = FALSE, len = 1)
}

checkKeep <- function(keep) {
  checkmate::assertLogical(keep, any.missing = FALSE, len = 1)
}

checkBinaryColumns <- function(binaryColumns, x) {
  checkmate::assertCharacter(binaryColumns, any.missing = FALSE)
  if (!all(binaryColumns %in% colnames(x))) {
    cli::cli_abort("binaryColumns not found in x")
  }
}

checkNewColumn <- function(newColumn) {
  checkmate::assertCharacter(newColumn, any.missing = FALSE, len = 1)
}

checkLabel <- function(label, binaryColumns) {
  checkmate::assertCharacter(
    label,
    any.missing = FALSE, len = length(binaryColumns)
  )
}

checkCumulativeQuantity <- function(cumulativeQuantity, cohort) {
  if (!is.logical(cumulativeQuantity) | length(cumulativeQuantity) != 1) {
    cli::cli_warn("cumulativeQuantity must be TRUE or FALSE")
  }
}

checkInitialQuantity <- function(initialQuantity, cohort) {
  if (!is.logical(initialQuantity) | length(initialQuantity) != 1) {
    cli::cli_warn("initialQuantity must be TRUE or FALSE")
  }
}

# other functions
checkColumns <- function(x, columns) {
  if (!all(columns %in% colnames(x))) {
    cli::cli_abort(paste0(
      substitute(x), " must contain: `", paste0(columns, collapse = "`, `"),
      "` as column(s)"
    ))
  }
}

isInteger <- function(integer) {
  if (!is.numeric(integer) | length(integer) > 1) {
    return(FALSE)
  } else {
    if (!is.infinite(integer) &&
      abs(integer - round(integer)) > sqrt(.Machine$double.eps)) {
      return(FALSE)
    } else {
      return(TRUE)
    }
  }
}

checkImputeDurationCount <- function(imputeDurationCount) {
  error <- "imputeDurationCount should be TRUE or FALSE"
  if (length(imputeDurationCount) != 1) {
    cli::cli_abort(error)
  }
  if (!(imputeDurationCount %in% c(TRUE, FALSE))) {
    cli::cli_abort(error)
  }
  invisible(imputeDurationCount)
}

checkImputeDailyDoseCount <- function(imputeDailyDoseCount) {
  error <- "imputeDailyDoseCount should be TRUE or FALSE"
  if (length(imputeDailyDoseCount) != 1) {
    cli::cli_abort(error)
  }
  if (!(imputeDailyDoseCount %in% c(TRUE, FALSE))) {
    cli::cli_abort(error)
  }
  invisible(imputeDailyDoseCount)
}

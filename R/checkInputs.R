checkInputs <- function(...) {
  inputs <- list(...)
  lapply(names(inputs), function(x) {checkInput(inputs[[x]], x)})
  checkDependantVariables(inputs)
  invisible(NULL)
}

checkInput <- function(x, nam) {
  listChecks <- c(
    "cdm", "conceptSetList", "name", "summariseMode", "fixedTime",
    "daysPriorHistory", "gapEra", "priorUseWashout", "cohortDateRange",
    "imputeDuration", "durationRange", "attrition", "x", "reason", "tableRef"
  )
  if (!(nam %in% listChecks)) {
    cli::cli_abort(paste("Input parameter could not be checked:", nam))
  }
  eval(parse(text = paste0(
    "output <- check", toupper(substr(nam, 1, 1)), substr(nam, 2, nchar(nam)),
    "(x)"
  )))
  return(output)
}

checkDependantVariables <- function(inputs) {
  nam <- names(inputs)
  if (all(c("name", "cdm") %in% nam)) {
    if (inputs$name %in% names(inputs$cdm)) {
      cli::cli_alert_warning(
        "A cohort with this name already exist in the cdm object. It will be overwritten."
      )
    }
  }
}

checkCdm <- function(cdm) {
  if (!("cdm_reference" %in% class(cdm))) {
    cli::cli_abort(
      "Argument cdm is not a valid cdm reference, please use
      CDMConnector::cdmFromCon() to create a valid cdm reference"
    )
  }
}

checkConceptSetList <- function(x) {
  errorMessage <- "conceptSetList must be a uniquely named list of integerish,
  no NA are allowed"
  if (!is.list(x)) {
    cli::cli_abort(errorMessage)
  }
  if (!all(sapply(x, is.numeric))) {
    cli::cli_abort(errorMessage)
  }
  x <- unlist(x)
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

checkName <- function(name) {
  checkmate::assertCharacter(name, len = 1, any.missing = FALSE)
  if (name %in% CDMConnector::tbl_group("all")) {
    cli::cli_abort(
      'name can not one of the stadard tables of the cdm. To see standard
      tables: CDMConnector::tbl_group("all")'
    )
  }
}

checkSummariseMode <- function(summariseMode) {
  if (!(summariseMode %in% c("AllEras", "FirstEra", "FixedTime"))) {
    cli::cli_abort(
      "`summariseMode` should be one of: AllEras, FirstEra, FixedTime"
    )
  }
}

checkFixedTime <- function(fixedTime) {
  checkmate::assertIntegerish(fixedTime, lower = 1, any.missing = F, len = 1)
}

checkDaysPriorHistory <- function(daysPriorHistory) {
  checkmate::assertIntegerish(
    daysPriorHistory, lower = 0, any.missing = F, len = 1, null.ok = T
  )
}

checkGapEra <- function(gapEra) {
  checkmate::assertIntegerish(gapEra, lower = 0, any.missing = F, len = 1)
}

checkPriorUseWashout <- function(priorUseWashout) {
  checkmate::assertIntegerish(
    priorUseWashout, lower = 0, any.missing = F, len = 1
  )
}

checkCohortDateRange <- function(cohortDateRange) {
  checkmate::assertDate(cohortDateRange, null.ok = T, len = 2)
  if (!is.na(cohortDateRange[1]) &
      !is.na(cohortDateRange[1]) &
      cohortDateRange[1] > cohortDateRange[2]) {
    cli::cli_abort(
      "cohortDateRange[1] should be equal or smaller than cohortDateRange[2]"
    )
  }
}

checkImputeDuration <- function(imputeDuration) {
  if (is.character(imputeDuration)) {
    checkmate::assertChoice(
      imputeDuration,
      c("eliminate", "median", "mean", "quantile25", "quantile75")
    )
  } else {
    checkmate::assertCount(
      imputeDuration, positive = TRUE
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

checkX <- function(x) {
  errorMessage <- "x should be a table with at least 'cohort_definition_id' and
  'subject_id' as columns."
  if (!("tbl" %in% class(x))) {
    cli::cli_abort(errorMessage)
  }
  if (!all(c("cohort_definition_id", "subject_id") %in% colnames(x))) {
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
      'cohort_definition_id', 'number_records', 'number_subjects', 'reason_id',
      'reason', 'excluded_records', 'excluded_subjects'
    ) %in% colnames(attrition))) {
      cli::cli_abort(errorMessage)
    }
  }
}

checkReason <- function(reason) {
  checkmate::assertCharacter(reason, len = 1, min.chars = 1)
}

checkDrugUtilisationCohortSet <- function(cs) {
  expectedColnames <- c(
    "cohort_definition_id", "cohort_name", "summarise_mode", "fixed_time",
    "days_prior_history", "gap_era", "prior_use_washout",
    "cohort_dates_range_start", "cohort_dates_range_end", "impute_duration",
    "duration_range_min", "duration_range_max"
  )
  return(
    length(colnames(cs)) == length(expectedColnames) &
      all(expectedColnames %in% colnames(cs))
  )
}

checkConsistentCohortSet<- function(cs,
                                    conceptSetList,
                                    gapEra,
                                    imputeDuration,
                                    durationRange,
                                    missingGapEra,
                                    missingImputeDuration,
                                    missingDurationRange) {
  notPresent <- names(conceptSetList)[!(conceptSetList %in% cs$cohort_name)]
  if (length(notPresent) > 0) {
    cli::cli_alert_warning(paste0(
      "Different names in conceptSetList (",
      paste0(notPresent, collapse = ", "), ") than in the created cohortSet."
    ))
  }
  if (missingGapEra == TRUE) {
    gapEra <- cs$gap_era
  } else {
    if (gapEra != cs$gapEra) {
      cli::cli_alert_warning(glue::glue(
        "gapEra is different than at the cohort creation stage (input: {gapEra}, cohortSet: {cs$gap_era})."
      ))
    }
  }
  if (missingImputeDuration == TRUE) {
    imputeDuration <- cs$impute_duration
  } else {
    if (imputeDuration != cs$impute_duration) {
      cli::cli_alert_warning(glue::glue(
        "imputeDuration is different than at the cohort creation stage (input: {imputeDuration}, cohortSet: {cs$impute_duration})."
      ))
    }
  }
  if (missingDurationRange == TRUE) {
    durationRange <- c(cs$duration_range_min, cs$duration_range_max)
  } else {
    if (!identical(durationRange, c(cs$duration_range_min, cs$duration_range_max))) {
      cli::cli_alert_warning(glue::glue_collapse(
        "durationRange is different than at the cohort creation stage (input: {durationRange}, cohortSet: {c(cs$duration_range_min, cs$duration_range_max)})"
      ))
    }
  }
  parameters <- list(
    gapEra = gapEra, imputeDuration = imputeDuration,
    durationRange = durationRange
  )
  return(parameters)
}


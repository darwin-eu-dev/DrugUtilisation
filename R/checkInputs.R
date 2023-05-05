checkInputs <- function(...) {
  inputs <- list(...)
  lapply(names(inputs), function(x) {checkInput(inputs[[x]], x)})
  checkDependantVariables(inputs)
  invisible(NULL)
}

checkInput <- function(x, nam) {
  listChecks <- c(
    "cdm", "conceptSetList", "name", "temporary", "summariseMode", "fixedTime",
    "daysPriorHistory", "gapEra", "priorUseWashout", "cohortDatesRange",
    "imputeDuration", "durationRange", "attrition", "x", "reason"
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
      cli::cli_abort("A cohort with this name already exist in the cdm object.")
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

checkTemporary <- function(temporary) {
  checkmate::assertLogical(temporary, any.missing = FALSE, len = 1)
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
    daysPriorHistory, lower = 0, any.missing = F, len = 1
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

checkCohortDatesRange <- function(cohortDatesRange) {
  checkmate::assertDate(cohortDatesRange, null.ok = T, len = 2)
  if (!is.na(cohortDatesRange[1]) &
      !is.na(cohortDatesRange[1]) &
      cohortDatesRange[1] > cohortDatesRange[2]) {
    cli::cli_abort(
      "cohortDatesRange[1] should be equal or smaller than cohortDatesRange[2]"
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

checkReason <- function(reason) {
  checkmate::assertCharacter(reason, len = 1, min.chars = 1)
}

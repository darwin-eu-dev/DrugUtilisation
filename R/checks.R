#' @noRd
checkCdm <- function (cdm, contain) {
  if (!("cdm_reference" %in% class(cdm))) {
    cli::cli_abort(
      "`cdm` must be a `cdm_reference`, please use CDMConnector::cdmFromCon to
      create a cdm reeference."
    )
  }
  if (length(contain) > 0) {
    contain <- contain[contain %in% colnames(cdm)]
    if (length(contain > 0)) {
      verb <- ifelse(length(contain) == 1, "is", "are")
      contain <- paste0(contain, collapse = ", ")
      cli::cli_abort(paste(contain, verb, "not contained in cdm reference."))
    }
  }
}

#' @noRd
checkConceptSetList <- function(conceptSetList) {
  checkmate::assertList(
    conceptSetList, types = "integerish", any.missing = FALSE, min.len = 1
  )
  checkmate::assertTRUE(length(conceptSetList) == length(names(conceptSetList)))
  checkmate::assertTRUE(
    length(unique(names(conceptSetList))) == length(names(conceptSetList))
  )
}

#' @noRd
checkCohortName <- function(name, names) {
  checkmate::assertCharacter(name, len = 1, any.missing = FALSE)
  if (!(name %in% names)) {
    cli::cli_abort("cohortName is not present in the cdm object")
  }
}

#' @noRd
checkStudyPeriod <- function(studyPeriod) {
  checkmate::assertDate(studyPeriod, len = 2)
  if (!is.na(studyPeriod[1]) & !is.na(studyPeriod[2]) &
      studyPeriod[1] > studyPeriod[2]) {
    cli::cli_abort(
      "studyPeriod[1] should be smaller or equal to studyPeriod[2]"
    )
  }
}

#' @noRd
checkSummariseMode <- function(summariseMode, fixedTime) {
  if (!(summariseMode %in% c("AllEras", "FirstEra", "FixedTime"))) {
    cli::cli_abort(
      "`summariseMode` should be one of: AllEras, FirstEra, FixedTime"
    )
  }
  if (summariseMode == "FixedTime") {
    checkmate::assertIntegerish(
      fixedTime, any.missing = FALSE, len = 1, lower = 0
    )
  }
}

#' @noRd
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

#' @noRd
checkRange <- function(x) {
  if (!is.numeric(x) | length(x) == 2 | !all(is.na(x)) | x[1] > x[2]) {
    cli::cli_abort(glue::glue(
      "{deparse(substitute(x))} has to be numeric of length 2 with no NA and
      {deparse(substitute(x))}[1] <= {deparse(substitute(x))}[2]"
    ))
  }
}

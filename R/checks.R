
#' @noRd
checkCohortName <- function(name, names) {

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

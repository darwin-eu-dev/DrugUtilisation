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

# checks used throughout the package
#' @noRd
checkPath <- function(path) {
  if(typeof(path) != "character" || length(path) != 1) {
    cli::cli_abort(paste0(
      "{path} is not a character of length 1"
    ))
  }

  if (!file.exists(path)) {
    stop(glue::glue("Invalid path {path}"))
  } else {
    if (dir.exists(path)) {
      conceptSets <- dplyr::tibble(concept_set_path = list.files(
        path = path,
        full.names = TRUE
      ))
    } else {
      conceptSets <- dplyr::tibble(concept_set_path = .env$path)
    }
    conceptSets <- conceptSets %>%
      dplyr::filter(tools::file_ext(.data$concept_set_path) == "json") %>%
      dplyr::mutate(
        concept_set_name =
          tools::file_path_sans_ext(basename(.data$concept_set_path))
      ) %>%
      dplyr::mutate(cohort_definition_id = dplyr::row_number())
    if (conceptSets %>% nrow() == 0) {
      stop(glue::glue("No 'json' file found in {path}"))
    }
  }
  return(conceptSets)
}

#' @noRd
checkCdm <- function(cdm, tables = NULL) {
  if (!isTRUE(inherits(cdm, "cdm_reference"))) {
    cli::cli_abort("cdm must be a CDMConnector CDM reference object")
  }
  if (!is.null(tables)) {
    tables <- tables[!(tables %in% names(cdm))]
    if (length(tables) > 0) {
      cli::cli_abort(paste0(
        "tables: ",
        paste0(tables, collapse = ", "),
        " are not present in the cdm object"
      ))
    }
  }
  invisible(NULL)
}

#' @noRd
checkPatternTibble <- function(x) {
  if (!isTRUE(inherits(x, "tbl_dbi"))) {
    cli::cli_abort("x is not a valid table")
  }
  checkColnames <- all(c("amount", "amount_unit", "numerator", "numerator_unit", "denominator", "denominator_unit") %in% colnames(pattern_tibble))
  if(!checkColnames) {
    cli::cli_abort(" 'amount', 'amount_unit', 'numerator', 'numerator_unit', 'denominator' and 'denominator_unit' are not all columns of {x}")
  }
  invisible(NULL)
}

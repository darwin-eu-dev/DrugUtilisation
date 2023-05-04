checkInputs <- function(...) {
  lapply(list(...), checInput) %>%
    unlist() %>%
    invisible()
}

checkInput <- function(x) {
  nam <- deparse(substitute(x))
  output <- NULL
  if (nam == "x") {

  } else if (nam == "ageGroups") {

  } else if (nam == "name"){
    if (nam %in% cdmTableNames) {
      cli::cli_abort("'name' can not be the name of a table in the cdm.")
    }
  } else {
    cli::cli_abort("input could not be check in checkInputs")
  }
  return(output)
}

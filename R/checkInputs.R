checkInputs <- function(...) {
  inputs <- list(...)
  outputs <- lapply(inputs, checInput) %>%
    unlist() %>%
    invisible()
  checkDependantVariables(inputs)
  return(outputs)
}

checkInput <- function(x) {
  nam <- deparse(substitute(x))
  output <- NULL
  if (nam == "cdm") {
    checkCdm(x)
  } else if (nam == "conceptSetList") {
    checkConceptSetList(x)
  } else if (nam == "name"){
    checkName(x)
  } else if (nam == "temporary") {
    checkTemporary(x)
  } else {
    cli::cli_abort("input could not be check in checkInputs")
  }
  return(output)
}

checkDependantVariables <- function(inputs) {

}

checkCdm <- function(cdm) {
  if (!("cdm_reference" %in% class(cdm))) {
    cli::cli_abort(
      "Argument cdm is not a valid cdm reference, please use
      CDMConnector::cdmFromCon() to create a valid cdm reference"
    )
  }
}

checkConceptSetList <- function(checkConceptSetList) {
  checkmate::assertList(
    conceptSetList, types = "integerish", any.missing = FALSE, min.len = 1
  )
  checkmate::assertTRUE(length(conceptSetList) == length(names(conceptSetList)))
  checkmate::assertTRUE(
    length(unique(names(conceptSetList))) == length(names(conceptSetList))
  )
}

checkName <- function(name) {
  checkmate::assertCharacter(name, len = 1, any.missing = FALSE)
}

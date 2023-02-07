
readConcepSet <- function(conceptSetPath) {
  checkmate::assertCharacter(conceptSetPath, any.missing = FALSE, len = 1)
  if (!file.exists(conceptSetPath)) {
    stop("Provided path does not exist.")
  }
  if (dir.exists(conceptSetPath)) {
    conceptSetPath <- list.files(conceptSetPath, full.names = TRUE)
  }
  conceptSet <- lapply(conceptSetPath, readIndividualConceptSet) %>%
    dplyr::bind_rows(.id = "concept_set_id")
  attr(conceptSet, "conceptSetNames") <- tools::file_path_sans_ext(
    basename(conceptSetPath)
  ) %>%
    stringr::str_replace_all("_", " ") %>%
    dplyr::as_tibble() %>%
    dplyr::rename("concept_set_name" = "value") %>%
    dplyr::mutate(concept_set_id = dplyr::row_number())
  return(conceptSet)
}

readIndividualConceptSet <- function(conceptSetPath) {
  jsonlite::read_json()
}

getDescendants <- function(conceptId, cdm) {
  checkmate::assertNumeric(conceptId)
  checkmate::assertClass(cdm, "cdm_reference")
  if (isFALSE(all(c("concept", "concept_ancestor") %in% names(cdm)))) {
    stop("concept and concept_ancestor tables must be contained in the cdm object.")
  }
  cdm$concept_ancestor %>%
    dplyr::filter(.data$ancestor_concept_id %in% .env$conceptId) %>%
    dplyr::pull("descendant_concept_id")
}

getConceptList <- function(conceptSet, cdm) {

}

generateConceptCohortSet <- function(conceptSetPath,
                                     cdm,
                                     cohortStem = "cohort",
                                     temporal = FALSE) {
  return(cdm)
}





        "CONCEPT_CLASS_ID": "Procedure",
        "CONCEPT_CODE": "28036006",
        "CONCEPT_ID": 4101713,
        "CONCEPT_NAME": "High density lipoprotein cholesterol measurement",
        "DOMAIN_ID": "Measurement",
        "INVALID_REASON": "V",
        "INVALID_REASON_CAPTION": "Valid",
        "STANDARD_CONCEPT": "S",
        "STANDARD_CONCEPT_CAPTION": "Standard",
        "VOCABULARY_ID": "SNOMED",
        "VALID_START_DATE": "2002-01-31",
        "VALID_END_DATE": "2099-12-31"
      },
      "isExcluded": false,
      "includeDescendants": true,
      "includeMapped": false

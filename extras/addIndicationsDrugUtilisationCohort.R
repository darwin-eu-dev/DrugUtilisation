library(DrugUtilisation)
library(CodelistGenerator)
library(CDMConnector)
library(dplyr)

# Mock data
con <- DBI::dbConnect(duckdb::duckdb(), ":memory:")
connectionDetails <- list(
  con = con,
  writeSchema = "main",
  cdmPrefix = NULL,
  writePrefix = NULL
)
cdm <- mockDrugUtilisation(
  connectionDetails = connectionDetails,
  numberIndividual = 100
)


# using CodelistGenerator
conceptList <- CodelistGenerator::getDrugIngredientCodes(cdm, "acetaminophen")
conceptList

cdm <- generateDrugUtilisationCohortSet(
  cdm = cdm,
  name = "acetaminophen_users",
  conceptSet = conceptList,
  limit = "All",
  gapEra = 30,
  priorUseWashout = 0
)

indications <-
  list(
    sinusitis = c(257012, 4294548, 40481087),
    bronchitis = c(260139, 258780)
  )

cdm <-
  generateConceptCohortSet(cdm, name = "indications_cohort", indications)

cohortCount(cdm[["indications_cohort"]]) %>%
  left_join(
    cohortSet(cdm[["indications_cohort"]]) %>%
      select(cohort_definition_id, cohort_name),
    by = "cohort_definition_id"
  )

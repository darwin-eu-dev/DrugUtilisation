
test_that("Basic functionality", {
  # basic functionality
  cdm <- mockDrugUtilisation(connectionDetails)

  conceptSet <- CodelistGenerator::getDrugIngredientCodes(cdm = cdm, name = "acetaminophen")
  ingredientConceptId <- 1125315

  # indexDate

  # censorDate

  # multiple conceptSets

  # gapEra

  # restrictIncident

  # nameStyle
})

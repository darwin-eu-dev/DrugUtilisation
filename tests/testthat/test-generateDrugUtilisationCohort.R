test_that("test inputs",{
  cdm <- mockDrugUtilisation()
  expect_error(generateDrugUtilisationCohort())
  expect_error(generateDrugUtilisationCohort(cdm = cdm))
  expect_no_error(generateDrugUtilisationCohort(
    cdm = cdm, ingredientConceptId = 1
  ))
  # cdm,
  #                                           ingredientConceptId,
  #                                           conceptSetPath = NULL,
  #                                           studyStartDate = NULL,
  #                                           studyEndDate = NULL,
  #                                           summariseMode = "AllEras",
  #                                           fixedTime,
  #                                           daysPriorHistory = 0,
  #                                           gapEra = 30,
  #                                           imputeDuration = "eliminate",
  #                                           durationRange = c(1, NA))
})

test_that("test inputs", {
  cdm <- mockDrugUtilisation(connectionDetails)
  indications <- list(headache = 378253, influenza = 4266367, rand = 12345678)
  expect_no_error(cdm <- generateConceptCohortSet(
    cdm, "indications_cohort", indications
  ))
})

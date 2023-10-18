test_that("test inputs", {
  cdm <- mockDrugUtilisation(connectionDetails)
  indications <- list(headache = 378253, influenza = 4266367, rand = 12345678)
  expect_no_error(cdm <- generateConceptCohortSet(
    cdm, indications, "indications_cohort"
  ))

  #check cdm reference in attributes
  expect_true("indications_cohort" %in% names(cdm))
  expect_true(
    !is.null(attr(cdm[["indications_cohort"]], "cdm_reference", exact = TRUE))
  )

})

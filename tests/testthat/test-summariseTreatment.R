test_that("test summariseTreatment", {
  cdm <- mockDrugUtilisation()
  expect_no_error(
    x <- cdm$cohort1 %>%
      summariseTreatmentFromCohort(
        treatmentCohortName = "cohort2",
        window = list(c(0, 30), c(31, 365)),
        minCellCount = 0
      )
  )
  expect_true(inherits(x, "summarised_result"))
})

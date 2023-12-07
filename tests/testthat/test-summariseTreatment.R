test_that("test summariseTreatment", {
  cdm <- mockDrugUtilisation(connectionDetails)
  expect_no_error(
    x <- cdm$cohort1 %>%
      summariseTreatment(
        treatmentCohortName = "cohort2",
        window = list(c(0, 30), c(31, 365)),
        minCellCount = 0
      )
  )
})

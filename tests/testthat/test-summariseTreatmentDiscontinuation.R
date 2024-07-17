test_that("multiplication works", {
  cdm <- mockDrugUtilisation(
    connectionDetails = connectionDetails
  )

 disc_surv <- CohortSurvival::estimateSingleEventSurvival(cdm = cdm,
                                              targetCohortTable = "cohort1",
                                              outcomeCohortTable = "cohort1",
                                              outcomeDateVariable = "cohort_end_date")


 disc_wrapper <- summariseTreatmentDiscontinuation(cdm = cdm,
                                    targetCohortTable = "cohort1")

 expect_equal(disc_surv, disc_wrapper)

 })

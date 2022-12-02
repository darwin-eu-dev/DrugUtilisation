test_that("overlap function", {
  cohort <- tibble::tibble(
    cohort_definition_id = c("1", "2","1", "2", "2"),
    subject_id = c("1", "1", "2", "2", "3"),
    cohort_start_date = c(
      as.Date("2010-01-01"), as.Date("2009-12-01"),
      as.Date("2010-01-01"), as.Date("2009-01-01"),
      as.Date("2010-01-01")),
    cohort_end_date = c(
      as.Date("2015-01-01"), as.Date("2013-01-01"),
      as.Date("2015-01-01"), as.Date("2009-01-02"),
      as.Date("2015-01-01"))
  )

  cdm <- mockDrugUtilisation(cohort1 = cohort)

  result <- getOverlappingCohortSubjects(cdm = cdm, targetCohortId = "2",
                                         targetCohortTable = "cohort1",
                                         interestCohortId = "1",
                                         interestCohortTable = "cohort1",
                                         lookbackWindow = c(0,180))
  result <- result%>% dplyr::collect()
  expect_true(all(result$overlap_cohort == c(1,0)))
})

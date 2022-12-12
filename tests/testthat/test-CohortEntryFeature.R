test_that("overlap function", {
  cohort1 <- tibble::tibble(
    cohort_definition_id = c(1, 1, 1, 1),
    subject_id = c(1, 1, 2, 3),
    cohort_start_date = c(
      as.Date("2010-01-01"), as.Date("2013-01-01"),
      as.Date("2010-01-02"), as.Date("2010-01-01")),
    cohort_end_date = c(
      as.Date("2012-01-01"), as.Date("2015-01-01"),
      as.Date("2015-01-01"), as.Date("2015-01-01"))
  )

  cohort2 <- tibble::tibble(
    cohort_definition_id = c(2, 2, 2, 3),
    subject_id = c(1, 2, 2, 2),
    cohort_start_date = c(
      as.Date("2009-12-01"), as.Date("2009-01-01"),
      as.Date("2009-11-01"), as.Date("2009-11-01")),
    cohort_end_date = c(
      as.Date("2011-01-01"), as.Date("2009-01-02"),
      as.Date("2015-01-01"), as.Date("2015-01-01"))
  )

  cdm <- mockDrugUtilisation(cohort1 = cohort1, cohort2 = cohort2)

  result <- getOverlappingCohortSubjects(
    cdm = cdm,
    targetCohortTable = "cohort1",
    targetCohortId = c("1"),
    overlapCohortTable = "cohort2",
    overlapCohortId = c("2", "3"),
    lookbackWindow = c(-180, 0)
  )
  result <- result%>% dplyr::collect()
  expect_true(all(result[result$subject_id == 1,]$overlap_cohort2_2 == c(1,0)))
  expect_true(all(result[result$subject_id == 2,]$overlap_cohort2_2 == 1))
  expect_true(all(result[result$subject_id == 3,]$overlap_cohort2_2 == 0))

  expect_true(all(result[result$subject_id == 1,]$overlap_cohort2_3 == c(0,0)))
  expect_true(all(result[result$subject_id == 2,]$overlap_cohort2_3 == 1))
  expect_true(all(result[result$subject_id == 3,]$overlap_cohort2_3 == 0))
})

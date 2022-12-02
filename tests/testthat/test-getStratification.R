test_that("test initial errors",{
  cdm <- mockDrugUtilisation(
    cohort1 = dplyr::tibble(
      cohort_definition_id = c(1, 1, 1, 1, 2),
      subject_id = c(1, 2, 3, 1, 1),
      cohort_start_date = as.Date(c(
        "2020-01-01", "2022-05-07", "2012-12-15", "2010-05-20", "2020-02-03"
      )),
      cohort_end_date = as.Date(c(
        "2021-01-01", "2022-05-08", "2012-12-18", "2010-07-20", "2021-02-03"
      ))
    )
  )
  expect_error(getStratification())
  expect_error(getStratification(cdm = cdm))
  expect_error(getStratification(cdm = cdm, targetCohortName = 1))
  expect_error(getStratification(cdm = cdm, targetCohortName = NA))
  expect_error(getStratification(cdm = cdm, targetCohortName = "no"))
  expect_error(getStratification(cdm = cdm, targetCohortName = "cohort1"))
  expect_error(getStratification(
    cdm = cdm, targetCohortName = "cohort1", targetCohortId = NA
  ))
  expect_error(getStratification(
    cdm = cdm, targetCohortName = "cohort1", targetCohortId = "1"
  ))
  expect_error(getStratification(
    cdm = cdm, targetCohortName = "cohort1", sex = "Both", targetCohortId = 3
  ))
  x <- getStratification(
    cdm = cdm, targetCohortName = "cohort1", sex = "Both", targetCohortId = 1
  )
  expect_error(getStratification(
    cdm = cdm, targetCohortName = "cohort1", sex = c("Both", "Both"),
    targetCohortId = 1
  ))
  expect_error(getStratification(
    cdm = cdm, targetCohortName = "cohort1", sex = c("Both", NA),
    targetCohortId = 1
  ))
  expect_error(getStratification(
    cdm = cdm, targetCohortName = "cohort1", sex = c("Both", "x"),
    targetCohortId = 1
  ))
  x <- getStratification(
    cdm = cdm, targetCohortName = "cohort1", sex = c("Both", "Male"),
    targetCohortId = 1
  )
  expect_error(getStratification(
    cdm = cdm, targetCohortName = "cohort1", ageGroup = NA,
    targetCohortId = 1
  ))
  expect_error(getStratification(
    cdm = cdm, targetCohortName = "cohort1", ageGroup = list(),
    targetCohortId = 1
  ))
  expect_error(getStratification(
    cdm = cdm, targetCohortName = "cohort1", ageGroup = list("A"),
    targetCohortId = 1
  ))
  expect_error(getStratification(
    cdm = cdm, targetCohortName = "cohort1", ageGroup = list(1),
    targetCohortId = 1
  ))
  expect_error(getStratification(
    cdm = cdm, targetCohortName = "cohort1", ageGroup = list(c(-1,1)),
    targetCohortId = 1
  ))
  expect_error(getStratification(
    cdm = cdm, targetCohortName = "cohort1", ageGroup = list(c(4,1)),
    targetCohortId = 1
  ))
  expect_error(getStratification(
    cdm = cdm, targetCohortName = "cohort1", ageGroup = list(c(0,19), c(0)),
    targetCohortId = 1
  ))
  x <- getStratification(
    cdm = cdm, targetCohortName = "cohort1", ageGroup = list(c(0,19), c(0,0)),
    targetCohortId = 1
  )
  x <- getStratification(
    cdm = cdm, targetCohortName = "cohort1", ageGroup = list(c(0,19), c(0,NA)),
    targetCohortId = 1
  )
  x <- getStratification(
    cdm = cdm, targetCohortName = "cohort1", ageGroup = list(c(0,19), c(NA,10)),
    targetCohortId = 1
  )
  x <- getStratification(
    cdm = cdm, targetCohortName = "cohort1", ageGroup = list(c(0,19), c(NA,NA)),
    targetCohortId = 1
  )
  expect_error(getStratification(
    cdm = cdm, targetCohortName = "cohort1", indexYearGroup = list(),
    targetCohortId = 1
  ))
  expect_error(getStratification(
    cdm = cdm, targetCohortName = "cohort1", indexYearGroup = list("hola"),
    targetCohortId = 1
  ))
  expect_error(getStratification(
    cdm = cdm,
    targetCohortName = "cohort1",
    indexYearGroup = list(100, c(10:20), NA),
    targetCohortId = 1
  ))
  expect_error(getStratification(
    cdm = cdm,
    targetCohortName = "cohort1",
    indexYearGroup = list(100, c(10:20), as.numeric(NA)),
    targetCohortId = 1
  ))
  x <- getStratification(
    cdm = cdm,
    targetCohortName = "cohort1",
    indexYearGroup = list(100, c(10:20), c(20, 45)),
    targetCohortId = 1
  )
})

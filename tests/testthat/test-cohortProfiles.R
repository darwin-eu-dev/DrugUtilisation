test_that("test getAge", {
  cdm <- mockDrugUtilisation(
    person = dplyr::tibble(
      person_id = as.integer(c(1, 2, 3, 4, 5)),
      year_of_birth = as.integer(c(1990, 1991, 1992, 1993, 1994)),
      month_of_birth = as.integer(c(12, 2, 8, NA, NA)),
      day_of_birth = as.integer(c(NA, 10, 15, 25, NA)),
      gender_concept_id = c(8532, NA, 1234, 8507, 8532)
    ),
    cohort2 = dplyr::tibble(
      person_id = c(1, 2, 3, 4, 5),
      observation_period_start_date = as.Date(c(
        "2000-01-01", "2001-09-12", "2000-05-25", "2002-12-31", "2001-11-11"
      )),
      observation_period_end_date = as.Date(c(
        "2030-01-01", "2030-09-12", "2030-05-25", "2030-12-31", "2030-11-11"
      ))
    ),
    cohort1 = dplyr::tibble(
      cohort_definition_id = c(1, 1, 2, 2, 2, 2, 2),
      subject_id = c(1, 2, 3, 4, 5, 1, 2),
      cohort_start_date = as.Date(c(
        "2000-01-01", "2021-09-12", "2012-08-12", "2003-01-01", "2014-10-18",
        "2014-10-18", "2014-10-18"
      )),
      cohort_end_date = as.Date(c(
        "2000-01-01", "2021-09-12", "2012-08-12", "2003-01-01", "2014-10-18",
        "2014-10-18", "2014-10-18"
      ))
    )
  )
  cdm[["observation_period"]] <- cdm$cohort2

  resImposeMonthDay <- getAge(cdm, "cohort1")
  resImposeDay <- getAge(cdm, "cohort1", imposeMonth = FALSE)
  resNoImpose <- getAge(cdm, "cohort1", imposeMonth = FALSE, imposeDay = FALSE)
  expect_true(all(resImposeMonthDay %>% dplyr::pull("age") ==
    c(10, 30, 20, 10, 20, 24, 23)))
  expect_true(all(resImposeDay %>% dplyr::pull("age") ==
    c(9, 30, 20, 10, 20, 23, 23)))
  expect_true(all(resNoImpose %>% dplyr::pull("age") ==
    c(9, 30, 19, 9, 20, 23, 23)))
  expect_true(
    getAge(cdm, "cohort1", cohortIds = 1) %>%
      dplyr::tally() %>%
      dplyr::pull() == 2
  )
  expect_true(
    getAge(cdm, "cohort1", cohortIds = 2) %>%
      dplyr::tally() %>%
      dplyr::pull() == 5
  )
})

test_that("test getPriorHistory", {
  cdm <- mockDrugUtilisation(
    person = dplyr::tibble(
      person_id = as.integer(c(1, 2, 3, 4, 5)),
      year_of_birth = as.integer(c(1990, 1991, 1992, 1993, 1994)),
      month_of_birth = as.integer(c(12, 2, 8, NA, NA)),
      day_of_birth = as.integer(c(NA, 10, 15, 25, NA)),
      gender_concept_id = c(8532, NA, 1234, 8507, 8532)
    ),
    cohort2 = dplyr::tibble(
      person_id = c(1, 2, 3, 4, 5),
      observation_period_start_date = as.Date(c(
        "2000-01-01", "2001-09-12", "2000-05-25", "2002-12-31", "2001-11-11"
      )),
      observation_period_end_date = as.Date(c(
        "2030-01-01", "2030-09-12", "2030-05-25", "2030-12-31", "2030-11-11"
      ))
    ),
    cohort1 = dplyr::tibble(
      cohort_definition_id = c(1, 1, 2, 2, 2, 2, 2),
      subject_id = c(1, 2, 3, 4, 5, 1, 2),
      cohort_start_date = as.Date(c(
        "2000-01-01", "2021-09-12", "2012-08-12", "2003-01-01", "2014-10-18",
        "2014-10-18", "2014-10-18"
      )),
      cohort_end_date = as.Date(c(
        "2000-01-01", "2021-09-12", "2012-08-12", "2003-01-01", "2014-10-18",
        "2014-10-18", "2014-10-18"
      ))
    )
  )
  cdm[["observation_period"]] <- cdm$cohort2

  resAddPriorHistory <- getPriorHistory(cdm, "cohort1")
  expect_true(
    resAddPriorHistory %>%
      dplyr::filter(cohort_definition_id == 1) %>%
      dplyr::tally() %>%
      dplyr::pull("n") == 2
  )
  expect_true(
    resAddPriorHistory %>%
      dplyr::filter(cohort_definition_id == 2) %>%
      dplyr::tally() %>%
      dplyr::pull("n") == 5
  )
  expect_true(
    all(resAddPriorHistory %>% dplyr::pull("prior_history") ==
      c(0, 7305, 4462, 1, 4724, 5404, 4784))
  )
  expect_true(
    getPriorHistory(cdm, "cohort1", cohortIds = 1) %>%
      dplyr::tally() %>%
      dplyr::pull() == 2
  )
  expect_true(
    getPriorHistory(cdm, "cohort1", cohortIds = 2) %>%
      dplyr::tally() %>%
      dplyr::pull() == 5
  )
})

test_that("test getSex", {
  cdm <- mockDrugUtilisation(
    person = dplyr::tibble(
      person_id = as.integer(c(1, 2, 3, 4, 5)),
      year_of_birth = as.integer(c(1990, 1991, 1992, 1993, 1994)),
      month_of_birth = as.integer(c(12, 2, 8, NA, NA)),
      day_of_birth = as.integer(c(NA, 10, 15, 25, NA)),
      gender_concept_id = c(8532, NA, 1234, 8507, 8532)
    ),
    cohort2 = dplyr::tibble(
      person_id = c(1, 2, 3, 4, 5),
      observation_period_start_date = as.Date(c(
        "2000-01-01", "2001-09-12", "2000-05-25", "2002-12-31", "2001-11-11"
      )),
      observation_period_end_date = as.Date(c(
        "2030-01-01", "2030-09-12", "2030-05-25", "2030-12-31", "2030-11-11"
      ))
    ),
    cohort1 = dplyr::tibble(
      cohort_definition_id = c(1, 1, 2, 2, 2, 2, 2),
      subject_id = c(1, 2, 3, 4, 5, 1, 2),
      cohort_start_date = as.Date(c(
        "2000-01-01", "2021-09-12", "2012-08-12", "2003-01-01", "2014-10-18",
        "2014-10-18", "2014-10-18"
      )),
      cohort_end_date = as.Date(c(
        "2000-01-01", "2021-09-12", "2012-08-12", "2003-01-01", "2014-10-18",
        "2014-10-18", "2014-10-18"
      ))
    )
  )
  cdm[["observation_period"]] <- cdm$cohort2

  expect_true(
    all(getSex(cdm, "cohort1") %>%
      dplyr::pull("sex") %in%
      c("Female", "Male", NA))
  )
  expect_true(
    getSex(cdm, "cohort1") %>%
      dplyr::pull("sex") %>%
      is.na() %>%
      sum() == 3
  )
  expect_true(
    all(getSex(cdm, "cohort1") %>%
      dplyr::filter(!is.na(.data$sex)) %>%
      dplyr::pull("sex") == c("Female", "Male", "Female", "Female"))
  )
  expect_true(
    getSex(cdm, "cohort1", cohortIds = 1) %>%
      dplyr::tally() %>%
      dplyr::pull() == 2
  )
  expect_true(
    getSex(cdm, "cohort1", cohortIds = 2) %>%
      dplyr::tally() %>%
      dplyr::pull() == 5
  )
})

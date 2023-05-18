test_that("test inputs", {
  cdm <- mockDrugUtilisation(connectionDetails)
  expect_error(generateDrugUtilisationCohortSet())
  expect_error(generateDrugUtilisationCohortSet(cdm = cdm))
  expect_error(generateDrugUtilisationCohortSet(cdm, "dus", 1))
  expect_error(generateDrugUtilisationCohortSet(cdm, "dus", list(1)))
  expect_error(generateDrugUtilisationCohortSet(cdm, "dus", list(acetaminophen = 1)))
  x <- generateDrugUtilisationCohortSet(cdm, "dus", list(acetaminophen = 1125360))
  expect_true(all(colnames(x$dus) == c(
    "cohort_definition_id", "subject_id", "cohort_start_date", "cohort_end_date"
  )))
  expect_error(generateDrugUtilisationCohortSet(
    cdm, "dus", list(acetaminophen = 1125360), cohortDateRange = 1
  ))
  expect_error(generateDrugUtilisationCohortSet(
    cdm, "dus", list(acetaminophen = 1125360), cohortDateRange = "2020-01-05"
  ))
  expect_error(generateDrugUtilisationCohortSet(
    cdm, "dus", list(acetaminophen = 1125360), summariseMode = "2020-01-05"
  ))
  xx <- generateDrugUtilisationCohortSet(
    cdm, "dus", list(acetaminophen = 1125360), summariseMode = "FixedTime"
  )
  expect_error(generateDrugUtilisationCohortSet(
    cdm, "dus", list(acetaminophen = 1125360), summariseMode = "FixedTime",
    fixedTime = "1"
  ))
  expect_error(generateDrugUtilisationCohortSet(
    cdm, "dus", list(acetaminophen = 1125360), daysPriorHistory = "7"
  ))
  expect_error(generateDrugUtilisationCohortSet(
    cdm, "dus", list(acetaminophen = 1125360), gapEra = "7"
  ))
  expect_error(generateDrugUtilisationCohortSet(
    cdm, "dus", list(acetaminophen = 1125360), imputeDuration = "7"
  ))
  expect_error(generateDrugUtilisationCohortSet(
    cdm, "dus", list(acetaminophen = 1125360), imputeDuration = -7
  ))
  expect_error(generateDrugUtilisationCohortSet(
    cdm, "dus", list(acetaminophen = 1125360), durationRange = -7
  ))
})

test_that("class", {
  cdm <- mockDrugUtilisation(connectionDetails)
  cdm <- generateDrugUtilisationCohortSet(cdm, "dus", list(acetaminophen = 1125360))
  expect_true("GeneratedCohortSet" %in% class(cdm$dus))
})

test_that("basic functionality drug_conceptId", {
  cdm <- mockDrugUtilisation(connectionDetails, seed = 2)
  acetaminophen <- list(acetaminophen = c(1125360, 2905077, 43135274))
  # check gap
  cdm1 <- generateDrugUtilisationCohortSet(
    cdm, "dus", acetaminophen, gapEra = 33
  )
  expect_true(cdm1$dus %>% dplyr::tally() %>% dplyr::pull() == 5)
  cdm1 <- generateDrugUtilisationCohortSet(
    cdm, "dus", acetaminophen, gapEra = 34
  )
  expect_true(cdm1$dus %>% dplyr::tally() %>% dplyr::pull() == 5)
  cdm1 <- generateDrugUtilisationCohortSet(
    cdm, "dus", acetaminophen, gapEra = 35
  )
  expect_true(cdm1$dus %>% dplyr::tally() %>% dplyr::pull() == 4)
  cdm1 <- generateDrugUtilisationCohortSet(
    cdm, "dus", acetaminophen, gapEra = 1500
  )
  expect_true(cdm1$dus %>% dplyr::tally() %>% dplyr::pull() == 4)
  # check first era
  cdm1 <- generateDrugUtilisationCohortSet(
    cdm, "dus", acetaminophen, gapEra = 33, summariseMode = "FirstEra"
  )
  expect_true(
    cdm1$dus %>%
      dplyr::filter(subject_id == 3) %>%
      dplyr::pull("cohort_start_date") == as.Date("2021-08-27")
  )
  expect_true(
    cdm1$dus %>%
      dplyr::filter(subject_id == 3) %>%
      dplyr::pull("cohort_end_date") == as.Date("2021-09-14")
  )
  cdm1 <- generateDrugUtilisationCohortSet(
    cdm, "dus", acetaminophen, gapEra = 34, summariseMode = "FirstEra"
  )
  expect_true(
    cdm1$dus %>%
      dplyr::filter(subject_id == 3) %>%
      dplyr::pull("cohort_start_date") == as.Date("2021-08-27")
  )
  expect_true(
    cdm1$dus %>%
      dplyr::filter(subject_id == 3) %>%
      dplyr::pull("cohort_end_date") == as.Date("2021-09-14")
  )
  cdm1 <- generateDrugUtilisationCohortSet(
    cdm, "dus", acetaminophen, gapEra = 35, summariseMode = "FirstEra"
  )
  expect_true(
    cdm1$dus %>%
      dplyr::filter(subject_id == 3) %>%
      dplyr::pull("cohort_start_date") == as.Date("2021-08-27")
  )
  expect_true(
    cdm1$dus %>%
      dplyr::filter(subject_id == 3) %>%
      dplyr::pull("cohort_end_date") == as.Date("2021-12-05")
  )
  cdm1 <- generateDrugUtilisationCohortSet(
    cdm, "dus", acetaminophen, gapEra = 1500, summariseMode = "FirstEra"
  )
  expect_true(
    cdm1$dus %>%
      dplyr::filter(subject_id == 3) %>%
      dplyr::pull("cohort_start_date") == as.Date("2021-08-27")
  )
  expect_true(
    cdm1$dus %>%
      dplyr::filter(subject_id == 3) %>%
      dplyr::pull("cohort_end_date") == as.Date("2021-12-05")
  )
  # check fixedTime
  cdm1 <- generateDrugUtilisationCohortSet(
    cdm, "dus", acetaminophen, summariseMode = "FixedTime", fixedTime = 30,
    gapEra = 35
  )
  expect_true(cdm1$dus %>% dplyr::tally() %>% dplyr::pull() == 4)
  x <- all(
    cdm1$dus %>%
      dplyr::mutate(
        dif = !!CDMConnector::datediff("cohort_start_date", "cohort_end_date") + 1
      ) %>%
      dplyr::pull("dif") == 30
  )
  expect_true(x)
  cdm1 <- generateDrugUtilisationCohortSet(
    cdm, "dus", acetaminophen, summariseMode = "FixedTime", fixedTime = 30,
    gapEra = 400
  )
  expect_true(cdm1$dus %>% dplyr::tally() %>% dplyr::pull() == 4)
  x <- all(
    cdm1$dus %>%
      dplyr::mutate(
        dif = !!CDMConnector::datediff("cohort_start_date", "cohort_end_date") + 1
      ) %>%
      dplyr::pull("dif") == 30
  )
  expect_true(x)
  cdm1 <- generateDrugUtilisationCohortSet(
    cdm, "dus", acetaminophen, summariseMode = "FixedTime", fixedTime = 30,
    gapEra = 3
  )
  expect_true(cdm1$dus %>% dplyr::tally() %>% dplyr::pull() == 4)
  x <- all(
    cdm1$dus %>%
      dplyr::mutate(
        dif = !!CDMConnector::datediff("cohort_start_date", "cohort_end_date") + 1
      ) %>%
      dplyr::pull("dif") == 30
  )
  expect_true(x)
  cdm1 <- generateDrugUtilisationCohortSet(
    cdm, "dus", acetaminophen, summariseMode = "FixedTime", fixedTime = 365,
    gapEra = 35
  )
  expect_true(cdm1$dus %>% dplyr::tally() %>% dplyr::pull() == 4)
  x <- all(
    cdm1$dus %>%
      dplyr::mutate(
        dif = !!CDMConnector::datediff("cohort_start_date", "cohort_end_date") + 1
      ) %>%
      dplyr::pull("dif") == 365
  )
  expect_true(x)

})

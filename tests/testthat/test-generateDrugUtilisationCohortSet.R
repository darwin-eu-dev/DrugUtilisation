test_that("test inputs", {
  cdm <- mockDrugUtilisation(connectionDetails)
  expect_error(generateDrugUtilisationCohortSet())
  expect_error(generateDrugUtilisationCohortSet(cdm = cdm))
  expect_error(generateDrugUtilisationCohortSet(cdm, "dus", 1))
  expect_error(generateDrugUtilisationCohortSet(cdm, "dus", list(1)))
  expect_no_error(generateDrugUtilisationCohortSet(cdm, "dus", list(acetaminophen = 1)))
  cdmNew <- generateDrugUtilisationCohortSet(cdm, "dus", list(acetaminophen = 1125360))
  expect_true("GeneratedCohortSet" %in% class(cdmNew$dus))
  expect_true(all(colnames(cdmNew$dus) == c(
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
  expect_error(generateDrugUtilisationCohortSet(
    cdm, "dus", list(acetaminophen = 1125360), summariseMode = "FixedTime"
  ))
  cdmNew <- generateDrugUtilisationCohortSet(
    cdm, "dus", list(acetaminophen = 1125360), summariseMode = "FixedTime",
    fixedTime = 365
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

test_that("basic functionality drug_conceptId", {
  skip_on_cran()
  cdm <- mockDrugUtilisation(
    connectionDetails,
    drug_exposure = dplyr::tibble(
      drug_exposure_id = 1:4,
      person_id = c(1, 1, 1, 1),
      drug_concept_id = sample(c(1125360, 2905077, 43135274), 4, replace = T),
      drug_exposure_start_date = as.Date(
        c("2020-04-01", "2020-06-01", "2021-02-12", "2021-03-01"), "%Y-%m-%d"
      ),
      drug_exposure_end_date = as.Date(
        c("2020-04-30", "2020-09-11", "2021-02-15", "2021-03-24"), "%Y-%m-%d"
      ),
      drug_type_concept_id = 38000177,
      quantity = 1
    )
  )
  acetaminophen <- list(acetaminophen = c(1125360, 2905077, 43135274))
  # check gap
  cdm1 <- generateDrugUtilisationCohortSet(
    cdm, "dus", acetaminophen, gapEra = 0
  )
  expect_true(cdm1$dus %>% dplyr::tally() %>% dplyr::pull() == 4)
  cdm1 <- generateDrugUtilisationCohortSet(
    cdm, "dus", acetaminophen, gapEra = 13
  )
  expect_true(cdm1$dus %>% dplyr::tally() %>% dplyr::pull() == 4)
  cdm1 <- generateDrugUtilisationCohortSet(
    cdm, "dus", acetaminophen, gapEra = 14
  )
  expect_true(cdm1$dus %>% dplyr::tally() %>% dplyr::pull() == 3)
  cdm1 <- generateDrugUtilisationCohortSet(
    cdm, "dus", acetaminophen, gapEra = 31
  )
  expect_true(cdm1$dus %>% dplyr::tally() %>% dplyr::pull() == 3)
  cdm1 <- generateDrugUtilisationCohortSet(
    cdm, "dus", acetaminophen, gapEra = 32
  )
  expect_true(cdm1$dus %>% dplyr::tally() %>% dplyr::pull() == 2)
  cdm1 <- generateDrugUtilisationCohortSet(
    cdm, "dus", acetaminophen, gapEra = 153
  )
  expect_true(cdm1$dus %>% dplyr::tally() %>% dplyr::pull() == 2)
  cdm1 <- generateDrugUtilisationCohortSet(
    cdm, "dus", acetaminophen, gapEra = 154
  )
  expect_true(cdm1$dus %>% dplyr::tally() %>% dplyr::pull() == 1)
  cdm1 <- generateDrugUtilisationCohortSet(
    cdm, "dus", acetaminophen, gapEra = 1500
  )
  expect_true(cdm1$dus %>% dplyr::tally() %>% dplyr::pull() == 1)
  # check first era
  cdm1 <- generateDrugUtilisationCohortSet(
    cdm, "dus", acetaminophen, gapEra = 1, summariseMode = "FirstEra"
  )
  expect_true(cdm1$dus %>% dplyr::tally() %>% dplyr::pull() == 1)
  expect_true(
    cdm1$dus %>% dplyr::pull("cohort_start_date") == as.Date("2020-04-01")
  )
  expect_true(
    cdm1$dus %>% dplyr::pull("cohort_end_date") == as.Date("2020-04-30")
  )
  cdm1 <- generateDrugUtilisationCohortSet(
    cdm, "dus", acetaminophen, gapEra = 40, summariseMode = "FirstEra"
  )
  expect_true(cdm1$dus %>% dplyr::tally() %>% dplyr::pull() == 1)
  expect_true(
    cdm1$dus %>% dplyr::pull("cohort_start_date") == as.Date("2020-04-01")
  )
  expect_true(
    cdm1$dus %>% dplyr::pull("cohort_end_date") == as.Date("2020-09-11")
  )
  cdm1 <- generateDrugUtilisationCohortSet(
    cdm, "dus", acetaminophen, gapEra = 1500, summariseMode = "FirstEra"
  )
  expect_true(
    cdm1$dus %>% dplyr::pull("cohort_start_date") == as.Date("2020-04-01")
  )
  expect_true(
    cdm1$dus %>% dplyr::pull("cohort_end_date") == as.Date("2021-03-24")
  )
  # check fixedTime
  cdm1 <- generateDrugUtilisationCohortSet(
    cdm, "dus", acetaminophen, summariseMode = "FixedTime", fixedTime = 30,
    gapEra = 0
  )
  expect_true(cdm1$dus %>% dplyr::tally() %>% dplyr::pull() == 1)
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
  expect_true(cdm1$dus %>% dplyr::tally() %>% dplyr::pull() == 1)
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
    gapEra = 0
  )
  expect_true(cdm1$dus %>% dplyr::tally() %>% dplyr::pull() == 1)
  x <- all(
    cdm1$dus %>%
      dplyr::mutate(
        dif = !!CDMConnector::datediff("cohort_start_date", "cohort_end_date") + 1
      ) %>%
      dplyr::pull("dif") == 365
  )
  expect_true(x)
  cdm1 <- generateDrugUtilisationCohortSet(
    cdm, "dus", acetaminophen, summariseMode = "FixedTime", fixedTime = 365,
    gapEra = 1500
  )
  expect_true(cdm1$dus %>% dplyr::tally() %>% dplyr::pull() == 1)
  x <- all(
    cdm1$dus %>%
      dplyr::mutate(
        dif = !!CDMConnector::datediff("cohort_start_date", "cohort_end_date") + 1
      ) %>%
      dplyr::pull("dif") == 365
  )
  expect_true(x)
})

test_that("dates range", {
  skip_on_cran()
  cdm <- mockDrugUtilisation(connectionDetails)
  start <- as.Date("2010-01-01")
  end <- as.Date("2018-06-01")
  acetaminophen <- list("acetaminophen" = c(1125315, 43135274, 2905077, 1125360))
  expect_no_error(
    cdm <- generateDrugUtilisationCohortSet(
      cdm, "dus", acetaminophen, gapEra = 0, cohortDateRange = c(start, end)
    )
  )
  expect_true(
    cdm$dus %>%
      dplyr::filter(.data$cohort_start_date < start) %>%
      dplyr::tally() %>%
      dplyr::pull() == 0
  )
  expect_true(
    cdm$dus %>%
      dplyr::filter(.data$cohort_end_date > end) %>%
      dplyr::tally() %>%
      dplyr::pull() == 0
  )
  expect_true(
    cdm$drug_exposure %>%
      dplyr::filter(.data$drug_exposure_start_date <= .env$start) %>%
      dplyr::filter(.data$drug_exposure_end_date >= .env$start) %>%
      dplyr::summarise(n = dplyr::n_distinct(.data$person_id)) %>%
      dplyr::pull("n") ==
      cdm$dus %>%
      dplyr::filter(.data$cohort_start_date == .env$start) %>%
      dplyr::summarise(n = dplyr::n_distinct(.data$subject_id)) %>%
      dplyr::pull("n")
  )
  expect_true(
    cdm$drug_exposure %>%
      dplyr::filter(.data$drug_exposure_start_date <= .env$end) %>%
      dplyr::filter(.data$drug_exposure_end_date >= .env$end) %>%
      dplyr::summarise(n = dplyr::n_distinct(.data$person_id)) %>%
      dplyr::pull("n") ==
      cdm$dus %>%
      dplyr::filter(.data$cohort_end_date == .env$end) %>%
      dplyr::summarise(n = dplyr::n_distinct(.data$subject_id)) %>%
      dplyr::pull("n")
  )
})

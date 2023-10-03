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
    cdm, "dus", list(acetaminophen = 1125360),
    cohortDateRange = 1
  ))
  expect_error(generateDrugUtilisationCohortSet(
    cdm, "dus", list(acetaminophen = 1125360),
    cohortDateRange = "2020-01-05"
  ))
  expect_error(generateDrugUtilisationCohortSet(
    cdm, "dus", list(acetaminophen = 1125360),
    limit = "2020-01-05"
  ))
  expect_error(generateDrugUtilisationCohortSet(
    cdm, "dus", list(acetaminophen = 1125360),
    priorObservation = "7"
  ))
  expect_error(generateDrugUtilisationCohortSet(
    cdm, "dus", list(acetaminophen = 1125360),
    gapEra = "7"
  ))
  expect_error(generateDrugUtilisationCohortSet(
    cdm, "dus", list(acetaminophen = 1125360),
    imputeDuration = "7"
  ))
  expect_error(generateDrugUtilisationCohortSet(
    cdm, "dus", list(acetaminophen = 1125360),
    imputeDuration = -7
  ))
  expect_error(generateDrugUtilisationCohortSet(
    cdm, "dus", list(acetaminophen = 1125360),
    durationRange = -7
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
    cdm, "dus", acetaminophen,
    gapEra = 0
  )
  # check cdm reference in attributes
  expect_true(!is.null(attr(cdm1$dus, "cdm_reference", exact = TRUE)))

  expect_true(cdm1$dus %>% dplyr::tally() %>% dplyr::pull() == 4)
  cdm1 <- generateDrugUtilisationCohortSet(
    cdm, "dus", acetaminophen,
    gapEra = 13
  )
  expect_true(cdm1$dus %>% dplyr::tally() %>% dplyr::pull() == 4)
  cdm1 <- generateDrugUtilisationCohortSet(
    cdm, "dus", acetaminophen,
    gapEra = 14
  )
  expect_true(cdm1$dus %>% dplyr::tally() %>% dplyr::pull() == 3)
  cdm1 <- generateDrugUtilisationCohortSet(
    cdm, "dus", acetaminophen,
    gapEra = 31
  )
  expect_true(cdm1$dus %>% dplyr::tally() %>% dplyr::pull() == 3)
  cdm1 <- generateDrugUtilisationCohortSet(
    cdm, "dus", acetaminophen,
    gapEra = 32
  )
  expect_true(cdm1$dus %>% dplyr::tally() %>% dplyr::pull() == 2)
  cdm1 <- generateDrugUtilisationCohortSet(
    cdm, "dus", acetaminophen,
    gapEra = 153
  )
  expect_true(cdm1$dus %>% dplyr::tally() %>% dplyr::pull() == 2)
  cdm1 <- generateDrugUtilisationCohortSet(
    cdm, "dus", acetaminophen,
    gapEra = 154
  )
  expect_true(cdm1$dus %>% dplyr::tally() %>% dplyr::pull() == 1)
  cdm1 <- generateDrugUtilisationCohortSet(
    cdm, "dus", acetaminophen,
    gapEra = 1500
  )
  expect_true(cdm1$dus %>% dplyr::tally() %>% dplyr::pull() == 1)
  # check first era
  cdm1 <- generateDrugUtilisationCohortSet(
    cdm, "dus", acetaminophen,
    gapEra = 1, limit = "First"
  )
  expect_true(cdm1$dus %>% dplyr::tally() %>% dplyr::pull() == 1)
  expect_true(
    cdm1$dus %>% dplyr::pull("cohort_start_date") == as.Date("2020-04-01")
  )
  expect_true(
    cdm1$dus %>% dplyr::pull("cohort_end_date") == as.Date("2020-04-30")
  )
  cdm1 <- generateDrugUtilisationCohortSet(
    cdm, "dus", acetaminophen,
    gapEra = 40, limit = "First"
  )
  expect_true(cdm1$dus %>% dplyr::tally() %>% dplyr::pull() == 1)
  expect_true(
    cdm1$dus %>% dplyr::pull("cohort_start_date") == as.Date("2020-04-01")
  )
  expect_true(
    cdm1$dus %>% dplyr::pull("cohort_end_date") == as.Date("2020-09-11")
  )
  cdm1 <- generateDrugUtilisationCohortSet(
    cdm, "dus", acetaminophen,
    gapEra = 1500, limit = "First"
  )
  expect_true(
    cdm1$dus %>% dplyr::pull("cohort_start_date") == as.Date("2020-04-01")
  )
  expect_true(
    cdm1$dus %>% dplyr::pull("cohort_end_date") == as.Date("2021-03-24")
  )
})

test_that("dates range", {
  skip_on_cran()
  cdm <- mockDrugUtilisation(connectionDetails)
  start <- as.Date("2010-01-01")
  end <- as.Date("2018-06-01")
  acetaminophen <- list("acetaminophen" = c(1125315, 43135274, 2905077, 1125360))
  expect_no_error(
    cdm <- generateDrugUtilisationCohortSet(
      cdm, "dus", acetaminophen,
      gapEra = 0, cohortDateRange = c(start, end)
    )
  )
  expect_true(
    cdm[["dus"]] %>%
      dplyr::filter(.data$cohort_start_date < start) %>%
      dplyr::tally() %>%
      dplyr::pull() == 0
  )
  expect_true(
    cdm[["dus"]] %>%
      dplyr::filter(.data$cohort_end_date > end) %>%
      dplyr::tally() %>%
      dplyr::pull() == 0
  )
  expect_true(
    cdm[["drug_exposure"]] %>%
      dplyr::filter(.data$drug_exposure_start_date <= .env$start) %>%
      dplyr::filter(.data$drug_exposure_end_date >= .env$start) %>%
      dplyr::summarise(n = dplyr::n_distinct(.data$person_id)) %>%
      dplyr::pull("n") ==
      cdm[["dus"]] %>%
        dplyr::filter(.data$cohort_start_date == .env$start) %>%
        dplyr::summarise(n = dplyr::n_distinct(.data$subject_id)) %>%
        dplyr::pull("n")
  )
  expect_true(
    cdm[["drug_exposure"]] %>%
      dplyr::filter(.data$drug_exposure_start_date <= .env$end) %>%
      dplyr::filter(.data$drug_exposure_end_date >= .env$end) %>%
      dplyr::summarise(n = dplyr::n_distinct(.data$person_id)) %>%
      dplyr::pull("n") ==
      cdm[["dus"]] %>%
        dplyr::filter(.data$cohort_end_date == .env$end) %>%
        dplyr::summarise(n = dplyr::n_distinct(.data$subject_id)) %>%
        dplyr::pull("n")
  )
  cdm <- generateDrugUtilisationCohortSet(
    cdm, "dus_date", acetaminophen,
    gapEra = 0, cohortDateRange = c(start, end)
  )

  sapply(list(cdm[["dus_date"]] %>% dplyr::pull("cohort_start_date")), function(x) inherits(x, "Date"))

  sapply(list(cdm[["dus_date"]] %>% dplyr::pull("cohort_end_date")), function(x) inherits(x, "Date"))
})

test_that("priorUseWashout", {
  skip_on_cran()
  cdm <- mockDrugUtilisation(
    observation_period = dplyr::tibble(
      observation_period_id = c(1, 2),
      person_id = c(1, 2),
      observation_period_start_date = as.Date("2020-01-01"),
      observation_period_end_date = as.Date("2020-12-31"),
      period_type_concept_id = 44814724
    ),
    drug_exposure = dplyr::tibble(
      drug_exposure_id = c(1, 2, 3),
      person_id = c(1, 1, 2),
      drug_concept_id = c(1539462, 1539462, 1539462),
      drug_exposure_start_date = as.Date(c("2020-02-01", "2020-10-01", "2020-10-01")),
      drug_exposure_end_date = as.Date(c("2020-02-01", "2020-10-01", "2021-10-01")),
      drug_type_concept_id = 38000177,
      quantity = 1
    )
  )
  cdm <- generateDrugUtilisationCohortSet(
    cdm = cdm,
    name = "bp_cohorts_test",
    conceptSetList = list("bp_conceptList" = 1539462),
    limit = "First",
    priorObservation = 180,
    gapEra = 30,
    priorUseWashout = Inf,
    imputeDuration = "none",
    durationRange = c(0, Inf)
  )
  expect_true(
    cdm[["bp_cohorts_test"]] %>%
      dplyr::filter(subject_id == 2) %>%
      dplyr::tally() %>%
      dplyr::pull() == 1
  )
  expect_true(
    cdm[["bp_cohorts_test"]] %>%
      dplyr::filter(subject_id == 1) %>%
      dplyr::tally() %>%
      dplyr::pull() == 0
  )
  expect_true(
    cdm[["bp_cohorts_test"]] %>%
      dplyr::filter(subject_id == 2) %>%
      dplyr::pull("cohort_end_date") == as.Date("2020-12-31")
  )
})



test_that("test missing end date or out of durationRange", {
  skip_on_cran()
  cdm <- mockDrugUtilisation(
    connectionDetails,
    drug_exposure = dplyr::tibble(
      drug_exposure_id = 1:5,
      person_id = c(1, 1, 1, 1, 1),
      drug_concept_id = sample(c(1125360, 2905077, 43135274), 5, replace = T),
      drug_exposure_start_date = as.Date(
        c(NA, "2020-04-01", "2020-06-01", "2021-02-12", "2021-03-01"), "%Y-%m-%d"
      ),
      drug_exposure_end_date = as.Date(
        c(NA, "2020-04-02", "2020-09-11", NA, NA), "%Y-%m-%d"
      ),
      drug_type_concept_id = 38000177,
      quantity = 1
    )
  )
  acetaminophen <- list(acetaminophen = c(1125360, 2905077, 43135274))

  cdm1 <- generateDrugUtilisationCohortSet(
    cdm = cdm,
    name = "test_missing",
    durationRange = c(1, Inf),
    imputeDuration = 1,
    conceptSetList = acetaminophen
  )


  expect_true(cdm1[["test_missing"]] %>%
    dplyr::filter(cohort_start_date == as.Date("2021-03-01")) %>%
    dplyr::pull(cohort_end_date) == as.Date("2021-03-01"))
  expect_true(cdm1[["test_missing"]] %>%
    dplyr::filter(cohort_start_date == as.Date("2021-02-12")) %>%
    dplyr::pull(cohort_end_date) == as.Date("2021-02-12"))

  # test durationRange and missingEndDate work at the same time

  cdm2 <- generateDrugUtilisationCohortSet(
    cdm = cdm,
    name = "test_missing",
    durationRange = c(1, 10),
    imputeDuration = 10,
    conceptSetList = acetaminophen
  )

  expect_true(attr(cdm2$test_missing, "numberImputation") == 4)
  # check the number is correct with missingEndDate
  expect_true(cdm2[["test_missing"]] %>%
    dplyr::filter(cohort_start_date == as.Date("2021-03-01")) %>%
    dplyr::pull(cohort_end_date) == as.Date("2021-03-10"))
  expect_true(cdm2[["test_missing"]] %>%
    dplyr::filter(cohort_start_date == as.Date("2021-02-12")) %>%
    dplyr::pull(cohort_end_date) == as.Date("2021-02-21"))


  # test missingEndDate mean mode median works correctly
  cdm <- mockDrugUtilisation(
    connectionDetails,
    drug_exposure = dplyr::tibble(
      drug_exposure_id = 1:5,
      person_id = c(1, 1, 1, 1, 1),
      drug_concept_id = sample(c(1125360, 2905077, 43135274), 5, replace = T),
      drug_exposure_start_date = as.Date(
        c("2020-04-01", "2020-04-05", "2020-06-01", "2021-02-12", "2021-03-01"),
        "%Y-%m-%d"
      ),
      drug_exposure_end_date = as.Date(
        c("2020-04-02", "2020-04-06", "2020-06-03", NA, NA),
        "%Y-%m-%d"
      ),
      drug_type_concept_id = 38000177,
      quantity = 1
    )
  )

  acetaminophen <- list(acetaminophen = c(1125360, 2905077, 43135274))


  cdm2 <- generateDrugUtilisationCohortSet(
    cdm = cdm,
    name = "test_both",
    durationRange = c(1, 2),
    imputeDuration = "mean",
    conceptSetList = acetaminophen
  )

  # non-missing durations: 2,2,3 --> mean, mode and median are all 2 (floor())

  expect_true(cdm2[["test_both"]] %>%
    dplyr::filter(cohort_start_date == as.Date("2021-03-01")) %>%
    dplyr::pull(cohort_end_date) == as.Date("2021-03-02"))
  expect_true(cdm2[["test_both"]] %>%
    dplyr::filter(cohort_start_date == as.Date("2021-02-12")) %>%
    dplyr::pull(cohort_end_date) == as.Date("2021-02-13"))
})

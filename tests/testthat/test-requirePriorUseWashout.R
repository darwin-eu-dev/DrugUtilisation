test_that("input validation", {
  skip_on_cran()
  cdm <- mockDrugUtilisation()
  expect_no_error(
    requirePriorUseWashout(
      cohort = cdm$cohort1,
      priorUseWashout = 90
    )
  )

  expect_no_error(
    requirePriorUseWashout(
      cohort = cdm$cohort1,
      priorUseWashout = Inf
    )
  )

  expect_no_error(
    requirePriorUseWashout(
      cohort = cdm$cohort1,
      priorUseWashout = Inf,
      name = "cohort2"
    )
  )

  expect_error(
    requirePriorUseWashout(
      cohort = cdm$cohort1,
      priorUseWashout = -90
    )
  )

  expect_error(
    requirePriorUseWashout(
      cohort = cdm,
      priorUseWashout = 90
    )
  )

  expect_error(
    requirePriorUseWashout(
      cohort = "cohort1",
      priorUseWashout = 90
    )
  )

  expect_error(
    requirePriorUseWashout(
      cohort = cdm$cohort1,
      priorUseWashout = "90"
    )
  )

  expect_error(
    requirePriorUseWashout(
      cohort = cdm$cohort1,
      priorUseWashout = 90,
      name = cohort2
    )
  )

  expect_error(
    cdm$cohort3 <- requirePriorUseWashout(
      cohort = cdm$cohort1,
      priorUseWashout = 90,
      name = "cohort2"
    )
  )

  expect_message(
    requirePriorUseWashout(
      cohort = cdm$cohort1,
      priorUseWashout = 0
    )
  )

  CDMConnector::cdmDisconnect(cdm = cdm)


  cohort1 <- dplyr::tibble(
    cohort_definition_id = as.integer(c()),
    subject_id = as.integer(c()),
    cohort_start_date = as.Date(c(
    )),
    cohort_end_date = as.Date(c(
    ))
  )

  observationPeriod <- dplyr::tibble(
    observation_period_id = c(1, 2, 3),
    person_id = c(1, 2, 3),
    observation_period_start_date = as.Date(c(
      "2015-01-01", "2016-05-15", "2012-12-30"
    )),
    observation_period_end_date = as.Date(c(
      "2025-01-01", "2026-05-15", "2030-12-30"
    )),
    period_type_concept_id = 44814724
  )

  cdm <-
    mockDrugUtilisation(
      #connectionDetails,
      cohort1 = cohort1,
      observation_period = observationPeriod
    )

  expect_message(
    requirePriorUseWashout(cohort = cdm$cohort1,
                           priorUseWashout = 90,
                           name = "cohort2")
  )

  CDMConnector::cdmDisconnect(cdm = cdm)
})

test_that("requirePrioUseWashout example", {
  skip_on_cran()
  cohort1 <- dplyr::tibble(
    cohort_definition_id = c(1, 1, 1, 1),
    subject_id = c(1, 1, 2, 2),
    cohort_start_date = as.Date(c(
      "2020-01-01", "2020-06-01", "2020-01-02", "2020-01-01"
    )),
    cohort_end_date = as.Date(c(
      "2020-04-01", "2020-08-01", "2020-02-02", "2020-01-01"
    ))
  )

  observationPeriod <- dplyr::tibble(
    observation_period_id = c(1, 2, 3),
    person_id = c(1, 2, 3),
    observation_period_start_date = as.Date(c(
      "2015-01-01", "2016-05-15", "2012-12-30"
    )),
    observation_period_end_date = as.Date(c(
      "2025-01-01", "2026-05-15", "2030-12-30"
    )),
    period_type_concept_id = 44814724
  )

  cdm <-
    mockDrugUtilisation(
      cohort1 = cohort1,
      observation_period = observationPeriod
    )

  cdm$cohort2 <- requirePriorUseWashout(cohort = cdm$cohort1,
                                        priorUseWashout = 90,
                                        name = "cohort2")

  expect_true((cdm$cohort1 |>
                dplyr::tally() |>
                 dplyr::pull("n") |>
                 as.numeric()) !=
              (cdm$cohort2 |>
                  dplyr::tally() |>
                 dplyr::pull("n") |>
                as.numeric())
  )

  expect_equal(
    (cdm$cohort2 |>
       dplyr::tally() |>
       dplyr::pull("n") |>
       as.numeric()),
    2
  )

  cdm$cohort2 <- requirePriorUseWashout(cohort = cdm$cohort1,
                                        priorUseWashout = 10,
                                        name = "cohort2")

  expect_equal(
    (cdm$cohort2 |>
       dplyr::tally() |>
       dplyr::pull("n") |>
       as.numeric()),
    3
  )

  cdm$cohort2 <- requirePriorUseWashout(cohort = cdm$cohort1,
                                        priorUseWashout = Inf,
                                        name = "cohort2")

  expect_equal(
    (cdm$cohort2 |>
       dplyr::tally() |>
       dplyr::pull("n") |>
       as.numeric()),
    2
  )

  CDMConnector::cdmDisconnect(cdm = cdm)
})

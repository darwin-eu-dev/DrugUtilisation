test_that("input validation", {
  skip_on_cran()
  cdm <- mockDrugUtilisation(connectionDetails)
  expect_no_error(
    requirePriorDrugWashout(
      cohort = cdm$cohort1,
      priorUseWashout = 90
    )
  )

  expect_no_error(
    requirePriorDrugWashout(
      cohort = cdm$cohort1,
      priorUseWashout = Inf
    )
  )

  expect_no_error(
    requirePriorDrugWashout(
      cohort = cdm$cohort1,
      priorUseWashout = Inf,
      name = "cohort2"
    )
  )

  expect_message(
    requirePriorDrugWashout(
      cohort = cdm$cohort1,
      priorUseWashout = Inf,
      cohortId = c(1, 3),
      name = "cohort2"
    )
  )

  expect_error(
    requirePriorDrugWashout(
      cohort = cdm$cohort1,
      priorUseWashout = -90
    )
  )

  expect_error(
    requirePriorDrugWashout(
      cohort = cdm$cohort1,
      priorUseWashout = Inf,
      cohortId = c(1, 3, 4),
      name = "cohort2"
    )
  )

  expect_error(
    requirePriorDrugWashout(
      cohort = cdm$cohort1,
      priorUseWashout = Inf,
      cohortId = "1",
      name = "cohort2"
    )
  )

  expect_error(
    requirePriorDrugWashout(
      cohort = cdm,
      priorUseWashout = 90
    )
  )

  expect_error(
    requirePriorDrugWashout(
      cohort = "cohort1",
      priorUseWashout = 90
    )
  )

  expect_error(
    requirePriorDrugWashout(
      cohort = cdm$cohort1,
      priorUseWashout = "90"
    )
  )

  expect_error(
    requirePriorDrugWashout(
      cohort = cdm$cohort1,
      priorUseWashout = 90,
      name = cohort2
    )
  )

  expect_error(
    cdm$cohort3 <- requirePriorDrugWashout(
      cohort = cdm$cohort1,
      priorUseWashout = 90,
      name = "cohort2"
    )
  )

  expect_no_error(
    requirePriorDrugWashout(
      cohort = cdm$cohort1,
      priorUseWashout = 0
    )
  )

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
      connectionDetails,
      cohort1 = cohort1,
      observation_period = observationPeriod
    )

  expect_no_error(
    requirePriorDrugWashout(cohort = cdm$cohort1,
                           priorUseWashout = 90,
                           name = "cohort2")
  )
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

  cdm$cohort2 <- requirePriorDrugWashout(cohort = cdm$cohort1,
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

  cdm$cohort2 <- requirePriorDrugWashout(cohort = cdm$cohort1,
                                        priorUseWashout = 10,
                                        name = "cohort2")

  expect_equal(
    (cdm$cohort2 |>
       dplyr::tally() |>
       dplyr::pull("n") |>
       as.numeric()),
    3
  )

  cdm$cohort2 <- requirePriorDrugWashout(cohort = cdm$cohort1,
                                        priorUseWashout = Inf,
                                        name = "cohort2")

  expect_equal(
    (cdm$cohort2 |>
       dplyr::tally() |>
       dplyr::pull("n") |>
       as.numeric()),
    2
  )

  cohort1 <- dplyr::tibble(
    cohort_definition_id = c(1, 1, 2, 2),
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

  cdm$cohort3 <- requirePriorDrugWashout(cohort = cdm$cohort1,
                                        priorUseWashout = 90,
                                        name = "cohort3")

  expect_equal(
    (cdm$cohort3 |>
       dplyr::tally() |>
       dplyr::pull("n") |>
       as.numeric()),
    2
  )

  cdm$cohort3 <- requirePriorDrugWashout(cohort = cdm$cohort1,
                                        priorUseWashout = 0,
                                        name = "cohort3")

  expect_equal(
    cdm$cohort3 |> dplyr::tally() |> dplyr::pull("n") |> as.numeric(),
    4
  )

  cdm$cohort3 <- cdm$cohort1 |>
    requirePriorDrugWashout(
      priorUseWashout = 90, cohortId = 1, name = "cohort3")

  expect_equal(
    cdm$cohort3 |> dplyr::tally() |> dplyr::pull("n") |> as.numeric(),
    3
  )

  cdm$cohort3 <- requirePriorDrugWashout(cohort = cdm$cohort1,
                                        priorUseWashout = 90,
                                        cohortId = 2,
                                        name = "cohort3")

  expect_equal(
    (cdm$cohort3 |>
       dplyr::tally() |>
       dplyr::pull("n") |>
       as.numeric()),
    3
  )

  cdm$cohort3 <- requirePriorDrugWashout(cohort = cdm$cohort1,
                                        priorUseWashout = 90,
                                        cohortId = c(1,2),
                                        name = "cohort3")

  expect_equal(
    (cdm$cohort3 |>
       dplyr::tally() |>
       dplyr::pull("n") |>
       as.numeric()),
    2
  )
})

test_that("test cohortId, example 2", {
  skip_on_cran()
  cohort1 <- dplyr::tibble(
    cohort_definition_id = c(1, 2, 1, 2),
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

  cdm$cohort3 <- requirePriorDrugWashout(cohort = cdm$cohort1,
                                        priorUseWashout = 90,
                                        name = "cohort3")

  expect_equal(
    (cdm$cohort3 |>
       dplyr::tally() |>
       dplyr::pull("n") |>
       as.numeric()),
    4
  )

  cdm$cohort3 <- requirePriorDrugWashout(cohort = cdm$cohort1,
                                        priorUseWashout = 0,
                                        name = "cohort3")

  expect_equal(
    (cdm$cohort3 |>
       dplyr::tally() |>
       dplyr::pull("n") |>
       as.numeric()),
    4
  )

  cdm$cohort3 <- requirePriorDrugWashout(cohort = cdm$cohort1,
                                        priorUseWashout = 90,
                                        cohortId = 1,
                                        name = "cohort3")

  expect_equal(
    (cdm$cohort3 |>
       dplyr::tally() |>
       dplyr::pull("n") |>
       as.numeric()),
    4
  )

  cdm$cohort3 <- requirePriorDrugWashout(cohort = cdm$cohort1,
                                        priorUseWashout = 90,
                                        cohortId = 2,
                                        name = "cohort3")

  expect_equal(
    (cdm$cohort3 |>
       dplyr::tally() |>
       dplyr::pull("n") |>
       as.numeric()),
    4
  )

  cdm$cohort3 <- requirePriorDrugWashout(cohort = cdm$cohort1,
                                        priorUseWashout = 90,
                                        cohortId = c(1,2),
                                        name = "cohort3")

  expect_equal(
    (cdm$cohort3 |>
       dplyr::tally() |>
       dplyr::pull("n") |>
       as.numeric()),
    4
  )
})

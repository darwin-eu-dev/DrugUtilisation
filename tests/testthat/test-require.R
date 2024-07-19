test_that("input validation", {
  skip_on_cran()
  cdm <- mockDrugUtilisation(connectionDetails)
  expect_no_error(
    requirePriorDrugWashout(
      cohort = cdm$cohort1,
      days = 90
    )
  )

  expect_no_error(
    requirePriorDrugWashout(
      cohort = cdm$cohort1,
      days = Inf
    )
  )

  expect_no_error(
    requirePriorDrugWashout(
      cohort = cdm$cohort1,
      days = Inf,
      name = "cohort2"
    )
  )

  expect_message(
    requirePriorDrugWashout(
      cohort = cdm$cohort1,
      days = Inf,
      cohortId = c(1, 3),
      name = "cohort2"
    )
  )

  expect_error(
    requirePriorDrugWashout(
      cohort = cdm$cohort1,
      days = -90
    )
  )

  expect_error(
    requirePriorDrugWashout(
      cohort = cdm$cohort1,
      days = Inf,
      cohortId = c(1, 3, 4),
      name = "cohort2"
    )
  )

  expect_error(
    requirePriorDrugWashout(
      cohort = cdm$cohort1,
      days = Inf,
      cohortId = "1",
      name = "cohort2"
    )
  )

  expect_error(
    requirePriorDrugWashout(
      cohort = cdm,
      days = 90
    )
  )

  expect_error(
    requirePriorDrugWashout(
      cohort = "cohort1",
      days = 90
    )
  )

  expect_error(
    requirePriorDrugWashout(
      cohort = cdm$cohort1,
      days = "90"
    )
  )

  expect_error(
    requirePriorDrugWashout(
      cohort = cdm$cohort1,
      days = 90,
      name = cohort2
    )
  )

  expect_error(
    cdm$cohort3 <- requirePriorDrugWashout(
      cohort = cdm$cohort1,
      days = 90,
      name = "cohort2"
    )
  )

  expect_no_error(
    requirePriorDrugWashout(
      cohort = cdm$cohort1,
      days = 0
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
                           days = 90,
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
                                        days = 90,
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
                                        days = 10,
                                        name = "cohort2")

  expect_equal(
    (cdm$cohort2 |>
       dplyr::tally() |>
       dplyr::pull("n") |>
       as.numeric()),
    3
  )

  cdm$cohort2 <- requirePriorDrugWashout(cohort = cdm$cohort1,
                                        days = Inf,
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
                                        days = 90,
                                        name = "cohort3")

  expect_equal(
    (cdm$cohort3 |>
       dplyr::tally() |>
       dplyr::pull("n") |>
       as.numeric()),
    2
  )

  cdm$cohort3 <- requirePriorDrugWashout(cohort = cdm$cohort1,
                                        days = 0,
                                        name = "cohort3")

  expect_equal(
    cdm$cohort3 |> dplyr::tally() |> dplyr::pull("n") |> as.numeric(),
    4
  )

  cdm$cohort3 <- cdm$cohort1 |>
    requirePriorDrugWashout(
      days = 90, cohortId = 1, name = "cohort3")

  expect_equal(
    cdm$cohort3 |> dplyr::tally() |> dplyr::pull("n") |> as.numeric(),
    3
  )

  cdm$cohort3 <- requirePriorDrugWashout(cohort = cdm$cohort1,
                                        days = 90,
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
                                        days = 90,
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
                                        days = 90,
                                        name = "cohort3")

  expect_equal(
    (cdm$cohort3 |>
       dplyr::tally() |>
       dplyr::pull("n") |>
       as.numeric()),
    4
  )

  cdm$cohort3 <- requirePriorDrugWashout(cohort = cdm$cohort1,
                                        days = 0,
                                        name = "cohort3")

  expect_equal(
    (cdm$cohort3 |>
       dplyr::tally() |>
       dplyr::pull("n") |>
       as.numeric()),
    4
  )

  cdm$cohort3 <- requirePriorDrugWashout(cohort = cdm$cohort1,
                                        days = 90,
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
                                        days = 90,
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
                                        days = 90,
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

test_that("requireDrugInDateRange", {
  skip_on_cran()
  cdm <- mockDrugUtilisation(connectionDetails)

  cdm$cohort3 <- requireDrugInDateRange(
    cohort = cdm$cohort1,
    dateRange = as.Date(c("2020-01-01", "2020-12-12")),
    indexDate = "cohort_start_date",
    cohortId = NULL,
    name = "cohort3"
  )
  expect_true(nrow(cdm$cohort3 |> dplyr::collect()) == 1)
  expect_true(cdm$cohort3 |> dplyr::pull("subject_id") == 1)
  expect_true(all(attrition(cdm$cohort3)$reason == c(
    'Qualifying initial records', 'require cohort_start_date between 2020-01-01 to 2020-12-12',
    'Qualifying initial records', 'require cohort_start_date between 2020-01-01 to 2020-12-12',
    'Qualifying initial records', 'require cohort_start_date between 2020-01-01 to 2020-12-12'
  )))

  cdm$cohort4 <- requireDrugInDateRange(
    cohort = cdm$cohort1,
    dateRange = as.Date(c("2020-01-01", "2020-12-12")),
    indexDate = "cohort_start_date",
    cohortId = 2:3,
    name = "cohort4"
  )
  expect_true(nrow(cdm$cohort4 |> dplyr::collect()) == 4)
  expect_true(cdm$cohort4 |> dplyr::pull("cohort_definition_id") |> unique() == 1)
  expect_true(all(attrition(cdm$cohort4)$reason == c(
    'Qualifying initial records',
    'Qualifying initial records', 'require cohort_start_date between 2020-01-01 to 2020-12-12',
    'Qualifying initial records', 'require cohort_start_date between 2020-01-01 to 2020-12-12'
  )))

  cdm$cohort5 <- requireDrugInDateRange(
    cohort = cdm$cohort1,
    dateRange = as.Date(c(NA, "2000-01-01")),
    indexDate = "cohort_start_date",
    name = "cohort5"
  )
  expect_true(nrow(cdm$cohort5 |> dplyr::collect()) == 2)
  expect_true(all(cdm$cohort5 |> dplyr::pull("cohort_definition_id") |> unique() |> sort() == c(1, 3)))
  expect_true(all(attrition(cdm$cohort5)$reason == c(
    'Qualifying initial records', 'require cohort_start_date before 2000-01-01',
    'Qualifying initial records', 'require cohort_start_date before 2000-01-01',
    'Qualifying initial records', 'require cohort_start_date before 2000-01-01'
  )))

  cdm$cohort6 <- requireDrugInDateRange(
    cohort = cdm$cohort1,
    dateRange = as.Date(c("2020-01-01", NA)),
    indexDate = "cohort_end_date",
    name = "cohort6"
  )
  expect_true(nrow(cdm$cohort6 |> dplyr::collect()) == 4)
  expect_true(all(cdm$cohort6 |> dplyr::pull("cohort_definition_id") |> sort() == c(1, 1, 2, 3)))
  expect_true(all(cdm$cohort6 |> dplyr::pull("subject_id") |> unique() |> sort() == c(1, 2, 4, 9)))
  expect_true(all(attrition(cdm$cohort6)$reason == c(
    'Qualifying initial records', 'require cohort_end_date after 2020-01-01',
    'Qualifying initial records', 'require cohort_end_date after 2020-01-01',
    'Qualifying initial records', 'require cohort_end_date after 2020-01-01'
  )))

  cohort1 <- cdm$cohort1 |> collectCohort()
  cdm$cohort1 <- requireDrugInDateRange(
    cohort = cdm$cohort1,
    dateRange = as.Date(c(NA, NA)),
    indexDate = "cohort_end_date"
  )
  expect_true(all(attrition(cdm$cohort1)$reason == c(
    'Qualifying initial records', 'No date restrictions to cohort_end_date',
    'Qualifying initial records', 'No date restrictions to cohort_end_date',
    'Qualifying initial records', 'No date restrictions to cohort_end_date'
  )))
  expect_equal(cohort1, cdm$cohort1 |> collectCohort())
})

test_that("requireObservationBeforeDrug", {
  skip_on_cran()
  cdm <- mockDrugUtilisation(connectionDetails)

  cdm$cohort3 <- requireObservationBeforeDrug(
    cohort = cdm$cohort1,
    priorObservation = 0,
    cohortId = NULL,
    name = "cohort3"
  )
  expect_equal(cdm$cohort1 |> collectCohort(), cdm$cohort3 |> collectCohort())
  expect_true(all(attrition(cdm$cohort3)$reason == c(
    'Qualifying initial records', 'require prior observation of 0 days',
    'Qualifying initial records', 'require prior observation of 0 days',
    'Qualifying initial records', 'require prior observation of 0 days'
  )))

  cdm$cohort4 <- requireObservationBeforeDrug(
    cohort = cdm$cohort1,
    priorObservation = 999999,
    cohortId = 1,
    name = "cohort4"
  )
  expect_false(1 %in% (cdm$cohort4 |> dplyr::pull("cohort_definition_id")))
  expect_equal(cdm$cohort1 |> collectCohort(2:3), cdm$cohort4 |> collectCohort(2:3))
  expect_true(all(attrition(cdm$cohort4)$reason == c(
    'Qualifying initial records', 'require prior observation of 999999 days',
    'Qualifying initial records',
    'Qualifying initial records'
  )))

  cohort <- requireObservationBeforeDrug(
    cohort = cdm$cohort1,
    priorObservation = 200
  )
  expect_true(omopgenerics::tableName(cohort) == "cohort1")
  expect_true(all(cohort |> dplyr::pull("cohort_definition_id") |> sort() == c(1, 1, 2, 3, 3)))
  expect_true(all(cohort |> dplyr::pull("subject_id") |> sort() == c(3, 4, 5, 6, 10)))
  expect_true(all(cohort |> dplyr::pull("cohort_start_date") |> sort() == c("1991-05-25", "1997-11-07", "2010-02-10", "2019-07-17", "2022-01-26")))
})

test_that("requireIsFirstDrugEntry", {
  skip_on_cran()
  cdm <- mockDrugUtilisation(
    connectionDetails,
    cohort = dplyr::tibble(
      cohort_definition_id = c(1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2),
      subject_id = c(1, 1, 1, 2, 3, 3, 3, 3, 1, 1, 2, 3),
      cohort_start_date = as.Date(c(
        "2020-04-01", "2020-05-01", "2020-06-01", "2022-05-21", "1983-08-02",
        "1983-12-02", "1993-08-02", "2005-08-02", "2020-04-01", "2020-05-01",
        "2022-05-30", "2000-08-02"
      )),
      cohort_end_date = as.Date(c(
        "2020-04-01", "2020-05-01", "2020-06-01", "2022-05-21", "1983-08-02",
        "1983-12-02", "1993-08-02", "2005-08-02", "2020-04-01", "2020-05-01",
        "2022-05-30", "2000-08-02"
      ))
    )
  )

  cdm$cohort3 <- requireIsFirstDrugEntry(
    cohort = cdm$cohort,
    cohortId = NULL,
    name = "cohort3"
  )
  expect_true(all(attrition(cdm$cohort3)$reason == c(
    "Qualifying initial records", "require is the first entry", "Qualifying initial records", "require is the first entry"
  )))
  expect_true(all(cdm$cohort3 |> dplyr::pull("cohort_start_date") |> sort() == c(
    "1983-08-02", "1993-08-02", "2020-04-01", "2020-04-01", "2022-05-21", "2022-05-30"
  )))
  expect_true(all(cdm$cohort3 |> dplyr::pull("cohort_definition_id") |> sort() == c(
    1, 1, 1, 2, 2, 2
  )))
  expect_true(all(cdm$cohort3 |> dplyr::pull("subject_id") |> sort() == c(
    1, 1, 2, 2, 3, 3
  )))

  cohort <- requireIsFirstDrugEntry(
    cohort = cdm$cohort,
    cohortId = 1
  )
  expect_true(all(attrition(cohort)$reason == c(
    "Qualifying initial records", "require is the first entry", "Qualifying initial records"
  )))
  expect_true(all(cohort |> dplyr::pull("cohort_start_date") |> sort() == c(
    "1983-08-02", "1993-08-02", "2000-08-02", "2005-08-02", "2020-04-01", "2020-04-01", "2020-05-01", "2022-05-21", "2022-05-30"
  )))
  expect_true(all(cohort |> dplyr::pull("cohort_definition_id") |> sort() == c(
    1, 1, 1, 2, 2, 2, 2, 2, 2
  )))
  expect_true(all(cohort |> dplyr::pull("subject_id") |> sort() == c(
    1, 1, 1, 2, 2, 3, 3, 3, 3
  )))

})

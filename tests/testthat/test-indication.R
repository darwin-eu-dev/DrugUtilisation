test_that("test case single indication", {
  skip_on_cran()
  targetCohortName <- dplyr::tibble(
    cohort_definition_id = c(1, 1, 1, 2),
    subject_id = c(1, 1, 2, 3),
    cohort_start_date = as.Date(c(
      "2020-01-01", "2020-06-01", "2020-01-02", "2020-01-01"
    )),
    cohort_end_date = as.Date(c(
      "2020-04-01", "2020-08-01", "2020-02-02", "2020-03-01"
    ))
  )
  indicationCohortName <- dplyr::tibble(
    cohort_definition_id = c(1, 1, 2, 1),
    subject_id = c(1, 3, 1, 1),
    cohort_start_date = as.Date(
      c(
        "2019-12-30",
        "2020-01-01",
        "2020-05-25",
        "2020-05-25"
      )
    ),
    cohort_end_date = as.Date(
      c(
        "2019-12-30",
        "2020-01-01",
        "2020-05-25",
        "2020-05-25"
      )
    )
  )
  attr(indicationCohortName, "cohort_set") <- dplyr::tibble(
    cohort_definition_id = c(1, 2),
    cohort_name = c("asthma", "covid")
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
  condition_occurrence <- dplyr::tibble(
    condition_occurrence_id = 1,
    condition_concept_id = 0,
    condition_type_concept_id = 0,
    person_id = 1,
    condition_start_date = as.Date("2020-05-31"),
    condition_end_date = as.Date("2020-05-31")
  )

  cdm <- mockDrugUtilisation(
    con = connection(),
    writeSchema = schema(),
    cohort1 = targetCohortName,
    cohort2 = indicationCohortName,
    condition_occurrence = condition_occurrence,
    observation_period = observationPeriod
  )
  # check it works without cdm object specified

  expect_no_error(cdm[["cohort1"]] |>
    addIndication(
      indicationCohortName = "cohort2", indicationWindow = list(c(0, 0)),
      unknownIndicationTable = NULL
    ))

  # check overwrites existing variable with warning
  expect_warning(cdm$new <- cdm[["cohort1"]] |>
    dplyr::mutate(a = 1) |>
    addIndication(
      indicationCohortName = "cohort2",
      indicationWindow = list(
        c(0, 0),
        c(-Inf, 0)
      ),
      indexDate = "cohort_end_date",
      unknownIndicationTable = NULL
    ) |>
    addIndication(
      indicationCohortName = "cohort2",
      indicationWindow = list(
        c(0, 0),
        c(-Inf, 0)
      ),
      indexDate = "cohort_end_date",
      unknownIndicationTable = NULL
    ))
  expect_true("a" %in% colnames(cdm$new))



  # check for indication 0
  res0 <- cdm[["cohort1"]] |>
    addIndication(
      indicationCohortName = "cohort2", indicationWindow = list(c(0, 0)),
      unknownIndicationTable = NULL
    )
  expect_true(length(setdiff(colnames(res0), colnames(cdm[["cohort1"]]))) == 1)
  expect_true(all(
    c("indication_0_to_0") %in%
      setdiff(colnames(res0), colnames(cdm[["cohort1"]]))
  ))

  expect_true(
    res0 |>
      dplyr::filter(.data$subject_id == 3) |>
      dplyr::pull("indication_0_to_0") == "asthma"
  )
  expect_true(
    all(res0 |>
      dplyr::filter(.data$subject_id != 3) |>
      dplyr::pull("indication_0_to_0") == "none")
  )

  # check for indication 1
  res1 <- cdm[["cohort1"]] |>
    addIndication(
      indicationCohortName = "cohort2", indicationWindow = list(c(-1, 0)),
      unknownIndicationTable = NULL
    )
  expect_true(length(setdiff(colnames(res1), colnames(cdm[["cohort1"]]))) == 1)
  expect_true(all(
    c("indication_m1_to_0") %in%
      setdiff(colnames(res1), colnames(cdm[["cohort1"]]))
  ))


  expect_true(
    res1 |>
      dplyr::filter(.data$subject_id == 3) |>
      dplyr::pull("indication_m1_to_0") == "asthma"
  )
  expect_true(
    all(res1 |>
      dplyr::filter(.data$subject_id != 3) |>
      dplyr::pull("indication_m1_to_0") == "none")
  )

  # check for indication 2
  res2 <- cdm[["cohort1"]] |>
    addIndication(
      indicationCohortName = "cohort2", indicationWindow = list(c(-2, 0)),
      unknownIndicationTable = NULL
    )

  expect_true(length(setdiff(colnames(res2), colnames(cdm[["cohort1"]]))) == 1)
  expect_true(all(
    c("indication_m2_to_0") %in%
      setdiff(colnames(res2), colnames(cdm[["cohort1"]]))
  ))

  expect_true(all(
    res2 |>
      dplyr::arrange(
        .data$subject_id,
        .data$cohort_start_date,
        .data$cohort_definition_id,
        .data$cohort_end_date
      ) |>
      dplyr::pull("indication_m2_to_0") ==
      c("asthma", "none", "none", "asthma")
  ))

  # check for all indication Gap
  resinf <- cdm[["cohort1"]] |>
    addIndication(
      indicationCohortName = "cohort2", indicationWindow = list(c(-Inf, 0)),
      unknownIndicationTable = NULL
    )
  expect_true(length(setdiff(colnames(resinf), colnames(cdm[["cohort1"]]))) == 1)
  expect_true(all(
    c("indication_minf_to_0") %in%
      setdiff(colnames(resinf), colnames(cdm[["cohort1"]]))
  ))


  expect_true(any(
    identical(
      resinf |>
        dplyr::arrange(.data$subject_id, .data$cohort_start_date) |>
        dplyr::pull("indication_minf_to_0"),
      c("asthma", "asthma and covid", "none", "asthma")
    ),
    identical(
      resinf |>
        dplyr::arrange(.data$subject_id, .data$cohort_start_date) |>
        dplyr::pull("indication_minf_to_0"),
      c("asthma", "asthma and covid", "none", "asthma")
    )
  ))

  mockDisconnect(cdm = cdm)
})

test_that("test case single indication with unknown indication table", {
  skip_on_cran()
  targetCohortName <- dplyr::tibble(
    cohort_definition_id = c(1, 1, 1, 2),
    subject_id = c(1, 1, 2, 3),
    cohort_start_date = as.Date(c(
      "2020-01-01", "2020-06-01", "2020-01-02", "2020-01-01"
    )),
    cohort_end_date = as.Date(c(
      "2020-04-01", "2020-08-01", "2020-02-02", "2020-03-01"
    ))
  )
  indicationCohortName <- dplyr::tibble(
    cohort_definition_id = c(1, 1, 2, 1),
    subject_id = c(1, 3, 1, 1),
    cohort_start_date = as.Date(c(
      "2019-12-30", "2020-01-01", "2020-05-25", "2020-05-25"
    )),
    cohort_end_date = as.Date(c(
      "2019-12-30", "2020-01-01", "2020-05-25", "2020-05-25"
    ))
  )
  condition_occurrence <- dplyr::tibble(
    person_id = 1,
    condition_start_date = as.Date("2020-05-31"),
    condition_end_date = as.Date("2020-05-31"),
    condition_occurrence_id = 1,
    condition_concept_id = 0,
    condition_type_concept_id = 0
  )
  attr(indicationCohortName, "cohort_set") <- dplyr::tibble(
    cohort_definition_id = c(1, 2),
    cohort_name = c("asthma", "covid")
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
  cdm <- mockDrugUtilisation(
    con = connection(),
    writeSchema = schema(),
    cohort1 = targetCohortName,
    cohort2 = indicationCohortName, condition_occurrence = condition_occurrence,
    observation_period = observationPeriod
  )

  # check for indication 0
  res0 <- cdm[["cohort1"]] |>
    addIndication(
      indicationCohortName = "cohort2", indicationWindow = list(c(0, 0)),
      unknownIndicationTable = "condition_occurrence"
    )
  expect_true(length(setdiff(colnames(res0), colnames(cdm[["cohort1"]]))) == 1)
  expect_true(all(
    c("indication_0_to_0") %in%
      setdiff(colnames(res0), colnames(cdm[["cohort1"]]))
  ))

  expect_true(identical(
    res0 |>
      dplyr::arrange(.data$subject_id, .data$cohort_start_date) |>
      dplyr::pull("indication_0_to_0"),
    c("none", "none", "none", "asthma")
  ))

  # check for indication 1
  res1 <- cdm[["cohort1"]] |>
    addIndication(
      indicationCohortName = "cohort2", indicationWindow = list(c(-1, 0)),
      unknownIndicationTable = "condition_occurrence"
    )
  expect_true(length(setdiff(colnames(res1), colnames(cdm[["cohort1"]]))) == 1)
  expect_true(all(
    c("indication_m1_to_0") %in%
      setdiff(colnames(res1), colnames(cdm[["cohort1"]]))
  ))

  expect_true(identical(
    res1 |>
      dplyr::arrange(.data$subject_id, .data$cohort_start_date) |>
      dplyr::pull("indication_m1_to_0"),
    c("none", "unknown", "none", "asthma")
  ))

  # check for indication 6
  res6 <- cdm[["cohort1"]] |>
    addIndication(
      indicationCohortName = "cohort2", indicationWindow = list(c(-6, 0)),
      unknownIndicationTable = "condition_occurrence"
    )
  expect_true(length(setdiff(colnames(res6), colnames(cdm[["cohort1"]]))) == 1)
  expect_true(all(
    c("indication_m6_to_0") %in%
      setdiff(colnames(res6), colnames(cdm[["cohort1"]]))
  ))

  expect_true(identical(
    res6 |>
      dplyr::arrange(.data$subject_id, .data$cohort_start_date) |>
      dplyr::pull("indication_m6_to_0"),
    c("asthma", "unknown", "none", "asthma")
  ))

  # check all gaps simultaniously
  res016 <- cdm[["cohort1"]] |>
    addIndication(
      indicationCohortName = "cohort2", indicationWindow = list(c(0, 0), c(-1, 0), c(-6, 0)),
      unknownIndicationTable = "condition_occurrence"
    )
  expect_true(length(setdiff(colnames(res016), colnames(cdm[["cohort1"]]))) == 3)
  expect_true(all(
    c(
      "indication_0_to_0", "indication_m1_to_0",
      "indication_m6_to_0"
    ) %in%
      setdiff(colnames(res016), colnames(cdm[["cohort1"]]))
  ))

  expect_true(identical(
    res016 |>
      dplyr::arrange(.data$subject_id, .data$cohort_start_date) |>
      dplyr::pull("indication_0_to_0"),
    res0 |>
      dplyr::arrange(.data$subject_id, .data$cohort_start_date) |>
      dplyr::pull("indication_0_to_0")
  ))
  expect_true(identical(
    res016 |>
      dplyr::arrange(.data$subject_id, .data$cohort_start_date) |>
      dplyr::pull("indication_m1_to_0"),
    res1 |>
      dplyr::arrange(.data$subject_id, .data$cohort_start_date) |>
      dplyr::pull("indication_m1_to_0")
  ))
  expect_true(identical(
    res016 |>
      dplyr::arrange(.data$subject_id, .data$cohort_start_date) |>
      dplyr::pull("indication_m6_to_0"),
    res6 |>
      dplyr::arrange(.data$subject_id, .data$cohort_start_date) |>
      dplyr::pull("indication_m6_to_0")
  ))

  mockDisconnect(cdm = cdm)
})

test_that("test indicationDate", {
  skip_on_cran()
  targetCohortName <- dplyr::tibble(
    cohort_definition_id = c(1, 1, 1, 2),
    subject_id = c(1, 1, 2, 3),
    cohort_start_date = as.Date(c(
      "2020-01-01", "2020-06-01", "2020-01-02", "2020-01-01"
    )),
    cohort_end_date = as.Date(c(
      "2020-04-01", "2020-08-01", "2020-02-02", "2020-03-01"
    ))
  )
  indicationCohortName <- dplyr::tibble(
    cohort_definition_id = c(1, 1, 2, 1),
    subject_id = c(1, 3, 1, 1),
    cohort_start_date = as.Date(c(
      "2019-12-30", "2020-01-01", "2020-05-25", "2020-05-25"
    )),
    cohort_end_date = as.Date(c(
      "2019-12-30", "2020-01-01", "2020-05-25", "2020-05-25"
    ))
  )
  attr(indicationCohortName, "cohort_set") <- dplyr::tibble(
    cohort_definition_id = c(1, 2),
    cohort_name = c("asthma", "covid")
  )
  condition_occurrence <- dplyr::tibble(
    person_id = 1,
    condition_start_date = as.Date("2020-05-31"),
    condition_end_date = as.Date("2020-05-31"),
    condition_occurrence_id = 1,
    condition_concept_id = 0,
    condition_type_concept_id = 0
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
      con = connection(),
      writeSchema = schema(),
      cohort1 = targetCohortName,
      cohort2 = indicationCohortName,
      condition_occurrence = condition_occurrence,
      observation_period = observationPeriod
    )

  # original
  res012inf <- cdm[["cohort1"]] |>
    addIndication(
      indicationCohortName = "cohort2",
      indicationWindow = list(c(0, 0), c(-1, 0), c(-2, 0), c(-Inf, 0)), unknownIndicationTable = NULL
    )
  expect_equal(
    sort(setdiff(colnames(res012inf), colnames(cdm[["cohort1"]]))),
    sort(c(
      "indication_0_to_0", "indication_m1_to_0", "indication_m2_to_0",
      "indication_minf_to_0"
    ))
  )

  # change indicationDate
  cdm[["cohort1"]] <- cdm[["cohort1"]] |>
    dplyr::rename("start_date" = "cohort_start_date")
  res012infS <- cdm[["cohort1"]] |>
    addIndication(
      indicationCohortName = "cohort2",
      indicationWindow = list(c(0, 0), c(-1, 0), c(-2, 0), c(-Inf, 0)), unknownIndicationTable = NULL,
      indexDate = "start_date"
    )
  expect_equal(
    sort(setdiff(colnames(res012infS), colnames(cdm[["cohort1"]]))),
    sort(c(
      "indication_0_to_0", "indication_m1_to_0", "indication_m2_to_0",
      "indication_minf_to_0"
    ))
  )
  expect_true(
    setdiff(colnames(res012inf), colnames(res012infS)) == "cohort_start_date"
  )
  expect_true(identical(
    res012inf |>
      dplyr::arrange(.data$subject_id, .data$cohort_start_date) |>
      dplyr::pull("indication_0_to_0"),
    res012infS |>
      dplyr::arrange(.data$subject_id, .data$start_date) |>
      dplyr::pull("indication_0_to_0")
  ))
  expect_true(identical(
    res012inf |>
      dplyr::arrange(.data$subject_id, .data$cohort_start_date) |>
      dplyr::pull("indication_m1_to_0"),
    res012infS |>
      dplyr::arrange(.data$subject_id, .data$start_date) |>
      dplyr::pull("indication_m1_to_0")
  ))
  expect_true(identical(
    res012inf |>
      dplyr::arrange(.data$subject_id, .data$cohort_start_date) |>
      dplyr::pull("indication_m2_to_0"),
    res012infS |>
      dplyr::arrange(.data$subject_id, .data$start_date) |>
      dplyr::pull("indication_m2_to_0")
  ))

  mockDisconnect(cdm = cdm)
})

test_that("test attributes", {
  skip_on_cran()
  targetCohortName <- dplyr::tibble(
    cohort_definition_id = c(1, 1, 1, 2),
    subject_id = c(1, 1, 2, 3),
    cohort_start_date = as.Date(c(
      "2020-01-01", "2020-06-01", "2020-01-02", "2020-01-01"
    )),
    cohort_end_date = as.Date(c(
      "2020-04-01", "2020-08-01", "2020-02-02", "2020-03-01"
    ))
  )
  indicationCohortName <- dplyr::tibble(
    cohort_definition_id = c(1, 1, 2, 1),
    subject_id = c(1, 3, 1, 1),
    cohort_start_date = as.Date(c(
      "2019-12-30", "2020-01-01", "2020-05-25", "2020-05-25"
    )),
    cohort_end_date = as.Date(c(
      "2019-12-30", "2020-01-01", "2020-05-25", "2020-05-25"
    ))
  )
  attr(indicationCohortName, "cohort_set") <- dplyr::tibble(
    cohort_definition_id = c(1, 2),
    cohort_name = c("asthma", "covid")
  )
  condition_occurrence <- dplyr::tibble(
    person_id = 1,
    condition_start_date = as.Date("2020-05-31"),
    condition_end_date = as.Date("2020-05-31"),
    condition_occurrence_id = 1,
    condition_concept_id = 0,
    condition_type_concept_id = 0
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
      con = connection(),
      writeSchema = schema(),
      cohort1 = targetCohortName,
      cohort2 = indicationCohortName,
      condition_occurrence = condition_occurrence,
      observation_period = observationPeriod
    )

  cdm[["cohort1new"]] <- cdm[["cohort1"]] |>
    addIndication(
      indicationCohortName = "cohort2",
      indicationWindow = list(c(0, 0), c(-1, 0), c(-2, 0), c(-Inf, 0)), unknownIndicationTable = NULL
    )
  expect_identical(
    sort(names(attributes(cdm[["cohort1"]]))),
    sort(names(attributes(cdm[["cohort1new"]])))
  )


  expect_identical(
    class(cdm[["cohort1"]])[class(cdm[["cohort1"]]) != "GeneratedCohortSet"],
    class(cdm[["cohort1new"]])[class(cdm[["cohort1new"]]) != "GeneratedCohortSet"]
  )

  mockDisconnect(cdm = cdm)
})

test_that("summariseIndication", {
  targetCohortName <- dplyr::tibble(
    cohort_definition_id = c(1, 1, 1, 2),
    subject_id = c(1, 1, 2, 3),
    cohort_start_date = as.Date(c(
      "2020-01-01", "2020-06-01", "2020-01-02", "2020-01-01"
    )),
    cohort_end_date = as.Date(c(
      "2020-04-01", "2020-08-01", "2020-02-02", "2020-03-01"
    ))
  )
  indicationCohortName <- dplyr::tibble(
    cohort_definition_id = c(1, 1, 2, 1),
    subject_id = c(1, 3, 1, 1),
    cohort_start_date = as.Date(c(
      "2019-12-30", "2020-01-01", "2020-05-25", "2020-05-25"
    )),
    cohort_end_date = as.Date(c(
      "2019-12-30", "2020-01-01", "2020-05-25", "2020-05-25"
    ))
  )
  attr(indicationCohortName, "cohort_set") <- dplyr::tibble(
    cohort_definition_id = c(1, 2),
    cohort_name = c("asthma", "covid")
  )
  condition_occurrence <- dplyr::tibble(
    person_id = 1,
    condition_start_date = as.Date("2020-05-31"),
    condition_end_date = as.Date("2020-05-31"),
    condition_occurrence_id = 1,
    condition_concept_id = 0,
    condition_type_concept_id = 0
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
      con = connection(),
      writeSchema = schema(),
      cohort1 = targetCohortName,
      cohort2 = indicationCohortName,
      condition_occurrence = condition_occurrence,
      observation_period = observationPeriod
    )

  result <- cdm[["cohort1"]] |>
    summariseIndication(
      indicationCohortName = "cohort2",
      unknownIndicationTable = "condition_occurrence",
      indicationWindow = list(c(0, 0), c(-7, 0), c(-30, 0), c(-Inf, 0))
    )

  expect_true(inherits(result, "summarised_result"))
  expect_true(any(grepl("Indication on index date", result$variable_name)))
  expect_true(any(grepl("Indication from 7 days before to the index date", result$variable_name)))
  expect_true(any(grepl("Indication from 30 days before to the index date", result$variable_name)))
  expect_true(any(grepl("Indication any time before or on index date", result$variable_name)))

  res <- cdm[["cohort1"]] |>
    PatientProfiles::addAge(
      ageGroup = list("<40" = c(0, 39), ">=40" = c(40, 150))
    ) |>
    PatientProfiles::addSex()

  result <- res |>
    summariseIndication(
      strata = list(
        "age_group",
        "sex",
        c("age_group", "sex")
      ),
      indicationCohortName = "cohort2",
      unknownIndicationTable = "condition_occurrence",
      indicationWindow = list(c(0, 0), c(-7, 0), c(-30, 0), c(-Inf, 0))
    )


  expect_true(inherits(result, "summarised_result"))
  x <- tidyr::expand_grid(
    group_level = omopgenerics::settings(res) |> dplyr::pull("cohort_name"),
    strata_name = c("overall", "age_group", "sex", "age_group &&& sex")
  ) |>
    dplyr::left_join(
      dplyr::tibble(
        strata_name = c(
          "age_group", "age_group", "sex", "sex", "age_group &&& sex",
          "age_group &&& sex", "age_group &&& sex", "age_group &&& sex",
          "overall"
        ),
        strata_level = c(
          "<40", ">=40", "Male", "Female", "<40 &&& Female", "<40 &&& Male",
          ">=40 &&& Female", ">=40 &&& Male", "overall"
        )
      ),
      by = "strata_name", relationship = "many-to-many"
    )
  expect_identical(
    nrow(result),
    result |>
      dplyr::inner_join(
        x,
        by = c("group_level", "strata_name", "strata_level")
      ) |>
      nrow()
  )
  expect_true(any(grepl("Indication on index date", result$variable_name)))
  expect_true(any(grepl("Indication from 7 days before to the index date", result$variable_name)))
  expect_true(any(grepl("Indication from 30 days before to the index date", result$variable_name)))
  expect_true(any(grepl("Indication any time before or on index date", result$variable_name)))

  expect_identical(
    "summarise_indication", unique(settings(result)$result_type)
  )

  mockDisconnect(cdm = cdm)
})

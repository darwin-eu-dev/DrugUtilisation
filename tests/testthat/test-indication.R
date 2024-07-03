
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

  cdm <-
    mockDrugUtilisation(
      connectionDetails,
      cohort1 = targetCohortName,
      cohort2 = indicationCohortName,
      condition_occurrence = condition_occurrence,
      observation_period = observationPeriod
    )
  #check it works without cdm object specified

  expect_no_error(cdm[["cohort1"]] %>%
    addIndication(indicationCohortName = "cohort2", indicationGap = 0,
      unknownIndicationTable = NULL
    ))

  # check for indication 0
  res0 <- cdm[["cohort1"]] %>%
    addIndication(
      indicationCohortName = "cohort2", indicationGap = 0,
      unknownIndicationTable = NULL
    )
  expect_true(length(setdiff(colnames(res0), colnames(cdm[["cohort1"]]))) == 3)
  expect_true(all(
    c("indication_gap_0_asthma", "indication_gap_0_covid",
      "indication_gap_0_none") %in%
      setdiff(colnames(res0), colnames(cdm[["cohort1"]]))
  ))

  res0 <- res0 %>% indicationToStrata()

  expect_true(
    res0 %>%
      dplyr::filter(.data$subject_id == 3) %>%
      dplyr::pull("indication_gap_0") == "Asthma"
  )
  expect_true(
    all(res0 %>%
          dplyr::filter(.data$subject_id != 3) %>%
          dplyr::pull("indication_gap_0") == "None")
  )

  # check for indication 1
  res1 <- cdm[["cohort1"]] %>%
    addIndication(
      indicationCohortName = "cohort2", indicationGap = 1,
      unknownIndicationTable = NULL
    )
  expect_true(length(setdiff(colnames(res1), colnames(cdm[["cohort1"]]))) == 3)
  expect_true(all(
    c("indication_gap_1_asthma", "indication_gap_1_covid",
      "indication_gap_1_none") %in%
      setdiff(colnames(res1), colnames(cdm[["cohort1"]]))
  ))

  res1 <- res1 %>% indicationToStrata()

  expect_true(
    res1 %>%
      dplyr::filter(.data$subject_id == 3) %>%
      dplyr::pull("indication_gap_1") == "Asthma"
  )
  expect_true(
    all(res1 %>%
          dplyr::filter(.data$subject_id != 3) %>%
          dplyr::pull("indication_gap_1") == "None")
  )

  # check for indication 2
  res2 <- cdm[["cohort1"]] %>%
    addIndication(
      indicationCohortName = "cohort2", indicationGap = 2,
      unknownIndicationTable = NULL
    )

  expect_true(length(setdiff(colnames(res2), colnames(cdm[["cohort1"]]))) == 3)
  expect_true(all(
    c("indication_gap_2_asthma", "indication_gap_2_covid",
      "indication_gap_2_none") %in%
      setdiff(colnames(res2), colnames(cdm[["cohort1"]]))
  ))

  res2 <- res2 %>% indicationToStrata()

  expect_true(all(
    res2 %>%
      dplyr::arrange(
        .data$subject_id,
        .data$cohort_start_date,
        .data$cohort_definition_id,
        .data$cohort_end_date
      ) %>%
      dplyr::pull("indication_gap_2")==
    c("Asthma", "None", "None", "Asthma")
  ))

  # check for all indication Gap
  resinf <- cdm[["cohort1"]] %>%
    addIndication(
      indicationCohortName = "cohort2", indicationGap = Inf,
      unknownIndicationTable = NULL
    )
  expect_true(length(setdiff(colnames(resinf), colnames(cdm[["cohort1"]]))) == 3)
  expect_true(all(
    c("indication_gap_inf_asthma", "indication_gap_inf_covid",
      "indication_gap_inf_none") %in%
      setdiff(colnames(resinf), colnames(cdm[["cohort1"]]))
  ))

  resinf <- resinf %>% indicationToStrata()

  expect_true(any(identical(
    resinf %>%
      dplyr::arrange(.data$subject_id, .data$cohort_start_date) %>%
      dplyr::pull("indication_gap_inf"),
    c("Asthma", "Covid and Asthma", "None", "Asthma")
  ),
  identical(
    resinf %>%
      dplyr::arrange(.data$subject_id, .data$cohort_start_date) %>%
      dplyr::pull("indication_gap_inf"),
    c("Asthma", "Asthma and Covid", "None", "Asthma")
  )
  ))

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
  cdm <-mockDrugUtilisation(
    connectionDetails, cohort1 = targetCohortName,
    cohort2 = indicationCohortName, condition_occurrence = condition_occurrence,
    observation_period = observationPeriod
  )

  # check for indication 0
  res0 <- cdm[["cohort1"]] %>%
    addIndication(
      indicationCohortName = "cohort2", indicationGap = 0,
      unknownIndicationTable = "condition_occurrence"
    )
  expect_true(length(setdiff(colnames(res0), colnames(cdm[["cohort1"]]))) == 4)
  expect_true(all(
    c("indication_gap_0_asthma", "indication_gap_0_covid",
      "indication_gap_0_none", "indication_gap_0_unknown") %in%
      setdiff(colnames(res0), colnames(cdm[["cohort1"]]))
  ))
  res0 <- res0 %>% indicationToStrata()
  expect_true(identical(
    res0 %>%
      dplyr::arrange(.data$subject_id, .data$cohort_start_date) %>%
      dplyr::pull("indication_gap_0"),
    c("None", "None", "None", "Asthma")
  ))

  # check for indication 1
  res1 <- cdm[["cohort1"]] %>%
    addIndication(
      indicationCohortName = "cohort2", indicationGap = 1,
      unknownIndicationTable = "condition_occurrence"
    )
  expect_true(length(setdiff(colnames(res1), colnames(cdm[["cohort1"]]))) == 4)
  expect_true(all(
    c("indication_gap_1_asthma", "indication_gap_1_covid",
      "indication_gap_1_none", "indication_gap_1_unknown") %in%
      setdiff(colnames(res1), colnames(cdm[["cohort1"]]))
  ))
  res1 <- res1 %>% indicationToStrata()
  expect_true(identical(
    res1 %>%
      dplyr::arrange(.data$subject_id, .data$cohort_start_date) %>%
      dplyr::pull("indication_gap_1"),
    c("None", "Unknown", "None", "Asthma")
  ))

  # check for indication 6
  res6 <- cdm[["cohort1"]] %>%
    addIndication(
      indicationCohortName = "cohort2", indicationGap = 6,
      unknownIndicationTable = "condition_occurrence"
    )
  expect_true(length(setdiff(colnames(res6), colnames(cdm[["cohort1"]]))) == 4)
  expect_true(all(
    c("indication_gap_6_asthma", "indication_gap_6_covid",
      "indication_gap_6_none", "indication_gap_6_unknown") %in%
      setdiff(colnames(res6), colnames(cdm[["cohort1"]]))
  ))
  res6 <- res6 %>% indicationToStrata()
  expect_true(identical(
    res6 %>%
      dplyr::arrange(.data$subject_id, .data$cohort_start_date) %>%
      dplyr::pull("indication_gap_6"),
    c("Asthma", "Unknown", "None", "Asthma")
  ))

  # check all gaps simultaniously
  res016 <- cdm[["cohort1"]] %>%
    addIndication(
      indicationCohortName = "cohort2", indicationGap = c(0, 1, 6),
      unknownIndicationTable = "condition_occurrence"
    )
  expect_true(length(setdiff(colnames(res016), colnames(cdm[["cohort1"]]))) == 12)
  expect_true(all(
    c("indication_gap_0_asthma", "indication_gap_0_covid",
      "indication_gap_0_none", "indication_gap_0_unknown",
      "indication_gap_1_asthma", "indication_gap_1_covid",
      "indication_gap_1_none", "indication_gap_1_unknown",
      "indication_gap_6_asthma", "indication_gap_6_covid",
      "indication_gap_6_none", "indication_gap_6_unknown") %in%
      setdiff(colnames(res016), colnames(cdm[["cohort1"]]))
  ))
  res016 <- res016 %>% indicationToStrata()
  expect_true(identical(
    res016 %>%
      dplyr::arrange(.data$subject_id, .data$cohort_start_date) %>%
      dplyr::pull("indication_gap_0"),
    res0 %>%
      dplyr::arrange(.data$subject_id, .data$cohort_start_date) %>%
      dplyr::pull("indication_gap_0")
  ))
  expect_true(identical(
    res016 %>%
      dplyr::arrange(.data$subject_id, .data$cohort_start_date) %>%
      dplyr::pull("indication_gap_1"),
    res1 %>%
      dplyr::arrange(.data$subject_id, .data$cohort_start_date) %>%
      dplyr::pull("indication_gap_1")
  ))
  expect_true(identical(
    res016 %>%
      dplyr::arrange(.data$subject_id, .data$cohort_start_date) %>%
      dplyr::pull("indication_gap_6"),
    res6 %>%
      dplyr::arrange(.data$subject_id, .data$cohort_start_date) %>%
      dplyr::pull("indication_gap_6")
  ))
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
      connectionDetails,
      cohort1 = targetCohortName,
      cohort2 = indicationCohortName,
      condition_occurrence = condition_occurrence,
      observation_period = observationPeriod
    )

  # original
  res012inf <- cdm[["cohort1"]] %>%
    addIndication(
      indicationCohortName = "cohort2",
      indicationGap = c(0, 1, 2, Inf), unknownIndicationTable = NULL
    ) %>% dplyr::relocate(indication_gap_inf_asthma) %>%
    indicationToStrata()
  expect_equal(
    sort(setdiff(colnames(res012inf), colnames(cdm[["cohort1"]]))),
    sort(c(
      "indication_gap_0", "indication_gap_1", "indication_gap_2",
      "indication_gap_inf"
    ))
  )

  # change indicationDate
  cdm[["cohort1"]] <- cdm[["cohort1"]] %>%
    dplyr::rename("start_date" = "cohort_start_date")
  res012infS <- cdm[["cohort1"]] %>%
    addIndication(
      indicationCohortName = "cohort2",
      indicationGap = c(0, 1, 2, Inf), unknownIndicationTable = NULL,
      indicationDate = "start_date"
    ) %>% dplyr::relocate(indication_gap_inf_asthma) %>%
    indicationToStrata()
  expect_equal(
    sort(setdiff(colnames(res012infS), colnames(cdm[["cohort1"]]))),
    sort(c(
      "indication_gap_0", "indication_gap_1", "indication_gap_2",
      "indication_gap_inf"
    ))
  )
  expect_true(
    setdiff(colnames(res012inf), colnames(res012infS)) == "cohort_start_date"
  )
  expect_true(identical(
    res012inf %>%
      dplyr::arrange(.data$subject_id, .data$cohort_start_date) %>%
      dplyr::pull("indication_gap_0"),
    res012infS %>%
      dplyr::arrange(.data$subject_id, .data$start_date) %>%
      dplyr::pull("indication_gap_0")
  ))
  expect_true(identical(
    res012inf %>%
      dplyr::arrange(.data$subject_id, .data$cohort_start_date) %>%
      dplyr::pull("indication_gap_1"),
    res012infS %>%
      dplyr::arrange(.data$subject_id, .data$start_date) %>%
      dplyr::pull("indication_gap_1")
  ))
  expect_true(identical(
    res012inf %>%
      dplyr::arrange(.data$subject_id, .data$cohort_start_date) %>%
      dplyr::pull("indication_gap_2"),
    res012infS %>%
      dplyr::arrange(.data$subject_id, .data$start_date) %>%
      dplyr::pull("indication_gap_2")
  ))
  expect_true(identical(
    res012inf %>%
      dplyr::arrange(.data$subject_id, .data$cohort_start_date) %>%
      dplyr::pull("indication_gap_inf"),
    res012infS %>%
      dplyr::arrange(.data$subject_id, .data$start_date) %>%
      dplyr::pull("indication_gap_inf")
  ))

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
      connectionDetails,
      cohort1 = targetCohortName,
      cohort2 = indicationCohortName,
      condition_occurrence = condition_occurrence,
      observation_period = observationPeriod
    )

  cdm[["cohort1new"]] <- cdm[["cohort1"]] %>%
    addIndication(
      indicationCohortName = "cohort2",
      indicationGap = c(0, 1, 2, Inf), unknownIndicationTable = NULL
    )
  expect_identical(
    sort(names(attributes(cdm[["cohort1"]]))),
    sort(names(attributes(cdm[["cohort1new"]])))
  )

  cdm[["cohort1new"]] <- cdm[["cohort1new"]] %>% indicationToStrata()
  expect_identical(
    sort(names(attributes(cdm[["cohort1"]]))),
    sort(names(attributes(cdm[["cohort1new"]])))
  )
  expect_identical(
    class(cdm[["cohort1"]])[class(cdm[["cohort1"]]) != "GeneratedCohortSet"],
    class(cdm[["cohort1new"]])[class(cdm[["cohort1new"]]) != "GeneratedCohortSet"]
  )

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
      connectionDetails,
      cohort1 = targetCohortName,
      cohort2 = indicationCohortName,
      condition_occurrence = condition_occurrence,
      observation_period = observationPeriod
    )

  res <- cdm[["cohort1"]] %>%
    addIndication(
      indicationCohortName = "cohort2", indicationGap = c(0, 7, 30, Inf),
      unknownIndicationTable = "condition_occurrence"
    )

  result <- summariseIndication(res)

  expect_true(inherits(result, "summarised_result"))
  expect_true(any(grepl("Indication on index date", result$variable_name)))
  expect_true(any(grepl("Indication during prior 7 days", result$variable_name)))
  expect_true(any(grepl("Indication during prior 30 days", result$variable_name)))
  expect_true(any(grepl("Indication any time prior", result$variable_name)))

  res <- res %>%
    PatientProfiles::addAge(
      ageGroup = list("<40" = c(0, 39), ">=40" = c(40, 150))
    ) %>%
    PatientProfiles::addSex()

  result <- summariseIndication(
    res, strata = list("age_group", "sex", c("age_group", "sex"))
  )

  expect_true(inherits(result, "summarised_result"))
  x <- tidyr::expand_grid(
    group_level = omopgenerics::settings(res) %>% dplyr::pull("cohort_name"),
    strata_name = c("overall", "age_group", "sex", "age_group &&& sex")
  ) %>%
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
    result %>%
      dplyr::inner_join(
        x, by = c("group_level", "strata_name", "strata_level")
      ) %>%
      nrow()
  )
  expect_true(any(grepl("Indication on index date", result$variable_name)))
  expect_true(any(grepl("Indication during prior 7 days", result$variable_name)))
  expect_true(any(grepl("Indication during prior 30 days", result$variable_name)))
  expect_true(any(grepl("Indication any time prior", result$variable_name)))

  expect_identical(
    "summarised_indication", unique(settings(result)$result_type))

})

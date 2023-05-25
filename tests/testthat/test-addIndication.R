
test_that("test case single indication", {
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
    cohort_definition_id = c(1, 1, 2, 3, 1),
    subject_id = c(1, 3, 1, 2, 1),
    cohort_start_date = as.Date(
      c(
        "2019-12-30",
        "2020-01-01",
        "2020-05-25",
        "2020-01-01",
        "2020-05-25"
      )
    ),
    cohort_end_date = as.Date(
      c(
        "2019-12-30",
        "2020-01-01",
        "2020-05-25",
        "2020-01-01",
        "2020-05-25"
      )
    )
  )
  attr(indicationCohortName, "cohort_set") <- dplyr::tibble(
    cohort_definition_id = c(1, 2),
    cohort_name = c("asthma", "covid")
  )
  condition_occurrence <- dplyr::tibble(
    person_id = 1,
    condition_start_date = as.Date("2020-05-31"),
    condition_end_date = as.Date("2020-05-31")
  )

  cdm <-
    mockDrugUtilisation(
      connectionDetails,
      cohort1 = targetCohortName,
      cohort2 = indicationCohortName,
      condition_occurrence = condition_occurrence
    )

  # check for indication 0
  res0 <- cdm$cohort1 %>%
    addIndication(
      cdm = cdm, indicationCohortName = "cohort2", indicationGap = 0,
      unknownIndicationTable = NULL
    )
  expect_true(
    setdiff(colnames(res0), colnames(cdm$cohort1)) == "indication_gap_0"
  )
  expect_true(
    res0 %>%
      dplyr::filter(.data$subject_id == 3) %>%
      dplyr::pull("indication_gap_0") == "asthma"
  )
  expect_true(
    all(res0 %>%
          dplyr::filter(.data$subject_id != 3) %>%
          dplyr::pull("indication_gap_0") == "no indication")
  )

  # check for indication 1
  res1 <- cdm$cohort1 %>%
    addIndication(
      cdm = cdm, indicationCohortName = "cohort2", indicationGap = 1,
      unknownIndicationTable = NULL
    )
  expect_true(
    setdiff(colnames(res1), colnames(cdm$cohort1)) == "indication_gap_1"
  )
  expect_true(
    res1 %>%
      dplyr::filter(.data$subject_id == 3) %>%
      dplyr::pull("indication_gap_1") == "asthma"
  )
  expect_true(
    all(res1 %>%
          dplyr::filter(.data$subject_id != 3) %>%
          dplyr::pull("indication_gap_1") == "no indication")
  )

  # check for indication 2
  res2 <- cdm$cohort1 %>%
    addIndication(
      cdm = cdm, indicationCohortName = "cohort2", indicationGap = 2,
      unknownIndicationTable = NULL
    )
  expect_true(
    setdiff(colnames(res2), colnames(cdm$cohort1)) == "indication_gap_2"
  )
  expect_true(identical(
    res2 %>%
      dplyr::arrange(.data$subject_id, .data$cohort_start_date) %>%
      dplyr::pull("indication_gap_2"),
    c("asthma", "no indication", "no indication", "asthma")
  ))

  # check for all indication Gap
  resinf <- cdm$cohort1 %>%
    addIndication(
      cdm = cdm, indicationCohortName = "cohort2", indicationGap = Inf,
      unknownIndicationTable = NULL
    )
  expect_true(
    setdiff(colnames(resinf), colnames(cdm$cohort1)) == "indication_gap_inf"
  )
  expect_true(identical(
    resinf %>%
      dplyr::arrange(.data$subject_id, .data$cohort_start_date) %>%
      dplyr::pull("indication_gap_inf"),
    c("asthma", "asthma&&covid", "no indication", "asthma")
  ))

})

test_that("test case single indication with unknown indication table", {
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
    cohort_definition_id = c(1, 1, 2, 3, 1),
    subject_id = c(1, 3, 1, 2, 1),
    cohort_start_date = as.Date(c(
      "2019-12-30", "2020-01-01", "2020-05-25", "2020-01-01", "2020-05-25"
    )),
    cohort_end_date = as.Date(c(
      "2019-12-30", "2020-01-01", "2020-05-25", "2020-01-01", "2020-05-25"
    ))
  )
  condition_occurrence <- dplyr::tibble(
    person_id = 1,
    condition_start_date = as.Date("2020-05-31"),
    condition_end_date = as.Date("2020-05-31")
  )
  attr(indicationCohortName, "cohort_set") <- dplyr::tibble(
    cohort_definition_id = c(1, 2),
    cohort_name = c("asthma", "covid")
  )

  cdm <-mockDrugUtilisation(
    connectionDetails, cohort1 = targetCohortName,
    cohort2 = indicationCohortName, condition_occurrence = condition_occurrence
  )

  # check for indication 0
  res0 <- cdm$cohort1 %>%
    addIndication(
      cdm = cdm, indicationCohortName = "cohort2", indicationGap = 0,
      unknownIndicationTable = "condition_occurrence"
    )
  expect_true(
    setdiff(colnames(res0), colnames(cdm$cohort1)) == "indication_gap_0"
  )
  expect_true(identical(
    res0 %>%
      dplyr::arrange(.data$subject_id, .data$cohort_start_date) %>%
      dplyr::pull("indication_gap_0"),
    c("no indication", "no indication", "no indication", "asthma")
  ))

  # check for indication 1
  res1 <- cdm$cohort1 %>%
    addIndication(
      cdm = cdm, indicationCohortName = "cohort2", indicationGap = 1,
      unknownIndicationTable = "condition_occurrence"
    )
  expect_true(
    setdiff(colnames(res1), colnames(cdm$cohort1)) == "indication_gap_1"
  )
  expect_true(identical(
    res1 %>%
      dplyr::arrange(.data$subject_id, .data$cohort_start_date) %>%
      dplyr::pull("indication_gap_1"),
    c("no indication", "unknown indication", "no indication", "asthma")
  ))

  # check for indication 6
  res6 <- cdm$cohort1 %>%
    addIndication(
      cdm = cdm, indicationCohortName = "cohort2", indicationGap = 6,
      unknownIndicationTable = "condition_occurrence"
    )
  expect_true(
    setdiff(colnames(res6), colnames(cdm$cohort1)) == "indication_gap_6"
  )
  expect_true(identical(
    res6 %>%
      dplyr::arrange(.data$subject_id, .data$cohort_start_date) %>%
      dplyr::pull("indication_gap_6"),
    c("asthma", "unknown indication", "no indication", "asthma")
  ))

  # check all gaps simultaniously
  res016 <- cdm$cohort1 %>%
    addIndication(
      cdm = cdm, indicationCohortName = "cohort2", indicationGap = c(0, 1, 6),
      unknownIndicationTable = "condition_occurrence"
    )
  expect_true(all(
    setdiff(colnames(res016), colnames(cdm$cohort1)) ==
      c("indication_gap_0", "indication_gap_1", "indication_gap_6")
  ))
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
    cohort_definition_id = c(1, 1, 2, 3, 1),
    subject_id = c(1, 3, 1, 2, 1),
    cohort_start_date = as.Date(
      c(
        "2019-12-30",
        "2020-01-01",
        "2020-05-25",
        "2020-01-01",
        "2020-05-25"
      )
    ),
    cohort_end_date = as.Date(
      c(
        "2019-12-30",
        "2020-01-01",
        "2020-05-25",
        "2020-01-01",
        "2020-05-25"
      )
    )
  )
  attr(indicationCohortName, "cohort_set") <- dplyr::tibble(
    cohort_definition_id = c(1, 2),
    cohort_name = c("asthma", "covid")
  )
  condition_occurrence <- dplyr::tibble(
    person_id = 1,
    condition_start_date = as.Date("2020-05-31"),
    condition_end_date = as.Date("2020-05-31")
  )

  cdm <-
    mockDrugUtilisation(
      connectionDetails,
      cohort1 = targetCohortName,
      cohort2 = indicationCohortName,
      condition_occurrence = condition_occurrence
    )

  # original
  res012inf <- cdm$cohort1 %>%
    addIndication(
      cdm = cdm, indicationCohortName = "cohort2",
      indicationGap = c(0, 1, 2, Inf), unknownIndicationTable = NULL
    )
  expect_true(all(
    setdiff(colnames(res012inf), colnames(cdm$cohort1)) == c(
      "indication_gap_0", "indication_gap_1", "indication_gap_2",
      "indication_gap_inf"
    )
  ))

  # change indicationDate
  cdm$cohort1 <- cdm$cohort1 %>%
    dplyr::rename("start_date" = "cohort_start_date")
  res012infS <- cdm$cohort1 %>%
    addIndication(
      cdm = cdm, indicationCohortName = "cohort2",
      indicationGap = c(0, 1, 2, Inf), unknownIndicationTable = NULL,
      indicationDate = "start_date"
    )
  expect_true(all(
    setdiff(colnames(res012infS), colnames(cdm$cohort1)) == c(
      "indication_gap_0", "indication_gap_1", "indication_gap_2",
      "indication_gap_inf"
    )
  ))
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

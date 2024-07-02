test_that("tableIndication works", {
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

  # default
  default <- tableIndication(result)
  expect_true("gt_tbl" %in% class(default))
  expect_true(all(colnames(default$`_data`) == c(
    'Database name', 'Variable name', 'Indication', '[header]Cohort name\n[header_level]Cohort 1', '[header]Cohort name\n[header_level]Cohort 2'
  )))
  expect_true(all(default$`_data`$`Database name` == c(
    'DUS MOCK', '', '', '', 'DUS MOCK', '', '', '', 'DUS MOCK', '', '', '', 'DUS MOCK', '', '', ''
  )))

  tib <- tableIndication(result, header = "variable", groupColumn = "cdm_name", type = "tibble")
  expect_true(nrow(tib) == 2)
  expect_true(all(colnames(tib) == c(
    'Database name', 'Cohort name', '[header_level]Indication on index date\n[header_level]Asthma',
    '[header_level]Indication on index date\n[header_level]Covid', '[header_level]Indication on index date\n[header_level]None',
    '[header_level]Indication on index date\n[header_level]Unknown', '[header_level]Indication during prior 7 days\n[header_level]Covid',
    '[header_level]Indication during prior 7 days\n[header_level]Asthma', '[header_level]Indication during prior 7 days\n[header_level]None',
    '[header_level]Indication during prior 7 days\n[header_level]Unknown', '[header_level]Indication during prior 30 days\n[header_level]Asthma',
    '[header_level]Indication during prior 30 days\n[header_level]Covid', '[header_level]Indication during prior 30 days\n[header_level]None',
    '[header_level]Indication during prior 30 days\n[header_level]Unknown', '[header_level]Indication any time prior\n[header_level]Asthma',
    '[header_level]Indication any time prior\n[header_level]Covid', '[header_level]Indication any time prior\n[header_level]None',
    '[header_level]Indication any time prior\n[header_level]Unknown'
  )))

  # strata
  res <- res %>%
    PatientProfiles::addAge(
      ageGroup = list("<40" = c(0, 39), ">=40" = c(40, 150))
    ) %>%
    PatientProfiles::addSex()

  result <- summariseIndication(
    res, strata = list("age_group", "sex", c("age_group", "sex"))
  )

  fx <- tableIndication(result, cdmName = FALSE, cohortName = FALSE, type = "flextable")
  expect_true("flextable" %in% class(fx))
  expect_true(all(colnames(fx$body$dataset) == c(
    'Variable name', 'Age group', 'Sex', 'Indication', 'Estimate value', 'Additional name', 'Additional level'
  )))
  expect_true(all(fx$body$dataset$`Variable name` |> levels() == c(
    "Indication any time prior", "Indication during prior 30 days", "Indication during prior 7 days", "Indication on index date"
  )))

  # expected errors
  expect_error(tableIndication(result, header = "variable"))
  expect_error(tableIndication(result, groupColumn = "cdm_name", cdmName = FALSE))
})

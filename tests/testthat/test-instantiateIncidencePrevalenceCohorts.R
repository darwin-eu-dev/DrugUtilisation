
test_that("mock db: checks on working example", {
  #checks for Era generation
  cdm <- mockDrugUtilisation(seed=1)
  incidencePrevalenceCohortName <- "test"
  out_put_1 <- instantiateIncidencePrevalenceCohorts(
    cdm,
    conceptIds = 1,
    gapEra = 834,
    incidencePrevalenceCohortName,
    cohortDefinitionId = NULL
  )

  expect_true(
    out_put_1 %>% dplyr::summarise(max(cohort_start_date, na.rm = TRUE)) %>% dplyr::pull() == cdm$drug_exposure %>%
      dplyr::filter(drug_concept_id == 1) %>% dplyr::summarise(max(drug_exposure_start_date, na.rm = TRUE)) %>% dplyr::pull()
  )

  expect_true(
    out_put_1 %>% dplyr::summarise(max(cohort_start_date, na.rm = TRUE)) %>% dplyr::pull() == cdm$drug_exposure %>%
      dplyr::filter(drug_concept_id == 1) %>% dplyr::summarise(max(drug_exposure_start_date, na.rm = TRUE)) %>% dplyr::pull()
  )

  expect_true(
    out_put_1 %>% dplyr::summarise(min(cohort_start_date, na.rm = TRUE)) %>% dplyr::pull() == cdm$drug_exposure %>%
      dplyr::filter(drug_concept_id == 1) %>% dplyr::summarise(min(drug_exposure_start_date, na.rm = TRUE)) %>% dplyr::pull()
  )

  expect_true(out_put_1 %>% dplyr::filter(subject_id != 1) %>% dplyr::count() %>% dplyr::pull() == 0)

  expect_true(out_put_1 %>% dplyr::filter(subject_id == 1) %>% dplyr::count() %>% dplyr::pull() != 0)

  out_put_2 <- instantiateIncidencePrevalenceCohorts(
    cdm,
    conceptIds = 1,
    gapEra = 835,
    incidencePrevalenceCohortName,
    cohortDefinitionId = NULL
  )

  expect_true(
    out_put_2 %>% dplyr::summarise(max(cohort_start_date, na.rm = TRUE)) %>% dplyr::pull() == cdm$drug_exposure %>%
      dplyr::filter(drug_concept_id == 1) %>% dplyr::summarise(max(drug_exposure_start_date, na.rm = TRUE)) %>% dplyr::pull()
  )

  out_put_3 <- instantiateIncidencePrevalenceCohorts(
    cdm,
    conceptIds = 1,
    gapEra = 2971,
    incidencePrevalenceCohortName,
    cohortDefinitionId = NULL
  )

  expect_true(
    out_put_3 %>% dplyr::summarise(max(cohort_start_date, na.rm = TRUE)) %>% dplyr::pull() == cdm$drug_exposure %>%
      dplyr::filter(drug_concept_id == 1) %>% dplyr::summarise(max(drug_exposure_start_date, na.rm = TRUE)) %>% dplyr::pull()
  )

  out_put_4 <- instantiateIncidencePrevalenceCohorts(
    cdm,
    conceptIds = 1,
    gapEra = 2972,
    incidencePrevalenceCohortName,
    cohortDefinitionId = NULL
  )

  expect_true(
    out_put_4 %>% dplyr::summarise(max(cohort_start_date, na.rm = TRUE)) %>% dplyr::pull() != cdm$drug_exposure %>%
      dplyr::filter(drug_concept_id == 1) %>% dplyr::summarise(max(drug_exposure_start_date, na.rm = TRUE)) %>% dplyr::pull()
  )

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})



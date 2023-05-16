test_that("test inputs", {
  cdm <- mockDrugUtilisation()
  expect_error(generateDrugUtilisationCohortSet())
  expect_error(generateDrugUtilisationCohortSet(cdm = cdm))
  expect_error(generateDrugUtilisationCohortSet(cdm, "dus", 1))
  expect_error(generateDrugUtilisationCohortSet(cdm, "dus", list(1)))
  x <- generateDrugUtilisationCohortSet(cdm, "dus", list(albuterol = 1))
  expect_true(all(colnames(x$dus) == c(
    "cohort_definition_id", "subject_id", "cohort_start_date", "cohort_end_date"
  )))
  expect_error(generateDrugUtilisationCohortSet(
    cdm, "dus", list(albuterol = 1), cohortDateRange = 1
  ))
  expect_error(generateDrugUtilisationCohortSet(
    cdm, "dus", list(albuterol = 1), cohortDateRange = "2020-01-05"
  ))
  expect_error(generateDrugUtilisationCohortSet(
    cdm, "dus", list(albuterol = 1), summariseMode = "2020-01-05"
  ))
  xx <- generateDrugUtilisationCohortSet(
    cdm, "dus", list(albuterol = 1), summariseMode = "FixedTime"
  )
  expect_error(generateDrugUtilisationCohortSet(
    cdm, "dus", list(albuterol = 1), summariseMode = "FixedTime",
    fixedTime = "1"
  ))
  expect_error(generateDrugUtilisationCohortSet(
    cdm, "dus", list(albuterol = 1), daysPriorHistory = "7"
  ))
  expect_error(generateDrugUtilisationCohortSet(
    cdm, "dus", list(albuterol = 1), gapEra = "7"
  ))
  expect_error(generateDrugUtilisationCohortSet(
    cdm, "dus", list(albuterol = 1), imputeDuration = "7"
  ))
  expect_error(generateDrugUtilisationCohortSet(
    cdm, "dus", list(albuterol = 1), imputeDuration = -7
  ))
  expect_error(generateDrugUtilisationCohortSet(
    cdm, "dus", list(albuterol = 1), durationRange = -7
  ))
})

test_that("class", {
  cdm <- mockDrugUtilisation()
  cdm <- generateDrugUtilisationCohortSet(cdm, "dus", list(albuterol = 1))
  expect_true("GeneratedCohortSet" %in% class(cdm$dus))
})

test_that("basic functionality drug_conceptId", {
  #concept1 <- system.file(package = "DrugUtilisation", "concept1.json")
  cdm <- mockDrugUtilisation(seed = 1)
  cdm1 <- generateDrugUtilisationCohortSet(
    cdm, "dus", list(albuterol = 1),
    gapEra = 834,
    daysPriorHistory = NULL
  )
  expect_true(
      cdm1$dus %>%
        dplyr::summarise(max(cohort_start_date, na.rm = TRUE)) %>%
        dplyr::pull() ==
        cdm$drug_exposure %>%
        dplyr::filter(drug_concept_id == 1) %>%
        dplyr::summarise(max(drug_exposure_start_date, na.rm = TRUE)) %>%
        dplyr::pull()
    )
  expect_true(
    cdm1$dus %>%
      dplyr::summarise(min(cohort_start_date, na.rm = TRUE)) %>%
      dplyr::pull() ==
      cdm$drug_exposure %>%
      dplyr::filter(drug_concept_id == 1) %>%
      dplyr::summarise(min(drug_exposure_start_date, na.rm = TRUE)) %>%
      dplyr::pull()
  )
  expect_true(
    cdm1$dus %>%
      dplyr::summarise(max(cohort_end_date, na.rm = TRUE)) %>%
      dplyr::pull() ==
      cdm$drug_exposure %>%
      dplyr::filter(drug_concept_id == 1) %>%
      dplyr::summarise(max(drug_exposure_end_date, na.rm = TRUE)) %>%
      dplyr::pull()
  )
  expect_true(
    cdm1$dus %>%
      dplyr::summarise(min(cohort_end_date, na.rm = TRUE)) %>%
      dplyr::pull() ==
      cdm$drug_exposure %>%
      dplyr::filter(drug_concept_id == 1) %>%
      dplyr::summarise(min(drug_exposure_end_date, na.rm = TRUE)) %>%
      dplyr::pull()
  )
  expect_true(
    cdm1$dus %>%
      dplyr::filter(subject_id != 1) %>%
      dplyr::count() %>%
      dplyr::pull() == 0
  )
  expect_true(
    cdm1$dus %>%
      dplyr::filter(subject_id == 1) %>%
      dplyr::count() %>%
      dplyr::pull() != 0
  )

  cdm2 <- generateDrugUtilisationCohortSet(
    cdm, "dus", list(albuterol = 1),
    gapEra = 835,
    daysPriorHistory = NULL
  )
  expect_true(cdm2$dus %>% dplyr::tally() %>% dplyr::pull("n") == 3)

  cdm3 <- generateDrugUtilisationCohortSet(
    cdm, "dus", list(albuterol = 1),
    gapEra = 836,
    daysPriorHistory = NULL
  )
  expect_true(cdm3$dus %>% dplyr::tally() %>% dplyr::pull("n") == 2)

  cdm11 <- generateDrugUtilisationCohortSet(
    cdm, "dus", list(albuterol = 1),
    gapEra = 834,
    daysPriorHistory = NULL
  )
  expect_equal(cdm1$dus %>% dplyr::collect(), cdm11$dus %>% dplyr::collect())

  cdm4 <- generateDrugUtilisationCohortSet(
    cdm, "dus", list(albuterol = 1),
    gapEra = 2972
  )
  expect_true(cdm4$dus %>% dplyr::tally() %>% dplyr::pull("n") == 1)

  cdm4 <- generateDrugUtilisationCohortSet(
    cdm, "dus", list(albuterol = 1), gapEra = 2973
  )
  expect_true(cdm4$dus %>% dplyr::tally() %>% dplyr::pull("n") == 0)

  cdm5 <- generateDrugUtilisationCohortSet(
    cdm, "dus", list(albuterol = 1),
    summariseMode = "FirstEra",
    gapEra = 834,
    daysPriorHistory = NULL
  )
  expect_true(cdm5$dus %>% dplyr::tally() %>% dplyr::pull("n") == 1)

  cdm6 <- generateDrugUtilisationCohortSet(
    cdm, "dus", list(albuterol = 1),
    summariseMode = "FixedTime",
    fixedTime = 365,
    gapEra = 834,
    daysPriorHistory = NULL
  )
  expect_true(cdm6$dus %>% dplyr::tally() %>% dplyr::pull("n") == 1)

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)

})

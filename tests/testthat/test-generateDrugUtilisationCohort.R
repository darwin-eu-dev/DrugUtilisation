test_that("test inputs", {
  cdm <- mockDrugUtilisation()
  expect_error(generateDrugUtilisationCohort())
  expect_error(generateDrugUtilisationCohort(cdm = cdm))
  x <- generateDrugUtilisationCohort(
    cdm = cdm, ingredientConceptId = 1
  )
  expect_true(all(colnames(x) == c(
    "cohort_definition_id", "subject_id", "cohort_start_date", "cohort_end_date"
  )))
  expect_error(generateDrugUtilisationCohort(
    cdm = cdm, ingredientConceptId = "1"
  ))
  expect_error(generateDrugUtilisationCohort(
    cdm = cdm, ingredientConceptId = 1, conceptSetPath = here::here()
  ))
  expect_error(generateDrugUtilisationCohort(
    cdm = cdm, ingredientConceptId = 1, conceptSetPath = here::here("inst")
  ))
  expect_error(generateDrugUtilisationCohort(
    cdm = cdm, ingredientConceptId = 1, studyStartDate = 1
  ))
  expect_error(generateDrugUtilisationCohort(
    cdm = cdm, ingredientConceptId = 1, studyEndDate = "2020-01-05"
  ))
  expect_error(generateDrugUtilisationCohort(
    cdm = cdm, ingredientConceptId = 1, summariseMode = "2020-01-05"
  ))
  xx <- generateDrugUtilisationCohort(
    cdm = cdm, ingredientConceptId = 1, summariseMode = "FixedTime"
  )
  expect_error(generateDrugUtilisationCohort(
    cdm = cdm, ingredientConceptId = 1, summariseMode = "FixedTime",
    fixedTime = "1"
  ))
  expect_error(generateDrugUtilisationCohort(
    cdm = cdm, ingredientConceptId = 1, daysPriorHistory = "7"
  ))
  expect_error(generateDrugUtilisationCohort(
    cdm = cdm, ingredientConceptId = 1, gapEra = "7"
  ))
  expect_error(generateDrugUtilisationCohort(
    cdm = cdm, ingredientConceptId = 1, imputeDuration = "7"
  ))
  expect_error(generateDrugUtilisationCohort(
    cdm = cdm, ingredientConceptId = 1, imputeDuration = -7
  ))
  expect_error(generateDrugUtilisationCohort(
    cdm = cdm, ingredientConceptId = 1, durationRange = -7
  ))
})

test_that("basic functionality drug_conceptId", {
  concept1 <- system.file(package = "DrugUtilisation", "concept1.json")
  cdm <- mockDrugUtilisation(seed = 1)
  out_put_1 <- generateDrugUtilisationCohort(
    cdm,
    conceptSetPath = concept1,
    gapEra = 834,
    daysPriorHistory = NULL
  )
  expect_true(
      out_put_1 %>%
        dplyr::summarise(max(cohort_start_date, na.rm = TRUE)) %>%
        dplyr::pull() ==
        cdm$drug_exposure %>%
        dplyr::filter(drug_concept_id == 1) %>%
        dplyr::summarise(max(drug_exposure_start_date, na.rm = TRUE)) %>%
        dplyr::pull()
    )
  expect_true(
    out_put_1 %>%
      dplyr::summarise(min(cohort_start_date, na.rm = TRUE)) %>%
      dplyr::pull() ==
      cdm$drug_exposure %>%
      dplyr::filter(drug_concept_id == 1) %>%
      dplyr::summarise(min(drug_exposure_start_date, na.rm = TRUE)) %>%
      dplyr::pull()
  )
  expect_true(
    out_put_1 %>%
      dplyr::summarise(max(cohort_end_date, na.rm = TRUE)) %>%
      dplyr::pull() ==
      cdm$drug_exposure %>%
      dplyr::filter(drug_concept_id == 1) %>%
      dplyr::summarise(max(drug_exposure_end_date, na.rm = TRUE)) %>%
      dplyr::pull()
  )
  expect_true(
    out_put_1 %>%
      dplyr::summarise(min(cohort_end_date, na.rm = TRUE)) %>%
      dplyr::pull() ==
      cdm$drug_exposure %>%
      dplyr::filter(drug_concept_id == 1) %>%
      dplyr::summarise(min(drug_exposure_end_date, na.rm = TRUE)) %>%
      dplyr::pull()
  )
  expect_true(
    out_put_1 %>%
      dplyr::filter(subject_id != 1) %>%
      dplyr::count() %>%
      dplyr::pull() == 0
  )
  expect_true(
    out_put_1 %>%
      dplyr::filter(subject_id == 1) %>%
      dplyr::count() %>%
      dplyr::pull() != 0
  )

  out_put_2 <- generateDrugUtilisationCohort(
    cdm,
    conceptSetPath = concept1,
    gapEra = 835,
    daysPriorHistory = NULL
  )
  expect_true(out_put_2 %>% dplyr::tally() %>% dplyr::pull("n") == 3)

  out_put_3 <- generateDrugUtilisationCohort(
    cdm,
    conceptSetPath = concept1,
    gapEra = 836,
    daysPriorHistory = NULL
  )
  expect_true(out_put_3 %>% dplyr::tally() %>% dplyr::pull("n") == 2)


  out_put_11 <- generateDrugUtilisationCohort(
    cdm,
    ingredientConceptId = 1,
    conceptSetPath = concept1,
    gapEra = 834,
    daysPriorHistory = NULL
  )
  expect_equal(out_put_1 %>% dplyr::collect(), out_put_11 %>% dplyr::collect())

  out_put_4 <- generateDrugUtilisationCohort(
    cdm,
    conceptSetPath = concept1,
    gapEra = 2972
  )
  expect_true(out_put_4 %>% dplyr::tally() %>% dplyr::pull("n") == 1)

  out_put_4 <- generateDrugUtilisationCohort(
    cdm,
    conceptSetPath = concept1,
    gapEra = 2973
  )
  expect_true(out_put_4 %>% dplyr::tally() %>% dplyr::pull("n") == 0)

  out_put_5 <- generateDrugUtilisationCohort(
    cdm,
    ingredientConceptId = 1,
    conceptSetPath = concept1,
    summariseMode = "FirstEra",
    gapEra = 834,
    daysPriorHistory = NULL
  )
  expect_true(out_put_5 %>% dplyr::tally() %>% dplyr::pull("n") == 1)

  out_put_6 <- generateDrugUtilisationCohort(
    cdm,
    ingredientConceptId = 1,
    conceptSetPath = concept1,
    summariseMode = "FixedTime",
    fixedTime = 365,
    gapEra = 834,
    daysPriorHistory = NULL
  )
  expect_true(out_put_6 %>% dplyr::tally() %>% dplyr::pull("n") == 1)

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)

})

test_that("basic functionality unique path", {
  concept1 <- system.file(package = "DrugUtilisation", "concept1")
  cdm <- mockDrugUtilisation(seed = 1)
  out_put_1 <- generateDrugUtilisationCohort(
    cdm,
    conceptSetPath = concept1,
    gapEra = 834,
    daysPriorHistory = NULL
  )
  expect_true(
    out_put_1 %>%
      dplyr::summarise(max(cohort_start_date, na.rm = TRUE)) %>%
      dplyr::pull() ==
      cdm$drug_exposure %>%
      dplyr::filter(drug_concept_id == 1) %>%
      dplyr::summarise(max(drug_exposure_start_date, na.rm = TRUE)) %>%
      dplyr::pull()
  )
  expect_true(
    out_put_1 %>%
      dplyr::summarise(min(cohort_start_date, na.rm = TRUE)) %>%
      dplyr::pull() ==
      cdm$drug_exposure %>%
      dplyr::filter(drug_concept_id == 1) %>%
      dplyr::summarise(min(drug_exposure_start_date, na.rm = TRUE)) %>%
      dplyr::pull()
  )
  expect_true(
    out_put_1 %>%
      dplyr::summarise(max(cohort_end_date, na.rm = TRUE)) %>%
      dplyr::pull() ==
      cdm$drug_exposure %>%
      dplyr::filter(drug_concept_id == 1) %>%
      dplyr::summarise(max(drug_exposure_end_date, na.rm = TRUE)) %>%
      dplyr::pull()
  )
  expect_true(
    out_put_1 %>%
      dplyr::summarise(min(cohort_end_date, na.rm = TRUE)) %>%
      dplyr::pull() ==
      cdm$drug_exposure %>%
      dplyr::filter(drug_concept_id == 1) %>%
      dplyr::summarise(min(drug_exposure_end_date, na.rm = TRUE)) %>%
      dplyr::pull()
  )
  expect_true(
    out_put_1 %>%
      dplyr::filter(subject_id != 1) %>%
      dplyr::count() %>%
      dplyr::pull() == 0
  )
  expect_true(
    out_put_1 %>%
      dplyr::filter(subject_id == 1) %>%
      dplyr::count() %>%
      dplyr::pull() != 0
  )

  out_put_2 <- generateDrugUtilisationCohort(
    cdm,
    conceptSetPath = concept1,
    gapEra = 835,
    daysPriorHistory = NULL
  )
  expect_true(out_put_2 %>% dplyr::tally() %>% dplyr::pull("n") == 3)

  out_put_3 <- generateDrugUtilisationCohort(
    cdm,
    conceptSetPath = concept1,
    gapEra = 836,
    daysPriorHistory = NULL
  )
  expect_true(out_put_3 %>% dplyr::tally() %>% dplyr::pull("n") == 2)


  out_put_11 <- generateDrugUtilisationCohort(
    cdm,
    ingredientConceptId = 1,
    conceptSetPath = concept1,
    gapEra = 834,
    daysPriorHistory = NULL
  )
  expect_equal(out_put_1 %>% dplyr::collect(), out_put_11 %>% dplyr::collect())

  out_put_4 <- generateDrugUtilisationCohort(
    cdm,
    conceptSetPath = concept1,
    gapEra = 2972
  )
  expect_true(out_put_4 %>% dplyr::tally() %>% dplyr::pull("n") == 1)

  out_put_4 <- generateDrugUtilisationCohort(
    cdm,
    conceptSetPath = concept1,
    gapEra = 2973
  )
  expect_true(out_put_4 %>% dplyr::tally() %>% dplyr::pull("n") == 0)

  out_put_5 <- generateDrugUtilisationCohort(
    cdm,
    ingredientConceptId = 1,
    conceptSetPath = concept1,
    summariseMode = "FirstEra",
    gapEra = 834,
    daysPriorHistory = NULL
  )
  expect_true(out_put_5 %>% dplyr::tally() %>% dplyr::pull("n") == 1)

  out_put_6 <- generateDrugUtilisationCohort(
    cdm,
    ingredientConceptId = 1,
    conceptSetPath = concept1,
    summariseMode = "FixedTime",
    fixedTime = 365,
    gapEra = 834,
    daysPriorHistory = NULL
  )
  expect_true(out_put_6 %>% dplyr::tally() %>% dplyr::pull("n") == 1)

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)

})

test_that("basic functionality multiple paths", {
  concept1 <- system.file(package = "DrugUtilisation", "concepts")
  cdm <- mockDrugUtilisation(seed = 1)
  out_put_1 <- generateDrugUtilisationCohort(
    cdm,
    conceptSetPath = concept1,
    gapEra = 834,
    daysPriorHistory = NULL
  ) %>%
    dplyr::filter(cohort_definition_id == 1)
  expect_true(
    out_put_1 %>%
      dplyr::summarise(max(cohort_start_date, na.rm = TRUE)) %>%
      dplyr::pull() ==
      cdm$drug_exposure %>%
      dplyr::filter(drug_concept_id == 1) %>%
      dplyr::summarise(max(drug_exposure_start_date, na.rm = TRUE)) %>%
      dplyr::pull()
  )
  expect_true(
    out_put_1 %>%
      dplyr::summarise(min(cohort_start_date, na.rm = TRUE)) %>%
      dplyr::pull() ==
      cdm$drug_exposure %>%
      dplyr::filter(drug_concept_id == 1) %>%
      dplyr::summarise(min(drug_exposure_start_date, na.rm = TRUE)) %>%
      dplyr::pull()
  )
  expect_true(
    out_put_1 %>%
      dplyr::summarise(max(cohort_end_date, na.rm = TRUE)) %>%
      dplyr::pull() ==
      cdm$drug_exposure %>%
      dplyr::filter(drug_concept_id == 1) %>%
      dplyr::summarise(max(drug_exposure_end_date, na.rm = TRUE)) %>%
      dplyr::pull()
  )
  expect_true(
    out_put_1 %>%
      dplyr::summarise(min(cohort_end_date, na.rm = TRUE)) %>%
      dplyr::pull() ==
      cdm$drug_exposure %>%
      dplyr::filter(drug_concept_id == 1) %>%
      dplyr::summarise(min(drug_exposure_end_date, na.rm = TRUE)) %>%
      dplyr::pull()
  )
  expect_true(
    out_put_1 %>%
      dplyr::filter(subject_id != 1) %>%
      dplyr::count() %>%
      dplyr::pull() == 0
  )
  expect_true(
    out_put_1 %>%
      dplyr::filter(subject_id == 1) %>%
      dplyr::count() %>%
      dplyr::pull() != 0
  )

  out_put_2 <- generateDrugUtilisationCohort(
    cdm,
    conceptSetPath = concept1,
    gapEra = 835,
    daysPriorHistory = NULL
  ) %>%
    dplyr::filter(cohort_definition_id == 1)
  expect_true(out_put_2 %>% dplyr::tally() %>% dplyr::pull("n") == 3)

  out_put_3 <- generateDrugUtilisationCohort(
    cdm,
    conceptSetPath = concept1,
    gapEra = 836,
    daysPriorHistory = NULL
  ) %>%
    dplyr::filter(cohort_definition_id == 1)
  expect_true(out_put_3 %>% dplyr::tally() %>% dplyr::pull("n") == 2)


  out_put_11 <- generateDrugUtilisationCohort(
    cdm,
    ingredientConceptId = 1,
    conceptSetPath = concept1,
    gapEra = 834,
    daysPriorHistory = NULL
  ) %>%
    dplyr::filter(cohort_definition_id == 1)
  expect_equal(out_put_1 %>% dplyr::collect(), out_put_11 %>% dplyr::collect())

  out_put_4 <- generateDrugUtilisationCohort(
    cdm,
    conceptSetPath = concept1,
    gapEra = 2972
  )
  expect_true(out_put_4 %>% dplyr::tally() %>% dplyr::pull("n") == 1)

  out_put_4 <- generateDrugUtilisationCohort(
    cdm,
    conceptSetPath = concept1,
    gapEra = 2973
  ) %>%
    dplyr::filter(cohort_definition_id == 1)
  expect_true(out_put_4 %>% dplyr::tally() %>% dplyr::pull("n") == 0)

  out_put_5 <- generateDrugUtilisationCohort(
    cdm,
    ingredientConceptId = 1,
    conceptSetPath = concept1,
    summariseMode = "FirstEra",
    gapEra = 834,
    daysPriorHistory = NULL
  ) %>%
    dplyr::filter(cohort_definition_id == 1)
  expect_true(out_put_5 %>% dplyr::tally() %>% dplyr::pull("n") == 1)

  out_put_6 <- generateDrugUtilisationCohort(
    cdm,
    ingredientConceptId = 1,
    conceptSetPath = concept1,
    summariseMode = "FixedTime",
    fixedTime = 365,
    gapEra = 834,
    daysPriorHistory = NULL
  ) %>%
    dplyr::filter(cohort_definition_id == 1)
  expect_true(out_put_6 %>% dplyr::tally() %>% dplyr::pull("n") == 1)

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)

})

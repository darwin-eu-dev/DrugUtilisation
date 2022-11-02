test_that("simple checks", {
  cdm <- mockDrugUtilisation(patient_size = 1000,drug_exposure_size = 2000)
  specifications <- cdm$drug_strength %>%
    dplyr::select("drug_concept_id","ingredient_concept_id") %>%
    dplyr::collect()

  expect_error(instantiateDrugUtilisationCohorts(cdm,
                                                 specifications,
                                                 studyTime = NULL,
                                                 gapEra = 2000,
                                                 eraJoinMode = "zero",
                                                 overlapMode = "max",
                                                 sameIndexMode = "sum",
                                                 drugUtilisationCohortName = "drugUtilisationCohortName",
                                                 imputeDuration = TRUE,
                                                 imputeDailyDose = TRUE,
                                                 durationLowerBound = NULL,
                                                 durationUpperBound = NULL,
                                                 dailyDoseLowerBound = NULL,
                                                 dailyDoseUpperBound = NULL,
                                                 verbose = FALSE))

  result <- instantiateDrugUtilisationCohorts(
    cdm,
    specifications,
    studyTime = NULL,
    gapEra = 2000,
    eraJoinMode = "zero",
    overlapMode = "max",
    sameIndexMode = "sum",
    drugUtilisationCohortName = "drugUtilisationCohortName",
    imputeDuration = FALSE,
    imputeDailyDose = FALSE,
    durationLowerBound = NULL,
    durationUpperBound = NULL,
    dailyDoseLowerBound = NULL,
    dailyDoseUpperBound = NULL,
    verbose = FALSE
  )

  result <- result %>% dplyr::collect()

  expect_true(all(result$exposed_days + result$not_exposed_days == result$study_days))
  expect_true(all(result$number_subexposures_with_overlap + result$number_continuous_exposures_no_overlap
                  == result$number_continuous_exposures))
  expect_true(all(result$number_eras_no_overlap + result$number_eras_with_overlap
                  == result$number_eras))
  expect_true(all(result$number_subexposures_no_overlap + result$number_subexposures_with_overlap
                  == result$number_subexposures))
  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)

})

test_that("test expect errors",{
  cdm <- mockDrugUtilisation(patient_size = 1000,drug_exposure_size = 2000)
  spec <- cdm$drug_strength %>%
    dplyr::select("drug_concept_id") %>%
    dplyr::collect()
  #throw error when set impute = TRUE but not provided in specification
  expect_error(instantiateDrugUtilisationCohorts(cdm,
                                                 specifications = spec,
                                                 ingredient_concept_id = 1,
                                                 studyTime = NULL,
                                                 gapEra = 2,
                                                 eraJoinMode = "first",
                                                 overlapMode = "max",
                                                 sameIndexMode = "sum",
                                                 drugUtilisationCohortName =
                                                   "drugUtilisationCohortName",
                                                 imputeDuration = TRUE,
                                                 imputeDailyDose = TRUE,
                                                 durationLowerBound = NULL,
                                                 durationUpperBound = NULL,
                                                 dailyDoseLowerBound = NULL,
                                                 dailyDoseUpperBound = NULL,
                                                 verbose = FALSE))

  #throw error when ingredient_concept_id is not integer
  expect_error(instantiateDrugUtilisationCohorts(cdm,
                                                 specifications = spec,
                                                 ingredient_concept_id = 1.3,
                                                 studyTime = NULL,
                                                 gapEra = 2,
                                                 eraJoinMode = "first",
                                                 overlapMode = "max",
                                                 sameIndexMode = "sum",
                                                 drugUtilisationCohortName =
                                                   "drugUtilisationCohortName",
                                                 imputeDuration = TRUE,
                                                 imputeDailyDose = FALSE,
                                                 durationLowerBound = NULL,
                                                 durationUpperBound = NULL,
                                                 dailyDoseLowerBound = NULL,
                                                 dailyDoseUpperBound = NULL,
                                                 verbose = FALSE))

})

test_that("simple checks and sums", {
  cdm <- mockDrugUtilisation(patient_size = 1000,drug_exposure_size = 2000)
  spec <- cdm$drug_strength %>%
    dplyr::select("drug_concept_id") %>%
    dplyr::collect()



  result <- instantiateDrugUtilisationCohorts(
    cdm,
    specifications = spec,
    ingredient_concept_id = 1,
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

  #test not_exposed_days
  expect_true(all(result$exposed_days + result$not_exposed_days == result$study_days))

  #test number_continuous_exposures_no_overlap
  expect_true(all(result$number_subexposures_with_overlap + result$number_continuous_exposures_no_overlap
                  == result$number_continuous_exposures))

  #test number_eras_no_overlap
  expect_true(all(result$number_eras_no_overlap + result$number_eras_with_overlap
                  == result$number_eras))

  #test number_subexposures_no_overlap
  expect_true(all(result$number_subexposures_no_overlap + result$number_subexposures_with_overlap
                  == result$number_subexposures))

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)

})


test_that("all output checks for single era with single gap (less than eraGap)", {

  drug_exposure <- tibble::tibble(
    drug_exposure_id = c(1, 2, 3, 4),
    drug_concept_id = c(1, 2, 3, 4),
    person_id = c(1, 1, 1, 1),
    drug_exposure_start_date = c(
      as.Date("2010-01-01"),
      as.Date("2010-09-01"),
      as.Date("2012-01-01"),
      as.Date("2012-09-05")),
    drug_exposure_end_date = c(
      as.Date("2011-01-01"),
      as.Date("2012-09-01"),
      as.Date("2012-01-10"),
      as.Date("2013-09-05")),
    quantity = c(1,2,3,4))

  drug_strength <- tibble::tibble(
    ingredient_concept_id = c(1, 1, 1, 1),
    drug_concept_id = c(1, 2, 3, 4),
    amount_value = c(1, 2, 3, 4),
    amount_unit_concept_id = c(8576, 8576, 8576, 8576)
  )



  cdm <- mockDrugUtilisation(drug_exposure = drug_exposure,
                             drug_strength = drug_strength)


  spec <- cdm$drug_strength %>%
    dplyr::select("drug_concept_id") %>%
    dplyr::collect()


  result <- instantiateDrugUtilisationCohorts(
    cdm,
    specifications = spec,
    ingredient_concept_id = 1,
    studyTime = NULL,
    gapEra = 3,
    eraJoinMode = "first",
    overlapMode = "sum",
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

  #test exposed days
  expect_true(result$exposed_days == difftime(as.Date("2013-09-05"), as.Date("2010-01-01")) + 1)


  #test initial and cumulative dose
  testInitialDose <- computeDailyDose(
    table = cdm$drug_exposure,
    cdm = cdm,
    ingredient_concept_id = ingredient_concept_id,
    verbose = FALSE
  )

  initialDose <- testInitialDose %>% dbplyr::window_order(.data$drug_exposure_start_date) %>%
    dplyr::mutate(subexposure_id = dplyr::row_number()) %>%
    dplyr::filter(subexposure_id == 1) %>% dplyr::collect()

  expect_true(result$initial_dose == initialDose$daily_dose)


  testPeriod <- getPeriods(
    x = testInitialDose,
    dialect =  CDMConnector::dbms(attr(cdm, "dbcon")),
    verbose = verbose
  )

  testCumulativeDose <- joinExposures(
    x = testPeriod,
    gapEra = 3,
    eraJoinMode = "first",
    sameIndexMode = "sum",
    dialect =  CDMConnector::dbms(attr(cdm, "dbcon")),
    verbose = verbose
  ) %>% dplyr::collect()

  expect_true(all.equal(result$cumulative_dose,sum(testCumulativeDose$daily_dose *
                                                     testCumulativeDose$days_exposed)))

  #test study days
  expect_true(result$study_days == difftime(as.Date("2013-09-05"), as.Date("2010-01-01")) + 1)

  #test number of exposures
  expect_true(result$number_exposures == 4)

  #test number_subexposures
  expect_true(result$number_subexposures  == 7)

  #test number_subexposures_with_overlap
  expect_true(result$number_subexposures_with_overlap == 2)

  #test continuous exposre
  expect_true(result$number_continuous_exposures == 2)

  #test number_continuous_exposures_with_overlap
  expect_true(result$number_continuous_exposures_with_overlap == 1)

  #test number era
  expect_true(result$number_eras == 1)

  #test number_eras_with_overlap
  expect_true(result$number_eras_with_overlap == 1)

  #test number_non_exposed_periods
  expect_true(result$number_non_exposed_periods == 1)

  #test number_gaps
  expect_true(result$number_gaps == 1)

  #test number_days_gap
  expect_true(result$number_days_gap == 3)

  #test cumulative_gap_dose
  expect_true(result$cumulative_gap_dose == sum(testCumulativeDose$gap *
                                                  testCumulativeDose$days_exposed * testCumulativeDose$daily_dose))
  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
  })






test_that("test impute and lower upper bound", {

  drug_exposure <- tibble::tibble(
    drug_exposure_id = c(1, 2, 3),
    drug_concept_id = c(1, 2, 3),
    person_id = c(1, 1, 1),
    drug_exposure_start_date = c(
      as.Date("2010-01-01"),
      as.Date("2010-01-01"),
      as.Date("2010-01-01")),
    drug_exposure_end_date = c(
      as.Date(NA),
      as.Date("2015-01-01"),
      as.Date("2010-01-01")),
    quantity = c(1, 2, 3))

  drug_strength <- tibble::tibble(
    ingredient_concept_id = c(1, 1, 1),
    drug_concept_id = c(1, 2, 3),
    amount_value = c(NA, 1000, 0),
    amount_unit_concept_id = c(8576, 8576, 8576)
  )



  cdm <- mockDrugUtilisation(drug_exposure = drug_exposure,
                             drug_strength = drug_strength)


  spec <- cdm$drug_strength %>%
    dplyr::select("drug_concept_id") %>%
    dplyr::collect()

  spec$default_duration <- 10
  spec$default_daily_dose <- 10

  eraJoinMode = "second"
  overlapMode = "first"

  #when there is no exp with same start, changing sameIndexMode should return same result
  result <- instantiateDrugUtilisationCohorts(
    cdm,
    specifications = spec,
    ingredient_concept_id = 1,
    studyTime = 1350, #limit end date to 2013-09-11
    gapEra = 3,
    eraJoinMode = eraJoinMode,
    overlapMode = overlapMode,
    sameIndexMode = "max",
    drugUtilisationCohortName = "drugUtilisationCohortName",
    imputeDuration = TRUE,
    imputeDailyDose = TRUE,
    durationLowerBound = 2,
    durationUpperBound = 365,
    dailyDoseLowerBound = 5,
    dailyDoseUpperBound = 100,
    verbose = FALSE
  ) %>% dplyr::collect()

  expect_true(result$exposed_days == 10)

  expect_true(result$cumulative_dose == 10 * 10)

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})




test_that("test same index", {

  drug_exposure <- tibble::tibble(
    drug_exposure_id = c(1, 2, 3),
    drug_concept_id = c(1, 2, 3),
    person_id = c(1, 1, 1),
    drug_exposure_start_date = c(
      as.Date("2010-01-01"),
      as.Date("2010-01-01"),
      as.Date("2010-01-01")),
    drug_exposure_end_date = c(
      as.Date("2010-01-02"),
      as.Date("2010-01-05"),
      as.Date("2010-01-10")),
    quantity = c(1, 2, 3))

  drug_strength <- tibble::tibble(
    ingredient_concept_id = c(1, 1, 1),
    drug_concept_id = c(1, 2, 3),
    amount_value = c(1, 10, 20),
    amount_unit_concept_id = c(8576, 8576, 8576)
  )



  cdm <- mockDrugUtilisation(drug_exposure = drug_exposure,
                             drug_strength = drug_strength)


  spec <- cdm$drug_strength %>%
    dplyr::select("drug_concept_id") %>%
    dplyr::collect()


  eraJoinMode = "second"
  overlapMode = "first"

  #when there is no exp with same start, changing sameIndexMode should return same result
  resultMax <- instantiateDrugUtilisationCohorts(
    cdm,
    specifications = spec,
    ingredient_concept_id = 1,
    studyTime = 1000, #limit end date to 2013-09-11
    gapEra = 10,
    eraJoinMode = eraJoinMode,
    overlapMode = overlapMode,
    sameIndexMode = "max",
    drugUtilisationCohortName = "drugUtilisationCohortName",
    imputeDuration = FALSE,
    imputeDailyDose = FALSE,
    durationLowerBound = NULL,
    durationUpperBound = NULL,
    dailyDoseLowerBound = NULL,
    dailyDoseUpperBound = NULL,
    verbose = FALSE
  ) %>% dplyr::collect()

  resultMin <- instantiateDrugUtilisationCohorts(
    cdm,
    specifications = spec,
    ingredient_concept_id = 1,
    studyTime = 1000, #limit end date to 2013-09-11
    gapEra = 10,
    eraJoinMode = eraJoinMode,
    overlapMode = overlapMode,
    sameIndexMode = "min",
    drugUtilisationCohortName = "drugUtilisationCohortName",
    imputeDuration = FALSE,
    imputeDailyDose = FALSE,
    durationLowerBound = NULL,
    durationUpperBound = NULL,
    dailyDoseLowerBound = NULL,
    dailyDoseUpperBound = NULL,
    verbose = FALSE
  ) %>% dplyr::collect()

  resultSum <- instantiateDrugUtilisationCohorts(
    cdm,
    specifications = spec,
    ingredient_concept_id = 1,
    studyTime = 1000, #limit end date to 2013-09-11
    gapEra = 10,
    eraJoinMode = eraJoinMode,
    overlapMode = overlapMode,
    sameIndexMode = "sum",
    drugUtilisationCohortName = "drugUtilisationCohortName",
    imputeDuration = FALSE,
    imputeDailyDose = FALSE,
    durationLowerBound = NULL,
    durationUpperBound = NULL,
    dailyDoseLowerBound = NULL,
    dailyDoseUpperBound = NULL,
    verbose = FALSE
  ) %>% dplyr::collect()

  expect_true(resultMax$cumulative_dose == 60)

  expect_true(resultSum$cumulative_dose == 1 * 1 + 2 * 10 + 3 * 20)

  expect_true(resultMin$cumulative_dose == 1 / 2 * 2 + 2 * 10 / 5 * 3 + 3 * 20 / 10 * 5)

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})








test_that("test same index for join exp", {

  drug_exposure <- tibble::tibble(
    drug_exposure_id = c(1, 2, 3),
    drug_concept_id = c(1, 2, 3),
    person_id = c(1, 1, 1),
    drug_exposure_start_date = c(
      as.Date("2010-01-01"),
      as.Date("2010-01-01"),
      as.Date("2010-01-05")),
    drug_exposure_end_date = c(
      as.Date("2010-01-03"),
      as.Date("2010-01-03"),
      as.Date("2010-01-10")),
    quantity = c(1, 2, 3))

  drug_strength <- tibble::tibble(
    ingredient_concept_id = c(1, 1, 1),
    drug_concept_id = c(1, 2, 3),
    amount_value = c(1, 10, 20),
    amount_unit_concept_id = c(8576, 8576, 8576)
  )



  cdm <- mockDrugUtilisation(drug_exposure = drug_exposure,
                             drug_strength = drug_strength)


  spec <- cdm$drug_strength %>%
    dplyr::select("drug_concept_id") %>%
    dplyr::collect()


  eraJoinMode = "first"
  overlapMode = "first"

  #when there is no exp with same start, changing sameIndexMode should return same result
  resultMax <- instantiateDrugUtilisationCohorts(
    cdm,
    specifications = spec,
    ingredient_concept_id = 1,
    studyTime = 1000, #limit end date to 2013-09-11
    gapEra = 10,
    eraJoinMode = eraJoinMode,
    overlapMode = overlapMode,
    sameIndexMode = "max",
    drugUtilisationCohortName = "drugUtilisationCohortName",
    imputeDuration = FALSE,
    imputeDailyDose = FALSE,
    durationLowerBound = NULL,
    durationUpperBound = NULL,
    dailyDoseLowerBound = NULL,
    dailyDoseUpperBound = NULL,
    verbose = FALSE
  ) %>% dplyr::collect()

  resultMin <- instantiateDrugUtilisationCohorts(
    cdm,
    specifications = spec,
    ingredient_concept_id = 1,
    studyTime = 1000, #limit end date to 2013-09-11
    gapEra = 10,
    eraJoinMode = eraJoinMode,
    overlapMode = overlapMode,
    sameIndexMode = "min",
    drugUtilisationCohortName = "drugUtilisationCohortName",
    imputeDuration = FALSE,
    imputeDailyDose = FALSE,
    durationLowerBound = NULL,
    durationUpperBound = NULL,
    dailyDoseLowerBound = NULL,
    dailyDoseUpperBound = NULL,
    verbose = FALSE
  ) %>% dplyr::collect()

  resultSum <- instantiateDrugUtilisationCohorts(
    cdm,
    specifications = spec,
    ingredient_concept_id = 1,
    studyTime = 1000, #limit end date to 2013-09-11
    gapEra = 10,
    eraJoinMode = eraJoinMode,
    overlapMode = overlapMode,
    sameIndexMode = "sum",
    drugUtilisationCohortName = "drugUtilisationCohortName",
    imputeDuration = FALSE,
    imputeDailyDose = FALSE,
    durationLowerBound = NULL,
    durationUpperBound = NULL,
    dailyDoseLowerBound = NULL,
    dailyDoseUpperBound = NULL,
    verbose = FALSE
  ) %>% dplyr::collect()

  #
    expect_true(resultMax$cumulative_dose == 20 + 20 / 3 + 20 * 3)

    expect_true(resultMin$cumulative_dose == 1 + 1 / 3 + 20 * 3)

    expect_true(resultSum$cumulative_dose == 21 + 21 / 3 + 20 * 3)

    DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})




test_that("test era join mode", {

  drug_exposure <- tibble::tibble(
    drug_exposure_id = c(1, 2),
    drug_concept_id = c(1, 2),
    person_id = c(1, 1),
    drug_exposure_start_date = c(
      as.Date("2010-01-01"),
      as.Date("2010-01-05")),
    drug_exposure_end_date = c(
      as.Date("2010-01-02"),
      as.Date("2010-01-10")),
    quantity = c(1, 2))

  drug_strength <- tibble::tibble(
    ingredient_concept_id = c(1, 1),
    drug_concept_id = c(1, 2),
    amount_value = c(1, 10),
    amount_unit_concept_id = c(8576, 8576)
  )



  cdm <- mockDrugUtilisation(drug_exposure = drug_exposure,
                             drug_strength = drug_strength)


  spec <- cdm$drug_strength %>%
    dplyr::select("drug_concept_id") %>%
    dplyr::collect()


  sameIndexMode = "max"
  overlapMode = "first"

  #when there is no exp with same start, changing sameIndexMode should return same result
  resultFirst <- instantiateDrugUtilisationCohorts(
    cdm,
    specifications = spec,
    ingredient_concept_id = 1,
    studyTime = 1000, #limit end date to 2013-09-11
    gapEra = 10,
    eraJoinMode = "first",
    overlapMode = overlapMode,
    sameIndexMode = sameIndexMode,
    drugUtilisationCohortName = "drugUtilisationCohortName",
    imputeDuration = FALSE,
    imputeDailyDose = FALSE,
    durationLowerBound = NULL,
    durationUpperBound = NULL,
    dailyDoseLowerBound = NULL,
    dailyDoseUpperBound = NULL,
    verbose = FALSE
  ) %>% dplyr::collect()

  resultSecond <- instantiateDrugUtilisationCohorts(
    cdm,
    specifications = spec,
    ingredient_concept_id = 1,
    studyTime = 1000, #limit end date to 2013-09-11
    gapEra = 10,
    eraJoinMode = "second",
    overlapMode = overlapMode,
    sameIndexMode = sameIndexMode,
    drugUtilisationCohortName = "drugUtilisationCohortName",
    imputeDuration = FALSE,
    imputeDailyDose = FALSE,
    durationLowerBound = NULL,
    durationUpperBound = NULL,
    dailyDoseLowerBound = NULL,
    dailyDoseUpperBound = NULL,
    verbose = FALSE
  ) %>% dplyr::collect()

  resultZero <- instantiateDrugUtilisationCohorts(
    cdm,
    specifications = spec,
    ingredient_concept_id = 1,
    studyTime = 1000, #limit end date to 2013-09-11
    gapEra = 10,
    eraJoinMode = "zero",
    overlapMode = overlapMode,
    sameIndexMode = sameIndexMode,
    drugUtilisationCohortName = "drugUtilisationCohortName",
    imputeDuration = FALSE,
    imputeDailyDose = FALSE,
    durationLowerBound = NULL,
    durationUpperBound = NULL,
    dailyDoseLowerBound = NULL,
    dailyDoseUpperBound = NULL,
    verbose = FALSE
  ) %>% dplyr::collect()


  resultJoin <- instantiateDrugUtilisationCohorts(
    cdm,
    specifications = spec,
    ingredient_concept_id = 1,
    studyTime = 1000, #limit end date to 2013-09-11
    gapEra = 10,
    eraJoinMode = "join",
    overlapMode = overlapMode,
    sameIndexMode = sameIndexMode,
    drugUtilisationCohortName = "drugUtilisationCohortName",
    imputeDuration = FALSE,
    imputeDailyDose = FALSE,
    durationLowerBound = NULL,
    durationUpperBound = NULL,
    dailyDoseLowerBound = NULL,
    dailyDoseUpperBound = NULL,
    verbose = FALSE
  ) %>% dplyr::collect()

  expect_true(resultFirst$cumulative_dose == 1 * 1 /2 * 4 + 2 * 10)

  expect_true(resultSecond$cumulative_dose == 1 * 1 /2 * 2 + 2 * 10 / 6 * 8)

  expect_true(resultJoin$cumulative_dose == 1 * 1 + 2 * 10)

  expect_true(resultZero$cumulative_dose == 1 * 1 + 2 * 10)

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})









test_that("test overlap mode", {

  drug_exposure <- tibble::tibble(
    drug_exposure_id = c(1, 2),
    drug_concept_id = c(1, 2),
    person_id = c(1, 1),
    drug_exposure_start_date = c(
      as.Date("2010-01-01"),
      as.Date("2010-01-02")),
    drug_exposure_end_date = c(
      as.Date("2010-01-02"),
      as.Date("2010-01-10")),
    quantity = c(1, 2))

  drug_strength <- tibble::tibble(
    ingredient_concept_id = c(1, 1),
    drug_concept_id = c(1, 2),
    amount_value = c(1, 10),
    amount_unit_concept_id = c(8576, 8576)
  )



  cdm <- mockDrugUtilisation(drug_exposure = drug_exposure,
                             drug_strength = drug_strength)


  spec <- cdm$drug_strength %>%
    dplyr::select("drug_concept_id") %>%
    dplyr::collect()


  sameIndexMode = "max"
  eraJoinMode = "first"

  #when there is no exp with same start, changing sameIndexMode should return same result
  resultSecond <- instantiateDrugUtilisationCohorts(
    cdm,
    specifications = spec,
    ingredient_concept_id = 1,
    studyTime = 1000, #limit end date to 2013-09-11
    gapEra = 10,
    eraJoinMode = eraJoinMode,
    overlapMode = "second",
    sameIndexMode = sameIndexMode,
    drugUtilisationCohortName = "drugUtilisationCohortName",
    imputeDuration = FALSE,
    imputeDailyDose = FALSE,
    durationLowerBound = NULL,
    durationUpperBound = NULL,
    dailyDoseLowerBound = NULL,
    dailyDoseUpperBound = NULL,
    verbose = FALSE
  ) %>% dplyr::collect()

  resultMin <- instantiateDrugUtilisationCohorts(
    cdm,
    specifications = spec,
    ingredient_concept_id = 1,
    studyTime = 1000, #limit end date to 2013-09-11
    gapEra = 10,
    eraJoinMode = eraJoinMode,
    overlapMode = "min",
    sameIndexMode = sameIndexMode,
    drugUtilisationCohortName = "drugUtilisationCohortName",
    imputeDuration = FALSE,
    imputeDailyDose = FALSE,
    durationLowerBound = NULL,
    durationUpperBound = NULL,
    dailyDoseLowerBound = NULL,
    dailyDoseUpperBound = NULL,
    verbose = FALSE
  ) %>% dplyr::collect()


  expect_true(resultSecond$cumulative_dose == 1 / 2 + 20)

  expect_true(resultMin$cumulative_dose == 1 + 20 / 9 * 8)

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

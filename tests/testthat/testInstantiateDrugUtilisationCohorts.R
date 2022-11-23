# test_that("test expect errors",{
#   cdm <- mockDrugUtilisation(patient_size = 1000,drug_exposure_size = 2000)
#
#   #throw error when set impute = TRUE but not provided in specification
#   expect_error(instantiateDrugUtilisationCohorts(cdm,
#                                                  ingredientConceptId = 1,
#                                                  gapEra = 2,
#                                                  eraJoinMode = "Previous",
#                                                  overlapMode = "Maximum",
#                                                  sameIndexMode = "Sum",
#                                                  imputeDuration = TRUE))
#
#   #throw error when ingredientConceptId is not integer
#   expect_error(instantiateDrugUtilisationCohorts(cdm,
#                                                  ingredientConceptId = 1.3,
#                                                  gapEra = 2,
#                                                  eraJoinMode = "first",
#                                                  overlapMode = "max",
#                                                  sameIndexMode = "sum"))
#
# })

test_that("simple checks and sums", {
  cdm <- mockDrugUtilisation(patient_size = 1000, drug_exposure_size = 2000)

  result <- instantiateDrugUtilisationCohorts(
    cdm = cdm,
    ingredientConceptId = 1,
    gapEra = 2000,
    eraJoinMode = "Zero",
    overlapMode = "Maximum",
    sameIndexMode = "Sum",
    imputeDuration = "eliminate",
    imputeDailyDose = "eliminate",
    cohortEntryPriorHistory = 0
  )

  result <- result$dose %>% dplyr::collect()

  #test not_exposed_days
  expect_true(all(result$exposed_days + result$not_exposed_days == result$study_days))

  #test number_continuous_exposures_no_overlap
  expect_true(all(result$number_continuous_exposures_with_overlap +
                    result$number_continuous_exposures_no_overlap ==
                    result$number_continuous_exposures))

  #test number_eras_no_overlap
  expect_true(all(result$number_eras_no_overlap + result$number_eras_with_overlap
                  == result$number_eras))

  #test number_subexposures_no_overlap
  expect_true(all(result$number_subexposures_no_overlap + result$number_subexposures_with_overlap
                  == result$number_subexposures))

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)

})

test_that("all output checks for single era with single gap (less than eraGap)", {
  drug_exposure <- dplyr::tibble(
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

  drug_strength <- dplyr::tibble(
    ingredient_concept_id = c(1, 1, 1, 1),
    drug_concept_id = c(1, 2, 3, 4),
    amount_value = c(1, 2, 3, 4),
    amount_unit_concept_id = c(8576, 8576, 8576, 8576)
  )

  cdm <- mockDrugUtilisation(drug_exposure = drug_exposure,
                             drug_strength = drug_strength)

  result <- instantiateDrugUtilisationCohorts(
    cdm,
    ingredientConceptId = 1,
    gapEra = 3,
    eraJoinMode = "Previous",
    overlapMode = "Sum",
    sameIndexMode = "Sum",
    imputeDuration = "eliminate",
    imputeDailyDose = "eliminate",
    verbose = FALSE,
    cohortEntryPriorHistory = 0
  )
  result <- result$dose %>% dplyr::collect()

  #test exposed days
  expect_true(result$exposed_days == as.integer(
    difftime(as.Date("2013-09-05"), as.Date("2010-01-01")) + 1))


  #test initial and cumulative dose
  testInitialDose <- computeDailyDose(
    table = cdm$drug_exposure,
    cdm = cdm,
    ingredient_concept_id = 1,
    verbose = FALSE
  ) %>%
    dplyr::select(-"days_exposed")

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
    eraJoinMode = "Previous",
    sameIndexMode = "Sum",
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

  drug_exposure <- dplyr::tibble(
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

  drug_strength <- dplyr::tibble(
    ingredient_concept_id = c(1, 1, 1),
    drug_concept_id = c(1, 2, 3),
    amount_value = c(NA, 1000, 0),
    amount_unit_concept_id = c(8576, 8576, 8576)
  )



  cdm <- mockDrugUtilisation(drug_exposure = drug_exposure,
                             drug_strength = drug_strength)


  eraJoinMode = "Subsequent"
  overlapMode = "Previous"

  #when there is no exp with same start, changing sameIndexMode should return same result
  result <- instantiateDrugUtilisationCohorts(
    cdm,
    ingredientConceptId = 1,
    summarizeMode = "FixedTime",
    studyTime = 1350, #limit end date to 2013-09-11
    gapEra = 3,
    eraJoinMode = eraJoinMode,
    overlapMode = overlapMode,
    sameIndexMode = "Maximum",
    imputeDuration = "eliminate",
    imputeDailyDose = "eliminate",
    durationRange = c(1, 3000),
    dailyDoseRange = c(0, 2000),
    verbose = FALSE,
    cohortEntryPriorHistory = 0
  )

  result <- result$dose %>% dplyr::collect()

  expect_true(result$exposed_days == 1350)

  expect_true(result$cumulative_dose == 2000/
                (as.numeric(difftime(as.Date("2015-01-01"), as.Date("2010-01-01")))+1)*1350)

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("test same index", {

  drug_exposure <- dplyr::tibble(
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

  drug_strength <- dplyr::tibble(
    ingredient_concept_id = c(1, 1, 1),
    drug_concept_id = c(1, 2, 3),
    amount_value = c(1, 10, 20),
    amount_unit_concept_id = c(8576, 8576, 8576)
  )



  cdm <- mockDrugUtilisation(drug_exposure = drug_exposure,
                             drug_strength = drug_strength)


  eraJoinMode = "Subsequent"
  overlapMode = "Previous"

  #when there is no exp with same start, changing sameIndexMode should return same result
  resultMax <- instantiateDrugUtilisationCohorts(
    cdm,
    ingredientConceptId = 1,
    summarizeMode = "FixedTime",
    studyTime = 1000, #limit end date to 2013-09-11
    gapEra = 10,
    eraJoinMode = eraJoinMode,
    overlapMode = overlapMode,
    sameIndexMode = "Maximum",
    imputeDuration = "eliminate",
    verbose = FALSE,
    cohortEntryPriorHistory = 0
  )

  resultMax <- resultMax$dose %>% dplyr::collect()

  resultMin <- instantiateDrugUtilisationCohorts(
    cdm,
    ingredientConceptId = 1,
    summarizeMode = "FixedTime",
    studyTime = 1000, #limit end date to 2013-09-11
    gapEra = 10,
    eraJoinMode = eraJoinMode,
    overlapMode = overlapMode,
    sameIndexMode = "Minimum",
    imputeDuration = "eliminate",
    imputeDailyDose = "eliminate",
    verbose = FALSE
  )

  resultMin <- resultMin$dose %>% dplyr::collect()

  resultSum <- instantiateDrugUtilisationCohorts(
    cdm,
    ingredientConceptId = 1,
    summarizeMode = "FixedTime",
    studyTime = 1000, #limit end date to 2013-09-11
    gapEra = 10,
    eraJoinMode = eraJoinMode,
    overlapMode = overlapMode,
    sameIndexMode = "Sum",
    imputeDuration = "eliminate",
    imputeDailyDose = "eliminate",
    verbose = FALSE,
    cohortEntryPriorHistory = 0
  )

  resultSum <- resultSum$dose %>% dplyr::collect()

  expect_true(resultMax$cumulative_dose == 60)

  expect_true(resultSum$cumulative_dose == 1 * 1 + 2 * 10 + 3 * 20)

  expect_true(resultMin$cumulative_dose == 1 / 2 * 2 + 2 * 10 / 5 * 3 + 3 * 20 / 10 * 5)

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("test same index for join exp", {

  drug_exposure <- dplyr::tibble(
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

  drug_strength <- dplyr::tibble(
    ingredient_concept_id = c(1, 1, 1),
    drug_concept_id = c(1, 2, 3),
    amount_value = c(1, 10, 20),
    amount_unit_concept_id = c(8576, 8576, 8576)
  )



  cdm <- mockDrugUtilisation(drug_exposure = drug_exposure,
                             drug_strength = drug_strength)



  eraJoinMode = "Previous"
  overlapMode = "Previous"

  #when there is no exp with same start, changing sameIndexMode should return same result
  resultMax <- instantiateDrugUtilisationCohorts(
    cdm,
    ingredientConceptId = 1,
    summarizeMode = "FixedTime",
    studyTime = 1000, #limit end date to 2013-09-11
    gapEra = 10,
    eraJoinMode = eraJoinMode,
    overlapMode = overlapMode,
    sameIndexMode = "Maximum",
    cohortEntryPriorHistory = 0,
    imputeDuration = "eliminate",
    imputeDailyDose = "eliminate",
    verbose = FALSE
  )

  resultMax <- resultMax$dose %>% dplyr::collect()

  resultMin <- instantiateDrugUtilisationCohorts(
    cdm,
    ingredientConceptId = 1,
    summarizeMode = "FixedTime",
    studyTime = 1000, #limit end date to 2013-09-11
    gapEra = 10,
    eraJoinMode = eraJoinMode,
    overlapMode = overlapMode,
    sameIndexMode = "Minimum",
    cohortEntryPriorHistory = 0,
    imputeDuration = "eliminate",
    imputeDailyDose = "eliminate",
    verbose = FALSE
  )

  resultMin <- resultMin$dose %>% dplyr::collect()

  resultSum <- instantiateDrugUtilisationCohorts(
    cdm,
    ingredientConceptId = 1,
    summarizeMode = "FixedTime",
    studyTime = 1000, #limit end date to 2013-09-11
    gapEra = 10,
    eraJoinMode = eraJoinMode,
    overlapMode = overlapMode,
    sameIndexMode = "Sum",
    imputeDuration = "eliminate",
    imputeDailyDose = "eliminate",
    verbose = FALSE
  )

  resultSum <- resultSum$dose %>% dplyr::collect()

  #
    expect_true(resultMax$cumulative_dose == 20 + 20 / 3 + 20 * 3)

    expect_true(resultMin$cumulative_dose == 1 + 1 / 3 + 20 * 3)

    expect_true(resultSum$cumulative_dose == 21 + 21 / 3 + 20 * 3)

    DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("test era join mode", {

  drug_exposure <- dplyr::tibble(
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

  drug_strength <- dplyr::tibble(
    ingredient_concept_id = c(1, 1),
    drug_concept_id = c(1, 2),
    amount_value = c(1, 10),
    amount_unit_concept_id = c(8576, 8576)
  )



  cdm <- mockDrugUtilisation(drug_exposure = drug_exposure,
                             drug_strength = drug_strength)


  sameIndexMode = "Maximum"
  overlapMode = "Previous"

  #when there is no exp with same start, changing sameIndexMode should return same result
  resultFirst <- instantiateDrugUtilisationCohorts(
    cdm,
    ingredientConceptId = 1,
    summarizeMode = "FixedTime",
    studyTime = 1000, #limit end date to 2013-09-11
    gapEra = 10,
    eraJoinMode = "Previous",
    overlapMode = overlapMode,
    sameIndexMode = sameIndexMode,
    imputeDuration = "eliminate",
    imputeDailyDose = "eliminate",
    verbose = FALSE,
    cohortEntryPriorHistory = 0
  )

  resultFirst <- resultFirst$dose %>% dplyr::collect()

  resultSecond <- instantiateDrugUtilisationCohorts(
    cdm,
    ingredientConceptId = 1,
    summarizeMode = "FixedTime",
    studyTime = 1000, #limit end date to 2013-09-11
    gapEra = 10,
    eraJoinMode = "Subsequent",
    overlapMode = overlapMode,
    sameIndexMode = sameIndexMode,
    imputeDuration = "eliminate",
    imputeDailyDose = "eliminate",
    cohortEntryPriorHistory = 0,
    verbose = FALSE
  )

  resultSecond <- resultSecond$dose %>% dplyr::collect()

  resultZero <- instantiateDrugUtilisationCohorts(
    cdm,
    ingredientConceptId = 1,
    summarizeMode = "FixedTime",
    studyTime = 1000, #limit end date to 2013-09-11
    gapEra = 10,
    eraJoinMode = "Zero",
    overlapMode = overlapMode,
    sameIndexMode = sameIndexMode,
    imputeDuration = "eliminate",
    imputeDailyDose = "eliminate",
    cohortEntryPriorHistory = 0,
    verbose = FALSE
  )

  resultZero <- resultZero$dose %>% dplyr::collect()

  resultJoin <- instantiateDrugUtilisationCohorts(
    cdm,
    ingredientConceptId = 1,
    summarizeMode = "FixedTime",
    studyTime = 1000, #limit end date to 2013-09-11
    gapEra = 10,
    eraJoinMode = "Join",
    overlapMode = overlapMode,
    sameIndexMode = sameIndexMode,
    imputeDuration = "eliminate",
    imputeDailyDose = "eliminate",
    cohortEntryPriorHistory = 0,
    verbose = FALSE
  )

  resultJoin <- resultJoin$dose %>% dplyr::collect()

  expect_true(resultFirst$cumulative_dose == 1 * 1 /2 * 4 + 2 * 10)

  expect_true(resultSecond$cumulative_dose == 1 * 1 /2 * 2 + 2 * 10 / 6 * 8)

  expect_true(resultJoin$cumulative_dose == 1 * 1 + 2 * 10)

  expect_true(resultZero$cumulative_dose == 1 * 1 + 2 * 10)

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("test overlap mode", {

  drug_exposure <- dplyr::tibble(
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

  drug_strength <- dplyr::tibble(
    ingredient_concept_id = c(1, 1),
    drug_concept_id = c(1, 2),
    amount_value = c(1, 10),
    amount_unit_concept_id = c(8576, 8576)
  )



  cdm <- mockDrugUtilisation(drug_exposure = drug_exposure,
                             drug_strength = drug_strength)


  sameIndexMode = "Maximum"
  eraJoinMode = "Previous"

  #when there is no exp with same start, changing sameIndexMode should return same result
  resultSecond <- instantiateDrugUtilisationCohorts(
    cdm,
    ingredientConceptId = 1,
    summarizeMode = "FixedTime",
    studyTime = 1000, #limit end date to 2013-09-11
    gapEra = 10,
    eraJoinMode = eraJoinMode,
    overlapMode = "Subsequent",
    sameIndexMode = sameIndexMode,
    imputeDuration = "eliminate",
    imputeDailyDose = "eliminate",
    cohortEntryPriorHistory = 0,
    verbose = FALSE
  )

  resultSecond <- resultSecond$dose %>% dplyr::collect()

  resultMin <- instantiateDrugUtilisationCohorts(
    cdm,
    ingredientConceptId = 1,
    summarizeMode = "FixedTime",
    studyTime = 1000, #limit end date to 2013-09-11
    gapEra = 10,
    eraJoinMode = eraJoinMode,
    overlapMode = "Minimum",
    sameIndexMode = sameIndexMode,
    imputeDuration = "eliminate",
    imputeDailyDose = "eliminate",
    cohortEntryPriorHistory = 0,
    verbose = FALSE
  )

  resultMin <- resultMin$dose %>% dplyr::collect()

  expect_true(resultSecond$cumulative_dose == 1 / 2 + 20)

 # expect_true(resultMin$cumulative_dose == 1 + 20 / 9 * 8)

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("test StudyStartDate and StudyEndDate", {

  drug_exposure <- dplyr::tibble(
    drug_exposure_id = c(1, 2, 3, 4, 5),
    drug_concept_id = c(1, 2, 3, 4, 5),
    person_id = c(1, 1, 2, 2, 3),
    drug_exposure_start_date = c(
      as.Date("2010-01-01"),
      as.Date("2010-01-02"),
      as.Date("2010-01-03"),
      as.Date("2010-02-02"),
      as.Date("2010-02-01")),
    drug_exposure_end_date = c(
      as.Date("2010-01-02"),
      as.Date("2010-01-10"),
      as.Date("2010-02-04"),
      as.Date("2010-02-05"),
      as.Date("2010-02-10")),
    quantity = c(1, 2, 3, 4, 5))

  drug_strength <- dplyr::tibble(
    ingredient_concept_id = c(1, 1, 1, 1, 1),
    drug_concept_id = c(1, 2, 3, 4, 5),
    amount_value = c(1, 2, 3, 4, 5),
  )



  cdm <- mockDrugUtilisation(drug_exposure = drug_exposure,
                             drug_strength = drug_strength)

  sameIndexMode = "Maximum"
  eraJoinMode = "Previous"

  #when there is no exp with same start, changing sameIndexMode should return same result
  resultStudyStart <- instantiateDrugUtilisationCohorts(
    cdm,
    ingredientConceptId = 1,
    summarizeMode = "FixedTime",
    studyTime = 1000, #limit end date to 2013-09-11
    gapEra = 10,
    eraJoinMode = eraJoinMode,
    overlapMode = "Subsequent",
    sameIndexMode = sameIndexMode,
    imputeDuration = "eliminate",
    imputeDailyDose = "eliminate",
    studyStartDate = as.Date("2010-01-02"),
    studyEndDate = as.Date("2010-01-05"),
    cohortEntryPriorHistory = 0,
    verbose = FALSE
  )

  resultStudyStart <- resultStudyStart$dose %>%
    dplyr::collect()

  expect_true(resultStudyStart$subject_id == 2)

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("test multi gap end", {

  drug_exposure <- dplyr::tibble(
    drug_exposure_id = c(1, 2, 3, 4),
    drug_concept_id = c(1, 2, 3, 4),
    person_id = c(1, 1, 1, 1),
    drug_exposure_start_date = c(
      as.Date("2010-01-01"),
      as.Date("2010-01-05"),
      as.Date("2010-01-05"),
      as.Date("2010-01-05")),
    drug_exposure_end_date = c(
      as.Date("2010-01-02"),
      as.Date("2010-01-05"),
      as.Date("2010-01-06"),
      as.Date("2010-01-07")),
    quantity = c(1, 2, 3, 4))

  drug_strength <- dplyr::tibble(
    ingredient_concept_id = c(1, 1, 1, 1),
    drug_concept_id = c(1, 2, 3, 4),
    amount_value = c(1, 2, 3, 4),
  )

  cdm <- mockDrugUtilisation(drug_exposure = drug_exposure,
                             drug_strength = drug_strength)

  sameIndexMode = "Maximum"
  eraJoinMode = "Subsequent"

  #when there is no exp with same start, changing sameIndexMode should return same result
  resultMultiGapEnd <- instantiateDrugUtilisationCohorts(
    cdm,
    ingredientConceptId = 1,
    summarizeMode = "FixedTime",
    studyTime = 1000, #limit end date to 2013-09-11
    gapEra = 10,
    eraJoinMode = eraJoinMode,
    overlapMode = "Subsequent",
    sameIndexMode = sameIndexMode,
    imputeDuration = "eliminate",
    imputeDailyDose = "eliminate",
    cohortEntryPriorHistory = 0,
    verbose = FALSE
  )

  resultMultiGapEnd <- resultMultiGapEnd$dose %>%
    dplyr::collect()

  expect_true(resultMultiGapEnd$cumulative_dose == 16 * 2 / 3 + 16 + 1)


  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("test not considered dose", {

  cdm <- mockDrugUtilisation()

  #when there is no exp with same start, changing sameIndexMode should return same result
  results <- instantiateDrugUtilisationCohorts(
    cdm,
    ingredientConceptId = 1,
    eraJoinMode = "Zero",
    overlapMode = "Sum",
    sameIndexMode = "Sum",
    cohortEntryPriorHistory = 0
  )

  results <- results$dose %>%
    dplyr::collect()

  expect_true(all(results$not_considered_dose == 0))

  drug_exposure <- dplyr::tibble(
    drug_exposure_id = c(1, 2, 3, 4),
    drug_concept_id = c(1, 1, 1, 1),
    person_id = c(1, 1, 1, 1),
    drug_exposure_start_date = c(
      as.Date("2010-01-01"),
      as.Date("2010-01-05"),
      as.Date("2010-01-05"),
      as.Date("2010-01-05")),
    drug_exposure_end_date = c(
      as.Date("2010-01-02"),
      as.Date("2010-01-05"),
      as.Date("2010-01-06"),
      as.Date("2010-01-07")),
    quantity = c(2, 4, 6, 6)
  )

  drug_strength <- dplyr::tibble(
    ingredient_concept_id = 1,
    drug_concept_id = 1,
    amount_value = 10,
  )

  cdm <- mockDrugUtilisation(
    drug_exposure = drug_exposure,
    drug_strength = drug_strength
  )

  results <- instantiateDrugUtilisationCohorts(
    cdm,
    ingredientConceptId = 1,
    eraJoinMode = "Zero",
    overlapMode = "Sum",
    cohortEntryPriorHistory = 0,
    sameIndexMode = "Sum")

  results <- results$dose %>% dplyr::collect()

  expect_true(results$not_considered_dose == 0)
  expect_true(results$cumulative_dose == 180)
  expect_true(results$not_considered_exposed_days == 3)
  expect_true(results$number_days_gap == 2)
  expect_true(results$cumulative_gap_dose == 0)
  expect_true(results$exposed_days == 7)

  results <- instantiateDrugUtilisationCohorts(
    cdm,
    ingredientConceptId = 1,
    eraJoinMode = "Zero",
    overlapMode = "Sum",
    cohortEntryPriorHistory = 0,
    sameIndexMode = "Minimum")

  results <- results$dose %>% dplyr::collect()

  expect_true(results$not_considered_dose == 100)
  expect_true(results$cumulative_dose == 80)
  expect_true(results$not_considered_exposed_days == 3)
  expect_true(results$number_days_gap == 2)
  expect_true(results$cumulative_gap_dose == 0)
  expect_true(results$exposed_days == 7)

  results <- instantiateDrugUtilisationCohorts(
    cdm,
    ingredientConceptId = 1,
    eraJoinMode = "Zero",
    overlapMode = "Sum",
    cohortEntryPriorHistory = 0,
    sameIndexMode = "Maximum")

  results <- results$dose %>% dplyr::collect()

  expect_true(results$not_considered_dose == 70)
  expect_true(results$cumulative_dose == 110)
  expect_true(results$not_considered_exposed_days == 3)
  expect_true(results$number_days_gap == 2)
  expect_true(results$cumulative_gap_dose == 0)
  expect_true(results$exposed_days == 7)

  results <- instantiateDrugUtilisationCohorts(
    cdm,
    ingredientConceptId = 1,
    eraJoinMode = "Join",
    overlapMode = "Sum",
    cohortEntryPriorHistory = 0,
    sameIndexMode = "Sum")

  results <- results$dose %>% dplyr::collect()

  expect_true(results$not_considered_dose == 0)
  expect_true(results$cumulative_dose == 180)
  expect_true(results$not_considered_exposed_days == 3)
  expect_true(results$number_days_gap == 0)
  expect_true(results$cumulative_gap_dose == 0)
  expect_true(results$exposed_days == 5)

  results <- instantiateDrugUtilisationCohorts(
    cdm,
    ingredientConceptId = 1,
    eraJoinMode = "Join",
    overlapMode = "Sum",
    cohortEntryPriorHistory = 0,
    sameIndexMode = "Minimum")

  results <- results$dose %>% dplyr::collect()

  expect_true(results$not_considered_dose == 100)
  expect_true(results$cumulative_dose == 80)
  expect_true(results$not_considered_exposed_days == 3)
  expect_true(results$number_days_gap == 0)
  expect_true(results$cumulative_gap_dose == 0)
  expect_true(results$exposed_days == 5)

  results <- instantiateDrugUtilisationCohorts(
    cdm,
    ingredientConceptId = 1,
    eraJoinMode = "Join",
    overlapMode = "Sum",
    cohortEntryPriorHistory = 0,
    sameIndexMode = "Maximum")

  results <- results$dose %>% dplyr::collect()

  expect_true(results$not_considered_dose == 70)
  expect_true(results$cumulative_dose == 110)
  expect_true(results$not_considered_exposed_days == 3)
  expect_true(results$number_days_gap == 0)
  expect_true(results$cumulative_gap_dose == 0)
  expect_true(results$exposed_days == 5)

  results <- instantiateDrugUtilisationCohorts(
    cdm,
    ingredientConceptId = 1,
    eraJoinMode = "Previous",
    overlapMode = "Sum",
    cohortEntryPriorHistory = 0,
    sameIndexMode = "Sum")

  results <- results$dose %>% dplyr::collect()

  expect_true(results$not_considered_dose == 0)
  expect_true(results$cumulative_dose == 200)
  expect_true(results$not_considered_exposed_days == 3)
  expect_true(results$number_days_gap == 2)
  expect_true(results$cumulative_gap_dose == 20)
  expect_true(results$exposed_days == 7)

  results <- instantiateDrugUtilisationCohorts(
    cdm,
    ingredientConceptId = 1,
    eraJoinMode = "Previous",
    overlapMode = "Sum",
    cohortEntryPriorHistory = 0,
    sameIndexMode = "Minimum")

  results <- results$dose %>% dplyr::collect()

  expect_true(results$not_considered_dose == 100)
  expect_true(results$cumulative_dose == 100)
  expect_true(results$not_considered_exposed_days == 3)
  expect_true(results$number_days_gap == 2)
  expect_true(results$cumulative_gap_dose == 20)
  expect_true(results$exposed_days == 7)

  results <- instantiateDrugUtilisationCohorts(
    cdm,
    ingredientConceptId = 1,
    eraJoinMode = "Previous",
    overlapMode = "Sum",
    cohortEntryPriorHistory = 0,
    sameIndexMode = "Maximum")

  results <- results$dose %>% dplyr::collect()

  expect_true(results$not_considered_dose == 70)
  expect_true(results$cumulative_dose == 130)
  expect_true(results$not_considered_exposed_days == 3)
  expect_true(results$number_days_gap == 2)
  expect_true(results$cumulative_gap_dose == 20)
  expect_true(results$exposed_days == 7)

  results <- instantiateDrugUtilisationCohorts(
    cdm,
    ingredientConceptId = 1,
    eraJoinMode = "Subsequent",
    overlapMode = "Sum",
    cohortEntryPriorHistory = 0,
    sameIndexMode = "Sum")

  results <- results$dose %>% dplyr::collect()

  expect_true(results$not_considered_dose == 0)
  expect_true(results$cumulative_dose == 360)
  expect_true(results$not_considered_exposed_days == 3)
  expect_true(results$number_days_gap == 2)
  expect_true(results$cumulative_gap_dose == 180)
  expect_true(results$exposed_days == 7)

  results <- instantiateDrugUtilisationCohorts(
    cdm,
    ingredientConceptId = 1,
    eraJoinMode = "Subsequent",
    overlapMode = "Sum",
    sameIndexMode = "Minimum",
    cohortEntryPriorHistory = 0)

  results <- results$dose %>% dplyr::collect()

  expect_true(results$not_considered_dose == 100)
  expect_true(results$cumulative_dose == 120)
  expect_true(results$not_considered_exposed_days == 3)
  expect_true(results$number_days_gap == 2)
  expect_true(results$cumulative_gap_dose == 40)
  expect_true(results$exposed_days == 7)

  results <- instantiateDrugUtilisationCohorts(
    cdm,
    ingredientConceptId = 1,
    eraJoinMode = "Subsequent",
    overlapMode = "Sum",
    sameIndexMode = "Maximum",
    cohortEntryPriorHistory = 0)

  results <- results$dose %>% dplyr::collect()

  expect_true(results$not_considered_dose == 70)
  expect_true(results$cumulative_dose == 190)
  expect_true(results$not_considered_exposed_days == 3)
  expect_true(results$number_days_gap == 2)
  expect_true(results$cumulative_gap_dose == 80)
  expect_true(results$exposed_days == 7)

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})




test_that("test concept from json",{
  ConceptSetPath = here::here("extras/")

  concept_ancestor <- dplyr::tibble(
    ancestor_concept_id = c(43144132,43144132,43144132, 40008920, 40008920, 40008920),
    descendant_concept_id = c(1,2,3,4,5,6),
  )

  cdm <- mockDrugUtilisation(
    concept_ancestor = concept_ancestor,
  )
  test <- instantiateDrugUtilisationCohorts(cdm,
                                    ingredientConceptId = 1,
                                    ConceptSetPath,
                                    studyStartDate = NULL,
                                    studyEndDate = NULL,
                                    summarizeMode = "AllEras",
                                    cohortEntryPriorHistory = 180,
                                    gapEra = 30,
                                    eraJoinMode = "Previous",
                                    overlapMode = "Previous",
                                    sameIndexMode = "Sum",
                                    imputeDuration = "eliminate",
                                    imputeDailyDose = "eliminate",
                                    verbose = FALSE)



})


# test_that("test cohort profile", {
#   drug_exposure <- dplyr::tibble(
#     drug_exposure_id = c(1, 2, 3, 4),
#     drug_concept_id = c(1, 2, 3, 4),
#     person_id = c(1, 1, 1, 1),
#     drug_exposure_start_date = c(
#       as.Date("2010-01-01"),
#       as.Date("2010-01-05"),
#       as.Date("2010-01-05"),
#       as.Date("2010-01-05")
#     ),
#     drug_exposure_end_date = c(
#       as.Date("2010-01-02"),
#       as.Date("2010-01-05"),
#       as.Date("2010-01-06"),
#       as.Date("2010-01-07")
#     ),
#     quantity = c(1, 2, 3, 4)
#   )
#
#   drug_strength <- dplyr::tibble(
#     ingredientConceptId = c(1, 1, 1, 1),
#     drug_concept_id = c(1, 2, 3, 4),
#     amount_value = c(1, 2, 3, 4),
#   )
#
#   cdm <- mockDrugUtilisation(drug_exposure = drug_exposure,
#                              drug_strength = drug_strength)
#
#
#   sameIndexMode = "Maximum"
#   eraJoinMode = "Subsequent"
#
#   #when there is no exp with same start, changing sameIndexMode should return same result
#   resultMultiGapEnd <- instantiateDrugUtilisationCohorts(
#     cdm,
#     ingredientConceptId = 1,
#     summarizeMode = "FixedTime",
#     studyTime = 1000,
#     #limit end date to 2013-09-11
#     gapEra = 10,
#     eraJoinMode = eraJoinMode,
#     overlapMode = "Subsequent",
#     sameIndexMode = sameIndexMode,
#     imputeDuration = "eliminate",
#     imputeDailyDose = "eliminate",
#     cohortEntryPriorHistory = 180,
#     verbose = FALSE
#   )
#
#   resultMultiGapEnd <-
#     resultMultiGapEnd$dose %>%
#     dplyr::collect()
#
#
#   DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
# })


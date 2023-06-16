
test_that("test input parameters errors", {
  skip_on_cran()
  cdm <- mockDrugUtilisation(
    connectionDetails,
    drug_strength = dplyr::tibble(
      drug_concept_id = c(1, 2, 3, 4),
      ingredient_concept_id = c(1, 1, 1, 1),
      amount_value = c(10, 20, 30, 40),
      amount_unit_concept_id = c(8576, 8576, 8576, 8576)
    ),
    cohort1 = dplyr::tibble(
      cohort_definition_id = c(1, 1, 1, 2),
      subject_id = c(1, 1, 1, 2),
      cohort_start_date = as.Date(c(
        "2020-01-01", "2022-01-01", "2020-01-01", "2020-01-01"
      )),
      cohort_end_date = as.Date(c(
        "2020-05-01", "2022-09-06", "2020-05-01", "2020-05-01"
      ))
    )
  )

  expect_error(addDrugUse())
  expect_error(addDrugUse(cdm = cdm))
})

test_that("test overlapMode", {
  skip_on_cran()
  cdm <- mockDrugUtilisation(
    connectionDetails,
    drug_exposure = dplyr::tibble(
      drug_exposure_id = 1:9,
      person_id = c(1, 1, 1, 1, 1, 2, 2, 2, 2),
      drug_concept_id = c(2, 3, 4, 4, 3, 2, 2, 3, 5),
      drug_exposure_start_date = as.Date(c(
        "2000-01-01", "2000-01-10", "2000-02-20", "2001-01-01", "2001-02-10",
        "2000-01-10", "2000-01-15", "2000-02-15", "2000-01-15"
      )),
      drug_exposure_end_date = as.Date(c(
        "2000-02-10", "2000-03-01", "2000-02-20", "2001-01-15", "2001-03-01",
        "2000-01-25", "2000-02-05", "2000-02-15", "2000-02-05"
      )),
      quantity = c(41, 52, 1, 15, 20, 16, 22, 1, 22)
    ),
    concept = dplyr::tibble(
      concept_id = c(1, 2, 3, 4, 5, 8576),
      concept_name = c("ingredient1", "drug2", "drug3", "drug4", "drug5", "milligram"),
      domain_id = c(rep("Drug", 5), "Unit"),
      vocabulary_id = c(rep("RxNorm", 5), "Unit"),
      standard_concept = "S",
      concept_class_id = c("Ingredient", rep("Drug", 4), "Unit"),
    ),
    concept_ancestor = dplyr::tibble(
      ancestor_concept_id = 1,
      descendant_concept_id = 2:5
    ),
    drug_strength = dplyr::tibble(
      drug_concept_id = c(2, 3, 4, 5),
      ingredient_concept_id = c(1, 1, 1, 1),
      amount_value = c(10, 20, 30, 40),
      amount = c("numeric", "numeric", "numeric", "numeric"),
      amount_unit_concept_id = c(8576, 8576, 8576, 8576),
      numerator_value = as.numeric(NA),
      numerator = as.character(NA),
      numerator_unit_concept_id = as.numeric(NA),
      denominator_value = as.numeric(NA),
      denominator = as.character(NA),
      denominator_unit_concept_id = as.numeric(NA)
    ),
    cohort1 = dplyr::tibble(
      cohort_definition_id = 1,
      subject_id = c(1, 1, 2),
      cohort_start_date = as.Date(c("2000-01-01", "2001-01-01", "2000-01-01")),
      cohort_end_date = as.Date(c("2000-03-01", "2001-03-01", "2000-03-01"))
    )
  )
  variables <- c(
    "exposed_days", "unexposed_days", "not_considered_days", "first_era_days",
    "number_exposures", "number_subexposures", "number_continuous_exposures",
    "number_eras", "number_gaps", "number_unexposed_periods",
    "number_subexposures_overlap", "number_eras_overlap",
    "number_continuous_exposure_overlap", "initial_daily_dose",
    "sum_all_exposed_dose", "sum_all_exposed_days", "duration", "gap_days",
    "number_subexposures_no_overlap", "number_eras_no_overlap",
    "number_continuous_exposures_no_overlap",
    "cumulative_dose", "cumulative_gap_dose", "cumulative_not_considered_dose"
  )

  # prev
  suppressWarnings(x <- addDrugUse(
    cohort = cdm$cohort1,
    cdm = cdm,
    ingredientConceptId = 1,
    supplementary = TRUE,
    gapEra = 30,
    eraJoinMode = "Previous",
    overlapMode = "Previous",
    sameIndexMode = "Sum",
    imputeDuration = "eliminate",
    imputeDailyDose = "eliminate",
    durationRange = c(1, Inf),
    dailyDoseRange = c(0, Inf)
  ))
  value_cohort_1 <- c(
    61, 0, 33, 61, 3, 5, 1, 1, 0, 0, 2, 1, 1, 10, 41 * 10 + 52 * 20 + 30, 41 + 52 + 1, 61,
    0, 3, 0, 0,
    41 * 10 + 20 * 20, 0, 32 * 20 + 30
  )
  xx <- x %>%
    dplyr::collect() %>%
    dplyr::filter(
      subject_id == 1 & cohort_start_date == as.Date("2000-01-01")
    )
  for (k in 1:length(value_cohort_1)) {
    expect_true(xx[[variables[k]]] == value_cohort_1[k])
  }

  # sub
  suppressWarnings(x <- addDrugUse(
    cohort = cdm$cohort1,
    cdm = cdm,
    ingredientConceptId = 1,
    supplementary = TRUE,
    gapEra = 30,
    eraJoinMode = "Previous",
    overlapMode = "Subsequent",
    sameIndexMode = "Sum",
    imputeDuration = "eliminate",
    imputeDailyDose = "eliminate",
    durationRange = c(1, Inf),
    dailyDoseRange = c(0, Inf)
  ))
  value_cohort_1 <- c(
    61, 0, 33, 61, 3, 5, 1, 1, 0, 0, 2, 1, 1, 10, 41 * 10 + 52 * 20 + 30, 41 + 52 + 1, 61,
    0, 3, 0, 0,
    10 * 9 + 20 * 51 + 30 * 1, 0, 10 * 32 + 20 * 1 + 30 * 0
  )
  xx <- x %>%
    dplyr::collect() %>%
    dplyr::filter(
      subject_id == 1 & cohort_start_date == as.Date("2000-01-01")
    )
  for (k in 1:length(value_cohort_1)) {
    expect_true(xx[[variables[k]]] == value_cohort_1[k])
  }

  # min
  suppressWarnings(x <- addDrugUse(
    cohort = cdm$cohort1,
    cdm = cdm,
    ingredientConceptId = 1,
    supplementary = TRUE,
    gapEra = 30,
    eraJoinMode = "Previous",
    overlapMode = "Minimum",
    sameIndexMode = "Sum",
    imputeDuration = "eliminate",
    imputeDailyDose = "eliminate",
    durationRange = c(1, Inf),
    dailyDoseRange = c(0, Inf)
  ))
  value_cohort_1 <- c(
    61, 0, 33, 61, 3, 5, 1, 1, 0, 0, 2, 1, 1, 10, 41 * 10 + 52 * 20 + 30, 41 + 52 + 1, 61,
    0, 3, 0, 0,
    10 * 41 + 20 * 20 + 30 * 0, 0, 10 * 0 + 20 * 32 + 30 * 1
  )
  xx <- x %>%
    dplyr::collect() %>%
    dplyr::filter(
      subject_id == 1 & cohort_start_date == as.Date("2000-01-01")
    )
  for (k in 1:length(value_cohort_1)) {
    expect_true(xx[[variables[k]]] == value_cohort_1[k])
  }

  # max
  suppressWarnings(x <- addDrugUse(
    cohort = cdm$cohort1,
    cdm = cdm,
    ingredientConceptId = 1,
    supplementary = TRUE,
    gapEra = 30,
    eraJoinMode = "Previous",
    overlapMode = "Maximum",
    sameIndexMode = "Sum",
    imputeDuration = "eliminate",
    imputeDailyDose = "eliminate",
    durationRange = c(1, Inf),
    dailyDoseRange = c(0, Inf)
  ))
  value_cohort_1 <- c(
    61, 0, 33, 61, 3, 5, 1, 1, 0, 0, 2, 1, 1, 10, 41 * 10 + 52 * 20 + 30, 41 + 52 + 1, 61,
    0, 3, 0, 0,
    9 * 10 + 51 * 20 + 1 * 30, 0, 32 * 10 + 1 * 20 + 0 * 30
  )
  xx <- x %>%
    dplyr::collect() %>%
    dplyr::filter(
      subject_id == 1 & cohort_start_date == as.Date("2000-01-01")
    )
  for (k in 1:length(value_cohort_1)) {
    expect_true(xx[[variables[k]]] == value_cohort_1[k])
  }

  # sum
  suppressWarnings(x <- addDrugUse(
    cohort = cdm$cohort1,
    cdm = cdm,
    ingredientConceptId = 1,
    supplementary = TRUE,
    gapEra = 30,
    eraJoinMode = "Previous",
    overlapMode = "Sum",
    sameIndexMode = "Sum",
    imputeDuration = "eliminate",
    imputeDailyDose = "eliminate",
    durationRange = c(1, Inf),
    dailyDoseRange = c(0, Inf)
  ))
  value_cohort_1 <- c(
    61, 0, 0, 61, 3, 5, 1, 1, 0, 0, 2, 1, 1, 10, 41 * 10 + 52 * 20 + 30, 41 + 52 + 1, 61,
    0, 3, 0, 0,
    41 * 10 + 52 * 20 + 1 * 30, 0, 0
  )
  xx <- x %>%
    dplyr::collect() %>%
    dplyr::filter(
      subject_id == 1 & cohort_start_date == as.Date("2000-01-01")
    )
  for (k in 1:length(value_cohort_1)) {
    expect_true(xx[[variables[k]]] == value_cohort_1[k])
  }
})

test_that("test gapEra and eraJoinMode", {
  skip_on_cran()
  cdm <- mockDrugUtilisation(
    connectionDetails,
    drug_exposure = dplyr::tibble(
      drug_exposure_id = 1:9,
      person_id = c(1, 1, 1, 1, 1, 2, 2, 2, 2),
      drug_concept_id = c(2, 3, 4, 4, 3, 4, 2, 3, 5),
      drug_exposure_start_date = as.Date(c(
        "2000-01-01", "2000-01-10", "2000-02-20", "2001-01-01", "2001-02-10",
        "2000-01-10", "2000-01-15", "2000-02-15", "2000-01-15"
      )),
      drug_exposure_end_date = as.Date(c(
        "2000-02-10", "2000-03-01", "2000-02-20", "2001-01-15", "2001-03-01",
        "2000-01-25", "2000-02-05", "2000-02-15", "2000-02-05"
      )),
      quantity = c(41, 52, 1, 15, 20, 16, 22, 1, 22)
    ),
    drug_strength = dplyr::tibble(
      drug_concept_id = c(2, 3, 4, 5),
      ingredient_concept_id = c(1, 1, 1, 1),
      amount_value = c(10, 20, 30, 40),
      amount = c("numeric", "numeric", "numeric", "numeric"),
      amount_unit_concept_id = c(8576, 8576, 8576, 8576),
      numerator_value = as.numeric(NA),
      numerator = as.character(NA),
      numerator_unit_concept_id = as.numeric(NA),
      denominator_value = as.numeric(NA),
      denominator = as.character(NA),
      denominator_unit_concept_id = as.numeric(NA)
    ),
    concept = dplyr::tibble(
      concept_id = c(1, 2, 3, 4, 5, 8576),
      concept_name = c("ingredient1", "drug2", "drug3", "drug4", "drug5", "milligram"),
      domain_id = c(rep("Drug", 5), "Unit"),
      vocabulary_id = c(rep("RxNorm", 5), "Unit"),
      standard_concept = "S",
      concept_class_id = c("Ingredient", rep("Drug", 4), "Unit"),
    ),
    concept_ancestor = dplyr::tibble(
      ancestor_concept_id = 1,
      descendant_concept_id = 2:5
    ),
    cohort1 = dplyr::tibble(
      cohort_definition_id = 1,
      subject_id = c(1, 1, 2),
      cohort_start_date = as.Date(c("2000-01-01", "2001-01-01", "2000-01-01")),
      cohort_end_date = as.Date(c("2000-03-01", "2001-03-01", "2000-03-01"))
    )
  )

  variables <- c(
    "exposed_days", "unexposed_days", "not_considered_days", "first_era_days",
    "number_exposures", "number_subexposures", "number_continuous_exposures",
    "number_eras", "number_gaps", "number_unexposed_periods",
    "number_subexposures_overlap", "number_eras_overlap",
    "number_continuous_exposure_overlap", "initial_daily_dose",
    "sum_all_exposed_dose", "sum_all_exposed_days", "duration", "gap_days",
    "number_subexposures_no_overlap", "number_eras_no_overlap",
    "number_continuous_exposures_no_overlap",
    "cumulative_dose", "cumulative_gap_dose", "cumulative_not_considered_dose"
  )

  # overall functionality
  suppressWarnings(x <- addDrugUse(
    cohort = cdm$cohort1,
    cdm = cdm,
    ingredientConceptId = 1,
    supplementary = TRUE,
    gapEra = 0,
    eraJoinMode = "Previous",
    overlapMode = "Sum",
    sameIndexMode = "Sum",
    imputeDuration = "eliminate",
    imputeDailyDose = "eliminate",
    durationRange = c(1, Inf),
    dailyDoseRange = c(0, Inf)
  ))
  values <- c(
    35, 25, 0, 15, 2, 3, 2,
    2, 0, 1,
    0, 0, 0, 30, 15 * 30 + 20 * 20, 15 + 20, 60,
    0, 3, 2, 2,
    15 * 30 + 20 * 20, 0, 0
  )
  xx <- x %>%
    dplyr::collect() %>%
    dplyr::filter(
      subject_id == 1 & cohort_start_date == as.Date("2001-01-01")
    )
  for (k in 1:length(values)) {
    expect_true(xx[[variables[k]]] == values[k])
  }

  # gapEra = 24
  suppressWarnings(x <- addDrugUse(
    cohort = cdm$cohort1,
    cdm = cdm,
    ingredientConceptId = 1,
    supplementary = TRUE,
    gapEra = 24,
    eraJoinMode = "Previous",
    overlapMode = "Sum",
    sameIndexMode = "Sum",
    imputeDuration = "eliminate",
    imputeDailyDose = "eliminate",
    durationRange = c(1, Inf),
    dailyDoseRange = c(0, Inf)
  ))
  values <- c(
    35, 25, 0, 15, 2, 3, 2,
    2, 0, 1,
    0, 0, 0, 30, 15 * 30 + 20 * 20, 15 + 20, 60,
    0, 3, 2, 2,
    15 * 30 + 20 * 20, 0, 0
  )
  xx <- x %>%
    dplyr::collect() %>%
    dplyr::filter(
      subject_id == 1 & cohort_start_date == as.Date("2001-01-01")
    )
  for (k in 1:length(values)) {
    expect_true(xx[[variables[k]]] == values[k])
  }

  # gapEra = 25 & joinMode = Zero
  suppressWarnings(x <- addDrugUse(
    cohort = cdm$cohort1,
    cdm = cdm,
    ingredientConceptId = 1,
    supplementary = TRUE,
    gapEra = 25,
    eraJoinMode = "Zero",
    overlapMode = "Sum",
    sameIndexMode = "Sum",
    imputeDuration = "eliminate",
    imputeDailyDose = "eliminate",
    durationRange = c(1, Inf),
    dailyDoseRange = c(0, Inf)
  ))
  values <- c(
    35, 0, 0, 60, 2, 3, 2,
    1, 1, 0,
    0, 0, 0, 30, 15 * 30 + 20 * 20, 15 + 20, 60,
    25, 3, 1, 2,
    15 * 30 + 20 * 20, 0, 0
  )
  xx <- x %>%
    dplyr::collect() %>%
    dplyr::filter(
      subject_id == 1 & cohort_start_date == as.Date("2001-01-01")
    )
  for (k in 1:length(values)) {
    expect_true(xx[[variables[k]]] == values[k])
  }

  # gapEra = 25 & joinMode = Previous
  suppressWarnings(x <- addDrugUse(
    cohort = cdm$cohort1,
    cdm = cdm,
    ingredientConceptId = 1,
    supplementary = TRUE,
    gapEra = 25,
    eraJoinMode = "Previous",
    overlapMode = "Sum",
    sameIndexMode = "Sum",
    imputeDuration = "eliminate",
    imputeDailyDose = "eliminate",
    durationRange = c(1, Inf),
    dailyDoseRange = c(0, Inf)
  ))
  values <- c(
    35, 0, 0, 60, 2, 3, 2,
    1, 1, 0,
    0, 0, 0, 30, 15 * 30 + 20 * 20, 15 + 20, 60,
    25, 3, 1, 2,
    15 * 30 + 20 * 20 + 25 * 30, 25 * 30, 0
  )
  xx <- x %>%
    dplyr::collect() %>%
    dplyr::filter(
      subject_id == 1 & cohort_start_date == as.Date("2001-01-01")
    )
  for (k in 1:length(values)) {
    expect_true(xx[[variables[k]]] == values[k])
  }

  # gapEra = 25 & joinMode = Subsequent
  suppressWarnings(x <- addDrugUse(
    cohort = cdm$cohort1,
    cdm = cdm,
    ingredientConceptId = 1,
    supplementary = TRUE,
    gapEra = 25,
    eraJoinMode = "Subsequent",
    overlapMode = "Sum",
    sameIndexMode = "Sum",
    imputeDuration = "eliminate",
    imputeDailyDose = "eliminate",
    durationRange = c(1, Inf),
    dailyDoseRange = c(0, Inf)
  ))
  values <- c(
    35, 0, 0, 60, 2, 3, 2,
    1, 1, 0,
    0, 0, 0, 30, 15 * 30 + 20 * 20, 15 + 20, 60,
    25, 3, 1, 2,
    15 * 30 + 20 * 20 + 25 * 20, 25 * 20, 0
  )
  xx <- x %>%
    dplyr::collect() %>%
    dplyr::filter(
      subject_id == 1 & cohort_start_date == as.Date("2001-01-01")
    )
  for (k in 1:length(values)) {
    expect_true(xx[[variables[k]]] == values[k])
  }
})

test_that("test gapEra, eraJoinMode & sameIndexOverlap", {
  skip_on_cran()
  cdm <- mockDrugUtilisation(
    connectionDetails,
    drug_exposure = dplyr::tibble(
      drug_exposure_id = 1:9,
      person_id = c(1, 1, 1, 1, 1, 2, 2, 2, 2),
      drug_concept_id = c(2, 3, 4, 4, 3, 4, 2, 3, 5),
      drug_exposure_start_date = as.Date(c(
        "2000-01-01", "2000-01-10", "2000-02-20", "2001-01-01", "2001-02-10",
        "2000-01-10", "2000-01-15", "2000-02-15", "2000-01-15"
      )),
      drug_exposure_end_date = as.Date(c(
        "2000-02-10", "2000-03-01", "2000-02-20", "2001-01-15", "2001-03-01",
        "2000-01-25", "2000-02-05", "2000-02-15", "2000-02-05"
      )),
      quantity = c(41, 52, 1, 15, 20, 16, 22, 1, 22)
    ),
    drug_strength = dplyr::tibble(
      drug_concept_id = c(2, 3, 4, 5),
      ingredient_concept_id = c(1, 1, 1, 1),
      amount_value = c(10, 20, 30, 40),
      amount = c("numeric", "numeric", "numeric", "numeric"),
      amount_unit_concept_id = c(8576, 8576, 8576, 8576),
      numerator_value = as.numeric(NA),
      numerator = as.character(NA),
      numerator_unit_concept_id = as.numeric(NA),
      denominator_value = as.numeric(NA),
      denominator = as.character(NA),
      denominator_unit_concept_id = as.numeric(NA)
    ),
    concept = dplyr::tibble(
      concept_id = c(1, 2, 3, 4, 5, 8576),
      concept_name = c("ingredient1", "drug2", "drug3", "drug4", "drug5", "milligram"),
      domain_id = c(rep("Drug", 5), "Unit"),
      vocabulary_id = c(rep("RxNorm", 5), "Unit"),
      standard_concept = "S",
      concept_class_id = c("Ingredient", rep("Drug", 4), "Unit"),
    ),
    concept_ancestor = dplyr::tibble(
      ancestor_concept_id = 1,
      descendant_concept_id = 2:5
    ),
    cohort1 = dplyr::tibble(
      cohort_definition_id = 1,
      subject_id = c(1, 1, 2),
      cohort_start_date = as.Date(c("2000-01-01", "2001-01-01", "2000-01-01")),
      cohort_end_date = as.Date(c("2000-03-01", "2001-03-01", "2000-03-01"))
    )
  )

  variables <- c(
    "exposed_days", "unexposed_days", "not_considered_days", "first_era_days",
    "number_exposures", "number_subexposures", "number_continuous_exposures",
    "number_eras", "number_gaps", "number_unexposed_periods",
    "number_subexposures_overlap", "number_eras_overlap",
    "number_continuous_exposure_overlap", "initial_daily_dose",
    "sum_all_exposed_dose", "sum_all_exposed_days", "duration", "gap_days",
    "number_subexposures_no_overlap", "number_eras_no_overlap",
    "number_continuous_exposures_no_overlap",
    "cumulative_dose", "cumulative_gap_dose", "cumulative_not_considered_dose"
  )

  # overall functionality
  suppressWarnings(x <- addDrugUse(
    cohort = cdm$cohort1,
    cdm = cdm,
    ingredientConceptId = 1,
    supplementary = TRUE,
    gapEra = 0,
    eraJoinMode = "Zero",
    overlapMode = "Sum",
    sameIndexMode = "Sum",
    imputeDuration = "eliminate",
    imputeDailyDose = "eliminate",
    durationRange = c(1, Inf),
    dailyDoseRange = c(0, Inf)
  ))
  values <- c(
    28, 33, 0, 27, 4, 7, 2,
    2, 0, 3,
    2, 1, 1, 0, 30 * 16 + 40 * 22 + 10 * 22 + 20 * 1, 16 + 22 + 22 + 1, 61,
    0, 5, 1, 1,
    30 * 16 + 40 * 22 + 10 * 22 + 20 * 1, 0, 0
  )
  xx <- x %>%
    dplyr::collect() %>%
    dplyr::filter(subject_id == 2)
  for (k in 1:length(values)) {
    expect_true(xx[[variables[k]]] == values[k])
  }

  parameters <- dplyr::tibble(
    overlapMode = c(
      "Sum", "Previous", "Subsequent", "Minimum", "Maximum", "Sum", "Sum",
      "Subsequent"
    ),
    sameIndexMode = c(
      "Sum", "Minimum", "Maximum", "Sum", "Sum", "Minimum", "Minimum", "Sum"
    ),
    eraJoinMode = c(
      "Zero", "Zero", "Previous", "Subsequent", "Previous", "Subsequent",
      "Previous", "Previous"
    ),
    gapEra = c(8, 8, 16, 8, 16, 16, 16, 16)
  )
  expected_result <- dplyr::tibble(
    first_era_length = c(27, 27, 37, 27, 37, 37, 37, 37),
    not_considered_days = c(0, 33, 33, 33, 33, 22, 22, 11),
    number_eras = c(2, 2, 1, 2, 1, 1, 1, 1),
    number_gaps = c(0, 0, 1, 0, 1, 1, 1, 1),
    unexposed_days = c(33, 33, 24, 33, 24, 24, 24, 24),
    gap_days = c(0, 0, 9, 0, 9, 9, 9, 9),
    cumulative_dose = c(1600, 610, 1410, 1050, 1720, 900, 810, 1720),
    cumulative_gap_dose = c(0, 0, 360, 0, 450, 180, 90, 450),
    cumulative_not_considered_dose = c(0, 990, 550, 550, 330, 880, 880, 330)
  )

  for (k in 1:nrow(parameters)) {
    suppressWarnings(x <- addDrugUse(
      cohort = cdm$cohort1,
      cdm = cdm,
      ingredientConceptId = 1,
      supplementary = TRUE,
      gapEra = parameters$gapEra[k],
      eraJoinMode = parameters$eraJoinMode[k],
      overlapMode = parameters$overlapMode[k],
      sameIndexMode = parameters$sameIndexMode[k],
      imputeDuration = "eliminate",
      imputeDailyDose = "eliminate",
      durationRange = c(1, Inf),
      dailyDoseRange = c(0, Inf)
    ))
    result <- x %>%
      dplyr::collect() %>%
      dplyr::filter(subject_id == 2)
    expect_true(result$first_era_days == expected_result$first_era_length[k])
    expect_true(
      result$not_considered_days == expected_result$not_considered_days[k]
    )
    expect_true(result$number_eras == expected_result$number_eras[k])
    expect_true(result$number_gaps == expected_result$number_gaps[k])
    expect_true(result$unexposed_days == expected_result$unexposed_days[k])
    expect_true(result$gap_days == expected_result$gap_days[k])
    expect_true(result$cumulative_dose == expected_result$cumulative_dose[k])
    expect_true(
      result$cumulative_gap_dose == expected_result$cumulative_gap_dose[k]
    )
    expect_true(
      result$cumulative_not_considered_dose ==
        expected_result$cumulative_not_considered_dose[k]
    )
  }
})

test_that("test splitSubexposures", {
  skip_on_cran()
  x <- dplyr::tibble(
    subject_id = as.integer(c(
      1,
      1, 1,
      1, 1,
      1, 1, 1,
      1, 1, 1, 1,
      2, 2, 2, 2,
      2
    )),
    cohort_start_date = as.Date(c(
      "2000-01-01",
      "2000-02-01", "2000-02-01",
      "2000-03-05", "2000-03-05",
      "2000-04-01", "2000-04-01", "2000-04-01",
      "2000-05-01", "2000-05-01", "2000-05-01", "2000-05-01",
      "2000-01-03", "2000-01-03", "2000-01-03", "2000-01-03",
      "2000-02-01"
    )),
    cohort_end_date = as.Date(c(
      "2000-01-10",
      "2000-02-15", "2000-02-15",
      "2000-03-12", "2000-03-12",
      "2000-04-21", "2000-04-21", "2000-04-21",
      "2000-05-11", "2000-05-11", "2000-05-11", "2000-05-11",
      "2000-01-20", "2000-01-20", "2000-01-20", "2000-01-20",
      "2000-02-10"
    )),
    drug_exposure_start_date = as.Date(c(
      "2000-01-01",
      "2000-02-01", "2000-02-04",
      "2000-03-01", "2000-03-06",
      "2000-04-08", "2000-04-09", "2000-04-11",
      "2000-05-01", "2000-05-01", "2000-05-09", "2000-05-11",
      "2000-01-01", "2000-01-03", "2000-01-09", "2000-01-20",
      "2000-02-08"
    )),
    drug_exposure_end_date = as.Date(c(
      "2000-01-10",
      "2000-02-10", "2000-02-20",
      "2000-03-10", "2000-03-06",
      "2000-04-15", "2000-04-11", "2000-04-11",
      "2000-05-01", "2000-05-05", "2000-05-12", "2000-05-11",
      "2000-01-03", "2000-01-03", "2000-01-15", "2000-01-20",
      "2000-02-10"
    ))
  )
  cdm <- mockDrugUtilisation(
    connectionDetails, extraTables = list("cohort1" = x)
  )
  y <- splitSubexposures(cdm[["cohort1"]], cdm) %>% dplyr::collect()

  # get first cohort entry
  yy <- y %>%
    dplyr::filter(
      .data$cohort_start_date == as.Date("2000-01-01") &
        .data$subject_id == 1
    )
  numberExpectedSubexposures <- 1
  countsPerSubexposure <- 1
  dayStartSubexposure <- 1
  dayEndSubexposure <- 10
  expect_true(length(unique(yy$subexposure_id)) == numberExpectedSubexposures)
  for (k in 1:length(countsPerSubexposure)) {
    yyy <- yy %>% dplyr::filter(.data$subexposure_id == k)
    expect_true(
      lubridate::day(yyy$subexposure_start_date[1]) == dayStartSubexposure[k]
    )
    expect_true(
      lubridate::day(yyy$subexposure_end_date[1]) == dayEndSubexposure[k]
    )
    if (countsPerSubexposure[k] == 0) {
      expect_true(nrow(yyy) == 1)
      expect_true(sum(is.na(yyy)) == 2)
    } else {
      expect_true(nrow(yyy) == countsPerSubexposure[k])
      expect_true(sum(is.na(yyy)) == 0)
    }
  }

  # get second cohort entry
  yy <- y %>%
    dplyr::filter(
      .data$cohort_start_date == as.Date("2000-02-01") &
        .data$subject_id == 1
    )
  numberExpectedSubexposures <- 3
  countsPerSubexposure <- c(1, 2, 1)
  dayStartSubexposure <- c(1, 4, 11)
  dayEndSubexposure <- c(3, 10, 15)
  expect_true(length(unique(yy$subexposure_id)) == numberExpectedSubexposures)
  for (k in 1:length(countsPerSubexposure)) {
    yyy <- yy %>% dplyr::filter(.data$subexposure_id == k)
    expect_true(
      lubridate::day(yyy$subexposure_start_date[1]) == dayStartSubexposure[k]
    )
    expect_true(
      lubridate::day(yyy$subexposure_end_date[1]) == dayEndSubexposure[k]
    )
    if (countsPerSubexposure[k] == 0) {
      expect_true(nrow(yyy) == 1)
      expect_true(sum(is.na(yyy)) == 2)
    } else {
      expect_true(nrow(yyy) == countsPerSubexposure[k])
      expect_true(sum(is.na(yyy)) == 0)
    }
  }

  # get third cohort entry
  yy <- y %>%
    dplyr::filter(
      .data$cohort_start_date == as.Date("2000-03-05") &
        .data$subject_id == 1
    )
  numberExpectedSubexposures <- 4
  countsPerSubexposure <- c(1, 2, 1, 0)
  dayStartSubexposure <- c(5, 6, 7, 11)
  dayEndSubexposure <- c(5, 6, 10, 12)
  expect_true(length(unique(yy$subexposure_id)) == numberExpectedSubexposures)
  for (k in 1:length(countsPerSubexposure)) {
    yyy <- yy %>% dplyr::filter(.data$subexposure_id == k)
    expect_true(
      lubridate::day(yyy$subexposure_start_date[1]) == dayStartSubexposure[k]
    )
    expect_true(
      lubridate::day(yyy$subexposure_end_date[1]) == dayEndSubexposure[k]
    )
    if (countsPerSubexposure[k] == 0) {
      expect_true(nrow(yyy) == 1)
      expect_true(sum(is.na(yyy)) == 2)
    } else {
      expect_true(nrow(yyy) == countsPerSubexposure[k])
      expect_true(sum(is.na(yyy)) == 0)
    }
  }

  # get fourth cohort entry
  yy <- y %>%
    dplyr::filter(
      .data$cohort_start_date == as.Date("2000-04-01") &
        .data$subject_id == 1
    )
  numberExpectedSubexposures <- 6
  countsPerSubexposure <- c(0, 1, 2, 3, 1, 0)
  dayStartSubexposure <- c(1, 8, 9, 11, 12, 16)
  dayEndSubexposure <- c(7, 8, 10, 11, 15, 21)
  expect_true(length(unique(yy$subexposure_id)) == numberExpectedSubexposures)
  for (k in 1:length(countsPerSubexposure)) {
    yyy <- yy %>% dplyr::filter(.data$subexposure_id == k)
    expect_true(
      lubridate::day(yyy$subexposure_start_date[1]) == dayStartSubexposure[k]
    )
    expect_true(
      lubridate::day(yyy$subexposure_end_date[1]) == dayEndSubexposure[k]
    )
    if (countsPerSubexposure[k] == 0) {
      expect_true(nrow(yyy) == 1)
      expect_true(sum(is.na(yyy)) == 2)
    } else {
      expect_true(nrow(yyy) == countsPerSubexposure[k])
      expect_true(sum(is.na(yyy)) == 0)
    }
  }

  # get fifth cohort entry
  yy <- y %>%
    dplyr::filter(
      .data$cohort_start_date == as.Date("2000-05-01") &
        .data$subject_id == 1
    )
  numberExpectedSubexposures <- 5
  countsPerSubexposure <- c(2, 1, 0, 1, 2)
  dayStartSubexposure <- c(1, 2, 6, 9, 11)
  dayEndSubexposure <- c(1, 5, 8, 10, 11)
  expect_true(length(unique(yy$subexposure_id)) == numberExpectedSubexposures)
  for (k in 1:length(countsPerSubexposure)) {
    yyy <- yy %>% dplyr::filter(.data$subexposure_id == k)
    expect_true(
      lubridate::day(yyy$subexposure_start_date[1]) == dayStartSubexposure[k]
    )
    expect_true(
      lubridate::day(yyy$subexposure_end_date[1]) == dayEndSubexposure[k]
    )
    if (countsPerSubexposure[k] == 0) {
      expect_true(nrow(yyy) == 1)
      expect_true(sum(is.na(yyy)) == 2)
    } else {
      expect_true(nrow(yyy) == countsPerSubexposure[k])
      expect_true(sum(is.na(yyy)) == 0)
    }
  }

  # get sixth cohort entry
  yy <- y %>%
    dplyr::filter(
      .data$cohort_start_date == as.Date("2000-01-03") &
        .data$subject_id == 2
    )
  numberExpectedSubexposures <- 5
  countsPerSubexposure <- c(2, 0, 1, 0, 1)
  dayStartSubexposure <- c(3, 4, 9, 16, 20)
  dayEndSubexposure <- c(3, 8, 15, 19, 20)
  expect_true(length(unique(yy$subexposure_id)) == numberExpectedSubexposures)
  for (k in 1:length(countsPerSubexposure)) {
    yyy <- yy %>% dplyr::filter(.data$subexposure_id == k)
    expect_true(
      lubridate::day(yyy$subexposure_start_date[1]) == dayStartSubexposure[k]
    )
    expect_true(
      lubridate::day(yyy$subexposure_end_date[1]) == dayEndSubexposure[k]
    )
    if (countsPerSubexposure[k] == 0) {
      expect_true(nrow(yyy) == 1)
      expect_true(sum(is.na(yyy)) == 2)
    } else {
      expect_true(nrow(yyy) == countsPerSubexposure[k])
      expect_true(sum(is.na(yyy)) == 0)
    }
  }

  # get seventh cohort entry
  yy <- y %>%
    dplyr::filter(
      .data$cohort_start_date == as.Date("2000-02-01") &
        .data$subject_id == 2
    )
  numberExpectedSubexposures <- 2
  countsPerSubexposure <- c(0, 1)
  dayStartSubexposure <- c(1, 8)
  dayEndSubexposure <- c(7, 10)
  expect_true(length(unique(yy$subexposure_id)) == numberExpectedSubexposures)
  for (k in 1:length(countsPerSubexposure)) {
    yyy <- yy %>% dplyr::filter(.data$subexposure_id == k)
    expect_true(
      lubridate::day(yyy$subexposure_start_date[1]) == dayStartSubexposure[k]
    )
    expect_true(
      lubridate::day(yyy$subexposure_end_date[1]) == dayEndSubexposure[k]
    )
    if (countsPerSubexposure[k] == 0) {
      expect_true(nrow(yyy) == 1)
      expect_true(sum(is.na(yyy)) == 2)
    } else {
      expect_true(nrow(yyy) == countsPerSubexposure[k])
      expect_true(sum(is.na(yyy)) == 0)
    }
  }
})

test_that("test empty targetCohortName", {
  skip_on_cran()
  cdm <- mockDrugUtilisation(
    connectionDetails,
    drug_exposure = dplyr::tibble(
      drug_exposure_id = 1:9,
      person_id = c(1, 1, 1, 1, 1, 2, 2, 2, 2),
      drug_concept_id = c(1, 2, 3, 3, 2, 3, 1, 2, 4),
      drug_exposure_start_date = as.Date(c(
        "2000-01-01", "2000-01-10", "2000-02-20", "2001-01-01", "2001-02-10",
        "2000-01-10", "2000-01-15", "2000-02-15", "2000-01-15"
      )),
      drug_exposure_end_date = as.Date(c(
        "2000-02-10", "2000-03-01", "2000-02-20", "2001-01-15", "2001-03-01",
        "2000-01-25", "2000-02-05", "2000-02-15", "2000-02-05"
      )),
      quantity = c(41, 52, 1, 15, 20, 16, 22, 1, 22)
    ),
    drug_strength = dplyr::tibble(
      drug_concept_id = c(1, 2, 3, 4),
      ingredient_concept_id = c(1, 1, 1, 1),
      amount_value = c(10, 20, 30, 40),
      amount_unit_concept_id = c(8576, 8576, 8576, 8576),
      numerator_value = as.numeric(NA),
      numerator_unit_concept_id = as.numeric(NA),
      denominator_value = as.numeric(NA),
      denominator_unit_concept_id = as.numeric(NA)
    )
  )

  cdm[["cohort1"]] <- cdm[["cohort1"]] %>%
    dplyr::filter(.data$subject_id < 1)

  expect_error(x <- addDrugUse(
    cohort = cdm$cohort1,
    cdm = cdm,
    ingredientConceptId = 1,
    supplementary = TRUE,
    gapEra = 30,
    eraJoinMode = "Previous",
    overlapMode = "Previous",
    sameIndexMode = "Sum",
    imputeDuration = "eliminate",
    imputeDailyDose = "eliminate",
    durationRange = c(1, Inf),
    dailyDoseRange = c(100, Inf)
  ))

})

test_that("expected errors on inputs", {
  skip_on_cran()
  # condition_occurrence is going to be the strataCohortTable, person the
  # doseTable
  cdm <- mockDrugUtilisation(
    connectionDetails,
    cohort1 = dplyr::tibble(
      cohort_definition_id = c(1, 1, 1, 2),
      subject_id = c(1, 1, 2, 1),
      cohort_start_date = as.Date(c(
        "2020-01-01", "2020-05-01", "2020-04-08", "2020-01-01"
      )),
      cohort_end_date = as.Date(c(
        "2020-01-10", "2020-06-01", "2020-07-18", "2020-01-11"
      ))
    ),
    extraTables = list(
      dose_table = dplyr::tibble(
        subject_id = c(1, 1, 2, 1),
        cohort_start_date = as.Date(c(
          "2020-01-01", "2020-05-01", "2020-04-08", "2020-01-01"
        )),
        cohort_end_date = as.Date(c(
          "2020-01-10", "2020-06-01", "2020-07-18", "2020-01-11"
        )),
        initial_dose = c(1, 2, 3, 6),
        cumulative_dose = c(5, 6, 9, 7),
        piscina = c(TRUE, FALSE, TRUE, FALSE),
        cara = c("a", "b", "b", "a")
      )
    )
  )
  # no inputs
  expect_error(result <- summariseDrugUse())
  # only cdm
  expect_error(result <- summariseDrugUse(
    cdm = cdm,
  ))
})

test_that("check output format", {
  skip_on_cran()
  cdm <- mockDrugUtilisation(
    connectionDetails,
    cohort = dplyr::tibble(
      cohort_definition_id = c(1, 1, 1, 2),
      subject_id = c(1, 1, 2, 1),
      cohort_start_date = as.Date(c(
        "2020-01-01", "2020-05-01", "2020-04-08", "2020-01-01"
      )),
      cohort_end_date = as.Date(c(
        "2020-01-10", "2020-06-01", "2020-07-18", "2020-01-11"
      )),
      initial_daily_dose = c(1, 2, 3, 6),
      cumulative_dose = c(5, 6, 9, 7),
      piscina = c(TRUE, FALSE, TRUE, FALSE),
      cara = c("a", "b", "b", "a")
    )
  )
  result <- cdm$cohort %>%
    summariseDrugUse(cdm = cdm)
  expect_true(all(c("tbl_df", "tbl", "data.frame") %in% class(result)))
  expect_true(all(colnames(result) %in% c(
    "group_name", "group_level", "strata_name", "strata_level", "variable",
    "variable_level", "variable_type", "estimate_type", "estimate", "cdm_name",
    "generated_by"
  )))
})

test_that("check all estimates", {
  all_estimates <- c(
    "min", "max", "mean", "median", "iqr", "range", "q05", "q10", "q15", "q20",
    "q25", "q30", "q35", "q40", "q45", "q55", "q60", "q65", "q70",
    "q75", "q80", "q85", "q90", "q95", "sd"
  )
  cdm <- mockDrugUtilisation(
    connectionDetails,
    extraTables = list(
      dose_table= dplyr::tibble(
        cohort_definition_id = c(1, 1, 1, 1),
        subject_id = c(1, 1, 2, 1),
        cohort_start_date = as.Date(c(
          "2020-01-01", "2020-05-01", "2020-04-08", "2020-01-01"
        )),
        cohort_end_date = as.Date(c(
          "2020-01-10", "2020-06-01", "2020-07-18", "2020-01-11"
        )),
        initial_dose = c(1, 2, 3, 6),
        cumulative_dose = c(5, 6, 9, 7),
        piscina = c(TRUE, FALSE, TRUE, FALSE),
        cara = c("a", "b", "b", "a")
      )
    )
  )
  attr(cdm$dose_table, "cohort_set") <- dplyr::tibble(
    cohort_definition_id = 1, cohort_name = "cohort1"
  )
  class(cdm$dose_table) <- c("GeneratedCohortSet", class(cdm$dose_table))
  for (k in 1:length(all_estimates)) {
    res <- summariseDrugUse(
      cdm$dose_table,
      cdm = cdm,
      drugUseVariables = c("initial_dose", "cumulative_dose"),
      drugUseEstimates = all_estimates[k]
    ) %>%
      dplyr::filter(.data$group_name == "Cohort name")
    expect_true(nrow(res[res$variable == c("initial_dose"), ]) == 1)
    expect_true(res$estimate_type[res$variable == c("initial_dose")] == all_estimates[k])
    expect_true(nrow(res[res$variable == c("cumulative_dose"), ]) == 1)
    expect_true(res$estimate_type[res$variable == c("cumulative_dose")] == all_estimates[k])
  }
  res <- summariseDrugUse(
    cdm$dose_table,
    cdm = cdm,
    drugUseVariables = c("initial_dose", "cumulative_dose"),
    drugUseEstimates = all_estimates
  )

  expect_true(grepl("summariseDrugUse", unique(res$generated_by)))
})

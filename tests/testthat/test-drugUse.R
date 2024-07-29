test_that("test flags", {
  skip_on_cran()
  cdm <- mockDrugUtilisation(con = connection(), writeSchema = schema(), seed = 1)
  x <- tidyr::expand_grid(
    duration = c(TRUE, FALSE), quantity = c(TRUE, FALSE), dose = c(TRUE, FALSE)
  )
  columnsDuration <- c("duration", "impute_duration_percentage")
  columnsQuantity <- c("cumulative_quantity", "initial_quantity")
  columnsDose <- c(
    "impute_daily_dose_percentage", "initial_daily_dose_milligram",
    "cumulative_dose_milligram"
  )
  for (k in seq_len(nrow(x))) {
    xx <- cdm$cohort1 |>
      addDrugUse(
        ingredientConceptId = 1539403, duration = x$duration[k],
        quantity = x$quantity[k], dose = x$dose[k]
      ) |>
      expect_no_error()
    expect_true(all(c("number_exposures", "number_eras") %in% colnames(xx)))
    if (x$duration[k]) {
      expect_true(all(columnsDuration %in% colnames(xx)))
    } else {
      expect_false(any(columnsDuration %in% colnames(xx)))
    }
    if (x$quantity[k]) {
      expect_true(all(columnsQuantity %in% colnames(xx)))
    } else {
      expect_false(any(columnsQuantity %in% colnames(xx)))
    }
    if (x$dose[k]) {
      expect_true(all(columnsDose %in% colnames(xx)))
    } else {
      expect_false(any(columnsDose %in% colnames(xx)))
    }
  }

  mockDisconnect(cdm = cdm)
})

test_that("test overlapMode", {
  skip_on_cran()
  cdm <- mockDrugUtilisation(
    con = connection(),
    writeSchema = schema(),
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
      quantity = c(41, 52, 1, 15, 20, 16, 22, 1, 22),
      drug_type_concept_id = 0
    ),
    concept = dplyr::tibble(
      concept_id = c(1, 2, 3, 4, 5, 8576),
      concept_name = c("ingredient1", "drug2", "drug3", "drug4", "drug5", "milligram"),
      domain_id = c(rep("Drug", 5), "Unit"),
      vocabulary_id = c(rep("RxNorm", 5), "Unit"),
      standard_concept = "S",
      concept_class_id = c("Ingredient", rep("Drug", 4), "Unit"),
      concept_code = 0,
      valid_start_date = as.Date("1900-01-01"),
      valid_end_date = as.Date("2100-01-01")
    ),
    concept_ancestor = dplyr::tibble(
      ancestor_concept_id = 1,
      descendant_concept_id = 2:5,
      min_levels_of_separation = 1,
      max_levels_of_separation = 1
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
      denominator_unit_concept_id = as.numeric(NA),
      valid_start_date = as.Date("1900-01-01"),
      valid_end_date = as.Date("2100-01-01")
    ),
    cohort1 = dplyr::tibble(
      cohort_definition_id = 1,
      subject_id = c(1, 1, 2),
      cohort_start_date = as.Date(c("2000-01-01", "2001-01-01", "2000-01-01")),
      cohort_end_date = as.Date(c("2000-03-01", "2001-03-01", "2000-03-01"))
    ),
    observation_period = dplyr::tibble(
      observation_period_id = 1,
      person_id = 1:2,
      observation_period_start_date = as.Date("1900-01-01"),
      observation_period_end_date = as.Date("2100-01-01"),
      period_type_concept_id = 0
    ),
    concept_relationship = dplyr::tibble(
      concept_id_1 = c(c(1, 2, 3, 4, 5)),
      concept_id_2 = c(19016586, 46275062, 35894935, 19135843, 19082107),
      relationship_id = c(rep("RxNorm has dose form", 5)),
      valid_start_date = as.Date("1900-01-01"),
      valid_end_date = as.Date("2100-01-01")
    )
  )
  variables <- c(
    "number_exposures", "number_eras", "initial_daily_dose_milligram", "duration",
    "cumulative_dose_milligram", "initial_quantity", "cumulative_quantity"
  )

  # check no error without cdm object specified
  expect_no_error(x <- addDrugUse(
    cohort = cdm[["cohort1"]],
    ingredientConceptId = 1,
    gapEra = 30,
    eraJoinMode = "Previous",
    overlapMode = "Sum",
    sameIndexMode = "Sum",
    imputeDuration = "none",
    imputeDailyDose = "none",
    durationRange = c(1, Inf),
    dailyDoseRange = c(0, Inf)
  ))

  # prev
  x <- addDrugUse(
    cohort = cdm[["cohort1"]],
    ingredientConceptId = 1,
    gapEra = 30,
    eraJoinMode = "Previous",
    overlapMode = "Previous",
    sameIndexMode = "Sum",
    imputeDuration = "none",
    imputeDailyDose = "none",
    durationRange = c(1, Inf),
    dailyDoseRange = c(0, Inf)
  )

  c("cdm_reference", "cohort_attrition", "cohort_set") %in%
    names(attributes(x)) |>
    all() |>
    expect_true()
  expect_true(all(variables %in% colnames(x)))

  value_cohort_1 <- c(3, 1, 10, 61, 810, 41, 94)
  xx <- x |>
    dplyr::collect() |>
    dplyr::filter(
      subject_id == 1 & cohort_start_date == as.Date("2000-01-01")
    )
  for (k in 1:length(value_cohort_1)) {
    expect_true(xx[[variables[k]]] == value_cohort_1[k])
  }

  # sub
  suppressWarnings(x <- addDrugUse(
    cohort = cdm[["cohort1"]],
    ingredientConceptId = 1,
    gapEra = 30,
    eraJoinMode = "Previous",
    overlapMode = "Subsequent",
    sameIndexMode = "Sum",
    imputeDuration = "none",
    imputeDailyDose = "none",
    durationRange = c(1, Inf),
    dailyDoseRange = c(0, Inf)
  ))
  value_cohort_1 <- c(3, 1, 10, 61, 1140, 41, 94)
  xx <- x |>
    dplyr::collect() |>
    dplyr::filter(
      subject_id == 1 & cohort_start_date == as.Date("2000-01-01")
    )
  for (k in 1:length(value_cohort_1)) {
    expect_true(xx[[variables[k]]] == value_cohort_1[k])
  }

  # min
  suppressWarnings(x <- addDrugUse(
    cohort = cdm[["cohort1"]],
    ingredientConceptId = 1,
    gapEra = 30,
    eraJoinMode = "Previous",
    overlapMode = "Minimum",
    sameIndexMode = "Sum",
    imputeDuration = "none",
    imputeDailyDose = "none",
    durationRange = c(1, Inf),
    dailyDoseRange = c(0, Inf)
  ))
  value_cohort_1 <- c(3, 1, 10, 61, 810, 41, 94)
  xx <- x |>
    dplyr::collect() |>
    dplyr::filter(
      subject_id == 1 & cohort_start_date == as.Date("2000-01-01")
    )
  for (k in 1:length(value_cohort_1)) {
    expect_true(xx[[variables[k]]] == value_cohort_1[k])
  }

  # max
  suppressWarnings(x <- addDrugUse(
    cohort = cdm[["cohort1"]],
    ingredientConceptId = 1,
    gapEra = 30,
    eraJoinMode = "Previous",
    overlapMode = "Maximum",
    sameIndexMode = "Sum",
    imputeDuration = "none",
    imputeDailyDose = "none",
    durationRange = c(1, Inf),
    dailyDoseRange = c(0, Inf)
  ))
  value_cohort_1 <- c(3, 1, 10, 61, 1140, 41, 94)
  xx <- x |>
    dplyr::collect() |>
    dplyr::filter(
      subject_id == 1 & cohort_start_date == as.Date("2000-01-01")
    )
  for (k in 1:length(value_cohort_1)) {
    expect_true(xx[[variables[k]]] == value_cohort_1[k])
  }

  # sum
  suppressWarnings(x <- addDrugUse(
    cohort = cdm[["cohort1"]],
    ingredientConceptId = 1,
    gapEra = 30,
    eraJoinMode = "Previous",
    overlapMode = "Sum",
    sameIndexMode = "Sum",
    imputeDuration = "none",
    imputeDailyDose = "none",
    durationRange = c(1, Inf),
    dailyDoseRange = c(0, Inf)
  ))
  value_cohort_1 <- c(3, 1, 10, 61, 1480, 41, 94)
  xx <- x |>
    dplyr::collect() |>
    dplyr::filter(
      subject_id == 1 & cohort_start_date == as.Date("2000-01-01")
    )
  for (k in 1:length(value_cohort_1)) {
    expect_true(xx[[variables[k]]] == value_cohort_1[k])
  }

  mockDisconnect(cdm = cdm)
})

test_that("test gapEra and eraJoinMode", {
  skip_on_cran()
  cdm <- mockDrugUtilisation(
    con = connection(),
    writeSchema = schema(),
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
      quantity = c(41, 52, 1, 15, 20, 16, 22, 1, 22),
      drug_type_concept_id = 0
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
      denominator_unit_concept_id = as.numeric(NA),
      valid_start_date = as.Date("1900-01-01"),
      valid_end_date = as.Date("2100-01-01")
    ),
    concept = dplyr::tibble(
      concept_id = c(1, 2, 3, 4, 5, 8576),
      concept_name = c("ingredient1", "drug2", "drug3", "drug4", "drug5", "milligram"),
      domain_id = c(rep("Drug", 5), "Unit"),
      vocabulary_id = c(rep("RxNorm", 5), "Unit"),
      standard_concept = "S",
      concept_class_id = c("Ingredient", rep("Drug", 4), "Unit"),
      concept_code = 0,
      valid_start_date = as.Date("1900-01-01"),
      valid_end_date = as.Date("2100-01-01")
    ),
    concept_ancestor = dplyr::tibble(
      ancestor_concept_id = 1,
      descendant_concept_id = 2:5,
      min_levels_of_separation = 1,
      max_levels_of_separation = 1
    ),
    cohort1 = dplyr::tibble(
      cohort_definition_id = 1,
      subject_id = c(1, 1, 2),
      cohort_start_date = as.Date(c("2000-01-01", "2001-01-01", "2000-01-01")),
      cohort_end_date = as.Date(c("2000-03-01", "2001-03-01", "2000-03-01"))
    ),
    concept_relationship = dplyr::tibble(
      concept_id_1 = c(1, 2, 3, 4, 5),
      concept_id_2 = c(19016586, 46275062, 35894935, 19135843, 19082107),
      relationship_id = c(rep("RxNorm has dose form", 5)),
      concept_code = 0,
      valid_start_date = as.Date("1900-01-01"),
      valid_end_date = as.Date("2100-01-01")
    ),
    observation_period = dplyr::tibble(
      observation_period_id = 1,
      person_id = 1:2,
      observation_period_start_date = as.Date("1900-01-01"),
      observation_period_end_date = as.Date("2100-01-01"),
      period_type_concept_id = 0
    )
  )

  variables <- c(
    "number_exposures", "number_eras", "initial_daily_dose_milligram",
    "duration", "cumulative_dose_milligram", "initial_quantity",
    "cumulative_quantity"
  )

  # overall functionality
  suppressWarnings(x <- addDrugUse(
    cohort = cdm[["cohort1"]],
    ingredientConceptId = 1,
    gapEra = 0,
    eraJoinMode = "Previous",
    overlapMode = "Sum",
    sameIndexMode = "Sum",
    imputeDuration = "none",
    imputeDailyDose = "none",
    durationRange = c(1, Inf),
    dailyDoseRange = c(0, Inf)
  ))

  value_cohort_1 <- c(2, 2, 30, 60, 850, 15, 35)
  xx <- x |>
    dplyr::collect() |>
    dplyr::filter(
      subject_id == 1 & cohort_start_date == as.Date("2001-01-01")
    )
  for (k in 1:length(value_cohort_1)) {
    expect_true(xx[[variables[k]]] == value_cohort_1[k])
  }

  # gapEra = 24
  suppressWarnings(x <- addDrugUse(
    cohort = cdm[["cohort1"]],
    cdm = cdm,
    ingredientConceptId = 1,
    gapEra = 24,
    eraJoinMode = "Previous",
    overlapMode = "Sum",
    sameIndexMode = "Sum",
    imputeDuration = "none",
    imputeDailyDose = "none",
    durationRange = c(1, Inf),
    dailyDoseRange = c(0, Inf)
  ))
  value_cohort_1 <- c(2, 2, 30, 60, 850, 15, 35)
  xx <- x |>
    dplyr::collect() |>
    dplyr::filter(
      subject_id == 1 & cohort_start_date == as.Date("2001-01-01")
    )
  for (k in 1:length(value_cohort_1)) {
    expect_true(xx[[variables[k]]] == value_cohort_1[k])
  }

  # gapEra = 25 & joinMode = Zero
  suppressWarnings(x <- addDrugUse(
    cohort = cdm[["cohort1"]],
    cdm = cdm,
    ingredientConceptId = 1,
    gapEra = 25,
    eraJoinMode = "Zero",
    overlapMode = "Sum",
    sameIndexMode = "Sum",
    imputeDuration = "none",
    imputeDailyDose = "none",
    durationRange = c(1, Inf),
    dailyDoseRange = c(0, Inf)
  ))
  value_cohort_1 <- c(2, 1, 30, 60, 850, 15, 35)
  xx <- x |>
    dplyr::collect() |>
    dplyr::filter(
      subject_id == 1 & cohort_start_date == as.Date("2001-01-01")
    )
  for (k in 1:length(value_cohort_1)) {
    expect_true(xx[[variables[k]]] == value_cohort_1[k])
  }

  # gapEra = 25 & joinMode = Previous
  suppressWarnings(x <- addDrugUse(
    cohort = cdm[["cohort1"]],
    cdm = cdm,
    ingredientConceptId = 1,
    gapEra = 25,
    eraJoinMode = "Previous",
    overlapMode = "Sum",
    sameIndexMode = "Sum",
    imputeDuration = "none",
    imputeDailyDose = "none",
    durationRange = c(1, Inf),
    dailyDoseRange = c(0, Inf)
  ))
  value_cohort_1 <- c(2, 1, 30, 60, 1600, 15, 35)
  xx <- x |>
    dplyr::collect() |>
    dplyr::filter(
      subject_id == 1 & cohort_start_date == as.Date("2001-01-01")
    )
  for (k in 1:length(value_cohort_1)) {
    expect_true(xx[[variables[k]]] == value_cohort_1[k])
  }

  # gapEra = 25 & joinMode = Subsequent
  suppressWarnings(x <- addDrugUse(
    cohort = cdm[["cohort1"]],
    cdm = cdm,
    ingredientConceptId = 1,
    gapEra = 25,
    eraJoinMode = "Subsequent",
    overlapMode = "Sum",
    sameIndexMode = "Sum",
    imputeDuration = "none",
    imputeDailyDose = "none",
    durationRange = c(1, Inf),
    dailyDoseRange = c(0, Inf)
  ))
  value_cohort_1 <- c(2, 1, 30, 60, 1350, 15, 35)
  xx <- x |>
    dplyr::collect() |>
    dplyr::filter(
      subject_id == 1 & cohort_start_date == as.Date("2001-01-01")
    )
  for (k in 1:length(value_cohort_1)) {
    expect_true(xx[[variables[k]]] == value_cohort_1[k])
  }

  mockDisconnect(cdm = cdm)
})

test_that("test gapEra, eraJoinMode & sameIndexOverlap", {
  skip_on_cran()
  cdm <- mockDrugUtilisation(
    con = connection(),
    writeSchema = schema(),
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
      quantity = c(41, 52, 1, 15, 20, 16, 22, 1, 22),
      drug_type_concept_id = 0
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
      denominator_unit_concept_id = as.numeric(NA),
      valid_start_date = as.Date("1900-01-01"),
      valid_end_date = as.Date("2100-01-01")
    ),
    observation_period = dplyr::tibble(
      observation_period_id = 1,
      person_id = 1:2,
      observation_period_start_date = as.Date("1900-01-01"),
      observation_period_end_date = as.Date("2100-01-01"),
      period_type_concept_id = 0
    ),
    concept = dplyr::tibble(
      concept_id = c(1, 2, 3, 4, 5, 8576),
      concept_name = c("ingredient1", "drug2", "drug3", "drug4", "drug5", "milligram"),
      domain_id = c(rep("Drug", 5), "Unit"),
      vocabulary_id = c(rep("RxNorm", 5), "Unit"),
      standard_concept = "S",
      concept_class_id = c("Ingredient", rep("Drug", 4), "Unit"),
      concept_code = 0,
      valid_start_date = as.Date("1900-01-01"),
      valid_end_date = as.Date("2100-01-01")
    ),
    concept_ancestor = dplyr::tibble(
      ancestor_concept_id = 1,
      descendant_concept_id = 2:5,
      min_levels_of_separation = 1,
      max_levels_of_separation = 1
    ),
    cohort1 = dplyr::tibble(
      cohort_definition_id = 1,
      subject_id = c(1, 1, 2),
      cohort_start_date = as.Date(c("2000-01-01", "2001-01-01", "2000-01-01")),
      cohort_end_date = as.Date(c("2000-03-01", "2001-03-01", "2000-03-01"))
    ),
    concept_relationship = dplyr::tibble(
      concept_id_1 = c(c(1, 2, 3, 4, 5)),
      concept_id_2 = c(19016586, 46275062, 35894935, 19135843, 19082107),
      relationship_id = c(rep("RxNorm has dose form", 5)),
      valid_start_date = as.Date("1900-01-01"),
      valid_end_date = as.Date("2100-01-01")
    )
  )

  variables <- c(
    "number_exposures", "number_eras", "initial_daily_dose_milligram",
    "duration", "cumulative_dose_milligram", "initial_quantity",
    "cumulative_quantity"
  )

  # overall functionality
  suppressWarnings(x <- addDrugUse(
    cohort = cdm[["cohort1"]],
    cdm = cdm,
    ingredientConceptId = 1,
    gapEra = 0,
    eraJoinMode = "Zero",
    overlapMode = "Sum",
    sameIndexMode = "Sum",
    imputeDuration = "none",
    imputeDailyDose = "none",
    durationRange = c(1, Inf),
    dailyDoseRange = c(0, Inf)
  ))
  value_cohort_1 <- c(4, 2, NA, 61, 1600, NA, 61)
  xx <- x |>
    dplyr::collect() |>
    dplyr::filter(subject_id == 2)
  for (k in 1:length(value_cohort_1)) {
    expect_equal(xx[[variables[k]]], value_cohort_1[k])
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
    number_eras = c(2, 2, 1, 2, 1, 1, 1, 1),
    cumulative_dose_milligram = c(1600, 610, 1410, 1050, 1720, 900, 810, 1720)
  )

  for (k in 1:nrow(parameters)) {
    suppressWarnings(x <- addDrugUse(
      cohort = cdm[["cohort1"]],
      cdm = cdm,
      ingredientConceptId = 1,
      gapEra = parameters$gapEra[k],
      eraJoinMode = parameters$eraJoinMode[k],
      overlapMode = parameters$overlapMode[k],
      sameIndexMode = parameters$sameIndexMode[k],
      imputeDuration = "none",
      imputeDailyDose = "none",
      durationRange = c(1, Inf),
      dailyDoseRange = c(0, Inf)
    ))
    result <- x |>
      dplyr::collect() |>
      dplyr::filter(subject_id == 2)
    expect_true(result$number_eras == expected_result$number_eras[k])
    expect_true(result$cumulative_dose_milligram == expected_result$cumulative_dose_milligram[k])
  }

  mockDisconnect(cdm = cdm)
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
  cdm <- mockDrugUtilisation(con = connection(), writeSchema = schema())
  cdm <- omopgenerics::insertTable(cdm = cdm, name = "cohort1", table = x)
  y <- splitSubexposures(cdm[["cohort1"]], cdm) |> dplyr::collect()

  # get first cohort entry
  yy <- y |>
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
    yyy <- yy |> dplyr::filter(.data$subexposure_id == k)
    expect_true(
      clock::get_day(yyy$subexposure_start_date[1]) == dayStartSubexposure[k]
    )
    expect_true(
      clock::get_day(yyy$subexposure_end_date[1]) == dayEndSubexposure[k]
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
  yy <- y |>
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
    yyy <- yy |> dplyr::filter(.data$subexposure_id == k)
    expect_true(
      clock::get_day(yyy$subexposure_start_date[1]) == dayStartSubexposure[k]
    )
    expect_true(
      clock::get_day(yyy$subexposure_end_date[1]) == dayEndSubexposure[k]
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
  yy <- y |>
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
    yyy <- yy |> dplyr::filter(.data$subexposure_id == k)
    expect_true(
      clock::get_day(yyy$subexposure_start_date[1]) == dayStartSubexposure[k]
    )
    expect_true(
      clock::get_day(yyy$subexposure_end_date[1]) == dayEndSubexposure[k]
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
  yy <- y |>
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
    yyy <- yy |> dplyr::filter(.data$subexposure_id == k)
    expect_true(
      clock::get_day(yyy$subexposure_start_date[1]) == dayStartSubexposure[k]
    )
    expect_true(
      clock::get_day(yyy$subexposure_end_date[1]) == dayEndSubexposure[k]
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
  yy <- y |>
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
    yyy <- yy |> dplyr::filter(.data$subexposure_id == k)
    expect_true(
      clock::get_day(yyy$subexposure_start_date[1]) == dayStartSubexposure[k]
    )
    expect_true(
      clock::get_day(yyy$subexposure_end_date[1]) == dayEndSubexposure[k]
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
  yy <- y |>
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
    yyy <- yy |> dplyr::filter(.data$subexposure_id == k)
    expect_true(
      clock::get_day(yyy$subexposure_start_date[1]) == dayStartSubexposure[k]
    )
    expect_true(
      clock::get_day(yyy$subexposure_end_date[1]) == dayEndSubexposure[k]
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
  yy <- y |>
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
    yyy <- yy |> dplyr::filter(.data$subexposure_id == k)
    expect_true(
      clock::get_day(yyy$subexposure_start_date[1]) == dayStartSubexposure[k]
    )
    expect_true(
      clock::get_day(yyy$subexposure_end_date[1]) == dayEndSubexposure[k]
    )
    if (countsPerSubexposure[k] == 0) {
      expect_true(nrow(yyy) == 1)
      expect_true(sum(is.na(yyy)) == 2)
    } else {
      expect_true(nrow(yyy) == countsPerSubexposure[k])
      expect_true(sum(is.na(yyy)) == 0)
    }
  }

  mockDisconnect(cdm = cdm)
})

test_that("test empty targetCohortName", {
  skip_on_cran()
  cdm <- mockDrugUtilisation(con = connection(), writeSchema = schema())

  cdm[["cohort1"]] <- cdm[["cohort1"]] |>
    dplyr::filter(.data$subject_id < 1)

  expect_error(x <- addDrugUse(
    cohort = cdm[["cohort1"]],
    ingredientConceptId = 1,
    gapEra = 30,
    eraJoinMode = "Previous",
    overlapMode = "Previous",
    sameIndexMode = "Sum",
    imputeDuration = "none",
    imputeDailyDose = "none",
    durationRange = c(1, Inf),
    dailyDoseRange = c(100, Inf)
  ))

  mockDisconnect(cdm = cdm)
})

test_that("expected errors on inputs", {
  skip_on_cran()
  # condition_occurrence is going to be the strataCohortTable, person the
  # doseTable
  cdm <- mockDrugUtilisation(
    con = connection(),
    writeSchema = schema(),
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
    observation_period = dplyr::tibble(
      person_id = 1:2,
      observation_period_id = 1:2,
      observation_period_start_date = as.Date("2000-01-01"),
      observation_period_end_date = as.Date("2030-01-01"),
      period_type_concept_id = 0
    )
  )
  dose_table <- dplyr::tibble(
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
  cdm <- omopgenerics::insertTable(cdm = cdm, name = "dose_table", table = dose_table)
  # no inputs
  expect_error(result <- summariseDrugUse())
  # only cdm
  expect_error(result <- summariseDrugUse(
    cdm = cdm,
  ))

  mockDisconnect(cdm = cdm)
})

test_that("check output format", {
  skip_on_cran()
  cdm <- mockDrugUtilisation(
    con = connection(),
    writeSchema = schema(),
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
    ),
    observation_period = dplyr::tibble(
      person_id = 1:2,
      observation_period_id = 1:2,
      observation_period_start_date = as.Date("2000-01-01"),
      observation_period_end_date = as.Date("2030-01-01"),
      period_type_concept_id = 0
    )
  )
  result <- cdm[["cohort"]] |>
    summariseDrugUse()
  expect_true(all(c("tbl_df", "tbl", "data.frame") %in% class(result)))
  expect_true(inherits(result, "summarised_result"))

  mockDisconnect(cdm = cdm)
})

test_that("check all estimates", {
  skip_on_cran()
  all_estimates <- c(
    "min", "max", "mean", "median", # "iqr", "range",
    "q05", "q10", "q15", "q20",
    "q25", "q30", "q35", "q40", "q45", "q55", "q60", "q65", "q70",
    "q75", "q80", "q85", "q90", "q95", "sd"
  )
  cdm <- mockDrugUtilisation(
    con = connection(),
    writeSchema = schema(),
    observation_period = dplyr::tibble(
      person_id = 1:2,
      observation_period_id = 1:2,
      observation_period_start_date = as.Date("2000-01-01"),
      observation_period_end_date = as.Date("2030-01-01"),
      period_type_concept_id = 0
    )
  )
  dose_table <- dplyr::tibble(
    cohort_definition_id = c(1, 1, 1, 1),
    subject_id = c(1, 1, 2, 1),
    cohort_start_date = as.Date(c(
      "2020-01-01", "2020-05-01", "2020-04-08", "2021-01-01"
    )),
    cohort_end_date = as.Date(c(
      "2020-01-10", "2020-06-01", "2020-07-18", "2021-01-11"
    )),
    initial_daily_dose = c(1, 2, 3, 6),
    cumulative_dose = c(5, 6, 9, 7),
    piscina = c(TRUE, FALSE, TRUE, FALSE),
    cara = c("a", "b", "b", "a")
  )
  cdm <- omopgenerics::insertTable(cdm = cdm, name = "dose_table", table = dose_table)
  cdm$dose_table <- cdm$dose_table |>
    omopgenerics::newCohortTable(cohortSetRef = dplyr::tibble(
      cohort_definition_id = 1, cohort_name = "cohort_1"
    ))
  for (k in 1:length(all_estimates)) {
    res <- summariseDrugUse(
      cdm[["dose_table"]],
      estimates = all_estimates[k]
    ) |>
      dplyr::filter(.data$group_name == "cohort_name")
    expect_true(nrow(res[res$variable_name == c("initial_daily_dose"), ]) == 1)
    expect_true(res$estimate_name[res$variable_name == c("initial_daily_dose")] == all_estimates[k])
    expect_true(nrow(res[res$variable_name == c("cumulative_dose"), ]) == 1)
    expect_true(res$estimate_name[res$variable_name == c("cumulative_dose")] == all_estimates[k])
  }
  res <- summariseDrugUse(
    cdm[["dose_table"]],
    cdm = cdm,
    estimates = all_estimates
  )

  mockDisconnect(cdm = cdm)
})

test_that("check all variables", {
  skip_on_cran()
  all_estimates <- c(
    "min", "max", "mean", "median", # "iqr", "range",
    "q05", "q10", "q15", "q20",
    "q25", "q30", "q35", "q40", "q45", "q55", "q60", "q65", "q70",
    "q75", "q80", "q85", "q90", "q95", "sd"
  )
  cdm <- mockDrugUtilisation(
    con = connection(),
    writeSchema = schema(),
    concept_relationship = dplyr::tibble(
      concept_id_1 = c(1125315, 43135274, 2905077, 1125360),
      concept_id_2 = c(19016586, 46275062, 35894935, 19135843),
      relationship_id = c(rep("RxNorm has dose form", 4)),
      valid_start_date = as.Date("2000-01-01"),
      valid_end_date = as.Date("2100-01-01")
    )
  )
  cdm <- generateDrugUtilisationCohortSet(
    cdm, "dus", list(acetaminophen = c(1125315, 43135274, 2905077, 1125360))
  )

  result <- cdm[["dus"]] |>
    addDrugUse(ingredientConceptId = 1125315) |>
    summariseDrugUse() |>
    expect_no_error()
  expect_true(all(c(
    "number subjects", "number records", "duration", "number_exposures",
    "cumulative_quantity", "initial_quantity", "impute_duration_percentage",
    "number_eras", "impute_daily_dose_percentage", "initial_daily_dose_milligram",
    "cumulative_dose_milligram"
  ) %in% result$variable_name))

  mockDisconnect(cdm = cdm)
})

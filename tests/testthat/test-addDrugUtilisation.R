
test_that("Basic functionality", {
  skip_on_cran()
  # basic functionality
  cdm <- mockDrugUtilisation(
    con = connection(),
    writeSchema = schema(),
    drug_exposure = dplyr::tibble(
      drug_exposure_id = 1:12,
      person_id = c(1, 1, 1, 2, 2, 3, 3, 1, 2, 4, 4, 1),
      drug_concept_id = c(
        1125360, 2905077, 1125360, 1125360, 1125315, 1125360, 1125360, 1503327,
        1503328, 1503297, 1503297, 1125360
      ),
      drug_exposure_start_date = as.Date(c(
        "2020-01-15", "2020-01-20", "2020-02-20", "2021-02-15", "2021-05-12",
        "2022-01-12", "2022-11-15", "2020-01-01", "2021-03-11", "2010-01-01",
        "2010-03-15", "2025-01-01"
      )),
      drug_exposure_end_date = as.Date(c(
        "2020-01-25", "2020-03-15", "2020-02-28", "2021-03-15", "2021-05-25",
        "2022-02-15", "2022-12-14", "2020-04-13", "2021-04-20", "2010-01-05",
        "2010-05-12", "2025-12-31"
      )),
      drug_type_concept_id = 0,
      quantity = c(10, 20, 30, 1, 10, 5, 15, 20, 30, 14, 10, 2)
    ),
    dus_cohort = dplyr::tibble(
      cohort_definition_id = c(1, 2, 1, 1, 1, 2),
      subject_id = c(1, 1, 2, 3, 4, 4),
      cohort_start_date = as.Date(c(
        "2020-01-15", "2020-01-24", "2021-01-15", "2022-02-01", "2010-01-05",
        "2010-01-05"
      )),
      cohort_end_date = as.Date(c(
        "2020-02-28", "2020-02-10", "2021-06-08", "2022-12-01", "2010-03-15",
        "2010-03-15"
      )),
      extra_column = "asd"
    ),
    observation_period = dplyr::tibble(
      observation_period_id = 1:4,
      person_id = 1:4,
      observation_period_start_date = as.Date("2000-01-01"),
      observation_period_end_date = as.Date("2030-01-01"),
      period_type_concept_id = 0
    )
  )

  # basic functionality
  expect_no_error(
    x0 <- cdm$dus_cohort |>
      addDrugUtilisation(ingredientConceptId = 1125315, gapEra = 1) |>
      dplyr::collect() |>
      dplyr::arrange(cohort_definition_id, subject_id, cohort_start_date)
  )
  expect_true(all(colnames(cdm$dus_cohort) %in% colnames(x0)))
  expect_identical(colnames(x0) |> sort(), c(
    "cohort_definition_id", "cohort_end_date", "cohort_start_date",
    "cumulative_dose_milligram_ingredient_1125315_descendants_1125315",
    "cumulative_quantity_ingredient_1125315_descendants",
    "exposed_time_ingredient_1125315_descendants", "extra_column",
    "initial_daily_dose_milligram_ingredient_1125315_descendants_1125315",
    "initial_quantity_ingredient_1125315_descendants",
    "number_eras_ingredient_1125315_descendants",
    "number_exposures_ingredient_1125315_descendants", "subject_id",
    "time_to_exposure_ingredient_1125315_descendants"
  ))
  expect_identical(x0$number_exposures_ingredient_1125315_descendants, c(
    3L, 2L, 1L, 0L, 0L, 0L))
  expect_identical(x0$number_eras_ingredient_1125315_descendants, c(
    1L, 2L, 1L, 0L, 0L, 0L))
  expect_identical(x0$time_to_exposure_ingredient_1125315_descendants, c(
    0L, 31L, 287L, NA, NA, NA))
  expect_identical(x0$cumulative_quantity_ingredient_1125315_descendants, c(
    60, 11, 15, 0, 0, 0))
  expect_identical(x0$initial_quantity_ingredient_1125315_descendants, c(
    10, 1, 15, 0, 0, 0))
  expect_identical(x0$exposed_time_ingredient_1125315_descendants, c(
    45L, 43L, 17L, 0L, 0L, 0L))
  expect_equal(
    x0$cumulative_dose_milligram_ingredient_1125315_descendants_1125315,
    c(5000+9600*20*40/56+15000, 500*1*29/29, 17/30*15*500, 0, 0, 0)
  )
  expect_equal(
    x0$initial_daily_dose_milligram_ingredient_1125315_descendants_1125315,
    c(500*10/11, 500*1/29, 500*15/30, 0, 0, 0)
  )

  # restrictIncident
  expect_no_error(
    x1 <- cdm$dus_cohort |>
      addDrugUtilisation(ingredientConceptId = 1125315, gapEra = 1, restrictIncident = F) |>
      dplyr::collect() |>
      dplyr::arrange(cohort_definition_id, subject_id, cohort_start_date)
  )
  expect_true(all(colnames(cdm$dus_cohort) %in% colnames(x1)))
  expect_identical(
    x1$number_exposures_ingredient_1125315_descendants,
    c(3L, 2L, 2L, 0L, 2L, 0L)
  )
  expect_equal(
    x1$cumulative_dose_milligram_ingredient_1125315_descendants_1125315,
    c(5000+9600*20*40/56+15000, 500*1*29/29, 17/30*15*500+15*5*500/35, 0, 10*500*2/11+9600*20*18/56, 0)
  )
  expect_equal(
    x1$initial_daily_dose_milligram_ingredient_1125315_descendants_1125315,
    c(500*10/11, 500*1/29, 5*500/35, 0, 10*500/11+9600*20/56, 0)
  )

  # gapEra
  expect_no_error(
    x2 <- cdm$dus_cohort |>
      addDrugUtilisation(ingredientConceptId = 1125315, gapEra = 57) |>
      dplyr::collect() |>
      dplyr::arrange(cohort_definition_id, subject_id, cohort_start_date)
  )
  expect_identical(
    x2$number_eras_ingredient_1125315_descendants,
    c(1L, 2L, 1L, 0L, 0L, 0L)
  )
  expect_no_error(
    x3 <- cdm$dus_cohort |>
      addDrugUtilisation(ingredientConceptId = 1125315, gapEra = 58) |>
      dplyr::collect() |>
      dplyr::arrange(cohort_definition_id, subject_id, cohort_start_date)
  )
  expect_identical(
    x3$number_eras_ingredient_1125315_descendants,
    c(1L, 1L, 1L, 0L, 0L, 0L)
  )

  # two conceptSets
  codes <- CodelistGenerator::getDrugIngredientCodes(
    cdm = cdm, name = c("acetaminophen", "metformin")
  )

  # two ingredients

  # two conceptSets + two ingredients

  # indexDate

  # censorDate

  # multiple conceptSets

  # multiple igredients

  # nameStyle

  mockDisconnect(cdm = cdm)
})

test_that("gapEra consecutive prescriptions", {
  skip_on_cran()
  cdm <- mockDrugUtilisation(
    con = connection(),
    writeSchema = schema(),
    drug_exposure = dplyr::tibble(
      drug_exposure_id = 1:2,
      person_id = c(1, 1),
      drug_concept_id = c(1125360, 2905077),
      drug_exposure_start_date = as.Date(c("2020-01-01", "2020-01-20")),
      drug_exposure_end_date = as.Date(c("2020-01-19", "2020-03-15")),
      drug_type_concept_id = 0,
      quantity = 10
    ),
    dus_cohort = dplyr::tibble(
      cohort_definition_id = 1L,
      subject_id = 1L,
      cohort_start_date = as.Date("2000-01-01"),
      cohort_end_date = as.Date("2022-12-01")
    ),
    observation_period = dplyr::tibble(
      observation_period_id = 1,
      person_id = 1,
      observation_period_start_date = as.Date("2000-01-01"),
      observation_period_end_date = as.Date("2030-01-01"),
      period_type_concept_id = 0
    )
  )

  expect_no_error(
    x <- cdm$dus_cohort |>
      addDrugUtilisation(ingredientConceptId = 1125315, gapEra = 1) |>
      dplyr::collect()
  )
  expect_identical(x$number_exposures_ingredient_1125315_descendants, 2L)
  expect_identical(x$number_eras_ingredient_1125315_descendants, 1L)

  mockDisconnect(cdm = cdm)
})

test_that("test subfunctions", {
  skip_on_cran()
  cdm <- mockDrugUtilisation(
    con = connection(),
    writeSchema = schema(),
    drug_exposure = dplyr::tibble(
      drug_exposure_id = 1:12,
      person_id = c(1, 1, 1, 2, 2, 3, 3, 1, 2, 4, 4, 1),
      drug_concept_id = c(
        1125360, 2905077, 1125360, 1125360, 1125315, 1125360, 1125360, 1503327,
        1503328, 1503297, 1503297, 1125360
      ),
      drug_exposure_start_date = as.Date(c(
        "2020-01-15", "2020-01-20", "2020-02-20", "2021-02-15", "2021-05-12",
        "2022-01-12", "2022-11-15", "2020-01-01", "2021-03-11", "2010-01-01",
        "2010-03-15", "2025-01-01"
      )),
      drug_exposure_end_date = as.Date(c(
        "2020-01-25", "2020-03-15", "2020-02-28", "2021-03-15", "2021-05-25",
        "2022-02-15", "2022-12-14", "2020-04-13", "2021-04-20", "2010-01-05",
        "2010-05-12", "2025-12-31"
      )),
      drug_type_concept_id = 0,
      quantity = c(10, 20, 30, 1, 10, 5, 15, 20, 30, 14, 10, 2)
    ),
    dus_cohort = dplyr::tibble(
      cohort_definition_id = c(1, 2, 1, 1, 1, 2),
      subject_id = c(1, 1, 2, 3, 4, 4),
      cohort_start_date = as.Date(c(
        "2020-01-15", "2020-01-24", "2021-01-15", "2022-02-01", "2010-01-05",
        "2010-01-05"
      )),
      cohort_end_date = as.Date(c(
        "2020-02-28", "2020-02-10", "2021-06-08", "2022-12-01", "2010-03-15",
        "2010-03-15"
      )),
      extra_column = "asd"
    ),
    observation_period = dplyr::tibble(
      observation_period_id = 1:4,
      person_id = 1:4,
      observation_period_start_date = as.Date("2000-01-01"),
      observation_period_end_date = as.Date("2030-01-01"),
      period_type_concept_id = 0
    )
  )

  # main
  expect_no_error(
    x0 <- cdm$dus_cohort |>
      addDrugUtilisation(ingredientConceptId = 1125315, gapEra = 1) |>
      dplyr::collect() |>
      dplyr::arrange(cohort_definition_id, subject_id, cohort_start_date)
  )

  ## addNumberExposures
  codes <- CodelistGenerator::getDrugIngredientCodes(
    cdm = cdm, name = "acetaminophen")
  names(codes) <- "acetaminophen"
  expect_identical(
    x0$number_exposures_ingredient_1125315_descendants,
    cdm$dus_cohort |>
      addNumberExposures(conceptSet = codes) |>
      dplyr::collect() |>
      dplyr::arrange(cohort_definition_id, subject_id, cohort_start_date) |>
      dplyr::pull("number_exposures_acetaminophen")
  )

  ## addCumulativeDose
  expect_identical(
    x0$cumulative_dose_milligram_ingredient_1125315_descendants_1125315,
    cdm$dus_cohort |>
      addCumulativeDose(ingredientConceptId = 1125315) |>
      dplyr::collect() |>
      dplyr::arrange(cohort_definition_id, subject_id, cohort_start_date) |>
      dplyr::pull("cumulative_dose_ingredient_1125315_descendants_1125315")
  )

  ## addInitialDailyDose
  expect_identical(
    x0$initial_daily_dose_milligram_ingredient_1125315_descendants_1125315,
    cdm$dus_cohort |>
      addInitialDailyDose(ingredientConceptId = 1125315) |>
      dplyr::collect() |>
      dplyr::arrange(cohort_definition_id, subject_id, cohort_start_date) |>
      dplyr::pull("initial_daily_dose_ingredient_1125315_descendants_1125315")
  )

  ## addCumulativeQuantity
  expect_identical(
    x0$cumulative_quantity_ingredient_1125315_descendants,
    cdm$dus_cohort |>
      addCumulativeQuantity(conceptSet = codes) |>
      dplyr::collect() |>
      dplyr::arrange(cohort_definition_id, subject_id, cohort_start_date) |>
      dplyr::pull("cumulative_quantity_acetaminophen")
  )

  ## addInitialQuantity
  expect_identical(
    x0$initial_quantity_ingredient_1125315_descendants,
    cdm$dus_cohort |>
      addInitialQuantity(conceptSet = codes) |>
      dplyr::collect() |>
      dplyr::arrange(cohort_definition_id, subject_id, cohort_start_date) |>
      dplyr::pull("initial_quantity_acetaminophen")
  )

  ## addTimeToExposure
  expect_identical(
    x0$time_to_exposure_ingredient_1125315_descendants,
    cdm$dus_cohort |>
      addTimeToExposure(conceptSet = codes) |>
      dplyr::collect() |>
      dplyr::arrange(cohort_definition_id, subject_id, cohort_start_date) |>
      dplyr::pull("time_to_exposure_acetaminophen")
  )

  ## addExposedTime
  expect_identical(
    x0$exposed_time_ingredient_1125315_descendants,
    cdm$dus_cohort |>
      addExposedTime(conceptSet = codes, gapEra = 1) |>
      dplyr::collect() |>
      dplyr::arrange(cohort_definition_id, subject_id, cohort_start_date) |>
      dplyr::pull("exposed_time_acetaminophen")
  )

  ## addNumberEras
  expect_identical(
    x0$number_eras_ingredient_1125315_descendants,
    cdm$dus_cohort |>
      addNumberEras(conceptSet = codes, gapEra = 1) |>
      dplyr::collect() |>
      dplyr::arrange(cohort_definition_id, subject_id, cohort_start_date) |>
      dplyr::pull("number_eras_acetaminophen")
  )

  # errors: check correct call to parent frame
  expect_snapshot(addNumberEras(cdm$dus_cohort, NULL, gapEra = 1), error = TRUE)
  expect_snapshot(addExposedTime(cdm$dus_cohort, NULL, gapEra = 1), error = TRUE)
  expect_snapshot(addTimeToExposure(cdm$dus_cohort, NULL), error = TRUE)
  expect_snapshot(addInitialQuantity(cdm$dus_cohort, NULL), error = TRUE)
  expect_snapshot(addCumulativeQuantity(cdm$dus_cohort, NULL), error = TRUE)
  expect_snapshot(addNumberExposures(cdm$dus_cohort, NULL), error = TRUE)
  expect_snapshot(addCumulativeDose(cdm$dus_cohort, NULL), error = TRUE)
  expect_snapshot(addInitialDailyDose(cdm$dus_cohort, NULL), error = TRUE)
  expect_snapshot(addDrugUtilisation(cdm$dus_cohort, gapEra = 1), error = TRUE)

  mockDisconnect(cdm = cdm)
})

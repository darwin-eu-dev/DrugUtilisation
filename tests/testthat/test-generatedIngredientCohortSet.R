test_that("test same results for ingredient cohorts", {
  skip_on_cran()
  cdm <- mockDrugUtilisation(con = connection(), writeSchema = schema())

  cdm <- generateIngredientCohortSet(
    cdm = cdm,
    ingredient = "acetaminophen",
    name = "test_cohort_1"
  )


  cdm <- generateDrugUtilisationCohortSet(
    cdm = cdm,
    conceptSet = CodelistGenerator::getDrugIngredientCodes(
      cdm = cdm, name = "acetaminophen"
    ),
    name = "test_cohort_2"
  )

  # Collect data from DuckDB tables into R data frames
  cohort_1_df <- cdm$test_cohort_1 |>
    dplyr::collect() |>
    dplyr::arrange(subject_id, cohort_start_date)
  cohort_2_df <- cdm$test_cohort_2 |>
    dplyr::collect() |>
    dplyr::arrange(subject_id, cohort_start_date)

  attr(cohort_1_df, "cohort_set") <- attr(cohort_1_df, "cohort_set") |>
    dplyr::select(-c(
      "dose_form", "ingredient_range_min", "ingredient_range_max"
    ))

  expect_equal(cohort_1_df, cohort_2_df)

  mockDisconnect(cdm = cdm)
})

test_that("options", {
  skip_on_cran()
  cdm <- mockDrugUtilisation(
    con = connection(),
    writeSchema = schema()
  )

  expect_no_error(generateIngredientCohortSet(
    cdm = cdm,
    ingredient = "acetaminophen",
    doseUnit = "milligram",
    name = "test_cohort_1"
  ))
})

test_that("handle empty ingredient name gracefully", {
  skip_on_cran()
  cdm <- mockDrugUtilisation(con = connection(), writeSchema = schema())

  expect_error(generateIngredientCohortSet(
    cdm = cdm, ingredient = "", name = "empty_ingredient_test"
  ))

  expect_error(generateIngredientCohortSet(
    cdm = cdm, ingredient = "nonexistent", name = "nonexistent_ingredient_test"
  ))

  mockDisconnect(cdm = cdm)
})

test_that("date works", {
  skip_on_cran()
  cdm <- mockDrugUtilisation(con = connection(), writeSchema = schema())

  cdm <- generateIngredientCohortSet(
    cdm = cdm,
    ingredient = "acetaminophen",
    name = "date_range_test"
  )

  cdm$date_range_test <- cdm$date_range_test |>
    requireDrugInDateRange(
      dateRange = c(as.Date("2020-01-01"), as.Date("2020-12-31"))
    ) |>
    requireDrugInDateRange(
      dateRange = c(as.Date("2020-01-01"), as.Date("2020-12-31")),
      indexDate = "cohort_end_date"
    )

  cohort_df <- cdm$date_range_test |> dplyr::collect()

  expect_true(all(
    cohort_df$cohort_start_date >= as.Date("2020-01-01") &
      cohort_df$cohort_end_date <= as.Date("2020-12-31")
  ))

  mockDisconnect(cdm = cdm)
})

test_that("ingredient list and vector both work", {
  skip_on_cran()
  cdm <- mockDrugUtilisation(con = connection(), writeSchema = schema())

  ingredient1 <- c("simvastatin", "acetaminophen", "metformin")

  cdm <- generateIngredientCohortSet(
    cdm = cdm,
    ingredient = ingredient1,
    name = "test_vector"
  )

  expect_true(length(cdm$test_vector |> dplyr::pull("cohort_definition_id") |> unique()) == 3)

  ingredient2 <- list(
    "test_1" = c("simvastatin", "acetaminophen"),
    "test_2" = "metformin"
  )

  cdm <- generateIngredientCohortSet(
    cdm = cdm,
    ingredient = ingredient2,
    name = "test_list"
  )
  expect_true(length(cdm$test_list |> dplyr::pull("cohort_definition_id") |> unique()) == 2)

  expect_true(all(
    settings(cdm$test_vector) |> dplyr::pull("cohort_name") |> sort() == c(
      "161_acetaminophen", "36567_simvastatin", "6809_metformin"
    )
  ))

  expect_true(all(settings(cdm$test_list) |> dplyr::pull("cohort_name") |>
    sort() == c("test_1", "test_2")))

  mockDisconnect(cdm = cdm)
})

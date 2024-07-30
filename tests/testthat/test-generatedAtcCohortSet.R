test_that("test same results for ingredient cohorts", {
  skip_on_cran()
  cdm <- mockDrugUtilisation(con = connection(), writeSchema = schema())

  expect_warning(cdm <- generateAtcCohortSet(cdm = cdm, name = "test_cohort_1"))

  codes <- CodelistGenerator::getATCCodes(cdm)
  names(codes) <- tolower(names(codes))
  cdm <- generateDrugUtilisationCohortSet(
    cdm = cdm,
    conceptSet = codes,
    name = "test_cohort_2"
  )

  # Collect data from DuckDB tables into R data frames
  cohort_1_df <- cdm$test_cohort_1 |> dplyr::collect()
  cohort_2_df <- cdm$test_cohort_2 |> dplyr::collect()
  attr(cohort_1_df, "cohort_set") <- attr(cohort_1_df, "cohort_set") |>
    dplyr::select(-"dose_form")

  expect_equal(cohort_1_df, cohort_2_df)

  mockDisconnect(cdm = cdm)
})

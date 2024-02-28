test_that("test same results for ingredient cohorts", {
  cdm <- DrugUtilisation::mockDrugUtilisation()


  cdm <- generateIngredientCohortSet(cdm,
    ingredient = "acetaminophen",
    name = "test_cohort_1"
  )


  cdm <- DrugUtilisation::generateDrugUtilisationCohortSet(cdm,
    conceptSet = CodelistGenerator::getDrugIngredientCodes(cdm,
      name = "acetaminophen"
    ),
    name = "test_cohort_2"
  )

  # Collect data from DuckDB tables into R data frames
  cohort_1_df <- cdm$test_cohort_1 %>% dplyr::collect() %>% dplyr::arrange(subject_id, cohort_start_date)
  cohort_2_df <- cdm$test_cohort_2 %>% dplyr::collect() %>% dplyr::arrange(subject_id, cohort_start_date)

  expect_equal(cohort_1_df, cohort_2_df)

})

test_that("handle empty ingredient name gracefully", {
  cdm <- DrugUtilisation::mockDrugUtilisation()

  expect_error(generateIngredientCohortSet(cdm, ingredient = "", name = "empty_ingredient_test"))

  expect_error(generateIngredientCohortSet(cdm,
                                           ingredient = "nonexistent",
                                           name = "nonexistent_ingredient_test"))

})



test_that("date works", {

  cdm <- DrugUtilisation::mockDrugUtilisation()

  cdm <- generateIngredientCohortSet(cdm,
                                     ingredient = "acetaminophen",
                                     cohortDateRange = c(as.Date("2020-01-01"),
                                                         as.Date("2020-12-31")),
                                     name = "date_range_test")

  cohort_df <- cdm$date_range_test %>% dplyr::collect()

  expect_true(all(cohort_df$cohort_start_date >= as.Date("2020-01-01") &
                    cohort_df$cohort_end_date <= as.Date("2020-12-31")))


})

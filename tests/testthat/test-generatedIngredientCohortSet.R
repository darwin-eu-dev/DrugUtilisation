test_that("test same results for ingredient cohorts", {
  cdm <- DrugUtilisation::mockDrugUtilisation()

  cdm <- generateIngredientCohortSet(
    cdm = cdm,
    ingredient = "acetaminophen",
    name = "test_cohort_1"
  )


  cdm <- DrugUtilisation::generateDrugUtilisationCohortSet(
    cdm = cdm,
    conceptSet = CodelistGenerator::getDrugIngredientCodes(
      cdm = cdm,
      name = "acetaminophen"
    ),
    name = "test_cohort_2"
  )

  # Collect data from DuckDB tables into R data frames
  cohort_1_df <- cdm$test_cohort_1 %>%
    dplyr::collect() %>%
    dplyr::arrange(subject_id, cohort_start_date)
  cohort_2_df <- cdm$test_cohort_2 %>%
    dplyr::collect() %>%
    dplyr::arrange(subject_id, cohort_start_date)

  attr(cohort_1_df, "cohort_set") <- attr(cohort_1_df, "cohort_set") |>
    dplyr::select(-c(
      "dose_form", "ingredient_range_min", "ingredient_range_max"
    ))

  expect_equal(cohort_1_df, cohort_2_df)

})

test_that("handle empty ingredient name gracefully", {
  cdm <- DrugUtilisation::mockDrugUtilisation()

  expect_error(generateIngredientCohortSet(
    cdm = cdm, ingredient = "", name = "empty_ingredient_test"
  ))

  expect_error(generateIngredientCohortSet(
    cdm = cdm, ingredient = "nonexistent", name = "nonexistent_ingredient_test"
  ))

})

# test_that("date works", {
#
#   cdm <- DrugUtilisation::mockDrugUtilisation()
#
#   cdm <- generateIngredientCohortSet(
#     cdm = cdm,
#     ingredient = "acetaminophen",
#     cohortDateRange = c(as.Date("2020-01-01"), as.Date("2020-12-31")),
#     name = "date_range_test"
#   )
#
#   cohort_df <- cdm$date_range_test %>% dplyr::collect()
#
#   expect_true(all(
#     cohort_df$cohort_start_date >= as.Date("2020-01-01") &
#       cohort_df$cohort_end_date <= as.Date("2020-12-31")
#   ))
#
# })


test_that("ingredient list and vector both work", {

  cdm <- DrugUtilisation::mockDrugUtilisation()

  ingredient1 = c("simvastatin", "acetaminophen", "metformin")

  cdm <- generateIngredientCohortSet(
    cdm = cdm,
    ingredient = ingredient1,
    name = "test_vector"
  )

  expect_true(length(cdm$test_vector |> dplyr::pull("cohort_definition_id") |> unique()) == 3)

  ingredient2 = list( "test_1" = c("simvastatin", "acetaminophen"),
                      "test_2" = "metformin")

  cdm <- generateIngredientCohortSet(
    cdm = cdm,
    ingredient = ingredient2,
    name = "test_list"
  )
  expect_true(length(cdm$test_list |> dplyr::pull("cohort_definition_id") |> unique()) == 2)

  expect_true(all(CDMConnector::cohort_set(cdm$test_vector) |> dplyr::pull("cohort_name")|>
    sort() == c("acetaminophen","metformin", "simvastatin")))

  expect_true(all(CDMConnector::cohort_set(cdm$test_list) |> dplyr::pull("cohort_name")|>
                    sort() == c("test_1","test_2")))


})

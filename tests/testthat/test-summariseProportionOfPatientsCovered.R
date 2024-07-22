test_that("simple working example", {

  # basic example one cohort entry
  # id 1 - in cohort for 5 days, exits the database after 25
  # id 2 - in cohort for 15 days, exits the database after 15
  # id 3 - in cohort for 20 days, exits the database after 25
  # id 4 - in cohort for 20 days, exits the database after 25

  cdm <- mockDrugUtilisation(
    connectionDetails = connectionDetails,
    drug_exposure = dplyr::tibble(
      drug_exposure_id = 1:4,
      person_id = c(1, 2, 3, 4),
      drug_concept_id = c(1125360),
      drug_exposure_start_date = as.Date(c("2000-01-01", "2002-01-01", "2010-01-01", "2011-01-01")),
      drug_exposure_end_date = as.Date(c("2000-01-05", "2003-01-15", "2020-01-20", "2021-01-20")),
      drug_type_concept_id = 0,
      quantity = 0
    ),
    dus_cohort = dplyr::tibble(
      cohort_definition_id = 1,
      subject_id = c(1, 2, 3, 4),
      cohort_start_date = as.Date(c("2000-01-01", "2002-01-01", "2010-01-01", "2011-01-01")),
      cohort_end_date = as.Date(c("2000-01-05", "2002-01-15", "2010-01-20", "2011-01-20"))
    ),
    observation_period = dplyr::tibble(
      observation_period_id = 1:4,
      person_id = 1:4,
      observation_period_start_date = as.Date(c("2000-01-01", "2002-01-01", "2010-01-01", "2011-01-01")),
      observation_period_end_date = as.Date(c("2000-01-25", "2002-01-15", "2010-01-25", "2011-01-25")),
      period_type_concept_id = 0
    )
  )

  ppc <- cdm$dus_cohort |>
    summariseProportionOfPatientsCovered()
  expect_true(inherits(ppc, "summarised_result"))
  # from first cohort entry to last cohort exit
  expect_true(max(as.numeric(ppc$additional_level)) ==
                clock::date_count_between(start = as.Date("2000-01-01"),
                                          end = as.Date("2000-01-20"), "day"))

  ppc <- cdm$dus_cohort |>
    summariseProportionOfPatientsCovered(followUpDays = 30)
  expect_true(inherits(ppc, "summarised_result"))
  expect_true(max(as.numeric(ppc$additional_level)) == 30)

  # everyone treated on day zero
  expect_true(ppc |>
    dplyr::filter(additional_level == 0,
                  estimate_name == "ppc") |>
    dplyr::pull("estimate_value") == "1")

  # on day 4, 4 of 4 people being treated
  expect_true(ppc |>
                dplyr::filter(additional_level == 4,
                              estimate_name == "ppc") |>
                dplyr::pull("estimate_value") == "1")

  # on day 6, 3 of 4 people being treated
  expect_true(ppc |>
                dplyr::filter(additional_level == 6,
                              estimate_name == "ppc") |>
                dplyr::pull("estimate_value") == "0.75")

  # on day 16, 2 of 3 people being treated, 1 has left
  expect_true(ppc |>
                dplyr::filter(additional_level == 16,
                              estimate_name == "ppc") |>
                dplyr::pull("estimate_value") == as.character(2/3))

  # on day 21, 0 of 3 people being treated, 1 has left
  expect_true(ppc |>
                dplyr::filter(additional_level == 21,
                              estimate_name == "ppc") |>
                dplyr::pull("estimate_value") == "0")
})

test_that("multiple cohort entries", {
  # id 1 - in cohort for 5 days, out for 5 days, in again for 5, exits database after 20
  # id 2 - in cohort for 15 days, exits the database after 25

  cdm <- mockDrugUtilisation(
    connectionDetails = connectionDetails,
    drug_exposure = dplyr::tibble(
      drug_exposure_id = 1:3,
      person_id = c(1, 1, 2),
      drug_concept_id = c(1125360),
      drug_exposure_start_date = as.Date(c("2000-01-01", "2000-01-10", "2010-01-01")),
      drug_exposure_end_date = as.Date(c("2000-01-05", "2000-01-15", "2010-01-15")),
      drug_type_concept_id = 0,
      quantity = 0
    ),
    dus_cohort = dplyr::tibble(
      cohort_definition_id = 1,
      subject_id = c(1, 1, 2),
      cohort_start_date = as.Date(c("2000-01-01", "2000-01-10", "2010-01-01")),
      cohort_end_date = as.Date(c("2000-01-05", "2000-01-15", "2010-01-15"))
    ),
    observation_period = dplyr::tibble(
      observation_period_id = 1:2,
      person_id = 1:2,
      observation_period_start_date = as.Date(c("2000-01-01", "2010-01-01")),
      observation_period_end_date = as.Date(c("2000-01-20", "2010-01-25")),
      period_type_concept_id = 0
    )
  )

  ppc <- cdm$dus_cohort |>
    summariseProportionOfPatientsCovered(followUpDays = 25)

  # everyone treated on day four
  expect_true(ppc |>
                dplyr::filter(additional_level == 4,
                              estimate_name == "ppc") |>
                dplyr::pull("estimate_value") == "1")

  # one of two treated on day 6
  expect_true(ppc |>
                dplyr::filter(additional_level == 6,
                              estimate_name == "ppc") |>
                dplyr::pull("estimate_value") == "0.5")

  # both treated on day 10
  expect_true(ppc |>
                dplyr::filter(additional_level == 10,
                              estimate_name == "ppc") |>
                dplyr::pull("estimate_value") == "1")

  # one person left on day 21, untreated
  expect_true(ppc |>
                dplyr::filter(additional_level == 10,
                              estimate_name == "ppc") |>
                dplyr::pull("estimate_value") == "1")
  expect_true(ppc |>
                dplyr::filter(additional_level == 21,
                              estimate_name == "ppc") |>
                dplyr::pull("estimate_value") == "0")
})

test_that("multiple cohorts", {

  # id 1 - in cohort 1 for 5 days, exits the database after 25
  # id 2 - in cohort 1 for 15 days, exits the database after 15
  # id 3 - in cohort 1 for 20 days, exits the database after 25
  # id 4 - in cohort 2 for 25 days, exits the database after 30
  cdm <- mockDrugUtilisation(
    connectionDetails = connectionDetails,
    drug_exposure = dplyr::tibble(
      drug_exposure_id = 1:4,
      person_id = c(1, 2, 3, 4),
      drug_concept_id = c(1125360),
      drug_exposure_start_date = as.Date(c("2000-01-01", "2002-01-01", "2010-01-01", "2011-01-01")),
      drug_exposure_end_date = as.Date(c("2000-01-05", "2003-01-15", "2020-01-20", "2021-01-20")),
      drug_type_concept_id = 0,
      quantity = 0
    ),
    dus_cohort = dplyr::tibble(
      cohort_definition_id = c(1,1,1,2),
      subject_id = c(1, 2, 3, 4),
      cohort_start_date = as.Date(c("2000-01-01", "2002-01-01", "2010-01-01", "2011-01-01")),
      cohort_end_date = as.Date(c("2000-01-05", "2002-01-15", "2010-01-20", "2011-01-25"))
    ),
    observation_period = dplyr::tibble(
      observation_period_id = 1:4,
      person_id = 1:4,
      observation_period_start_date = as.Date(c("2000-01-01", "2002-01-01", "2010-01-01", "2011-01-01")),
      observation_period_end_date = as.Date(c("2000-01-25", "2002-01-15", "2010-01-25", "2011-01-30")),
      period_type_concept_id = 0
    )
  )

  ppc <- cdm$dus_cohort |>
    summariseProportionOfPatientsCovered()
  ppc_cohort_1 <- ppc |> dplyr::filter(group_level == "cohort_1")
  ppc_cohort_2 <- ppc |> dplyr::filter(group_level == "cohort_2")

  # cohort 1 should have follow up end after 20 days, cohort 2 after 25 days
  expect_true(max(as.numeric(ppc_cohort_1$additional_level)) ==
                clock::date_count_between(start = as.Date("2000-01-01"),
                                          end = as.Date("2000-01-20"), "day"))
  expect_true(max(as.numeric(ppc_cohort_2$additional_level)) ==
                clock::date_count_between(start = as.Date("2000-01-01"),
                                          end = as.Date("2000-01-25"), "day"))

  # everyone treated on day zero
  expect_true(ppc_cohort_1 |>
                dplyr::filter(additional_level == 0,
                              estimate_name == "denominator_count") |>
                dplyr::pull("estimate_value") == "3")
  expect_true(ppc_cohort_1 |>
                dplyr::filter(additional_level == 0,
                              estimate_name == "numerator_count") |>
                dplyr::pull("estimate_value") == "3")
  expect_true(ppc_cohort_2 |>
                dplyr::filter(additional_level == 0,
                              estimate_name == "denominator_count") |>
                dplyr::pull("estimate_value") == "1")
  expect_true(ppc_cohort_2 |>
                dplyr::filter(additional_level == 0,
                              estimate_name == "numerator_count") |>
                dplyr::pull("estimate_value") == "1")

  # on day 12,
  # cohort 1 - 2 of 3 people being treated
  expect_true(ppc_cohort_1 |>
                dplyr::filter(additional_level == 12,
                              estimate_name == "ppc") |>
                dplyr::pull("estimate_value") == as.character(2/3))
  # cohort 2 - 1 of 1 people being treated
  expect_true(ppc_cohort_2 |>
                dplyr::filter(additional_level == 12,
                              estimate_name == "ppc") |>
                dplyr::pull("estimate_value") == "1")

})

test_that("stratification", {



})

test_that("expected errors", {

  cdm <- mockDrugUtilisation(
    connectionDetails = connectionDetails,
    dus_cohort = dplyr::tibble(
      cohort_definition_id = c(1,1,1,2),
      subject_id = c(1, 2, 3, 4),
      cohort_start_date = as.Date(c("2000-01-01", "2002-01-01", "2010-01-01", "2011-01-01")),
      cohort_end_date = as.Date(c("2000-01-05", "2002-01-15", "2010-01-20", "2011-01-25"))
    ),
    observation_period = dplyr::tibble(
      observation_period_id = 1:4,
      person_id = 1:4,
      observation_period_start_date = as.Date(c("2000-01-01", "2002-01-01", "2010-01-01", "2011-01-01")),
      observation_period_end_date = as.Date(c("2000-01-25", "2002-01-15", "2010-01-25", "2011-01-30")),
      period_type_concept_id = 0
    )
  )

  ppc <- cdm$dus_cohort |>
    summariseProportionOfPatientsCovered()

  # no error if empty cohort table
  expect_warning(ppc <- cdm$dus_cohort |>
    dplyr::filter(subject_id == 99) |>
    summariseProportionOfPatientsCovered())
  expect_true(nrow(ppc) == 0)

  # if cohort ID exists no error
  ppc <- cdm$dus_cohort |>
    summariseProportionOfPatientsCovered(cohortId = 2)
  expect_true(all(ppc$group_level == "cohort_2"))
  # but error if cohort id not in settings
  expect_error(ppc <- cdm$dus_cohort |>
    summariseProportionOfPatientsCovered(cohortId = 99))

  # followUpDays must be a single number above zero
  expect_error(ppc <- cdm$dus_cohort |>
    summariseProportionOfPatientsCovered(followUpDays = -10))
  expect_error(ppc <- cdm$dus_cohort |>
    summariseProportionOfPatientsCovered(followUpDays = c(1,2)))
  expect_error(ppc <- cdm$dus_cohort |>
    summariseProportionOfPatientsCovered(followUpDays = "a"))

  # strata variable must exist
  expect_error(ppc <- cdm$dus_cohort |>
    summariseProportionOfPatientsCovered(strata = list("not_a_column")))

  })

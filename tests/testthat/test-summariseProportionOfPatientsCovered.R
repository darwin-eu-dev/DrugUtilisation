test_that("proportion of patients covered", {

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


  # multiple cohort enties


  # multiple cohorts and multiple cohort entries


  # check logic of follow up days


  cdm <- mockDrugUtilisation(
    connectionDetails = connectionDetails,
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
      quantity = 0
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

 ppc <- cdm$dus_cohort |>
    summariseProportionOfPatientsCovered()

  expect_true(inherits(ppc, "summarised_result"))



  # no error if empty cohort table
  expect_warning(ppc <- cdm$dus_cohort |>
    dplyr::filter(subject_id == 99) |>
    summariseProportionOfPatientsCovered())
  expect_true(nrow(ppc) == 0)

  # if cohort ID
  ppc <- cdm$dus_cohort |>
    summariseProportionOfPatientsCovered(cohortId = 2)
  expect_true(all(ppc$group_level == "cohort_2"))


  # expected errors
  expect_error(ppc <- cdm$dus_cohort |>
    summariseProportionOfPatientsCovered(cohortId = 99))





  })

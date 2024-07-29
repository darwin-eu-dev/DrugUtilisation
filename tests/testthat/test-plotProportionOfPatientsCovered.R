test_that("basic plot", {
  skip_on_cran()
  cdm <- mockDrugUtilisation(
    con = connection(),
    writeSchema = schema(),
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

  expect_no_error(plotProportionOfPatientsCovered(ppc))
  expect_no_error(plotProportionOfPatientsCovered(ppc, ylim = c(0.25, 1)))
  expect_no_error(plotProportionOfPatientsCovered(ppc, facet = "cdm_name"))
  expect_no_error(plotProportionOfPatientsCovered(ppc, colour = "group_level"))


  # expected warnings - empty result or result with no ppc
  expect_warning(plotProportionOfPatientsCovered(
    omopgenerics::emptySummarisedResult()
  ))
  expect_warning(plotProportionOfPatientsCovered(ppc |>
    dplyr::filter(estimate_name != "ppc")))
  # expected errors
  expect_error(plotProportionOfPatientsCovered(cars))
  expect_error(plotProportionOfPatientsCovered(ppc,
    facet = "not_a_var"
  ))
  expect_error(plotProportionOfPatientsCovered(ppc,
    colour = "not_a_var"
  ))

  mockDisconnect(cdm = cdm)
})

test_that("multiple cohorts", {
  skip_on_cran()
  # id 1 - in cohort 1 for 5 days, exits the database after 25
  # id 2 - in cohort 1 for 15 days, exits the database after 15
  # id 3 - in cohort 1 for 20 days, exits the database after 25
  # id 4 - in cohort 2 for 25 days, exits the database after 30
  cdm <- mockDrugUtilisation(
    con = connection(),
    writeSchema = schema(),
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
      cohort_definition_id = c(1, 1, 1, 2),
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

  expect_no_error(plotProportionOfPatientsCovered(ppc, facet = "group_level"))
  expect_no_error(plotProportionOfPatientsCovered(ppc, colour = "group_level"))

  mockDisconnect(cdm = cdm)
})

test_that("stratification", {
  skip_on_cran()
  # basic example one cohort entry
  # id 1 - in cohort for 5 days, break, 5 more days, exits the database after 25
  # id 2 - in cohort for 15 days, exits the database after 15
  # id 3 - in cohort for 20 days, exits the database after 25
  # id 4 - in cohort for 20 days, exits the database after 25

  cdm <- mockDrugUtilisation(
    con = connection(),
    writeSchema = schema(),
    dus_cohort = dplyr::tibble(
      cohort_definition_id = 1,
      subject_id = c(1, 1, 2, 3, 4),
      cohort_start_date = as.Date(c("2000-01-01", "2000-01-10", "2002-01-01", "2010-01-01", "2011-01-01")),
      cohort_end_date = as.Date(c("2000-01-05", "2000-01-15", "2002-01-15", "2010-01-20", "2011-01-20"))
    ),
    observation_period = dplyr::tibble(
      observation_period_id = 1:4,
      person_id = 1:4,
      observation_period_start_date = as.Date(c("2000-01-01", "2002-01-01", "2010-01-01", "2011-01-01")),
      observation_period_end_date = as.Date(c("2000-01-25", "2002-01-15", "2010-01-25", "2011-01-25")),
      period_type_concept_id = 0
    )
  )
  cdm$dus_cohort <- cdm$dus_cohort |>
    dplyr::mutate(
      var0 = "group",
      var1 = dplyr::if_else(subject_id == 1,
        "group_1", "group_2"
      ),
      var2 = dplyr::if_else(subject_id %in% c(1, 2),
        "group_a", "group_b"
      )
    )

  ppc <- cdm$dus_cohort |>
    summariseProportionOfPatientsCovered(strata = list(
      c("var1"),
      c("var2"),
      c("var1", "var2")
    ))

  expect_no_error(plotProportionOfPatientsCovered(ppc,
    facet = "strata_name",
    colour = "strata_level"
  ))

  mockDisconnect(cdm = cdm)
})

test_that("simple working example", {
  # basic example one cohort entry
  # id 1 - in cohort for 5 days, exits the database after 25
  # id 2 - in cohort for 15 days, exits the database after 15
  # id 3 - in cohort for 20 days, exits the database after 25
  # id 4 - in cohort for 20 days, exits the database after 25

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
  expect_true(inherits(ppc, "summarised_result"))
  # from first cohort entry to last cohort exit
  expect_true(max(as.numeric(ppc$additional_level)) ==
    clock::date_count_between(
      start = as.Date("2000-01-01"),
      end = as.Date("2000-01-20"), "day"
    ))

  ppc <- cdm$dus_cohort |>
    summariseProportionOfPatientsCovered(followUpDays = 30)
  expect_true(inherits(ppc, "summarised_result"))
  expect_true(max(as.numeric(ppc$additional_level)) == 30)

  # everyone treated on day zero
  expect_true(ppc |>
    dplyr::filter(
      additional_level == 0,
      estimate_name == "ppc"
    ) |>
    dplyr::pull("estimate_value") == "100")

  # on day 4, 4 of 4 people being treated
  expect_true(ppc |>
    dplyr::filter(
      additional_level == 4,
      estimate_name == "ppc"
    ) |>
    dplyr::pull("estimate_value") == "100")

  # on day 6, 3 of 4 people being treated
  expect_true(ppc |>
    dplyr::filter(
      additional_level == 6,
      estimate_name == "ppc"
    ) |>
    dplyr::pull("estimate_value") == "75")

  # on day 16, 2 of 3 people being treated, 1 has left
  expect_true(ppc |>
    dplyr::filter(
      additional_level == 16,
      estimate_name == "ppc"
    ) |>
    dplyr::pull("estimate_value") == as.character(round((2 / 3) * 100, 2)))

  # on day 21, 0 of 3 people being treated, 1 has left
  expect_true(ppc |>
    dplyr::filter(
      additional_level == 21,
      estimate_name == "ppc"
    ) |>
    dplyr::pull("estimate_value") == "0")

  mockDisconnect(cdm = cdm)
})

test_that("multiple cohort entries", {
  skip_on_cran()
  # id 1 - in cohort for 5 days, out for 5 days, in again for 5, exits database after 20
  # id 2 - in cohort for 15 days, exits the database after 25

  cdm <- mockDrugUtilisation(
    con = connection(),
    writeSchema = schema(),
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
    dplyr::filter(
      additional_level == 4,
      estimate_name == "ppc"
    ) |>
    dplyr::pull("estimate_value") == "100")

  # one of two treated on day 6
  expect_true(ppc |>
    dplyr::filter(
      additional_level == 6,
      estimate_name == "ppc"
    ) |>
    dplyr::pull("estimate_value") == "50")

  # both treated on day 10
  expect_true(ppc |>
    dplyr::filter(
      additional_level == 10,
      estimate_name == "ppc"
    ) |>
    dplyr::pull("estimate_value") == "100")

  # one person left on day 21, untreated
  expect_true(ppc |>
    dplyr::filter(
      additional_level == 10,
      estimate_name == "ppc"
    ) |>
    dplyr::pull("estimate_value") == "100")
  expect_true(ppc |>
    dplyr::filter(
      additional_level == 21,
      estimate_name == "ppc"
    ) |>
    dplyr::pull("estimate_value") == "0")

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
  ppc_cohort_1 <- ppc |> dplyr::filter(group_level == "cohort_1")
  ppc_cohort_2 <- ppc |> dplyr::filter(group_level == "cohort_2")

  # cohort 1 should have follow up end after 20 days, cohort 2 after 25 days
  expect_true(max(as.numeric(ppc_cohort_1$additional_level)) ==
    clock::date_count_between(
      start = as.Date("2000-01-01"),
      end = as.Date("2000-01-20"), "day"
    ))
  expect_true(max(as.numeric(ppc_cohort_2$additional_level)) ==
    clock::date_count_between(
      start = as.Date("2000-01-01"),
      end = as.Date("2000-01-25"), "day"
    ))

  # everyone treated on day zero
  expect_true(ppc_cohort_1 |>
    dplyr::filter(
      additional_level == 0,
      estimate_name == "denominator_count"
    ) |>
    dplyr::pull("estimate_value") == "3")
  expect_true(ppc_cohort_1 |>
    dplyr::filter(
      additional_level == 0,
      estimate_name == "outcome_count"
    ) |>
    dplyr::pull("estimate_value") == "3")
  expect_true(ppc_cohort_2 |>
    dplyr::filter(
      additional_level == 0,
      estimate_name == "denominator_count"
    ) |>
    dplyr::pull("estimate_value") == "1")
  expect_true(ppc_cohort_2 |>
    dplyr::filter(
      additional_level == 0,
      estimate_name == "outcome_count"
    ) |>
    dplyr::pull("estimate_value") == "1")

  # on day 12,
  # cohort 1 - 2 of 3 people being treated
  expect_true(ppc_cohort_1 |>
    dplyr::filter(
      additional_level == 12,
      estimate_name == "ppc"
    ) |>
    dplyr::pull("estimate_value") == as.character(round((2 / 3) * 100, 2)))
  # cohort 2 - 1 of 1 people being treated
  expect_true(ppc_cohort_2 |>
    dplyr::filter(
      additional_level == 12,
      estimate_name == "ppc"
    ) |>
    dplyr::pull("estimate_value") == "100")


  # if one cohort is empty it should still work
  cdm$dus_cohort <- cdm$dus_cohort |>
    dplyr::filter(cohort_definition_id == 2)
  expect_no_error(cdm$dus_cohort |>
    summariseProportionOfPatientsCovered(followUpDays = 25))
  expect_no_error(cdm$dus_cohort |>
    summariseProportionOfPatientsCovered())
  expect_no_error(cdm$dus_cohort |>
    summariseProportionOfPatientsCovered(cohortId = 1))

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

  ppc_no_strata <- cdm$dus_cohort |>
    summariseProportionOfPatientsCovered(followUpDays = 30)
  ppc_no_strata_2 <- cdm$dus_cohort |>
    summariseProportionOfPatientsCovered(
      followUpDays = 30,
      strata = c("var0")
    )
  expect_identical(
    ppc_no_strata |>
      dplyr::select(!c("strata_name", "strata_level")),
    ppc_no_strata_2 |>
      dplyr::filter(strata_level == "group") |>
      dplyr::select(!c("strata_name", "strata_level"))
  )

  ppc <- cdm$dus_cohort |>
    summariseProportionOfPatientsCovered(
      followUpDays = 30,
      strata = c("var1")
    )
  expect_identical(
    ppc_no_strata,
    ppc |> dplyr::filter(strata_level == "overall")
  )

  ppc <- cdm$dus_cohort |>
    summariseProportionOfPatientsCovered(
      followUpDays = 30,
      strata = c("var1")
    )
  # var 1 - group_1 - only person one
  # day 0
  expect_true(ppc |>
    dplyr::filter(
      additional_level == 0,
      strata_level == "group_1",
      estimate_name == "denominator_count"
    ) |>
    dplyr::pull("estimate_value") == "1")
  expect_true(ppc |>
    dplyr::filter(
      additional_level == 0,
      strata_level == "group_1",
      estimate_name == "outcome_count"
    ) |>
    dplyr::pull("estimate_value") == "1")
  expect_true(ppc |>
    dplyr::filter(
      additional_level == 0,
      strata_level == "group_2",
      estimate_name == "denominator_count"
    ) |>
    dplyr::pull("estimate_value") == "3")
  expect_true(ppc |>
    dplyr::filter(
      additional_level == 0,
      strata_level == "group_2",
      estimate_name == "outcome_count"
    ) |>
    dplyr::pull("estimate_value") == "3")

  # day 1
  expect_true(ppc |>
    dplyr::filter(
      additional_level == 1,
      strata_level == "group_1",
      estimate_name == "denominator_count"
    ) |>
    dplyr::pull("estimate_value") == "1")
  expect_true(ppc |>
    dplyr::filter(
      additional_level == 1,
      strata_level == "group_1",
      estimate_name == "outcome_count"
    ) |>
    dplyr::pull("estimate_value") == "1")
  expect_true(ppc |>
    dplyr::filter(
      additional_level == 1,
      strata_level == "group_2",
      estimate_name == "denominator_count"
    ) |>
    dplyr::pull("estimate_value") == "3")
  expect_true(ppc |>
    dplyr::filter(
      additional_level == 1,
      strata_level == "group_2",
      estimate_name == "outcome_count"
    ) |>
    dplyr::pull("estimate_value") == "3")


  # day 8
  expect_true(ppc |>
    dplyr::filter(
      additional_level == 8,
      strata_level == "group_1",
      estimate_name == "denominator_count"
    ) |>
    dplyr::pull("estimate_value") == "1")
  expect_true(ppc |>
    dplyr::filter(
      additional_level == 8,
      strata_level == "group_1",
      estimate_name == "outcome_count"
    ) |>
    dplyr::pull("estimate_value") == "0")
  expect_true(ppc |>
    dplyr::filter(
      additional_level == 8,
      strata_level == "group_2",
      estimate_name == "denominator_count"
    ) |>
    dplyr::pull("estimate_value") == "3")
  expect_true(ppc |>
    dplyr::filter(
      additional_level == 8,
      strata_level == "group_2",
      estimate_name == "outcome_count"
    ) |>
    dplyr::pull("estimate_value") == "3")



  # multiple strata
  # person 1 is in var1 - group_1 and var2 group_a
  # person 2 is in var1 - group_2 and var2 group_a
  # person 3 and 4 is in var1 - group_2 and var2 group_b


  ppc <- cdm$dus_cohort |>
    summariseProportionOfPatientsCovered(strata = list(
      c("var1"),
      c("var2"),
      c("var1", "var2")
    ))

  # g1 and ga
  ppc_g1_ga <- ppc |> dplyr::filter(strata_level == "group_1 &&& group_a")
  ppc_g1 <- ppc |> dplyr::filter(strata_level == "group_1")
  # only person 1
  expect_identical(
    ppc_g1_ga |>
      dplyr::select(!c("strata_name", "strata_level")),
    ppc_g1_ga |>
      dplyr::select(!c("strata_name", "strata_level"))
  )

  # g2 and gb  - person 3 and 4
  ppc_g2_gb <- ppc |> dplyr::filter(strata_level == "group_2 &&& group_b")
  expect_true(ppc_g2_gb |>
    dplyr::filter(
      additional_level == 0,
      estimate_name == "denominator_count"
    ) |>
    dplyr::pull("estimate_value") == "2")
  expect_true(ppc_g2_gb |>
    dplyr::filter(
      additional_level == 0,
      estimate_name == "outcome_count"
    ) |>
    dplyr::pull("estimate_value") == "2")
  expect_true(ppc_g2_gb |>
    dplyr::filter(
      additional_level == 1,
      estimate_name == "denominator_count"
    ) |>
    dplyr::pull("estimate_value") == "2")
  expect_true(ppc_g2_gb |>
    dplyr::filter(
      additional_level == 1,
      estimate_name == "outcome_count"
    ) |>
    dplyr::pull("estimate_value") == "2")



  # nobody in g1 and gb
  ppc_g1_gb <- ppc |> dplyr::filter(strata_level == "group_1 &&& group_b")
  expect_true(nrow(ppc_g1_gb) == 0)

  mockDisconnect(cdm = cdm)
})

test_that("expected errors", {
  skip_on_cran()

  cdm <- mockDrugUtilisation(
    con = connection(),
    writeSchema = schema(),
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
    summariseProportionOfPatientsCovered(followUpDays = c(1, 2)))
  expect_error(ppc <- cdm$dus_cohort |>
    summariseProportionOfPatientsCovered(followUpDays = "a"))

  # strata variable must exist
  expect_error(ppc <- cdm$dus_cohort |>
    summariseProportionOfPatientsCovered(strata = list("not_a_column")))

  mockDisconnect(cdm = cdm)
})

test_that("suppression", {
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

  ppc_suppressed <- omopgenerics::suppress(ppc, 4)

  expect_true(ppc_suppressed |>
    dplyr::filter(
      additional_level == 0,
      estimate_name == "denominator_count"
    ) |>
    dplyr::pull("estimate_value") == "4")
  expect_true(is.na(ppc_suppressed |>
    dplyr::filter(
      additional_level == 15,
      estimate_name == "denominator_count"
    ) |>
    dplyr::pull("estimate_value")))

  mockDisconnect(cdm = cdm)
})

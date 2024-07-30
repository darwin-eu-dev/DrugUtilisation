test_that("summarise drug restart", {
  cdm <- mockDrugUtilisation(
    con = connection(),
    writeSchema = schema(),
    drug_exposure = dplyr::tibble(
      drug_exposure_id = 1:12,
      person_id = c(1, 1, 1, 2, 2, 2, 1, 1, 2, 4, 4, 1),
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
      cohort_definition_id = c(1, 1, 1, 1, 1, 2, 2, 2, 2),
      subject_id = c(1, 1, 2, 3, 4, 4, 1, 2, 3),
      cohort_start_date = as.Date(c(
        "2020-01-15", "2020-03-24", "2021-01-15", "2022-02-01", "2010-01-05",
        "2010-03-16", "2022-02-01", "2010-01-05", "2010-01-05"
      )),
      cohort_end_date = as.Date(c(
        "2020-02-28", "2020-05-10", "2021-06-08", "2022-12-01", "2010-03-15",
        "2010-03-30", "2023-02-01", "2010-05-05", "2010-01-05"
      )),
      censor_column = as.Date(c(
        "2021-02-28", "2021-05-10", "2022-06-08", "2023-12-01", "2010-05-15",
        "2011-03-30", "2022-02-01", "2011-05-06", "2010-03-05"
      ))
    ),
    observation_period = dplyr::tibble(
      observation_period_id = 1:4,
      person_id = 1:4,
      observation_period_start_date = as.Date("2000-01-01"),
      observation_period_end_date = as.Date("2030-01-01"),
      period_type_concept_id = 0
    )
  )

  conceptlist <- list("a" = 1125360, "b" = c(1503297, 1503327), "c" = 1503328)
  cdm <- generateDrugUtilisationCohortSet(cdm = cdm, name = "switch_cohort", conceptSet = conceptlist)
  resultsCohort <- cdm$dus_cohort |>
    summariseDrugRestart(switchCohortTable = "switch_cohort", switchCohortId = 1:2, followUpDays = c(100, 300, Inf))

  expect_true(inherits(resultsCohort, "summarised_result"))
  uniqueVars <- dplyr::tibble(
    group_level = c(rep("cohort_1", 12), rep("cohort_2", 12)),
    strata_name = "overall",
    strata_level = "overall",
    variable_name = c(
      rep("100 days", 4), rep("300 days", 4), rep("End of observation", 4),
      rep("100 days", 4), rep("300 days", 4), rep("End of observation", 4)
    ),
    variable_level = c(
      rep(c("restart", "switch", "restart and switch", "not treated"), 3),
      rep(c("restart", "switch", "restart and switch", "not treated"), 3)
    )
  )
  uniqueVarsRes <- resultsCohort |> dplyr::distinct(group_level, strata_name, strata_level, variable_name, variable_level)
  expect_equal(uniqueVars$group_level, uniqueVarsRes$group_level)
  expect_equal(uniqueVars$strata_name, uniqueVarsRes$strata_name)
  expect_equal(uniqueVars$strata_level, uniqueVarsRes$strata_level)
  expect_equal(uniqueVars$variable_name, uniqueVarsRes$variable_name)
  expect_equal(uniqueVars$variable_level, uniqueVarsRes$variable_level)
  expect_true(settings(resultsCohort)$result_type == "summarise_drug_restart")
  expect_true(is.na(settings(resultsCohort)$censor_date))
  expect_equal(
    resultsCohort |> dplyr::filter(estimate_name == "count") |> dplyr::pull("estimate_value"),
    c("1", "1", "0", "2", "1", "2", "0", "1", "0", "2", "1", "1", "0", "0", "0", "4", "0", "0", "0", "4", "0", "2", "0", "2")
  )
  # suppress
  resultsSup <- omopgenerics::suppress(resultsCohort)
  expect_equal(resultsSup$estimate_value |> unique(), c(NA_character_, "0"))

  # from cohort
  # conceptlist <- list("a" = 1125360, "b" = c(1503297, 1503327))
  # resultsConcept <- cdm$dus_cohort |>
  #   summariseDrugRestart(switchConceptSet = conceptlist, followUpDays = c(100, 300, Inf))
  # expect_equal(resultsConcept, resultsCohort)

  # strata
  resultsStra <- cdm$dus_cohort |>
    PatientProfiles::addDemographics(
      ageGroup = list(c(0, 50), c(51, 100))
    ) |>
    summariseDrugRestart(
      strata = list("age_group", "sex", c("age_group", "sex")),
      switchCohortTable = "switch_cohort",
      switchCohortId = 1:2,
      followUpDays = c(10)
    )
  expect_true(all(visOmopResults::strataColumns(resultsStra) == c("age_group", "sex")))

  # restrict
  restrict <- cdm$dus_cohort |>
    summariseDrugRestart(
      switchCohortTable = "switch_cohort",
      switchCohortId = 1:2,
      restrictToFirstDiscontinuation = FALSE,
      followUpDays = c(10)
    )
  expect_equal(
    restrict |>
      dplyr::filter(estimate_name == "count", group_level == "cohort_1") |>
      dplyr::pull("estimate_value"),
    c("0", "1", "0", "4")
  )

  # censor date
  expect_warning(
    censor <- cdm$dus_cohort |>
      summariseDrugRestart(
        switchCohortTable = "switch_cohort",
        switchCohortId = 1:2,
        censorDate = "cohort_start_date",
        followUpDays = c(10)
      )
  )
  expect_true(censor$estimate_value |> unique() == "0")

  # expected errors
  expect_error(summariseDrugRestart(cdm$dus_cohort))
  expect_warning(expect_error(summariseDrugRestart(cdm$dus_cohort, switchCohortTable = "switch_cohort", switchCohortId = 5)))

  mockDisconnect(cdm = cdm)
})

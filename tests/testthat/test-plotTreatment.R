test_that("plot treatment (from cohort) works", {
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
    ),
    seed = 1
  )

  treatment1 <- cdm$cohort1 |>
    summariseTreatment(
      treatmentCohortName = "cohort2",
      window = list(c(0, 30), c(31, 365))
    )

  expect_no_error(
    plot1 <- plotTreatment(treatment1)
  )

  expect_true(ggplot2::is.ggplot(plot1))

  treatment2 <- cdm$cohort1 |>
    summariseTreatment(
      treatmentCohortName = "cohort2",
      window = list(c(0, 30), c(31, 365)),
      indexDate = "cohort_end_date"
    )

  expect_no_error(
    plot2 <- plotTreatment(treatment2)
  )

  expect_true(ggplot2::is.ggplot(plot2))

  treatment3 <- cdm$cohort1 |>
    PatientProfiles::addSex() |>
    PatientProfiles::addAge(ageGroup = list("<40" = c(0, 39), ">40" = c(40, 150))) |>
    summariseTreatment(
      treatmentCohortName = "cohort2",
      window = list(c(0, 30), c(31, 365)),
      treatmentCohortId = 1,
      strata = list("sex", "age_group")
    )

  expect_no_error(
    plot3 <- plotTreatment(treatment3)
  )

  expect_true(ggplot2::is.ggplot(plot3))

  mockDisconnect(cdm = cdm)
})

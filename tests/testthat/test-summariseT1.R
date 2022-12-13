
test_that("test covariates", {
  cdm <- mockDrugUtilisation(
    cohort1 = tibble::tibble(
      cohort_definition_id = c(1, 1, 1, 2, 3),
      subject_id = c(1, 1, 2, 3, 4),
      cohort_start_date = as.Date(c(
        "2000-01-01", "2000-04-01", "2020-05-10", "2013-09-08", "1950-08-01"
      )),
      cohort_end_date = as.Date(c(
        "2000-03-01", "2001-04-01", "2021-05-10", "2014-09-08", "1951-08-01"
      ))
    ),
    cohort2 = tibble::tibble(
      cohort_definition_id = c(1, 1, 1, 2, 2, 2),
      subject_id = c(1, 1, 4, 1, 2, 4),
      cohort_start_date = as.Date(c(
        "1999-07-04", "2000-01-02", "1950-05-03", "1999-12-31", "2020-02-18",
        "1950-03-15"
      )),
      cohort_end_date = as.Date(c(
        "1999-07-04", "2000-01-02", "1950-05-03", "1999-12-31", "2020-02-18",
        "1950-03-15"
      ))
    )
  )
  set <- dplyr::tibble(cohortId = c(1, 2), cohortName = c("asthma", "covid"))

  result <- summariseT1(
    cdm = cdm,
    targetCohortName = "cohort1",
    targetCohortId = c(1, 2),
    covariatesTableName = "cohort2",
    covariatesSet = set,
    covariatesWindow = c(-180, 0)
  )
  expect_true(result$value[
    result$covariate == "covariates_asthma_-180;0" &
      result$cohort_definition_id == 1
    ] == 1)
  expect_true(result$value[
    result$covariate == "covariates_covid_-180;0" &
      result$cohort_definition_id == 1
  ] == 3)
  expect_true(result$value[
    result$covariate == "covariates_asthma_-180;0" &
      result$cohort_definition_id == 2
  ] == 0)
  expect_true(result$value[
    result$covariate == "covariates_covid_-180;0" &
      result$cohort_definition_id == 2
  ] == 0)

  result <- summariseT1(
    cdm = cdm,
    targetCohortName = "cohort1",
    measurementTableName = "cohort2",
    measurementSet = set,
    measurementWindow = c(-181, 0)
  )
  expect_true(result$value[
    result$covariate == "measurement_asthma_-181;0" &
      result$cohort_definition_id == 1
  ] == 2)
  expect_true(result$value[
    result$covariate == "measurement_covid_-181;0" &
      result$cohort_definition_id == 1
  ] == 3)
  expect_true(result$value[
    result$covariate == "measurement_asthma_-181;0" &
      result$cohort_definition_id == 2
  ] == 0)
  expect_true(result$value[
    result$covariate == "measurement_covid_-181;0" &
      result$cohort_definition_id == 2
  ] == 0)
  expect_true(result$value[
    result$covariate == "measurement_asthma_-181;0" &
      result$cohort_definition_id == 3
  ] == 1)
  expect_true(result$value[
    result$covariate == "measurement_covid_-181;0" &
      result$cohort_definition_id == 3
  ] == 1)

  result <- summariseT1(
    cdm = cdm,
    targetCohortName = "cohort1",
    measurementTableName = "cohort2",
    measurementSet = set,
    measurementWindow = c(-181, 0),
    covariatesTableName = "cohort2",
    covariatesSet = set,
    covariatesWindow = c(-180, 0)
  )
  expect_true(result$value[
    result$covariate == "covariates_asthma_-180;0" &
      result$cohort_definition_id == 1
  ] == 1)
  expect_true(result$value[
    result$covariate == "covariates_covid_-180;0" &
      result$cohort_definition_id == 1
  ] == 3)
  expect_true(result$value[
    result$covariate == "covariates_asthma_-180;0" &
      result$cohort_definition_id == 2
  ] == 0)
  expect_true(result$value[
    result$covariate == "covariates_covid_-180;0" &
      result$cohort_definition_id == 2
  ] == 0)
  expect_true(result$value[
    result$covariate == "measurement_asthma_-181;0" &
      result$cohort_definition_id == 1
  ] == 2)
  expect_true(result$value[
    result$covariate == "measurement_covid_-181;0" &
      result$cohort_definition_id == 1
  ] == 3)
  expect_true(result$value[
    result$covariate == "measurement_asthma_-181;0" &
      result$cohort_definition_id == 2
  ] == 0)
  expect_true(result$value[
    result$covariate == "measurement_covid_-181;0" &
      result$cohort_definition_id == 2
  ] == 0)
  expect_true(result$value[
    result$covariate == "measurement_asthma_-181;0" &
      result$cohort_definition_id == 3
  ] == 1)
  expect_true(result$value[
    result$covariate == "measurement_covid_-181;0" &
      result$cohort_definition_id == 3
  ] == 1)

})

test_that("test NA as Any", {
  cdm <- mockDrugUtilisation(
    cohort1 = tibble::tibble(
      cohort_definition_id = c(1, 1, 1, 2, 3),
      subject_id = c(1, 1, 2, 3, 4),
      cohort_start_date = as.Date(c(
        "2000-01-01", "2000-04-01", "2020-05-10", "2013-09-08", "1950-08-01"
      )),
      cohort_end_date = as.Date(c(
        "2000-03-01", "2001-04-01", "2021-05-10", "2014-09-08", "1951-08-01"
      ))
    ),
    cohort2 = tibble::tibble(
      cohort_definition_id = c(1, 1, 1, 2, 2, 2),
      subject_id = c(1, 1, 4, 1, 2, 4),
      cohort_start_date = as.Date(c(
        "1999-07-04", "2000-01-02", "1950-05-03", "1999-12-31", "2020-02-18",
        "1950-03-15"
      )),
      cohort_end_date = as.Date(c(
        "1999-07-04", "2000-01-02", "1950-05-03", "1999-12-31", "2020-02-18",
        "1950-03-15"
      ))
    )
  )
  set <- dplyr::tibble(cohortId = c(1, 2), cohortName = c("asthma", "covid"))

  result <- summariseT1(
    cdm = cdm,
    targetCohortName = "cohort1",
    measurementTableName = "cohort2",
    measurementSet = set,
    measurementWindow = c(NA, 0)
  )
  expect_true(result$value[
    result$covariate == "measurement_asthma_-Any;0" &
      result$cohort_definition_id == 1
  ] == 2)
  expect_true(result$value[
    result$covariate == "measurement_covid_-Any;0" &
      result$cohort_definition_id == 1
  ] == 3)
  expect_true(result$value[
    result$covariate == "measurement_asthma_-Any;0" &
      result$cohort_definition_id == 2
  ] == 0)
  expect_true(result$value[
    result$covariate == "measurement_covid_-Any;0" &
      result$cohort_definition_id == 2
  ] == 0)
  expect_true(result$value[
    result$covariate == "measurement_asthma_-Any;0" &
      result$cohort_definition_id == 3
  ] == 1)
  expect_true(result$value[
    result$covariate == "measurement_covid_-Any;0" &
      result$cohort_definition_id == 3
  ] == 1)
})


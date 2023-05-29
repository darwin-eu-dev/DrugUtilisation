test_that("test indicationDate", {
  dus_cohort <- dplyr::tibble(
    cohort_definition_id = c(1, 1, 1, 2),
    subject_id = c(1, 1, 2, 3),
    cohort_start_date = as.Date(c(
      "1990-04-19", "1991-04-19", "2010-11-14", "2000-05-25"
    )),
    cohort_end_date = as.Date(c(
      "1990-04-19", "1991-04-19", "2010-11-14", "2000-05-25"
    ))
  )
  attr(dus_cohort, "cohort_set") <- dplyr::tibble(
    cohort_definition_id = c(1, 2), cohort_name = c("exposed", "unexposed")
  )
  comorbidities <- dplyr::tibble(
    cohort_definition_id = c(1, 2, 2, 1),
    subject_id = c(1, 1, 3, 3),
    cohort_start_date = as.Date(c(
      "1990-01-01", "1990-06-01", "2000-01-01", "2000-06-01"
    )),
    cohort_end_date = as.Date(c(
      "1990-01-01", "1990-06-01", "2000-01-01", "2000-06-01"
    ))
  )
  attr(comorbidities, "cohort_set") <- dplyr::tibble(
    cohort_definition_id = c(1, 2), cohort_name = c("covid", "headache")
  )
  medication <- dplyr::tibble(
    cohort_definition_id = c(1, 1, 2, 1),
    subject_id = c(1, 1, 2, 3),
    cohort_start_date = as.Date(c(
      "1990-02-01", "1990-08-01", "2009-01-01", "1995-06-01"
    )),
    cohort_end_date = as.Date(c(
      "1990-02-01", "1990-08-01", "2009-01-01", "1995-06-01"
    ))
  )
  attr(medication, "cohort_set") <- dplyr::tibble(
    cohort_definition_id = c(1, 2, 3),
    cohort_name = c("acetaminophen", "ibuprophen", "naloxone")
  )
  cdm <- mockDrugUtilisation(
    connectionDetails, numberIndividuals = 3, dus_cohort = dus_cohort,
    comorbidities = comorbidities, medication = medication
  )

  expect_no_error(result <- summariseTableOne(
    cdm$dus_cohort, cdm, covariates = list(
      "medication" = c(-365, 0), "comorbidities" = c(-Inf, 0)
    ), minimumCellCount = 1
  ))
  expect_identical(
    result %>%
      dplyr::filter(cohort_name == "exposed") %>%
      dplyr::filter(variable == "covid_minf_to_0") %>%
      dplyr::filter(estimate == "count") %>%
      dplyr::pull("value") %>%
      as.numeric(),
    2
  )
  expect_identical(
    result %>%
      dplyr::filter(cohort_name == "exposed") %>%
      dplyr::filter(variable == "headache_minf_to_0") %>%
      dplyr::filter(estimate == "count") %>%
      dplyr::pull("value") %>%
      as.numeric(),
    1
  )
  expect_identical(
    result %>%
      dplyr::filter(cohort_name == "exposed") %>%
      dplyr::filter(variable == "acetaminophen_m365_to_0") %>%
      dplyr::filter(estimate == "count") %>%
      dplyr::pull("value") %>%
      as.numeric(),
    2
  )
  expect_identical(
    result %>%
      dplyr::filter(cohort_name == "exposed") %>%
      dplyr::filter(variable == "ibuprophen_m365_to_0") %>%
      dplyr::filter(estimate == "count") %>%
      dplyr::pull("value") %>%
      as.numeric(),
    0
  )
  expect_identical(
    result %>%
      dplyr::filter(cohort_name == "exposed") %>%
      dplyr::filter(variable == "naloxone_m365_to_0") %>%
      dplyr::filter(estimate == "count") %>%
      dplyr::pull("value") %>%
      as.numeric(),
    0
  )
  expect_identical(
    result %>%
      dplyr::filter(cohort_name == "unexposed") %>%
      dplyr::filter(variable == "covid_minf_to_0") %>%
      dplyr::filter(estimate == "count") %>%
      dplyr::pull("value") %>%
      as.numeric(),
    0
  )
  expect_identical(
    result %>%
      dplyr::filter(cohort_name == "unexposed") %>%
      dplyr::filter(variable == "headache_minf_to_0") %>%
      dplyr::filter(estimate == "count") %>%
      dplyr::pull("value") %>%
      as.numeric(),
    1
  )
  expect_identical(
    result %>%
      dplyr::filter(cohort_name == "unexposed") %>%
      dplyr::filter(variable == "acetaminophen_m365_to_0") %>%
      dplyr::filter(estimate == "count") %>%
      dplyr::pull("value") %>%
      as.numeric(),
    0
  )
  expect_identical(
    result %>%
      dplyr::filter(cohort_name == "unexposed") %>%
      dplyr::filter(variable == "ibuprophen_m365_to_0") %>%
      dplyr::filter(estimate == "count") %>%
      dplyr::pull("value") %>%
      as.numeric(),
    0
  )
  expect_identical(
    result %>%
      dplyr::filter(cohort_name == "unexposed") %>%
      dplyr::filter(variable == "naloxone_m365_to_0") %>%
      dplyr::filter(estimate == "count") %>%
      dplyr::pull("value") %>%
      as.numeric(),
    0
  )

  expect_no_error(result <- summariseTableOne(
    cdm$dus_cohort, cdm, windowVisitOcurrence = c(-365, 0), covariates = list(
      "medication" = c(-365, 0), "comorbidities" = c(-Inf, 0)
    ), minimumCellCount = 1
  ))

})

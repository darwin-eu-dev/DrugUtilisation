
test_that("basic functionality large scale characteristics", {
  person <- dplyr::tibble(
    person_id = c(1, 2),
    gender_concept_id = c(8507, 8532),
    year_of_birth = c(1990, 1992),
    month_of_birth = c(1, 1),
    day_of_birth = c(1, 1)
  )
  observation_period <- dplyr::tibble(
    observation_period_id = c(1, 2),
    person_id = c(1, 2),
    observation_period_start_date = as.Date(c("2011-10-07", "2000-01-01")),
    observation_period_end_date = as.Date(c("2031-10-07", "2030-01-01")),
    period_type_concept_id = 44814724
  )
  cohort_interest <- dplyr::tibble(
    cohort_definition_id = c(1, 1, 1, 2),
    subject_id = c(1, 1, 2, 2),
    cohort_start_date = as.Date(c(
      "2012-10-10", "2015-01-01", "2013-10-10", "2015-01-01"
    )),
    cohort_end_date = as.Date(c(
      "2012-10-10", "2015-01-01", "2013-10-10", "2015-01-01"
    ))
  )
  drug_exposure <- dplyr::tibble(
    drug_exposure_id = 1:11,
    person_id = c(rep(1, 8), rep(2, 3)),
    drug_concept_id = c(
      rep(1125315, 2), rep(1503328, 5), 1516978, 1125315, 1503328, 1516978
    ),
    drug_exposure_start_date = as.Date(c(
      "2010-10-01", "2012-12-31", "2010-01-01", "2012-09-01", "2013-04-01",
      "2014-10-31", "2015-05-01", "2015-10-01", "2012-01-01", "2012-10-01",
      "2014-10-12"
    )),
    drug_exposure_end_date = as.Date(c(
      "2010-12-01", "2013-05-12", "2011-01-01", "2012-10-01", "2013-05-01",
      "2014-12-31", "2015-05-02", "2016-10-01", "2012-01-01", "2012-10-30",
      "2015-01-10"
    )),
    drug_type_concept_id = 38000177,
    quantity = 1
  )
  condition_occurrence <- dplyr::tibble(
    condition_occurrence_id = 1:8,
    person_id = c(rep(1, 4), rep(2, 4)),
    condition_concept_id = c(
      317009, 378253, 378253, 4266367, 317009, 317009, 378253, 4266367
    ),
    condition_start_date = as.Date(c(
      "2012-10-01", "2012-01-01", "2014-01-01", "2010-01-01", "2015-02-01",
      "2012-01-01", "2013-10-01", "2014-10-10"
    )),
    condition_end_date = as.Date(c(
      "2013-01-01", "2012-04-01", "2014-10-12", "2015-01-01", "2015-03-01",
      "2012-04-01", "2013-12-01", NA
    )),
    condition_type_concept_id = 32020
  )
  cdm <- mockDrugUtilisation(
    connectionDetails, person = person, observation_period = observation_period,
    cohort_interest = cohort_interest, drug_exposure = drug_exposure,
    condition_occurrence = condition_occurrence
  )
  result <- cdm$cohort_interest %>%
    summariseLargeScaleCharacteristics(
      cdm, tablesToCharacterize = c("condition_occurrence", "drug_exposure"),
      minimumCellCount = 1
    )
})

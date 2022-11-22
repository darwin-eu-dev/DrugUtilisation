devtools::load_all()
test_that("mock db: checks inputs", {
  cdm <- mockDrugUtilisation(
    person = dplyr::tibble(
      cohort_definition_id = c(1,1,2,2),
      subject_id = c(1,2,1,2),
      cohort_start_date = as.Date(c(
        "2020-01-01",
        "2020-03-01",
        "2020-04-01",
        "2020-05-01"
      )),
      cohort_end_date = as.Date(c(
        "2020-01-01",
        "2020-03-01",
        "2020-04-01",
        "2020-05-01"
      ))
    ),
    condition_occurrence = dplyr::tibble(
      condition_occurrence_id = c(1,2,3,4,5,6),
      person_id = c(1,2,1,2,1,5),
      condition_concept_id = c(1,1,2,1,1,1),
      condition_start_date = as.Date(c(
        "2020-05-06",
        "2019-09-04",
        "2021-04-03",
        "2005-11-12",
        "2022-12-31",
        "2020-06-02"
      )),
      condition_end_date = as.Date(c(
        "2020-10-06",
        "2019-12-04",
        "2021-05-03",
        "2005-10-12",
        "2022-12-31",
        "2020-07-02"
      ))
    )
  )
  res <- largeScaleCharacterization(
    cdm = cdm,
    targetCohortName = "person",
    targetCohortId = 1,
    tablesToCharacterize = "condition_occurrence",
    overlap = TRUE
  )
})


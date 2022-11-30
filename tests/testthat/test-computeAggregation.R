test_that("check inputs", {
  cdm <- mockDrugUtilisation(
    person = dplyr::tibble(
      cohort_definition_id = 1,
      subject_id = 1,
      cohort_start_date = as.Date("2020-01-01"),
      cohort_end_date = as.Date("2020-01-01")
    ),
    condition_occurrence = dplyr::tibble(
      subject_id = 1,
      cohort_start_date = as.Date("2020-01-01"),
      cohort_end_date = as.Date("2020-01-01"),
      initial_dose = 100
    )
  )

  # expect error if no input
  expect_error(computeAggregation())

  # expect error if only cdm is provided
  expect_error(computeAggregation(cdm = cdm))

  # expect error if only cdm and doseCohortName are provided
  expect_error(computeAggregation(
    cdm = cdm,
    doseCohortName = "person"
  ))

  # expect ERROR or NO ERROR for sex
  expect_no_error(computeAggregation(
    cdm = cdm,
    doseCohortName = "person",
    sex = "Both"
  ))
  expect_no_error(computeAggregation(
    cdm = cdm,
    doseCohortName = "person",
    sex = c("Both", "Male")
  ))
  expect_no_error(computeAggregation(
    cdm = cdm,
    doseCohortName = "person",
    sex = c("Female", "Male")
  ))
  expect_no_error(computeAggregation(
    cdm = cdm,
    doseCohortName = "person",
    sex = c("Both", "Male", "Female")
  ))
  expect_error(computeAggregation(
    cdm = cdm,
    doseCohortName = "person",
    sex = c("Both", "Male", "Hola")
  ))
  expect_error(computeAggregation(
    cdm = cdm,
    doseCohortName = "person",
    sex = c("Both", "Both")
  ))
  expect_error(computeAggregation(
    cdm = cdm,
    doseCohortName = "person",
    sex = 1
  ))

  # expect ERROR or NO ERROR for ageGroup
  expect_error(computeAggregation(
    cdm = cdm,
    doseCohortName = "person",
    ageGroup = "Both"
  ))
  expect_error(computeAggregation(
    cdm = cdm,
    doseCohortName = "person",
    ageGroup = c(1,2)
  ))
  expect_error(computeAggregation(
    cdm = cdm,
    doseCohortName = "person",
    ageGroup = list("both")
  ))
  expect_error(computeAggregation(
    cdm = cdm,
    doseCohortName = "person",
    ageGroup = list(1)
  ))
  expect_error(computeAggregation(
    cdm = cdm,
    doseCohortName = "person",
    ageGroup = list(c(1,2,3))
  ))
  expect_error(computeAggregation(
    cdm = cdm,
    doseCohortName = "person",
    ageGroup = list(c(4,3))
  ))
  expect_no_error(computeAggregation(
    cdm = cdm,
    doseCohortName = "person",
    ageGroup = list(c(3,4))
  ))
  expect_error(computeAggregation(
    cdm = cdm,
    doseCohortName = "person",
    ageGroup = list(c(3,4), "hi")
  ))
  expect_error(computeAggregation(
    cdm = cdm,
    doseCohortName = "person",
    ageGroup = list(c(3,4), c(5,4))
  ))
  expect_no_error(computeAggregation(
    cdm = cdm,
    doseCohortName = "person",
    ageGroup = list(c(3,4), c(4,5))
  ))

  # expect ERROR or NO ERROR for indexYear
  expect_error(computeAggregation(
    cdm = cdm,
    doseCohortName = "person",
    indexYear = "Both"
  ))
  expect_no_error(computeAggregation(
    cdm = cdm,
    doseCohortName = "person",
    indexYear = 2000
  ))
  expect_no_error(computeAggregation(
    cdm = cdm,
    doseCohortName = "person",
    indexYear = 2000:2010
  ))

  # expect ERROR or NO ERROR for initialDose
  expect_error(computeAggregation(
    cdm = cdm,
    doseCohortName = "person",
    initialDose = "Both"
  ))
  expect_error(computeAggregation(
    cdm = cdm,
    doseCohortName = "person",
    initialDose = c(1,2)
  ))
  expect_error(computeAggregation(
    cdm = cdm,
    doseCohortName = "person",
    initialDose = list("both")
  ))
  expect_error(computeAggregation(
    cdm = cdm,
    doseCohortName = "person",
    initialDose = list(1)
  ))
  expect_error(computeAggregation(
    cdm = cdm,
    doseCohortName = "person",
    initialDose = list(c(1,2,3))
  ))
  expect_error(computeAggregation(
    cdm = cdm,
    doseCohortName = "person",
    initialDose = list(c(4,3))
  ))
  expect_no_error(computeAggregation(
    cdm = cdm,
    doseCohortName = "person",
    initialDose = list(c(3,4))
  ))
  expect_error(computeAggregation(
    cdm = cdm,
    doseCohortName = "person",
    initialDose = list(c(3,4), "hi")
  ))
  expect_error(computeAggregation(
    cdm = cdm,
    doseCohortName = "person",
    initialDose = list(c(3,4), c(5,4))
  ))
  expect_no_error(computeAggregation(
    cdm = cdm,
    doseCohortName = "person",
    initialDose = list(c(3,4), c(4,5))
  ))

  # check indications
  expect_error(computeAggregation(
    cdm = cdm,
    doseCohortName = "person",
    sex = "Both",
    indicationDefinitionSet = 1
  ))
  expect_error(computeAggregation(
    cdm = cdm,
    doseCohortName = "person",
    sex = "Both",
    unknownIndicationTables = 1
  ))
  expect_error(computeAggregation(
    cdm = cdm,
    doseCohortName = "person",
    sex = "Both",
    gapIndication = 1
  ))
  expect_error(computeAggregation(
    cdm = cdm,
    doseCohortName = "person",
    sex = "Both",
    indicationCohortName = "condition_occurrence"
  ))
  expect_error(computeAggregation(
    cdm = cdm,
    doseCohortName = "person",
    sex = "Both",
    indicationCohortName = "condition_occurrence",
    gapIndication = 0
  ))
  expect_warning(expect_error(computeAggregation(
    cdm = cdm,
    doseCohortName = "person",
    sex = "Both",
    indicationCohortName = "condition_occurrence",
    indicationDefinitionSet = dplyr::tibble(indication_id = 1),
    gapIndication = 0
  )))
  expect_warning(expect_error(computeAggregation(
    cdm = cdm,
    doseCohortName = "person",
    sex = "Both",
    indicationCohortName = "condition_occurrence",
    indicationDefinitionSet = dplyr::tibble(
      indication_id = 1,
      indication_nama = "hi"),
    gapIndication = 0
  )))
  expect_no_error(computeAggregation(
    cdm = cdm,
    doseCohortName = "person",
    sex = "Both",
    indicationCohortName = "condition_occurrence",
    indicationDefinitionSet = dplyr::tibble(
      indication_id = 1,
      indication_name = "hi"),
    gapIndication = 0
  ))
  expect_error(computeAggregation(
    cdm = cdm,
    doseCohortName = "person",
    sex = "Both",
    indicationCohortName = "condition_occurrence",
    indicationDefinitionSet = dplyr::tibble(
      indication_id = 1,
      indication_name = "hi"),
    gapIndication = 0,
    unknownIndicationTables = 1
  ))
  expect_error(computeAggregation(
    cdm = cdm,
    doseCohortName = "person",
    sex = "Both",
    indicationCohortName = "condition_occurrence",
    indicationDefinitionSet = dplyr::tibble(
      indication_id = 1,
      indication_name = "hi"),
    gapIndication = 0,
    unknownIndicationTables = "hi"
  ))
  expect_no_error(computeAggregation(
    cdm = cdm,
    doseCohortName = "person",
    sex = "Both",
    indicationCohortName = "condition_occurrence",
    indicationDefinitionSet = dplyr::tibble(
      indication_id = 1,
      indication_name = "hi"),
    gapIndication = 0,
    unknownIndicationTables = "drug_exposure"
  ))

  #check oneStrata
  expect_error(computeAggregation(
    cdm = cdm,
    doseCohortName = "person",
    sex = "Both",
    oneStrata = 1
  ))
  expect_no_error(computeAggregation(
    cdm = cdm,
    doseCohortName = "person",
    sex = "Both",
    oneStrata = FALSE
  ))
})

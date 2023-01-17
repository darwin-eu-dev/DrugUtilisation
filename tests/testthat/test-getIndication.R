
test_that("test case single indication", {
  targetCohortName <- dplyr::tibble(
    cohort_definition_id = c(1, 1, 1, 2),
    subject_id = c(1, 1, 2, 3),
    cohort_start_date = as.Date(c(
      "2020-01-01", "2020-06-01", "2020-01-02", "2020-01-01"
    )),
    cohort_end_date = as.Date(c(
      "2020-04-01", "2020-08-01", "2020-02-02", "2020-03-01"
    ))
  ) # this is the targetCohort
  indicationCohortName <- dplyr::tibble(
    cohort_definition_id = c(1, 1, 2, 3, 1),
    subject_id = c(1, 3, 1, 2, 1),
    cohort_start_date = as.Date(
      c(
        "2019-12-30",
        "2020-01-01",
        "2020-05-25",
        "2020-01-01",
        "2020-05-25"
      )
    ),
    cohort_end_date = as.Date(
      c(
        "2019-12-30",
        "2020-01-01",
        "2020-05-25",
        "2020-01-01",
        "2020-05-25"
      )
    )
  )
  condition_occurrence <- dplyr::tibble(
    person_id = 1,
    condition_start_date = as.Date("2020-05-31")
  )

  indicationDefinitionSet <- dplyr::tibble(
    cohortId = c(1, 2),
    cohortName = c("asthma", "covid")
  )

  cdm <-
    mockDrugUtilisation(
      cohort1 = targetCohortName,
      cohort2 = indicationCohortName,
      condition_occurrence = condition_occurrence
    )

  # check for indication 0
  res_0 <- suppressWarnings(getIndication(
    cdm = cdm,
    targetCohortName = "cohort1",
    indicationCohortName = "cohort2",
    targetCohortDefinitionIds = 1,
    indicationDefinitionSet = indicationDefinitionSet,
    indicationGap = 0,
    unknownIndicationTables = NULL
  ))


  expect_true(dplyr::all_equal(
    res_0[["0"]] %>% dplyr::collect(),
    dplyr::tibble(
      cohort_definition_id = c(1, 1, 1),
      subject_id = c(1, 1, 2),
      cohort_start_date = as.Date(c(
        "2020-01-01", "2020-06-01", "2020-01-02"
      )),
      cohort_end_date = as.Date(c(
        "2020-04-01", "2020-08-01", "2020-02-02"
      )),
      indication_id = c(-1, -1, -1)
    )
  ))
  # check for indication 1
  res_1 <- suppressWarnings(getIndication(
    cdm = cdm,
    targetCohortName = "cohort1",
    indicationCohortName = "cohort2",
    targetCohortDefinitionIds = 1,
    indicationDefinitionSet = indicationDefinitionSet,
    indicationGap = 1,
    unknownIndicationTables = NULL
  ))

  expect_true(dplyr::all_equal(
    res_1[["1"]] %>% dplyr::collect(),
    dplyr::tibble(
      cohort_definition_id = c(1, 1, 1),
      subject_id = c(1, 1, 2),
      cohort_start_date = as.Date(c(
        "2020-01-01", "2020-06-01", "2020-01-02"
      )),
      cohort_end_date = as.Date(c(
        "2020-04-01", "2020-08-01", "2020-02-02"
      )),
      indication_id = c(-1, -1, -1)
    )
  ))
  # check for indication 2
  res_2 <- suppressWarnings(getIndication(
    cdm = cdm,
    targetCohortName = "cohort1",
    indicationCohortName = "cohort2",
    targetCohortDefinitionIds = 1,
    indicationDefinitionSet = indicationDefinitionSet,
    unknownIndicationTables = NULL,
    indicationGap = 2
  ))

  expect_true(dplyr::all_equal(
    res_2[["2"]] %>% dplyr::collect(),
    dplyr::tibble(
      cohort_definition_id = c(1, 1, 1),
      subject_id = c(1, 1, 2),
      cohort_start_date = as.Date(c(
        "2020-01-01", "2020-06-01", "2020-01-02"
      )),
      cohort_end_date = as.Date(c(
        "2020-04-01", "2020-08-01", "2020-02-02"
      )),
      indication_id = c(1, -1, -1)
    )
  ))
  #check for all indication Gap
  res_NA <- suppressWarnings(getIndication(
    cdm = cdm,
    targetCohortName = "cohort1",
    indicationCohortName = "cohort2",
    targetCohortDefinitionIds = 1,
    indicationDefinitionSet = indicationDefinitionSet,
    unknownIndicationTables = NULL,
    indicationGap = NA
  ))


  # check for indication NA all indication after
  expect_true(dplyr::all_equal(
    res_NA[["Any"]] %>% dplyr::collect(),
    dplyr::tibble(
      cohort_definition_id = c(1, 1, 1, 1),
      subject_id = c(1, 1, 2, 1),
      cohort_start_date = as.Date(c(
        "2020-01-01", "2020-06-01", "2020-01-02", "2020-06-01"
      )),
      cohort_end_date = as.Date(c(
        "2020-04-01", "2020-08-01", "2020-02-02", "2020-08-01"
      )),
      indication_id = c(1, 1, -1, 2)
    )
  ))

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("test case single indication with unknown indication table", {
  targetCohortName <- dplyr::tibble(
    cohort_definition_id = c(1, 1, 1, 2),
    subject_id = c(1, 1, 2, 3),
    cohort_start_date = as.Date(c(
      "2020-01-01", "2020-06-01", "2020-01-02", "2020-01-01"
    )),
    cohort_end_date = as.Date(c(
      "2020-04-01", "2020-08-01", "2020-02-02", "2020-03-01"
    ))
  ) # this is the targetCohort
  indicationCohortName <- dplyr::tibble(
    cohort_definition_id = c(1, 1, 2, 3, 1),
    subject_id = c(1, 3, 1, 2, 1),
    cohort_start_date = as.Date(
      c(
        "2019-12-30",
        "2020-01-01",
        "2020-05-25",
        "2020-01-01",
        "2020-05-25"
      )
    ),
    cohort_end_date = as.Date(
      c(
        "2019-12-30",
        "2020-01-01",
        "2020-05-25",
        "2020-01-01",
        "2020-05-25"
      )
    )
  )
  condition_occurrence <- dplyr::tibble(
    person_id = 1,
    condition_start_date = as.Date("2020-05-31")
  )

  indicationDefinitionSet <- dplyr::tibble(
    cohortId = c(1, 2),
    cohortName = c("asthma", "covid")
  )

  cdm <-
    mockDrugUtilisation(
      cohort1 = targetCohortName,
      cohort2 = indicationCohortName,
      condition_occurrence = condition_occurrence
    )

  # check for indication 0
  res_0 <- suppressWarnings(getIndication(
    cdm = cdm,
    targetCohortName = "cohort1",
    indicationCohortName = "cohort2",
    targetCohortDefinitionIds = 1,
    indicationDefinitionSet = indicationDefinitionSet,
    indicationGap = 0,
    unknownIndicationTables = "condition_occurrence"
  ))


  expect_true(dplyr::all_equal(
    res_0[["0"]] %>% dplyr::collect(),
    dplyr::tibble(
      cohort_definition_id = c(1, 1, 1),
      subject_id = c(1, 1, 2),
      cohort_start_date = as.Date(c(
        "2020-01-01", "2020-06-01", "2020-01-02"
      )),
      cohort_end_date = as.Date(c(
        "2020-04-01", "2020-08-01", "2020-02-02"
      )),
      indication_id = c(-1, -1, -1)
    )
  ))
  # check for indication 1
  res_1 <- suppressWarnings(getIndication(
    cdm = cdm,
    targetCohortName = "cohort1",
    indicationCohortName = "cohort2",
    targetCohortDefinitionIds = 1,
    indicationDefinitionSet = indicationDefinitionSet,
    indicationGap = 1,
    unknownIndicationTables = "condition_occurrence"
  ))

  expect_true(dplyr::all_equal(
    res_1[["1"]] %>% dplyr::collect(),
    dplyr::tibble(
      cohort_definition_id = c(1, 1, 1),
      subject_id = c(1, 1, 2),
      cohort_start_date = as.Date(c(
        "2020-01-01", "2020-06-01", "2020-01-02"
      )),
      cohort_end_date = as.Date(c(
        "2020-04-01", "2020-08-01", "2020-02-02"
      )),
      indication_id = c(-1, 0, -1)
    )
  ))
  # check for indication 6
  res_6 <- suppressWarnings(getIndication(
    cdm = cdm,
    targetCohortName = "cohort1",
    indicationCohortName = "cohort2",
    targetCohortDefinitionIds = 1,
    indicationDefinitionSet = indicationDefinitionSet,
    unknownIndicationTables = "condition_occurrence",
    indicationGap = 6
  ))

  expect_true(dplyr::all_equal(
    res_6[["6"]] %>% dplyr::collect(),
    dplyr::tibble(
      cohort_definition_id = c(1, 1, 1),
      subject_id = c(1, 1, 2),
      cohort_start_date = as.Date(c(
        "2020-01-01", "2020-06-01", "2020-01-02"
      )),
      cohort_end_date = as.Date(c(
        "2020-04-01", "2020-08-01", "2020-02-02"
      )),
      indication_id = c(1, 0, -1)
    )
  ))

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("test case multiple indication with unknown indication table", {
  targetCohortName <- dplyr::tibble(
    cohort_definition_id = c(1, 1, 1, 2),
    subject_id = c(1, 1, 2, 3),
    cohort_start_date = as.Date(c(
      "2020-01-01", "2020-06-01", "2020-01-02", "2020-01-01"
    )),
    cohort_end_date = as.Date(c(
      "2020-04-01", "2020-08-01", "2020-02-02", "2020-03-01"
    ))
  ) # this is the targetCohort
  indicationCohortName <- dplyr::tibble(
    cohort_definition_id = c(1, 1, 2, 3, 1),
    subject_id = c(1, 3, 1, 2, 1),
    cohort_start_date = as.Date(
      c(
        "2019-12-30",
        "2020-01-01",
        "2020-05-25",
        "2020-01-01",
        "2020-05-25"
      )
    ),
    cohort_end_date = as.Date(
      c(
        "2019-12-30",
        "2020-01-01",
        "2020-05-25",
        "2020-01-01",
        "2020-05-25"
      )
    )
  )
  condition_occurrence <- dplyr::tibble(
    person_id = 1,
    condition_start_date = as.Date("2020-05-31")
  )

  indicationDefinitionSet <- dplyr::tibble(
    cohortId = c(1, 2),
    cohortName = c("asthma", "covid")
  )

  cdm <-
    mockDrugUtilisation(
      cohort1 = targetCohortName,
      cohort2 = indicationCohortName,
      condition_occurrence = condition_occurrence
    )

  # check for indication 0,1,6
  res_m <- suppressWarnings(getIndication(
    cdm = cdm,
    targetCohortName = "cohort1",
    indicationCohortName = "cohort2",
    targetCohortDefinitionIds = 1,
    indicationDefinitionSet = indicationDefinitionSet,
    indicationGap = c(0, 1, 6),
    unknownIndicationTables = "condition_occurrence"
  ))

  # check for indication 0
  expect_true(dplyr::all_equal(
    res_m[["0"]] %>% dplyr::collect(),
    dplyr::tibble(
      cohort_definition_id = c(1, 1, 1),
      subject_id = c(1, 1, 2),
      cohort_start_date = as.Date(c(
        "2020-01-01", "2020-06-01", "2020-01-02"
      )),
      cohort_end_date = as.Date(c(
        "2020-04-01", "2020-08-01", "2020-02-02"
      )),
      indication_id = c(-1, -1, -1)
    )
  ))

  # check for indication 1
  expect_true(dplyr::all_equal(
    res_m[["1"]] %>% dplyr::collect(),
    dplyr::tibble(
      cohort_definition_id = c(1, 1, 1),
      subject_id = c(1, 1, 2),
      cohort_start_date = as.Date(c(
        "2020-01-01", "2020-06-01", "2020-01-02"
      )),
      cohort_end_date = as.Date(c(
        "2020-04-01", "2020-08-01", "2020-02-02"
      )),
      indication_id = c(-1, 0, -1)
    )
  ))
  # check for indication 6
  expect_true(dplyr::all_equal(
    res_m[["6"]] %>% dplyr::collect(),
    dplyr::tibble(
      cohort_definition_id = c(1, 1, 1),
      subject_id = c(1, 1, 2),
      cohort_start_date = as.Date(c(
        "2020-01-01", "2020-06-01", "2020-01-02"
      )),
      cohort_end_date = as.Date(c(
        "2020-04-01", "2020-08-01", "2020-02-02"
      )),
      indication_id = c(1, 0, -1)
    )
  ))

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("test case multiple indication", {
  targetCohortName <- dplyr::tibble(
    cohort_definition_id = c(1, 1, 1, 2),
    subject_id = c(1, 1, 2, 3),
    cohort_start_date = as.Date(c(
      "2020-01-01", "2020-06-01", "2020-01-02", "2020-01-01"
    )),
    cohort_end_date = as.Date(c(
      "2020-04-01", "2020-08-01", "2020-02-02", "2020-03-01"
    ))
  ) # this is the targetCohort
  indicationCohortName <- dplyr::tibble(
    cohort_definition_id = c(1, 1, 2, 3, 1),
    subject_id = c(1, 3, 1, 2, 1),
    cohort_start_date = as.Date(
      c(
        "2019-12-30",
        "2020-01-01",
        "2020-05-25",
        "2020-01-01",
        "2020-05-25"
      )
    ),
    cohort_end_date = as.Date(
      c(
        "2019-12-30",
        "2020-01-01",
        "2020-05-25",
        "2020-01-01",
        "2020-05-25"
      )
    )
  )
  condition_occurrence <- dplyr::tibble(
    person_id = 1,
    condition_start_date = as.Date("2020-05-31")
  )

  indicationDefinitionSet <- dplyr::tibble(
    cohortId = c(1, 2),
    cohortName = c("asthma", "covid")
  )

  cdm <-
    mockDrugUtilisation(
      cohort1 = targetCohortName,
      cohort2 = indicationCohortName,
      condition_occurrence = condition_occurrence
    )

  # check for indication 0
  res_m <- suppressWarnings(getIndication(
    cdm = cdm,
    targetCohortName = "cohort1",
    indicationCohortName = "cohort2",
    targetCohortDefinitionIds = 1,
    indicationDefinitionSet = indicationDefinitionSet,
    indicationGap = c(0, 1, 2),
    unknownIndicationTables = NULL
  ))


  expect_true(dplyr::all_equal(
    res_m[["0"]] %>% dplyr::collect(),
    dplyr::tibble(
      cohort_definition_id = c(1, 1, 1),
      subject_id = c(1, 1, 2),
      cohort_start_date = as.Date(c(
        "2020-01-01", "2020-06-01", "2020-01-02"
      )),
      cohort_end_date = as.Date(c(
        "2020-04-01", "2020-08-01", "2020-02-02"
      )),
      indication_id = c(-1, -1, -1)
    )
  ))
  # check for indication 1

  expect_true(dplyr::all_equal(
    res_m[["1"]] %>% dplyr::collect(),
    dplyr::tibble(
      cohort_definition_id = c(1, 1, 1),
      subject_id = c(1, 1, 2),
      cohort_start_date = as.Date(c(
        "2020-01-01", "2020-06-01", "2020-01-02"
      )),
      cohort_end_date = as.Date(c(
        "2020-04-01", "2020-08-01", "2020-02-02"
      )),
      indication_id = c(-1, -1, -1)
    )
  ))
  # check for indication 2

  expect_true(dplyr::all_equal(
    res_m[["2"]] %>% dplyr::collect(),
    dplyr::tibble(
      cohort_definition_id = c(1, 1, 1),
      subject_id = c(1, 1, 2),
      cohort_start_date = as.Date(c(
        "2020-01-01", "2020-06-01", "2020-01-02"
      )),
      cohort_end_date = as.Date(c(
        "2020-04-01", "2020-08-01", "2020-02-02"
      )),
      indication_id = c(1, -1, -1)
    )
  ))



  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("test case indicationCohortName error message", {
  targetCohortName_1 <- dplyr::tibble(
    cohort_definition_id = c(1, 1, 1, 2),
    subject_id = c(1, 1, 2, 3),
    cohort_start_date = as.Date(c(
      "2020-01-01", "2020-06-01", "2020-01-02", "2020-01-01"
    )),
    cohort_end_date = as.Date(c(
      "2020-04-01", "2020-08-01", "2020-02-02", "2020-03-01"
    ))
  ) # this is the targetCohort
  targetCohortName_2 <- dplyr::tibble(
    cohort_definition_id = c(1, 1, 1, 2),
    subject_id = c(1, 1, 2, 3),
    cohort_start_date = as.Date(c(
      "2020-01-01", "2020-06-01", "2020-01-02", "2020-01-01"
    )),
    cohort_end_date = as.Date(c(
      "2020-01-01", "2020-06-01", "2020-01-02", "2020-01-01"
    ))
  )
  indicationCohortName <- dplyr::tibble(
    cohort_definition_id = c(1, 1, 2, 3, 1),
    subject_id = c(1, 3, 1, 2, 1),
    cohort_start_date = as.Date(
      c(
        "2019-12-30",
        "2020-01-01",
        "2020-05-25",
        "2020-01-01",
        "2020-05-25"
      )
    ),
    cohort_end_date = as.Date(
      c(
        "2019-12-30",
        "2020-01-01",
        "2020-05-25",
        "2020-01-01",
        "2020-05-25"
      )
    )
  )
  condition_occurrence <- dplyr::tibble(
    person_id = 1,
    condition_start_date = as.Date("2020-05-31")
  )

  indicationDefinitionSet <- dplyr::tibble(
    cohortId = c(1, 2),
    cohortName = c("asthma", "covid")
  )

  cdm <-
    mockDrugUtilisation(
      cohort1 = targetCohortName_1,
      cohort2 = indicationCohortName,
      condition_occurrence = condition_occurrence
    )

  # check for warning message
  expect_warning(
    getIndication(
      cdm = cdm,
      targetCohortName = "cohort2",
      indicationCohortName = "cohort1",
      targetCohortDefinitionIds = 1,
      indicationDefinitionSet = indicationDefinitionSet,
      indicationGap = 0,
      unknownIndicationTables = NULL
    )
  )

  cdm_1 <-
    mockDrugUtilisation(
      cohort1 = targetCohortName_2,
      cohort2 = indicationCohortName,
      condition_occurrence = condition_occurrence
    )

  expect_error(expect_warning(
    getIndication(
      cdm = cdm_1,
      targetCohortName = "cohort1",
      indicationCohortName = "cohort2",
      targetCohortDefinitionIds = 1,
      indicationDefinitionSet = indicationDefinitionSet,
      indicationGap = 0,
      unknownIndicationTables = NULL
    )
  ))

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("test input checks", {
  targetCohortName <- dplyr::tibble(
    cohort_definition_id = c(1, 1, 1, 2),
    subject_id = c(1, 1, 2, 3),
    cohort_start_date = as.Date(c(
      "2020-01-01", "2020-06-01", "2020-01-02", "2020-01-01"
    )),
    cohort_end_date = as.Date(c(
      "2020-04-01", "2020-08-01", "2020-02-02", "2020-03-01"
    ))
  ) # this is the targetCohort
  indicationCohortName <- dplyr::tibble(
    cohort_definition_id = c(1, 1, 2, 3, 1),
    subject_id = c(1, 3, 1, 2, 1),
    cohort_start_date = as.Date(
      c(
        "2019-12-30",
        "2020-01-01",
        "2020-05-25",
        "2020-01-01",
        "2020-05-25"
      )
    ),
    cohort_end_date = as.Date(
      c(
        "2019-12-30",
        "2020-01-01",
        "2020-05-25",
        "2020-01-01",
        "2020-05-25"
      )
    )
  )
  condition_occurrence <- dplyr::tibble(
    person_id = 1,
    condition_start_date = as.Date("2020-05-31")
  )

  indicationDefinitionSet <- dplyr::tibble(
    cohortId = c(1, 2),
    cohortName = c("asthma", "covid")
  )

  cdm <-
    mockDrugUtilisation(
      cohort1 = targetCohortName,
      cohort2 = indicationCohortName,
      condition_occurrence = condition_occurrence
    )




  expect_error(
    getIndication(
      cdm = cdm,
      targetCohortName = "cohort1",
      indicationCohortName = "cohort2",
      targetCohortDefinitionIds = 1,
      indicationDefinitionSet = indicationDefinitionSet,
      indicationGap = c(a, 1, 2),
      unknownIndicationTables = NULL
    )
  )

  expect_error(
    getIndication(
      cdm = a,
      targetCohortName = "cohort1",
      indicationCohortName = "cohort2",
      targetCohortDefinitionIds = 1,
      indicationDefinitionSet = indicationDefinitionSet,
      indicationGap = c(0, 1, 2),
      unknownIndicationTables = NULL
    )
  )

  expect_error(
    getIndication(
      cdm = cdm,
      targetCohortName = "cohort1",
      indicationCohortName = "cohort2",
      targetCohortDefinitionIds = 1,
      indicationDefinitionSet = s,
      indicationGap = c(0, 1, 2),
      unknownIndicationTables = NULL
    )
  )

  xx <- getIndication(
      cdm = cdm,
      targetCohortName = "cohort1",
      indicationCohortName = "cohort2",
      targetCohortDefinitionIds = 1,
      indicationDefinitionSet = indicationDefinitionSet,
      indicationGap = c(0, 1, NA),
      unknownIndicationTables = NULL
    )

  expect_error(
    getIndication(
      cdm = cdm,
      targetCohortName = "cohort1",
      indicationCohortName = "cohort2",
      targetCohortDefinitionIds = 1,
      indicationDefinitionSet = indicationDefinitionSet,
      indicationGap = c(0, 1, A),
      unknownIndicationTables = NULL
    )
  )

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("test case multiple indication with NA", {
  targetCohortName = dplyr::tibble(
    cohort_definition_id = c(1, 1, 1, 2),
    subject_id = c(1, 1, 2, 3),
    cohort_start_date = as.Date(c(
      "2020-01-01", "2020-06-01", "2020-01-02", "2020-01-01"
    )),
    cohort_end_date = as.Date(c(
      "2020-04-01", "2020-08-01", "2020-02-02", "2020-03-01"
    ))
  ) # this is the targetCohort
  indicationCohortName = dplyr::tibble(
    cohort_definition_id = c(1, 1, 2, 3, 1),
    subject_id = c(1, 3, 1, 2, 1),
    cohort_start_date = as.Date(
      c(
        "2019-12-30",
        "2020-01-01",
        "2020-05-25",
        "2020-01-01",
        "2020-05-25"
      )
    ),
    cohort_end_date = as.Date(
      c(
        "2019-12-30",
        "2020-01-01",
        "2020-05-25",
        "2020-01-01",
        "2020-05-25"
      )
    )
  )
  condition_occurrence = dplyr::tibble(person_id = 1,
                                       condition_start_date = as.Date("2020-05-31"))

  indicationDefinitionSet = dplyr::tibble(cohortId = c(1, 2),
                                          cohortName = c("asthma", "covid"))

  cdm <-
    mockDrugUtilisation(
      cohort1 = targetCohortName,
      cohort2 = indicationCohortName,
      condition_occurrence = condition_occurrence
    )

  #check for indication 0
  res_m <- suppressWarnings(getIndication(
    cdm = cdm,
    targetCohortName = "cohort1",
    indicationCohortName = "cohort2",
    targetCohortDefinitionIds = 1,
    indicationDefinitionSet = indicationDefinitionSet,
    indicationGap = c(0, 1, 2, NA),
    unknownIndicationTables = NULL
  ))


  expect_true(dplyr::all_equal(
    res_m[["0"]] %>% dplyr::collect(),
    dplyr::tibble(
      cohort_definition_id = c(1, 1, 1),
      subject_id = c(1, 1, 2),
      cohort_start_date = as.Date(c(
        "2020-01-01", "2020-06-01", "2020-01-02"
      )),
      cohort_end_date = as.Date(c(
        "2020-04-01", "2020-08-01", "2020-02-02"
      )),
      indication_id = c(-1, -1, -1)
    )
  ))
  #check for indication 1

  expect_true(dplyr::all_equal(
    res_m[["1"]] %>% dplyr::collect(),
    dplyr::tibble(
      cohort_definition_id = c(1, 1, 1),
      subject_id = c(1, 1, 2),
      cohort_start_date = as.Date(c(
        "2020-01-01", "2020-06-01", "2020-01-02"
      )),
      cohort_end_date = as.Date(c(
        "2020-04-01", "2020-08-01", "2020-02-02"
      )),
      indication_id = c(-1, -1, -1)
    )
  ))
  #check for indication 2

  expect_true(dplyr::all_equal(
    res_m[["2"]] %>% dplyr::collect(),
    dplyr::tibble(
      cohort_definition_id = c(1, 1, 1),
      subject_id = c(1, 1, 2),
      cohort_start_date = as.Date(c(
        "2020-01-01", "2020-06-01", "2020-01-02"
      )),
      cohort_end_date = as.Date(c(
        "2020-04-01", "2020-08-01", "2020-02-02"
      )),
      indication_id = c(1, -1, -1)
    )
  ))

  #check for indication NA

  expect_true(dplyr::all_equal(
    res_m[["Any"]] %>% dplyr::collect(),
    dplyr::tibble(
      cohort_definition_id = c(1, 1, 1, 1),
      subject_id = c(1, 1, 2, 1),
      cohort_start_date = as.Date(c(
        "2020-01-01", "2020-06-01", "2020-01-02", "2020-06-01"
      )),
      cohort_end_date = as.Date(c(
        "2020-04-01", "2020-08-01", "2020-02-02", "2020-08-01"
      )),
      indication_id = c(1, 1, -1, 2)
    )
  ))

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)

})

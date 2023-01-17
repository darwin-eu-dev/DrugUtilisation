library(testthat)

test_that("test case single indication table summary", {

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
res_0
result_1 <- summariseIndication(cdm = cdm, indicationList = res_0, minimumCellCounts = 1)
result_2 <- summariseIndication(cdm = cdm, indicationList = res_0, minimumCellCounts = 2)
result_3 <- summariseIndication(cdm = cdm, indicationList = res_0, minimumCellCounts = 3)
result_4 <- summariseIndication(cdm = cdm, indicationList = res_0, minimumCellCounts = 4)
result_5 <- summariseIndication(cdm = cdm, indicationList = res_0)
# check summary for single cohort_ID
expect_true(dplyr::all_equal(
  result_1,
  dplyr::tibble(
    cohort_definition_id = c(1, 1, 1, 1, 1, 1, 1, 1),
    variable = c(
      "number_observations",
      "cohort_start_date",
      "cohort_start_date",
      "cohort_end_date",
      "cohort_end_date",
      "indication_gap_0_No indication",
      "indication_gap_0_asthma",
      "indication_gap_0_covid"
    ),
    estimate = c(
      "count",
      "min",
      "max",
      "min",
      "max",
      "count",
      "count",
      "count"
    ),
    value = c(
      "3",
      "2020-01-01",
      "2020-06-01",
      "2020-02-02",
      "2020-08-01",
      "3",
      "0",
      "0"
    )
  )))
expect_true(dplyr::all_equal(
  result_2,
  dplyr::tibble(
    cohort_definition_id = c(1, 1, 1, 1, 1, 1, 1, 1),
    variable = c(
      "number_observations",
      "cohort_start_date",
      "cohort_start_date",
      "cohort_end_date",
      "cohort_end_date",
      "indication_gap_0_No indication",
      "indication_gap_0_asthma",
      "indication_gap_0_covid"
    ),
    estimate = c(
      "count",
      "min",
      "max",
      "min",
      "max",
      "count",
      "count",
      "count"
    ),
    value = c(
      "3",
      "2020-01-01",
      "2020-06-01",
      "2020-02-02",
      "2020-08-01",
      "3",
      "0",
      "0"
    )
  )))

expect_true(dplyr::all_equal(
  result_3,
  dplyr::tibble(
    cohort_definition_id = c(1, 1, 1, 1, 1, 1, 1, 1),
    variable = c(
      "number_observations",
      "cohort_start_date",
      "cohort_start_date",
      "cohort_end_date",
      "cohort_end_date",
      "indication_gap_0_No indication",
      "indication_gap_0_asthma",
      "indication_gap_0_covid"
    ),
    estimate = c(
      "count",
      "min",
      "max",
      "min",
      "max",
      "count",
      "count",
      "count"
    ),
    value = c(
      "3",
      "2020-01-01",
      "2020-06-01",
      "2020-02-02",
      "2020-08-01",
      "3",
      "0",
      "0"
    )
  )))

expect_true(dplyr::all_equal(
  result_4,
  dplyr::tibble(
    cohort_definition_id = c(1, 1, 1, 1, 1, 1, 1, 1),
    variable = c(
      "number_observations",
      "cohort_start_date",
      "cohort_start_date",
      "cohort_end_date",
      "cohort_end_date",
      "indication_gap_0_No indication",
      "indication_gap_0_asthma",
      "indication_gap_0_covid"
    ),
    estimate = c(
      "count",
      "min",
      "max",
      "min",
      "max",
      "count",
      "count",
      "count"
    ),
    value = c(
      "<4",
      NA,
      NA,
      NA,
      NA,
      NA,
      NA,
      NA

    )
  )))

expect_true(dplyr::all_equal(
  result_5,
  dplyr::tibble(
    cohort_definition_id = c(1, 1, 1, 1, 1, 1, 1, 1),
    variable = c(
      "number_observations",
      "cohort_start_date",
      "cohort_start_date",
      "cohort_end_date",
      "cohort_end_date",
      "indication_gap_0_No indication",
      "indication_gap_0_asthma",
      "indication_gap_0_covid"
    ),
    estimate = c(
      "count",
      "min",
      "max",
      "min",
      "max",
      "count",
      "count",
      "count"
    ),
    value = c(
      "<5",
      NA,
      NA,
      NA,
      NA,
      NA,
      NA,
      NA

    )
  )))


#target indication 2
res_0_2 <- suppressWarnings(getIndication(
  cdm = cdm,
  targetCohortName = "cohort1",
  indicationCohortName = "cohort2",
  targetCohortDefinitionIds = 2,
  indicationDefinitionSet = indicationDefinitionSet,
  indicationGap = 0,
  unknownIndicationTables = NULL
))

result_1_2 <- summariseIndication(cdm = cdm, indicationList = res_0_2, minimumCellCounts = 1)
result_5_2 <- summariseIndication(cdm = cdm, indicationList = res_0_2, minimumCellCounts = 5)

expect_true(dplyr::all_equal(
  result_1_2,
  dplyr::tibble(
    cohort_definition_id = c(2, 2, 2, 2, 2, 2, 2, 2),
    variable = c(
      "number_observations",
      "cohort_start_date",
      "cohort_start_date",
      "cohort_end_date",
      "cohort_end_date",
      "indication_gap_0_asthma",
      "indication_gap_0_No indication",
      "indication_gap_0_covid"
    ),
    estimate = c(
      "count",
      "min",
      "max",
      "min",
      "max",
      "count",
      "count",
      "count"
    ),
    value = c(
      "1",
      "2020-01-01",
      "2020-01-01",
      "2020-03-01",
      "2020-03-01",
      "1",
      "0",
      "0"

    )
  )))


expect_true(dplyr::all_equal(
  result_5_2,
  dplyr::tibble(
    cohort_definition_id = c(2, 2, 2, 2, 2, 2, 2, 2),
    variable = c(
      "number_observations",
      "cohort_start_date",
      "cohort_start_date",
      "cohort_end_date",
      "cohort_end_date",
      "indication_gap_0_No indication",
      "indication_gap_0_asthma",
      "indication_gap_0_covid"
    ),
    estimate = c(
      "count",
      "min",
      "max",
      "min",
      "max",
      "count",
      "count",
      "count"
    ),
    value = c(
      "<5",
      NA,
      NA,
      NA,
      NA,
      NA,
      NA,
      NA

    )
  )))
# check summary for multiple cohort_ID
res_m <- suppressWarnings(getIndication(
  cdm = cdm,
  targetCohortName = "cohort1",
  indicationCohortName = "cohort2",
  targetCohortDefinitionIds = c(1,2),
  indicationDefinitionSet = indicationDefinitionSet,
  indicationGap = 0,
  unknownIndicationTables = NULL
))

result_m <- summariseIndication(cdm = cdm, indicationList = res_m, minimumCellCounts = 1)

expect_true(dplyr::all_equal(
  result_m %>% dplyr::filter(cohort_definition_id %in% 2),
  dplyr::tibble(
    cohort_definition_id = c(2, 2, 2, 2, 2, 2, 2, 2),
    variable = c(
      "number_observations",
      "cohort_start_date",
      "cohort_start_date",
      "cohort_end_date",
      "cohort_end_date",
      "indication_gap_0_asthma",
      "indication_gap_0_No indication",
      "indication_gap_0_covid"
    ),
    estimate = c(
      "count",
      "min",
      "max",
      "min",
      "max",
      "count",
      "count",
      "count"
    ),
    value = c(
      "1",
      "2020-01-01",
      "2020-01-01",
      "2020-03-01",
      "2020-03-01",
      "1",
      "0",
      "0"

    )
  )))

expect_true(dplyr::all_equal(
  result_m %>% dplyr::filter(cohort_definition_id %in% 1),
  dplyr::tibble(
    cohort_definition_id = c(1, 1, 1, 1, 1, 1, 1, 1),
    variable = c(
      "number_observations",
      "cohort_start_date",
      "cohort_start_date",
      "cohort_end_date",
      "cohort_end_date",
      "indication_gap_0_No indication",
      "indication_gap_0_asthma",
      "indication_gap_0_covid"
    ),
    estimate = c(
      "count",
      "min",
      "max",
      "min",
      "max",
      "count",
      "count",
      "count"
    ),
    value = c(
      "3",
      "2020-01-01",
      "2020-06-01",
      "2020-02-02",
      "2020-08-01",
      "3",
      "0",
      "0"
    )
  )))

# check error message
expect_error(
  result_m <-
    summariseIndication(
      cdm = cdm,
      indicationList = res_m,
      minimumCellCounts = 1,
      cohortId = 3
    )
)

result_m <- summariseIndication(
      cdm = cdm,
      indicationList = res_m,
      minimumCellCounts = 1,
      cohortId = 2
    )

result_m <- summariseIndication(
       cdm = cdm,
       indicationList = res_m,
       minimumCellCounts = 1,
       cohortId = 1
     )
DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("test case multiple indication table summary", {
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

result_m <- summariseIndication(cdm = cdm, indicationList = res_m, minimumCellCounts = 1)

expect_true(dplyr::all_equal(
  result_m ,
  dplyr::tibble(
    cohort_definition_id = c(1, 1, 1, 1, 1, 1, 1, 1, 1,1, 1, 1, 1, 1),
    variable = c(
      "number_observations",
      "cohort_start_date",
      "cohort_start_date",
      "cohort_end_date",
      "cohort_end_date",
      "indication_gap_0_No indication",
      "indication_gap_0_asthma",
      "indication_gap_0_covid",
      "indication_gap_1_No indication",
      "indication_gap_1_asthma",
      "indication_gap_1_covid",
      "indication_gap_2_asthma",
      "indication_gap_2_No indication",
      "indication_gap_2_covid"
    ),
    estimate = c(
      "count",
      "min",
      "max",
      "min",
      "max",
      "count",
      "count",
      "count",
      "count",
      "count",
      "count",
      "count",
      "count",
      "count"
    ),
    value = c(
      "3",
      "2020-01-01",
      "2020-06-01",
      "2020-02-02",
      "2020-08-01",
      "3",
      "0",
      "0",
      "3",
      "0",
      "0",
      "1",
      "2",
      "0"
    )
  )))

result_m <- summariseIndication(cdm = cdm, indicationList = res_m)

expect_true(dplyr::all_equal(
  result_m ,
  dplyr::tibble(
    cohort_definition_id = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1),
    variable = c(
      "number_observations",
      "cohort_start_date",
      "cohort_start_date",
      "cohort_end_date",
      "cohort_end_date",
      "indication_gap_0_No indication",
      "indication_gap_0_asthma",
      "indication_gap_0_covid",
      "indication_gap_1_No indication",
      "indication_gap_1_asthma",
      "indication_gap_1_covid",
      "indication_gap_2_asthma",
      "indication_gap_2_No indication",
      "indication_gap_2_covid"
    ),
    estimate = c(
      "count",
      "min",
      "max",
      "min",
      "max",
      "count",
      "count",
      "count",
      "count",
      "count",
      "count",
      "count",
      "count",
      "count"
    ),
    value = c(
      "<5",
      NA,
      NA,
      NA,
      NA,
      NA,
      NA,
      NA,
      NA,
      NA,
      NA,
      NA,
      NA,
      NA
    )
  )))

expect_error(
  (result_m <-
     summariseIndication(
       cdm = cdm,
       indicationList = res_m,
       minimumCellCounts = 1,
       cohortId = 4
     )))

result_m <- summariseIndication(
       cdm = cdm,
       indicationList = res_m,
       minimumCellCounts = 1,
       cohortId = 1
     )

DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

# THE FOLLOWONG ERRORS MUST BE IMPROVED TO PROVIDE MORE ACCURATE INFORMATION
test_that("expected errors on inputs", {
  # condition_occurrence is going to be the strataCohortTable, person the
  # doseTable
  cdm <- mockDrugUtilisation(
    condition_occurrence = dplyr::tibble(
      cohort_definition_id = c(1, 1, 1, 2),
      subject_id = c(1, 1, 2, 1),
      cohort_start_date = as.Date(c(
        "2020-01-01", "2020-05-01", "2020-04-08", "2020-01-01"
      )),
      cohort_end_date = as.Date(c(
        "2020-01-10", "2020-06-01", "2020-07-18", "2020-01-11"
      ))
    ),
    person = dplyr::tibble(
      subject_id = c(1, 1, 2, 1),
      cohort_start_date = as.Date(c(
        "2020-01-01", "2020-05-01", "2020-04-08", "2020-01-01"
      )),
      cohort_end_date = as.Date(c(
        "2020-01-10", "2020-06-01", "2020-07-18", "2020-01-11"
      )),
      initial_dose = c(1, 2, 3, 6),
      cumulative_dose = c(5, 6, 9, 7),
      piscina = c(TRUE, FALSE, TRUE, FALSE),
      cara = c("a", "b", "b", "a")
    )
  )
  # no inputs
  expect_error(result <- summariseDoseTable())
  # only cdm
  expect_error(result <- summariseDoseTable(
    cdm = cdm,
  ))
  # only cdm only dose table should fail
  expect_error(result <- summariseDoseTable(
    cdm = cdm, doseTableName = "person"
  ))
  # only cdm only strata table should fail
  expect_error(result <- summariseDoseTable(
    cdm = cdm, strataCohortName = "condition_occurrence"
  ))
  # expect error if cdm is not a cdm_ref
  expect_error(summariseDoseTable(
    cdm = cdm,
    strataCohortName = "condition_occurrence",
    doseTableName = "person"
  ))
  # NO ERROR
  xx <- summariseDoseTable(
    cdm = cdm,
    strataCohortName = "condition_occurrence",
    doseTableName = "person",
    variables = c("initial_dose", "cumulative_dose")
  )
  # expect error if cdm is not a cdm_ref
  expect_error(summariseDoseTable(
    cdm = 1,
    strataCohortName = "condition_occurrence",
    doseTableName = "person",
    variables = c("initial_dose", "cumulative_dose")
  ))
  # expect error if doseTableName is not a character
  expect_error(summariseDoseTable(
    cdm = cdm,
    strataCohortName = "condition_occurrence",
    doseTableName = 1,
    variables = c("initial_dose", "cumulative_dose")
  ))
  # expect error if strataCohortName is not a character
  expect_error(summariseDoseTable(
    cdm = cdm,
    strataCohortName = 1,
    doseTableName = "person",
    variables = c("initial_dose", "cumulative_dose")
  ))
  # expect error if doseTableName is a vector
  expect_error(summariseDoseTable(
    cdm = cdm,
    strataCohortName = c("condition_occurrence", "drug_exposure"),
    doseTableName = "person",
    variables = c("initial_dose", "cumulative_dose")
  ))
  # expect error if doseTableName is a vector
  expect_error(summariseDoseTable(
    cdm = cdm,
    strataCohortName = "condition_occurrence",
    doseTableName = c("person", "drug_exposure"),
    variables = c("initial_dose", "cumulative_dose")
  ))
  # expect error if doseTableName is not a contained in cdm
  expect_error(summariseDoseTable(
    cdm = cdm,
    strataCohortName = "condition_occurrence",
    doseTableName = "x",
    variables = c("initial_dose", "cumulative_dose")
  ))
  # expect error if strataCohortName is not a contained in cdm
  expect_error(summariseDoseTable(
    cdm = cdm,
    strataCohortName = "x",
    doseTableName = "person",
    variables = c("initial_dose", "cumulative_dose")
  ))
  # expect error if strataCohortName is a vector
  # expect error if strataCohortName does not contains the required fields
  # expect error if cohortId is not numeric
  # expect error if variable is not character
  # expect error if variable contains a non numeric variable
  # expect error if variable contains not present variable
  # expect error if estimate is not character
  # expect error if estimate contains a non standard function
  # expect error if minimumCellCount is not numeric
  # expect error if minimumCellCount is negative
  # expect error if minimumCellCount is a vector
})

test_that("check output format", {
  cdm <- mockDrugUtilisation(
    condition_occurrence = dplyr::tibble(
      cohort_definition_id = c(1, 1, 1, 2),
      subject_id = c(1, 1, 2, 1),
      cohort_start_date = as.Date(c(
        "2020-01-01", "2020-05-01", "2020-04-08", "2020-01-01"
      )),
      cohort_end_date = as.Date(c(
        "2020-01-10", "2020-06-01", "2020-07-18", "2020-01-11"
      ))
    ),
    person = dplyr::tibble(
      subject_id = c(1, 1, 2, 1),
      cohort_start_date = as.Date(c(
        "2020-01-01", "2020-05-01", "2020-04-08", "2020-01-01"
      )),
      cohort_end_date = as.Date(c(
        "2020-01-10", "2020-06-01", "2020-07-18", "2020-01-11"
      )),
      initial_dose = c(1, 2, 3, 6),
      cumulative_dose = c(5, 6, 9, 7),
      piscina = c(TRUE, FALSE, TRUE, FALSE),
      cara = c("a", "b", "b", "a")
    )
  )
  result <- summariseDoseTable(
    cdm = cdm,
    strataCohortName = "condition_occurrence",
    doseTableName = "person",
    variables = c("initial_dose", "cumulative_dose")
  )
  expect_true(all(c("tbl_df", "tbl", "data.frame") %in% class(result)))
  expect_true(length(result) == 4)
  expect_true(all(colnames(result) %in% c(
    "cohort_definition_id", "variable", "estimate", "value"
  )))
})

test_that("check all estimates", {
  all_estimates <- c(
    "min", "max", "mean", "median", "iqr", "range", "q5", "q10", "q15", "q20",
    "q25", "q30", "q35", "q40", "q45", "q55", "q60", "q65", "q70",
    "q75", "q80", "q85", "q90", "q95", "std"
  )
  cdm <- mockDrugUtilisation(
    condition_occurrence = dplyr::tibble(
      cohort_definition_id = c(1, 1, 1, 2),
      subject_id = c(1, 1, 2, 1),
      cohort_start_date = as.Date(c(
        "2020-01-01", "2020-05-01", "2020-04-08", "2020-01-01"
      )),
      cohort_end_date = as.Date(c(
        "2020-01-10", "2020-06-01", "2020-07-18", "2020-01-11"
      ))
    ),
    person = dplyr::tibble(
      subject_id = c(1, 1, 2, 1),
      cohort_start_date = as.Date(c(
        "2020-01-01", "2020-05-01", "2020-04-08", "2020-01-01"
      )),
      cohort_end_date = as.Date(c(
        "2020-01-10", "2020-06-01", "2020-07-18", "2020-01-11"
      )),
      initial_dose = c(1, 2, 3, 6),
      cumulative_dose = c(5, 6, 9, 7),
      piscina = c(TRUE, FALSE, TRUE, FALSE),
      cara = c("a", "b", "b", "a")
    )
  )
  for (k in 1:length(all_estimates)) {
    res <- summariseDoseTable(
      cdm = cdm,
      strataCohortName = "condition_occurrence",
      doseTableName = "person",
      cohortId = 1,
      variables = c("initial_dose", "cumulative_dose"),
      estimates = all_estimates[k]
    )
    expect_true(nrow(res[res$variable == c("initial_dose"), ]) == 1)
    expect_true(res$estimate[res$variable == c("initial_dose")] == all_estimates[k])
    expect_true(nrow(res[res$variable == c("cumulative_dose"), ]) == 1)
    expect_true(res$estimate[res$variable == c("cumulative_dose")] == all_estimates[k])
  }
  res <- summariseDoseTable(
    cdm = cdm,
    strataCohortName = "condition_occurrence",
    doseTableName = "person",
    cohortId = 1,
    variables = c("initial_dose", "cumulative_dose"),
    estimates = all_estimates
  )
})

test_that("check obscure counts", {
  cdm <- mockDrugUtilisation(
    condition_occurrence = dplyr::tibble(
      cohort_definition_id = c(1, 1, 1, 2),
      subject_id = c(1, 1, 2, 1),
      cohort_start_date = as.Date(c(
        "2020-01-01", "2020-05-01", "2020-04-08", "2020-01-01"
      )),
      cohort_end_date = as.Date(c(
        "2020-01-10", "2020-06-01", "2020-07-18", "2020-01-11"
      ))
    ),
    person = dplyr::tibble(
      subject_id = c(1, 1, 2, 1),
      cohort_start_date = as.Date(c(
        "2020-01-01", "2020-05-01", "2020-04-08", "2020-01-01"
      )),
      cohort_end_date = as.Date(c(
        "2020-01-10", "2020-06-01", "2020-07-18", "2020-01-11"
      )),
      initial_dose = c(1, 2, 3, 6),
      cumulative_dose = c(5, 6, 9, 7),
      piscina = c(TRUE, FALSE, TRUE, FALSE),
      cara = c("a", "b", "b", "a")
    )
  )
  # expect obscure for cohort_id = 1 when minimumCellCounts >= 4
  expect_true(
    summariseDoseTable(
      cdm = cdm,
      strataCohortName = "condition_occurrence",
      doseTableName = "person",
      cohortId = 1,
      variables = c("initial_dose", "cumulative_dose"),
      minimumCellCounts = 3
    ) %>%
      dplyr::filter(is.na(.data$value)) %>%
      dplyr::tally() %>%
      dplyr::pull() == 0
  )
  expect_true(
    summariseDoseTable(
      cdm = cdm,
      strataCohortName = "condition_occurrence",
      doseTableName = "person",
      cohortId = 1,
      variables = c("initial_dose", "cumulative_dose"),
      minimumCellCounts = 4
    ) %>%
      dplyr::filter(is.na(.data$value)) %>%
      dplyr::tally() %>%
      dplyr::pull() == 20 # 20 because all variables should be obscured
  )
  # expect obscure for cohort_id = 2 when minimumCellCounts >= 2
  expect_true(
    summariseDoseTable(
      cdm = cdm,
      strataCohortName = "condition_occurrence",
      doseTableName = "person",
      cohortId = 2,
      variables = c("initial_dose", "cumulative_dose"),
      minimumCellCounts = 1
    ) %>%
      dplyr::filter(is.na(.data$value)) %>%
      dplyr::tally() %>%
      dplyr::pull() == 2 # 2 because std of a variable of length 1 is always NA
  )
  expect_true(
    summariseDoseTable(
      cdm = cdm,
      strataCohortName = "condition_occurrence",
      doseTableName = "person",
      cohortId = 2,
      variables = c("initial_dose", "cumulative_dose"),
      minimumCellCounts = 2
    ) %>%
      dplyr::filter(is.na(.data$value)) %>%
      dplyr::tally() %>%
      dplyr::pull() == 20 # 20 because all variables should be obscured
  )
  # if minimumCellCount is 1 no obscure, if it is 2 or 3 only cohort 1 is
  # obscured. If it is 4 both cohorts are obscured
  expect_true(
    summariseDoseTable(
      cdm = cdm,
      strataCohortName = "condition_occurrence",
      doseTableName = "person",
      variables = c("initial_dose", "cumulative_dose"),
      minimumCellCounts = 1
    ) %>%
      dplyr::filter(is.na(.data$value)) %>%
      dplyr::tally() %>%
      dplyr::pull() == 2 # 2 because std of a variable of length 1 is always NA
  )
  expect_true(
    summariseDoseTable(
      cdm = cdm,
      strataCohortName = "condition_occurrence",
      doseTableName = "person",
      variables = c("initial_dose", "cumulative_dose"),
      minimumCellCounts = 2
    ) %>%
      dplyr::filter(is.na(.data$value)) %>%
      dplyr::tally() %>%
      dplyr::pull() == 20 # 20 because all variables in cohort 2 are obscured
  )
  expect_true(
    summariseDoseTable(
      cdm = cdm,
      strataCohortName = "condition_occurrence",
      doseTableName = "person",
      variables = c("initial_dose", "cumulative_dose"),
      minimumCellCounts = 3
    ) %>%
      dplyr::filter(is.na(.data$value)) %>%
      dplyr::tally() %>%
      dplyr::pull() == 20 # 20 because all variables in cohort 2 are obscured
  )
  expect_true(
    summariseDoseTable(
      cdm = cdm,
      strataCohortName = "condition_occurrence",
      doseTableName = "person",
      variables = c("initial_dose", "cumulative_dose"),
      minimumCellCounts = 4
    ) %>%
      dplyr::filter(is.na(.data$value)) %>%
      dplyr::tally() %>%
      dplyr::pull() == 40 # 40 because all variables are obscured
  )
})

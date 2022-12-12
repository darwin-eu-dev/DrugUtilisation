library(testthat)
library(dplyr, warn.conflicts = FALSE)

test_that("test on synthea test server", {
  skip_if(Sys.getenv("TESTDB_USER") == "")

  con <- DBI::dbConnect(odbc::odbc(),
    Driver   = Sys.getenv("TESTDB_DRIVER"),
    Server   = Sys.getenv("TESTDB_SERVER"),
    Database = Sys.getenv("TESTDB_NAME"),
    UID      = Sys.getenv("TESTDB_USER"),
    PWD      = Sys.getenv("TESTDB_PWD"),
    Port     = Sys.getenv("TESTDB_PORT")
  )


  cdm <- CDMConnector::cdm_from_con(con,
    cdm_schema = Sys.getenv("TESTDB_CDM_SCHEMA"),
    write_schema = Sys.getenv("TESTDB_WRITE_SCHEMA"),
    cohort_tables = c("cohort")
  )

  result <- instantiateDrugUtilisationCohorts(
    cdm,
    ingredientConceptId = 1125315,
    eraJoinMode = "Previous",
    cohortEntryPriorHistory = NULL,
    overlapMode = "Sum",
    sameIndexMode = "Sum"
  )

  result <- result$dose %>% dplyr::collect()
  expect_true(all(c(
    "subject_id", "era_id", "cohort_start_date", "cohort_end_date",
    "exposed_days", "cumulative_dose", "study_days", "not_exposed_days",
    "initial_dose", "number_exposures", "number_subexposures", "number_subexposures_with_overlap",
    "number_continuous_exposures", "number_continuous_exposures_with_overlap", "number_eras",
    "number_non_exposed_periods", "number_gaps", "number_days_gap", "cumulative_gap_dose",
    "all_dose", "all_exposed_days", "number_subexposures_no_overlap", "number_continuous_exposures_no_overlap",
    "number_eras_no_overlap", "not_considered_dose", "not_considered_exposed_days", "prop_cum_gap_dose",
    "prop_not_considered_exp_days", "cohort_definition_id"
  ) %in% names(result)))

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

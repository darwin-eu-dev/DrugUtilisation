connection <- function(type = Sys.getenv("DB_TO_TEST", "duckdb")) {
  switch(type,
    "duckdb" = DBI::dbConnect(duckdb::duckdb(), ":memory:"),
    "sql server" = DBI::dbConnect(
      odbc::odbc(),
      Driver = "ODBC Driver 18 for SQL Server",
      Server = Sys.getenv("CDM5_SQL_SERVER_SERVER"),
      Database = Sys.getenv("CDM5_SQL_SERVER_CDM_DATABASE"),
      UID = Sys.getenv("CDM5_SQL_SERVER_USER"),
      PWD = Sys.getenv("CDM5_SQL_SERVER_PASSWORD"),
      TrustServerCertificate = "yes",
      Port = 1433
    ),
    "redshift" = DBI::dbConnect(
      RPostgres::Redshift(),
      dbname = Sys.getenv("CDM5_REDSHIFT_DBNAME"),
      port = Sys.getenv("CDM5_REDSHIFT_PORT"),
      host = Sys.getenv("CDM5_REDSHIFT_HOST"),
      user = Sys.getenv("CDM5_REDSHIFT_USER"),
      password = Sys.getenv("CDM5_REDSHIFT_PASSWORD")
    )
  )
}
schema <- function(type = Sys.getenv("DB_TO_TEST", "duckdb")) {
  switch(type,
    "duckdb" = c(schema = "main", prefix = "dus_"),
    "sql server" = c(schema = Sys.getenv("CDM5_SQL_SERVER_OHDSI_SCHEMA"), prefix = "dus_"),
    "redshift" = c(schema = Sys.getenv("CDM5_REDSHIFT_SCRATCH_SCHEMA"), prefix = "dus_")
  )
}
collectCohort <- function(cohort, id = NULL) {
  if (is.null(id)) id <- settings(cohort)$cohort_definition_id
  x <- cohort |>
    dplyr::filter(.data$cohort_definition_id %in% .env$id) |>
    dplyr::select("cohort_definition_id", "subject_id", "cohort_start_date", "cohort_end_date") |>
    dplyr::collect() |>
    dplyr::arrange(cohort_definition_id, subject_id, cohort_start_date, cohort_end_date)
  attr(x, "cohort_set") <- NULL
  attr(x, "cohort_attrition") <- NULL
  attr(x, "cohort_codelist") <- NULL
  return(x)
}

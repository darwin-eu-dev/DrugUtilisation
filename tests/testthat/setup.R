connection <- function(dbToTest) {
  switch(
    dbToTest,
    "duckdb" = list(
      con = DBI::dbConnect(duckdb::duckdb(), ":memory:"),
      writeSchema = "main"
    ),
    "sql server" = list(
      con = DBI::dbConnect(
        odbc::odbc(),
        Driver   = "ODBC Driver 18 for SQL Server",
        Server   = Sys.getenv("CDM5_SQL_SERVER_SERVER"),
        Database = Sys.getenv("CDM5_SQL_SERVER_CDM_DATABASE"),
        UID      = Sys.getenv("CDM5_SQL_SERVER_USER"),
        PWD      = Sys.getenv("CDM5_SQL_SERVER_PASSWORD"),
        TrustServerCertificate = "yes",
        Port     = 1433
      ),
      writeSchema = Sys.getenv("CDM5_SQL_SERVER_OHDSI_SCHEMA"),
      cdmPrefix = "test_dus_cdm_",
      writePrefix = "tsts_dus_write_"
    ),
    "redshift" = list(
      con = DBI::dbConnect(
        RPostgres::Redshift(), dbname = Sys.getenv("CDM5_REDSHIFT_DBNAME"),
        port = Sys.getenv("CDM5_REDSHIFT_PORT"),
        host = Sys.getenv("CDM5_REDSHIFT_HOST"),
        user = Sys.getenv("CDM5_REDSHIFT_USER"),
        password = Sys.getenv("CDM5_REDSHIFT_PASSWORD")
      ),
      writeSchema = Sys.getenv("CDM5_REDSHIFT_SCRATCH_SCHEMA"),
      cdmPrefix = "test_dus_cdm_",
      writePrefix = "tsts_dus_write_"
    )
  )
}
connectionDetails <- connection(Sys.getenv("DB_TO_TEST", "duckdb"))

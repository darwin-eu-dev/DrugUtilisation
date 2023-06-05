library(testthat)
library(DrugUtilisation)

availableConnections <- list(list(
  con = DBI::dbConnect(duckdb::duckdb(), ":memory:"),
  writeSchema = "main",
  writePrefix = NULL
))

if (Sys.getenv("CDM5_REDSHIFT_DBNAME") != "") {
  availableConnections <- availableConnections %>%
    append(value = list(list(
      con = DBI::dbConnect(
        RPostgres::Redshift(), dbname = Sys.getenv("CDM5_REDSHIFT_DBNAME"),
        port = Sys.getenv("CDM5_REDSHIFT_PORT"),
        host = Sys.getenv("CDM5_REDSHIFT_HOST"),
        user = Sys.getenv("CDM5_REDSHIFT_USER"),
        password = Sys.getenv("CDM5_REDSHIFT_PASSWORD")
      ),
      scratch_schema = Sys.getenv("CDM5_REDSHIFT_SCRATCH_SCHEMA"),
      write_schema = Sys.getenv("CDM5_REDSHIFT_OHDSI_SCHEMA")
    )))
}

for (k in seq_along(availableConnections)) {
  # connection details
  connectionDetails <- availableConnections[[k]]
  # test code in that dbms
  test_check("DrugUtilisation")
  # delete tables that have been aded by the mock datas
  # disconnectMock
}

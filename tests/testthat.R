library(testthat)
library(DrugUtilisation)

connectionDetails <- list(
  con = DBI::dbConnect(duckdb::duckdb(), ":memory:"),
  writeSchema = "main",
  writePrefix = NULL
)

# connectionDetails <- list(
#   con = DBI::dbConnect(
#     RPostgres::Postgres(),
#     dbname = "cdm_gold_202201",
#     port = Sys.getenv("DB_PORT"),
#     host = Sys.getenv("DB_HOST"),
#     user = Sys.getenv("DB_USER"),
#     password = Sys.getenv("DB_PASSWORD")
#   ),
#   writeSchema = "results",
#   writePrefix = NULL
# )

# connectionDetails <- list(
#   con = DBI::dbConnect(
#     RPostgres::Redshift(),
#     dbname = Sys.getenv("CDM5_REDSHIFT_DBNAME"),
#     port = Sys.getenv("CDM5_REDSHIFT_PORT"),
#     host = Sys.getenv("CDM5_REDSHIFT_HOST"),
#     user = Sys.getenv("CDM5_REDSHIFT_USER"),
#     password = Sys.getenv("CDM5_REDSHIFT_PASSWORD")
#   ),
#   writeSchema = Sys.getenv("CDM5_REDSHIFT_OHDSI_SCHEMA"),
#   writePrefix = NULL
# )

# list initial tables
initialTables <- CDMConnector::listTables(
  connectionDetails$con, connectionDetails$writeSchema
)
# test code in that dbms
test_check("DrugUtilisation")
# get final tables
finalTables <- CDMConnector::listTables(
  connectionDetails$con, connectionDetails$writeSchema
)
# to eliminate
tablesToEliminate <- finalTables[!(finalTables %in% initialTables)]
# eliminate new created tables
for (tableToEliminate in tablesToEliminate) {
  DBI::dbRemoveTable(
    connectionDetails$con,
    CDMConnector::inSchema(connectionDetails$writeSchema, tableToEliminate)
  )
}
DBI::dbDisconnect(connectionDetails$con)

library(testthat)
library(DrugUtilisation)

dbToTest <- c(
  "duckdb"
  #,"sqlserver"
  #,"redshift"
)

availableConnections <- list(list(
  con = DBI::dbConnect(duckdb::duckdb(), ":memory:"),
  writeSchema = "main",
  mockPrefix = NULL
))

# if (Sys.getenv("CDM5_SQL_SERVER_USER") != "") {
#   availableConnections <- availableConnections %>%
#     append(value = list(list(
#       con = DBI::dbConnect(
#         odbc::odbc(),
#         Driver   = "ODBC Driver 18 for SQL Server",
#         Server   = Sys.getenv("CDM5_SQL_SERVER_SERVER"),
#         Database = Sys.getenv("CDM5_SQL_SERVER_CDM_DATABASE"),
#         UID      = Sys.getenv("CDM5_SQL_SERVER_USER"),
#         PWD      = Sys.getenv("CDM5_SQL_SERVER_PASSWORD"),
#         TrustServerCertificate = "yes",
#         Port     = 1433
#       ),
#       writeSchema = Sys.getenv("CDM5_SQL_SERVER_OHDSI_SCHEMA"),
#       mockPrefix = "test_dus_"
#     )))
# }

# if (Sys.getenv("CDM5_REDSHIFT_DBNAME") != "") {
#   availableConnections <- availableConnections %>%
#     append(value = list(list(
#       con = DBI::dbConnect(
#         RPostgres::Redshift(), dbname = Sys.getenv("CDM5_REDSHIFT_DBNAME"),
#         port = Sys.getenv("CDM5_REDSHIFT_PORT"),
#         host = Sys.getenv("CDM5_REDSHIFT_HOST"),
#         user = Sys.getenv("CDM5_REDSHIFT_USER"),
#         password = Sys.getenv("CDM5_REDSHIFT_PASSWORD")
#       ),
#       writeSchema = Sys.getenv("CDM5_REDSHIFT_SCRATCH_SCHEMA"),
#       mockPrefix = "test_dus_"
#     )))
# }

for (k in seq_along(availableConnections)) {
  # connection details
  connectionDetails <- availableConnections[[k]]
  # test code in that dbms
  test_check("DrugUtilisation")
}

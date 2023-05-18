library(testthat)
library(DrugUtilisation)

equalTibble <- function(x, y) {
  colnamesX <- colnames(x)
  colnamesY <- colnames(y)
  if (!all(colnamesX %in% colnamesY)) {
    return(FALSE)
  }
  if (!all(colnamesY %in% colnamesX)) {
    return(FALSE)
  }
  y <- y %>%
    dplyr::select(dplyr::all_of(colnamesX))
  if (nrow(x) != nrow(y)) {
    return(FALSE)
  }
  x <- x %>%
    dplyr::arrange(!!!rlang::parse_exprs(colnamesX))
  y <- y %>%
    dplyr::arrange(!!!rlang::parse_exprs(colnamesX))
  for (colname in colnames(x)) {
    xx <- x[[colname]]
    yy <- y[[colname]]
    if (!all(is.na(xx) == is.na(yy))) {
      return(FALSE)
    }
    xx <- xx[!is.na(xx)]
    yy <- yy[!is.na(yy)]
    if (!all(xx == yy)) {
      return(FALSE)
    }
  }
  return(TRUE)
}

availableConnections <- list()

availableConnections$duckdb <- list(
  con = DBI::dbConnect(duckdb::duckdb(), ":memory:"),
  writeSchema = "main",
  writePrefix = NULL
)

if (Sys.getenv("DB_USER") != "") {
  availableConnections$postgres <- list(
    con = DBI::dbConnect(
      RPostgres::Postgres(),
      dbname = "cdm_gold_202201",
      port = Sys.getenv("DB_PORT"),
      host = Sys.getenv("DB_HOST"),
      user = Sys.getenv("DB_USER"),
      password = Sys.getenv("DB_PASSWORD")
    ),
    writeSchema = "results",
    writePrefix = NULL
  )
}

if (Sys.getenv("CDM5_REDSHIFT_DBNAME") != "") {
  availableConnections$redshift <- list(
    con = DBI::dbConnect(
      RPostgres::Redshift(),
      dbname = Sys.getenv("CDM5_REDSHIFT_DBNAME"),
      port = Sys.getenv("CDM5_REDSHIFT_PORT"),
      host = Sys.getenv("CDM5_REDSHIFT_HOST"),
      user = Sys.getenv("CDM5_REDSHIFT_USER"),
      password = Sys.getenv("CDM5_REDSHIFT_PASSWORD")
    ),
    writeSchema = Sys.getenv("CDM5_REDSHIFT_OHDSI_SCHEMA"),
    writePrefix = NULL
  )
}

for (connectionDetails in availableConnections) {
  # list initial tables
  initialTables <- CDMConnector::listTables(
    connectionDetails$db, connectionDetails$writeSchema
  )
  # test code in that dbms
  test_check("DrugUtilisation")
  # get final tables
  finalTables <- CDMConnector::listTables(
    connectionDetails$db, connectionDetails$writeSchema
  )
  # to eliminate
  tablesToEliminate <- finalTables[!(finalTables %in% initialTables)]
  # eliminate new created tables
  for (tableToEliminate in tablesToEliminate) {
    DBI::dbRemoveTable(
      connectionDetails$db,
      CDMConnector::inSchema(connectionDetails$writeSchema, tableToEliminate)
    )
  }
  DBI::dbDisconnect(connectionDetails$db)
}

connectionDetails <- list(
  con = DBI::dbConnect(duckdb::duckdb(), ":memory:"),
  writeSchema = "main",
  writePrefix = NULL
)

#' Explain function
#'
#' @param dialect dialect
#' @param years_to_add years_to_add
#' @param variable variable
#'
#' @noRd
sql_add_years<-function(dialect, years_to_add, variable){

  error_message <- checkmate::makeAssertCollection()
  year_check<-(years_to_add%%1==0)
  checkmate::assertTRUE(year_check,
                        add = error_message
  )
  checkmate::reportAssertions(collection = error_message)

  rendered_translated_sql <- SqlRender::translate(
    SqlRender::render("DATEADD(year, @years_to_add, @variable)",
                      years_to_add=years_to_add,
                      variable=variable),
    targetDialect = dialect)
  return(rendered_translated_sql)
}

#' Explain function
#'
#' @param dialect dialect
#' @param days_to_add days_to_add
#' @param variable variable
#'
#' @noRd
sql_add_days<-function(dialect, days_to_add, variable){

  # error_message <- checkmate::makeAssertCollection()
  # days_check<-(days_to_add%%1==0)
  # checkmate::assertTRUE(days_check,
  #                       add = error_message
  # )
  # checkmate::reportAssertions(collection = error_message)

  rendered_translated_sql <- SqlRender::translate(
    SqlRender::render("DATEADD(day, @days_to_add, @variable)",
                      days_to_add=days_to_add,
                      variable=variable),
    targetDialect = dialect)
  return(rendered_translated_sql)
}

#' Explain function
#'
#' @param dialect dialect
#' @param variable1 variable1
#' @param variable2 variable2
#'
#' @noRd
sqlDiffDays <- function(dialect, variable1, variable2) {
  SqlRender::translate(
    SqlRender::render("DATEDIFF(DAY, @variable1, @variable2)",
                      variable1 = variable1,
                      variable2 = variable2
    ),
    targetDialect = dialect
  )
}




#' Run a dplyr query and store the result in a permanent table
#'
#' @param x A dplyr query
#' @param name Name of the table to be created
#' @param schema Schema to create the new table in
#' Can be a length 1 or 2 vector. (e.g. schema = "my_schema", schema = c("my_schema", "dbo))
#' @param overwrite If the table already exists in the remote database should it be overwritten? (TRUE or FALSE)
#'
#' @return A dplyr reference to the newly created table
#' @export
#'
#' @examples
#' \dontrun{
#' library(CDMConnector)
#' library(SqlUtilities)
#'
#' con <- DBI::dbConnect(duckdb::duckdb(), dbdir = CDMConnector::eunomia_dir())
#' concept <- dplyr::tbl(con, "concept")
#'
#' rxnorm_count <- concept %>%
#'   dplyr::filter(domain_id == "Drug") %>%
#'   dplyr::mutate(isRxnorm = (vocabulary_id == "RxNorm")) %>%
#'   dplyr::count(isRxnorm) %>%
#'   computePermanent("rxnorm_count")
#'
#' DBI::dbDisconnect(con, shutdown = TRUE)
#' }
computePermanent <- function(x, name, schema = NULL, overwrite = FALSE) {
  checkmate::assertCharacter(schema, min.len = 1, max.len = 2, null.ok = TRUE)
  checkmate::assertCharacter(name, len = 1)
  checkmate::assertClass(x, "tbl_sql")
  checkmate::assertLogical(overwrite, len = 1)

  if (length(schema) == 2) {
    fullName <- paste(schema[[1]], schema[[2]], name, sep = ".")
  } else if (length(schema) == 1) {
    fullName <- paste(schema, name, sep = ".")
  } else {
    fullName <- name
  }

  existingTables <- CDMConnector::listTables(x$src$con, schema = schema)
  if (name %in% existingTables) {
    if (overwrite) {
      DBI::dbRemoveTable(x$src$con, DBI::SQL(fullName))
    } else if (!append) {
      rlang::abort(paste(fullName, "already exists. Set overwrite = TRUE to recreate it"))
    }
  }


  # TODO fix select * INTO translation for duckdb in SqlRender
  if (CDMConnector::dbms(x$src$con) == "duckdb") {
    sql <- glue::glue("CREATE TABLE {fullName} AS {dbplyr::sql_render(x)}")
  } else {
    sql <- glue::glue("SELECT * INTO {fullName} FROM ({dbplyr::sql_render(x)}) x")
    sql <- SqlRender::translate(sql, targetDialect = CDMConnector::dbms(x$src$con))
  }

  DBI::dbExecute(x$src$con, sql)

  if (length(schema) == 2) {
    ref <- dplyr::tbl(x$src$con, dbplyr::in_catalog(schema[[1]], schema[[2]], name))
  } else if (length(schema) == 1) {
    ref <- dplyr::tbl(x$src$con, dbplyr::in_schema(schema, name))
  } else {
    ref <- dplyr::tbl(x$src$con, name)
  }
  return(ref)
}

#' Run a dplyr query and add the result set to an existing
#'
#' @param x A dplyr query
#' @param name Name of the table to be appended. If it does not already exist it will be created.
#' @param schema Schema where the table exists.
#' Can be a length 1 or 2 vector. (e.g. schema = "my_schema", schema = c("my_schema", "dbo))
#'
#' @return A dplyr reference to the newly created table
#' @export
#'
#' @examples
#' \dontrun{
#' library(CDMConnector)
#' library(SqlUtilities)
#'
#' con <- DBI::dbConnect(duckdb::duckdb(), dbdir = CDMConnector::eunomia_dir())
#' concept <- dplyr::tbl(con, "concept")
#'
#' # create a table
#' rxnorm_count <- concept %>%
#'   dplyr::filter(domain_id == "Drug") %>%
#'   dplyr::mutate(isRxnorm = (vocabulary_id == "RxNorm")) %>%
#'   dplyr::count(domain_id, isRxnorm) %>%
#'   computePermanent("rxnorm_count")
#'
#' # append to an existing table
#' rxnorm_count <- concept %>%
#'   dplyr::filter(domain_id == "Procedure") %>%
#'   dplyr::mutate(isRxnorm = (vocabulary_id == "RxNorm")) %>%
#'   dplyr::count(domain_id, isRxnorm) %>%
#'   appendPermanent("rxnorm_count")
#'
#' DBI::dbDisconnect(con, shutdown = TRUE)
#'
#' }
appendPermanent <- function(x, name, schema = NULL) {
  checkmate::assertCharacter(schema, min.len = 1, max.len = 2, null.ok = TRUE)
  checkmate::assertCharacter(name, len = 1)
  checkmate::assertClass(x, "tbl_sql")

  if (length(schema) == 2) {
    fullName <- paste(schema[[1]], schema[[2]], name, sep = ".")
  } else if (length(schema) == 1) {
    fullName <- paste(schema, name, sep = ".")
  } else {
    fullName <- name
  }

  existingTables <- CDMConnector::listTables(x$src$con, schema = schema)
  if (!(name %in% existingTables)) {
    return(computePermanent(x = x, name = name, schema = schema, overwrite = FALSE))
  }

  sql <- glue::glue("INSERT INTO {fullName} {dbplyr::sql_render(x)}")
  sql <- SqlRender::translate(sql, targetDialect = CDMConnector::dbms(x$src$con))

  DBI::dbExecute(x$src$con, sql)

  if (length(schema) == 2) {
    ref <- dplyr::tbl(x$src$con, dbplyr::in_catalog(schema[[1]], schema[[2]], name))
  } else if (length(schema) == 1) {
    ref <- dplyr::tbl(x$src$con, dbplyr::in_schema(schema, name))
  } else {
    ref <- dplyr::tbl(x$src$con, name)
  }
  return(ref)
}




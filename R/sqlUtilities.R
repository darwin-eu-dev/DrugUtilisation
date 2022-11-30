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




getGender <- function(cdm,
                      cohortTable,
                      cohortIds = NULL) {
  # checks
  errorMessage <- checkmate::makeAssertCollection()
  checkDbType(cdm, type = "cdm_reference", errorMessage)
  checkTableExists(cdm, "person", errorMessage)

  if (!is.null(cohortIds) &&
      is.numeric(cohortIds)) {
    cohortIds <- as.character(cohortIds)
  }
  checkCharacter(cohortIds, errorMessage, null.ok = TRUE)
  checkCharacter(cohortTable, errorMessage, null.ok = FALSE)

  personDb <- dplyr::rename_with(cdm$person, tolower) %>%
    dplyr::compute()

  personDbNames <- c("person_id", "gender_concept_id")
  personDbNamesCheck <- all(personDbNames %in%
                              names(personDb %>%
                                      utils::head(1) %>%
                                      dplyr::collect() %>%
                                      dplyr::rename_with(tolower)))
  checkmate::assertTRUE(personDbNamesCheck, add = errorMessage)
  checkTableExists(cdm, cohortTable, errorMessage)
  checkmate::reportAssertions(collection = errorMessage)

  if (isTRUE(is.na(cohortIds))) {
    cohortIds <- NULL
  }

  if (!is.null(cohortIds)) {
    cohortDb <- cdm[[cohortTable]] %>%
      dplyr::filter(.data$cohort_definition_id %in% .env$cohortIds)
  } else {
    cohortDb <- cdm[[cohortTable]]}


  personDb <- cohortDb %>% dplyr::rename("person_id" = "subject_id") %>%
    dplyr::left_join((personDb) %>%
                       dplyr::distinct(), by = "person_id")


  studyPopDb <- personDb %>%
    dplyr::mutate(gender = ifelse(.data$gender_concept_id == "8507", "Male",
                                  ifelse(.data$gender_concept_id == "8532", "Female", NA)
    )) %>%
    dplyr::compute()


  studyPop <- studyPopDb %>%
    dplyr::select("person_id", "gender", "gender_concept_id")


  if ((studyPop %>% dplyr::count() %>% dplyr::pull()) == 0) {
    message("-- No people found for denominator population")
  }

  return(studyPop)
}





getAge <- function(cdm,
                   cohortTable,
                   ageAtCohortStart = TRUE,
                   cohortIds = NULL) {
  # checks
  errorMessage <- checkmate::makeAssertCollection()
  checkDbType(cdm, type = "cdm_reference", errorMessage)
  checkTableExists(cdm, "person", errorMessage)
  checkLogical(ageAtCohortStart, errorMessage, null.ok = FALSE)
  if (!is.null(cohortIds) &&
      is.numeric(cohortIds)) {
    cohortIds <- as.character(cohortIds)
  }
  checkCharacter(cohortIds, errorMessage, null.ok = TRUE)
  checkCharacter(cohortTable, errorMessage, null.ok = FALSE)

  personDb <- dplyr::rename_with(cdm$person, tolower) %>%
    dplyr::compute()

  personDbNames <- c("person_id","year_of_birth", "month_of_birth", "day_of_birth")
  personDbNamesCheck <- all(personDbNames %in%
                              names(personDb %>%
                                      utils::head(1) %>%
                                      dplyr::collect() %>%
                                      dplyr::rename_with(tolower)))
  checkmate::assertTRUE(personDbNamesCheck, add = errorMessage)
  # checkTableExists(cdm, cohortTable, errorMessage)

  cohortHeader <- names(cdm[[cohortTable]] %>%
                          utils::head(1) %>%
                          dplyr::collect() %>%
                          dplyr::rename_with(tolower))

  if (ageAtCohortStart == TRUE) {
    cohortStartDateExists <- 'cohort_start_date' %in% cohortHeader
    checkmate::assertTRUE(cohortStartDateExists, add = errorMessage)
    if (!isTRUE(cohortStartDateExists)) {
      errorMessage$push("- cohort_start_date column is not found")
    }
  }

  if (ageAtCohortStart == FALSE) {
    cohortEndDateExists <- 'cohort_end_date' %in% cohortHeader
    checkmate::assertTRUE(cohortEndDateExists, add = errorMessage)
    if (!isTRUE(cohortEndDateExists)) {
      errorMessage$push("- cohort_end_date column is not found")
    }
  }
  checkmate::reportAssertions(collection = errorMessage)

  if (isTRUE(is.na(cohortIds))) {
    cohortIds <- NULL
  }

  if (!is.null(cohortIds)) {
    cohortDb <- cdm[[cohortTable]] %>%
      dplyr::filter(.data$cohort_definition_id %in% .env$cohortIds) %>%
      dplyr::compute()} else {
        cohortDb <- cdm[[cohortTable]] %>%
          dplyr::compute()
      }

  if (ageAtCohortStart) {
    cohortDb <- cohortDb %>%
      dplyr::rename("person_id" = "subject_id") %>%
      dplyr::mutate(date_of_interest = .data$cohort_start_date) %>%
      dplyr::compute()
  } else {
    cohortDb <- cohortDb %>%
      dplyr::rename("person_id" = "subject_id") %>%
      dplyr::mutate(date_of_interest = .data$cohort_end_date) %>%
      dplyr::compute()}

  personDb <- cohortDb %>%
    dplyr::left_join((personDb) %>%
                       dplyr::distinct(), by = "person_id")

  studyPopDbMissingAge <- personDb %>%
    dplyr::filter(is.na(.data$year_of_birth)) %>%
    dplyr::mutate(dob=as.Date(NA))

  studyPopDb <- personDb %>%
    dplyr::filter(!is.na(.data$year_of_birth)) %>%
    dplyr::mutate(year_of_birth1=as.character(as.integer(.data$year_of_birth))) %>%
    dplyr::mutate(month_of_birth1=as.character(as.integer(dplyr::if_else(is.na(.data$month_of_birth), "01" , .data$month_of_birth)))) %>%
    dplyr::mutate(day_of_birth1=as.character(as.integer(dplyr::if_else(is.na(.data$day_of_birth), "01" , .data$day_of_birth)))) %>%
    dplyr::mutate(dob=as.Date(paste0(.data$year_of_birth1, "/",
                                     .data$month_of_birth1, "/",
                                     .data$day_of_birth1))) %>%
    dplyr::select(!c("year_of_birth1", "month_of_birth1", "day_of_birth1")) %>% dplyr::compute()


  studyPop <- studyPopDb %>% dplyr::full_join(studyPopDbMissingAge, by =
                                                c("cohort_start_date", "person_id", "date_of_interest", "gender_concept_id",
                                                  "year_of_birth", "month_of_birth", "day_of_birth", "dob"))

  if ((studyPop %>% dplyr::count() %>% dplyr::pull()) == 0) {
    message("-- No people found for denominator population")
  }

  sqlAge <- sqlGetAge(
    dialect = CDMConnector::dbms(attr(cdm, "dbcon")), "dob", "date_of_interest")

  studyPop %>% dplyr::mutate(age = dbplyr::sql(sqlAge)) %>%
    dplyr::select("person_id", "age", "dob", "date_of_interest") %>%
    dplyr::compute()
}




getPriorHistoryCohortEntry <- function(cdm, cohortTable, cohortIds = NULL) {
  # checks
  error_message <- checkmate::makeAssertCollection()
  checkDbType(db = cdm, messageStore = error_message)
  if (!is.null(cohortIds) &&
      is.numeric(cohortIds)) {
    cohortIds <- as.character(cohortIds)
  }
  checkCharacter(cohortIds, messageStore = error_message, null.ok = TRUE)
  checkmate::check_true(cohortTable %in% names(cdm))
  checkmate::reportAssertions(collection = error_message)

  if (isTRUE(is.na(cohortIds))) {
    cohortIds <- NULL
  }
  if (!is.null(cohortIds)) {
    cdm[[cohortTable]] %>%
      dplyr::filter(.data$cohort_definition_id %in% .env$cohortIds) %>%
      dplyr::left_join(cdm$observation_period, by = c("subject_id" = "person_id")) %>%
      dplyr::filter(.data$cohort_start_date < .data$observation_period_end_date) %>%
      dplyr::filter(.data$cohort_start_date >= .data$observation_period_start_date) %>%
      dplyr::mutate(number_of_days = dbplyr::sql(sqlDiffDays(CDMConnector::dbms(attr(cdm, "dbcon")), "observation_period_start_date", "cohort_start_date"))) %>%
      dplyr::select("cohort_definition_id", "subject_id", "cohort_start_date",
                    "cohort_end_date", "number_of_days")
  } else {
    cdm[[cohortTable]] %>%
      dplyr::left_join(cdm$observation_period, by = c("subject_id" = "person_id")) %>%
      dplyr::filter(.data$cohort_start_date < .data$observation_period_end_date) %>%
      dplyr::filter(.data$cohort_start_date >= .data$observation_period_start_date) %>%
      dplyr::mutate(number_of_days = dbplyr::sql(sqlDiffDays(CDMConnector::dbms(attr(cdm, "dbcon")),
                                                             "observation_period_start_date", "cohort_start_date"))) %>%
      dplyr::select("cohort_definition_id", "subject_id", "cohort_start_date",
                    "cohort_end_date", "number_of_days")
  }
}



checkDbType <- function(db, type = "cdm_reference", messageStore) {
  dbInheritsCheck <- inherits(db, type)
  checkmate::assertTRUE(dbInheritsCheck,
                        add = messageStore)
  if (!isTRUE(dbInheritsCheck)) {
    messageStore$push(glue::glue("- db must be a CDMConnector {type} object"))
  }
}


checkDate <- function(date, messageStore, null.ok = TRUE) {
  checkmate::assert_date(date,
                         add = messageStore,
                         null.ok = null.ok)
}


checkNumeric <- function(input, messageStore, null.ok = TRUE) {
  checkmate::assert_numeric(input,
                            add = messageStore,
                            null.ok = null.ok)
}

checkLogical <- function(input, messageStore, null.ok = TRUE) {
  checkmate::assert_logical(input,
                            add = messageStore,
                            null.ok = null.ok)
}


checkCharacter <- function(input, messageStore, null.ok = TRUE) {
  checkmate::assert_character(input,
                              add = messageStore,
                              null.ok = null.ok)
}


checkTableExists <- function(cdm, tableName, messageStore) {
  table_exists <- inherits(cdm[[tableName]], 'tbl_dbi')
  checkmate::assertTRUE(table_exists, add = messageStore)
  if (!isTRUE(table_exists)) {
    messageStore$push(glue::glue("- {tableName} is not found"))
  }
}


#' Compute the difference in days between 2 variables in a database table.
#'
#' @param dialect the database dialect
#' @param variable1 first date variable
#' @param variable2 second date variable
#'
#' @return the sql to calculate the difference in days between the 2 variables
#' @export
#'
#' @examples
#' \dontrun{
#' date_diff_sql_code <- sqlDiffDays(dialect = "duckdb",
#'                                   variable1 = "date_1",
#'                                   variable2 = "date_2")
#' }
sqlDiffDays <- function(dialect, variable1, variable2) {
  SqlRender::translate(
    SqlRender::render("DATEDIFF(DAY, @variable1, @variable2)",
                      variable1 = variable1,
                      variable2 = variable2),
    targetDialect = dialect)
}


#' Calculate accurate age in different sql
#'
#' @param dialect sql dialects: 'Microsoft Sql Server', 'Oracle',
#' 'PostgreSql', 'Amazon RedShift', 'Apache Impala',
#' 'IBM Netezza', 'Google BigQuery', 'Microsoft PDW', 'Snowflake',
#' @param dob the name of date of birth column in data
#' @param dateOfInterest the name of target date in data
#'
#' @return the sql to calculate age
#' @export
#'
#' @examples
#' \dontrun{
#' sql_age <- sqlGetAge(dialect = "duckdb",
#'                      dob = "dob",
#'                      date_of_interest = "date")
#' }
sqlGetAge <- function(dialect, dob, dateOfInterest) {
  SqlRender::translate(
    SqlRender::render("((YEAR(@date_of_interest) * 10000 + MONTH(@date_of_interest) * 100 +
                      DAY(@date_of_interest)-(YEAR(@dob)* 10000 + MONTH(@dob) * 100 + DAY(@dob))) / 10000)",
                      dob = dob,
                      date_of_interest = dateOfInterest),
    targetDialect = dialect)
}

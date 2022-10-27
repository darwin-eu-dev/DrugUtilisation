library(dplyr)
library(dbplyr)
library(CDMConnector)
library(DBI)
library(here)
library(tictoc)
library(odbc)

server   <- Sys.getenv("darwinDbDatabaseServer")
database <- Sys.getenv("darwinDbDatabase")
user     <- Sys.getenv("darwinDbUser")
password <- Sys.getenv("darwinDbPassword")
port     <- Sys.getenv("darwinDbDatabasePort")
driver   <- Sys.getenv("darwinDbDriver")
dbName   <- Sys.getenv("darwinDbName")

#Sys.setenv(DATABASECONNECTOR_JAR_FOLDER = jdbcDriversFolder)

db <- dbConnect(odbc(),
                Driver   = driver,
                Server   = server,
                Database = database,
                UID      = user,
                PWD      = password,
                Port     = port)

cdm <- cdm_from_con(db,
                    cdm_schema = "cdm_synthea_1M",
                    write_schema = "mcatalasabate")

specifications <- tibble(
  drug_concept_id = c(40162606, 19041324, 1127433, 40231918, 40221901, 19020053),
  ingredient_concept_id = 1125315
  )
studyTime = NULL
drugUtilisationCohortName = "dus_valporate"
imputeDuration = FALSE
imputeDailyDose = FALSE
durationLowerBound = NULL
durationUpperBound = NULL
dailyDoseLowerBound = NULL
dailyDoseUpperBound = NULL
verbose = FALSE


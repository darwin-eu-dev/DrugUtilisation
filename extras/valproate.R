library(CDMConnector)
library(DrugUtilisation)
library(dplyr)

server_dbi <- Sys.getenv("DB_SERVER_DBI_aurum_covid_202106")
user       <- Sys.getenv("DB_USER")
password   <- Sys.getenv("DB_PASSWORD")
port       <- Sys.getenv("DB_PORT")
host       <- Sys.getenv("DB_HOST")

db <- dbConnect(RPostgres::Postgres(),
                dbname = server_dbi,
                port = port,
                host = host,
                user = user,
                password = password)

cdm <- CDMConnector::cdm_from_con(db, cdm_schema = "public")

cohort <- generateDrugUtilisationCohort(
  cdm,
  ingredientConceptId = 745466,
  studyStartDate = as.Date("2010-01-01")
)


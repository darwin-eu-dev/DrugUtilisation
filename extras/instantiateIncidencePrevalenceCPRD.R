library(dplyr)
library(dbplyr)
library(CDMConnector)
library(DBI)
library(here)
library(tictoc)

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

cdm <- cdm_from_con(db,
                    cdm_schema = "public",
                    write_schema = "results")

devtools::load_all()

cdm <- instantiateIncidencePrevalenceCohorts(
  cdm = cdm,
  conceptSetFolder = "extras",
  gapEra = 30,
  overwrite = TRUE
)

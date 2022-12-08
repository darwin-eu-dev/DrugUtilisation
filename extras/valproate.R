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

indicationCohortSet <- CDMConnector::readCohortSet(here::here("inst", "indications_example"))

cdm$dus <- generateDrugUtilisationCohort(
  cdm,
  ingredientConceptId = 745466,
  studyStartDate = as.Date("2010-01-01")
)

attrition_dus <- attr(cdm$dus, "attrition")

indication <- getIndication(
  cdm = cdm,
  targetCohortName = "dus",
  indicationCohortName = "indication_valproate",
  indicationDefinitionSet = indicationCohortSet %>% select("indication_id" = "cohortId", "indication_name" = "cohortName"),
  indicationGap = 7,
  unknownIndicationTables = c("condition_occurrence")
)

stratas <- getStratification(
  cdm,
  "dus",
  sex = "Female",
  ageGroup = list(c(0, 24), c(25, 49), c(50, 74), c(75, 150)),
  )


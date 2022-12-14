library(CDMConnector)
library(DrugUtilisation)
library(dplyr)

server_dbi <- Sys.getenv("DB_SERVER_DBI_aurum_covid_202106")
user <- Sys.getenv("DB_USER")
password <- Sys.getenv("DB_PASSWORD")
port <- Sys.getenv("DB_PORT")
host <- Sys.getenv("DB_HOST")

db <- dbConnect(
  RPostgres::Postgres(),
  dbname = server_dbi,
  port = port,
  host = host,
  user = user,
  password = password
)

cdm <- CDMConnector::cdm_from_con(
  db,
  cdm_schema = "public", write_schema = "results"
)

indicationCohortSet <- CDMConnector::readCohortSet(
  here::here("inst", "indications_example")
)

cdm <- CDMConnector::generateCohortSet(
  cdm = cdm,
  cohortSet = indicationCohortSet,
  cohortTableName = "indications_valproate",
  overwrite = TRUE
)

cdm$dus <- generateDrugUtilisationCohort(
  cdm = cdm,
  ingredientConceptId = 745466,
  studyStartDate = NULL,
  studyEndDate = NULL,
  summariseMode = "AllEras",
  daysPriorHistory = NULL,
  gapEra = 30,
  imputeDuration = "eliminate",
  durationRange = c(1, NA)
)

attrition_dus <- attr(cdm$dus, "attrition")

indicationDefinitionSet <- indicationCohortSet %>%
  select("indication_id" = "cohortId", "indication_name" = "cohortName")
indication <- getIndication(
  cdm = cdm,
  targetCohortName = "dus",
  targetCohortDefinitionIds = 1,
  indicationCohortName = "indications_valproate",
  indicationDefinitionSet = indicationDefinitionSet,
  indicationGap = c(7, NA),
  unknownIndicationTables = c("condition_occurrence")
)

indicationDefinitionSet <- attr(indication, "indicationDefinitionSet")

# summary indications (gap = 7)
indication[["7"]] %>%
  inner_join(indicationDefinitionSet, copy = TRUE) %>%
  group_by(indication_name) %>%
  tally()

# summary indications (gap = Any)
indication[["Any"]] %>%
  inner_join(indicationDefinitionSet, copy = TRUE) %>%
  group_by(indication_name) %>%
  tally()

cdm$stratas <- getStratification(
  cdm = cdm,
  targetCohortName = "dus",
  targetCohortId = 1,
  sex = "Female",
  ageGroup = list(c(0,150),c(0, 24), c(25, 49), c(50, 74), c(75, 150)),
  indexYearGroup = list(c(2010, 2020), c(2010)),
  indicationTable = indication,
  oneStrata = FALSE
)

settingsStratas <- attr(cdm$stratas, "settings")

cdm$dose_table <- getDoseInformation(
  cdm = cdm,
  dusCohortName = "stratas",
  ingredientConceptId = 745466,
  gapEra = 30,
  eraJoinMode = "Zero", # Previous or Subsequent
  overlapMode = "Sum", # Previous, Subsequent, Minimum, Maximum
  sameIndexMode = "Sum", # Minimum, Maximum
  imputeDuration = "eliminate", # median, mean, quantile25, quantile75 or a number
  imputeDailyDose = "eliminate", # median, mean, quantile25, quantile75 or a number
  durationRange = c(1, NA),
  dailyDoseRange = c(0, NA)
)

# to summarise
doseSummary <- summariseDoseIndicationTable(
  cdm = cdm,
  strataCohortName = "stratas",
  cohortId = 1:5,
  doseTableName = "dose_table",
  variables = "initial_daily_dose",
  estimates = c("mean", "q25", "median", "q75", "std"),
  indicationList = NULL,
  minimumCellCounts = 5
)

lscSummary <- largeScaleCharacterization(
  cdm = cdm,
  targetCohortName = "stratas",
  targetCohortId = NULL,
  temporalWindows = list(
    c(NA, -366), c(-365, -91),
    c(-365, -31), c(-90, -1), c(-30, -1),
    c(0, 0), c(1, 30), c(1, 90),
    c(31, 365), c(91, 365), c(366, NA)
  ),
  tablesToCharacterize = c(
    "condition_occurrence", "drug_era",
    "procedure_occurrence", "measurement"
  ),
  overlap = TRUE,
  summarise = TRUE,
  minimumCellCount = 5
)

# Working on the table one function

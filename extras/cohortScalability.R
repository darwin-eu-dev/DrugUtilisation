library(DrugUtilisation)
library(CDMConnector)
library(DBI)
library(dplyr)
library(CodelistGenerator)
library(Capr)

server_dbi<-"cdm_iqvia_pharmetrics_plus_202203"
port<-Sys.getenv("DB_PORT")
host<-Sys.getenv("DB_HOST")
user<-Sys.getenv("DB_USER")
password<-Sys.getenv("DB_PASSWORD")

db <- dbConnect(RPostgres::Postgres(),
                dbname = server_dbi,
                port = port,
                host = host,
                user = user,
                password = password)

# connection details
databaseAcronym <- "PHARMETRICS"
cdmDatabaseSchema <- "public_100k"
resultsDatabaseSchema <- "results"
resultsStem <- "mc_dus_scaling"

cdm <- cdmFromCon(
  con = db,
  cdmSchema = c(schema = cdmDatabaseSchema),
  writeSchema = c(schema = resultsDatabaseSchema, prefix = resultsStem),
  cdmName = databaseAcronym
)

ingredients <- cdm$concept %>%
  filter(standard_concept == "S") %>%
  filter(concept_class_id == "Ingredient") %>%
  pull("concept_name")

repetitions <- 100
number <- c(1, 2, 5, 10)

instantiateDus <- function(cdm, codelist) {
  initialTime <- Sys.time()
  cdm <- generateDrugUtilisationCohortSet(
    cdm = cdm,
    name = "test_dus",
    conceptSet = codelist,
    gapEra = 0,
    priorObservation = 365
  )
  dropTable(cdm, starts_with("test_dus"))
  elapsedTime <- as.numeric(Sys.time() - initialTime)
  return(elapsedTime)
}
instantiateCohort <- function(cdm, codelist) {
  initialTime <- Sys.time()
  concepts <- lapply(seq_along(codelist), function(i) {
    cs(codelist[[i]], name = names(codelist)[[i]])
  })
  observationWindow <- continuousObservation(priorDays = 365)
  qualifiedLimit <- "All"
  persistenceWindow <-  0
  cohortDefinition <- lapply(concepts, function(x) {
    cohort(
      entry = entry(
        drugExposure(x),
        observationWindow = observationWindow,
        qualifiedLimit = qualifiedLimit
      ),
      exit = exit(
        endStrategy = drugExit(
          conceptSet = x,
          persistenceWindow = persistenceWindow
        )
      )
    )
  })
  names(cohortDefinition) <- names(codelist)
  cdm <- generateCohortSet(
    cdm = cdm,
    cohortSet = cohortDefinition,
    name = "test_capr",
    overwrite = TRUE
  )
  dropTable(cdm, starts_with("test_capr"))
  elapsedTime <- as.numeric(Sys.time() - initialTime)
  return(elapsedTime)
}

timings <- dplyr::tibble(
  fun = character(),
  number_ingredients = numeric(),
  repetition_id = numeric(),
  time = numeric()
)

for (num in number) {
  for (id in seq_len(repetitions)) {
    codes <- getDrugIngredientCodes(cdm = cdm, name = sample(ingredients, num))
    names(codes) <- paste0("cohort", seq_along(codes))
    t1 <- instantiateDus(cdm, codes)
    t2 <- instantiateCohort(cdm, codes)
    timings <- timings %>%
      union_all(tibble(
        fun = c("dus", "atlas"), number_ingredients = num, repetition_id = id,
        time = c(t1, t2)
      ))
    print(paste0("Number ingredients: ", num, ". Repetition ", id, " of ", repetitions))
  }
}

timings %>%
  group_by(fun, number_ingredients) %>%
  summarise(
    mean_time = mean(time), median_time = median(time), sd_time = sd(time),
    q25_time = quantile(time, 0.25), q75_time = quantile(time, 0.75),
    .groups = "drop"
  ) %>%
  ggplot(aes(x = number_ingredients, y = median_time, ymax = q75_time, ymin = q25_time, col = fun)) +
  geom_line() +
  geom_point() +
  geom_errorbar(width = .2, position = position_dodge(0.05)) +
  ylim(c(0, 35))

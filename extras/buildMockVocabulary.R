library(DBI)
library(CDMConnector)
library(dplyr)
library(dbplyr)

# Connection details
server_dbi <- "cdm_gold_202201"
user <- Sys.getenv("DB_USER")
password <- Sys.getenv("DB_PASSWORD")
port <- Sys.getenv("DB_PORT")
host <- Sys.getenv("DB_HOST")
dbms <- "postgresql"

db <- dbConnect(
  RPostgres::Postgres(),
  dbname = server_dbi,
  port = port,
  host = host,
  user = user,
  password = password
)

cdmDatabaseSchema <- "public"
resultsDatabaseSchema <- "results"

cdm <- cdmFromCon(
  con = db,
  cdmSchema = cdmDatabaseSchema,
  writeSchema = resultsDatabaseSchema
)

# keyWord <- "influenza"
# x <- cdm[["concept"]] |>
#   filter(grepl(keyWord, concept_name, ignore.case = T)) |>
#   filter(standard_concept == "S")
# x |>
#   filter(domain_id == "Condition") |>
#   print(n = 200)
# x |>
#   filter(domain_id == "Measurement") |>
#   print(n = 200)
# x |>
#   filter(domain_id == "Observation") |>
#   print(n = 200)
# x |>
#   filter(domain_id == "Procedure") |>
#   print(n = 200)
# cdm[["concept_ancestor"]] |>
#   filter(ancestor_concept_id == 1516976) |>
#   left_join(cdm[["concept"]] |> rename(descendant_concept_id = concept_id)) |>
#   select("concept_name", "descendant_concept_id") |>
#   filter(!grepl("Box", concept_name)) |>
#   arrange(descendant_concept_id) |>
#   print(n = 100)

listCodes <- c(
  378253, 3655501, 4141052, 3655500, 317009, 4062501, 3661634, 4214676, 4266367,
  761948, 432526, 4313306, 43135274, 1125360, 2905077, 1539462, 1539463,
  1503327, 1503328, 1516978, 1516980, 8507, 8532, 44814724, 38000177, 32020,
  9202, 1125315, 1539403, 1503297, 1516976, 8718, 9655, 9551, 8576, 8587, 8505
)

# prevList <- 0
# while (prevList != length(listCodes)) {
#   prevList <- length(listCodes)
#   listCodes <- c(
#     cdm[["concept_ancestor"]] |>
#       filter(descendant_concept_id %in% listCodes) |>
#       pull(ancestor_concept_id),
#     listCodes
#   ) |>
#     unique()
#   print(prevList)
# }

concept <- cdm[["concept"]] |>
  filter(concept_id %in% listCodes) |>
  collect()

concept_ancestor <- cdm[["concept_ancestor"]] |>
  filter(ancestor_concept_id %in% listCodes & descendant_concept_id %in% listCodes) |>
  collect()

drug_strength <- cdm[["drug_strength"]] |>
  filter(drug_concept_id %in% listCodes & ingredient_concept_id %in% listCodes) |>
  collect()

write_csv(concept, here("extras", "concept.csv"))
write_csv(concept_ancestor, here("extras", "concept_ancestor.csv"))
write_csv(drug_strength, here("extras", "drug_strength.csv"))

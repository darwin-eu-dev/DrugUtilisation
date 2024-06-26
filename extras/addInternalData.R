library(here)
library(readr)
library(usethis)

domainInformation <- read_csv(
  here("extras", "domain_information.csv"), show_col_types = FALSE
)

patternfile <- read_csv(
  here::here("extras", "pattern_drug_strength.csv"),
  col_types = list(
    pattern_id = "numeric",
    amount = "character",
    amount_unit = "character",
    amount_unit_concept_id = "numeric",
    numerator = "character",
    numerator_unit = "character",
    denominator = "character",
    denominator_unit = "character",
    numerator_unit_concept_id = "numeric",
    denominator_unit_concept_id = "numeric",
    number_concepts = "numeric",
    number_ingredients = "numeric",
    valid = "logical",
    pattern_name = "character",
    unit = "character"
  )
)

mockDrugStrength <- read_csv(
  here("extras", "drug_strength.csv"), show_col_types = FALSE
)
mockConcept <- read_csv(
  here("extras", "concept.csv"), show_col_types = FALSE
)
mockConceptAncestor <- read_csv(
  here("extras", "concept_ancestor.csv"), show_col_types = FALSE
)

namesTable <- readr::read_csv(
  here("extras", "namesTable.csv"),
  col_types = list(
    table_name = "c",
    start_date_name = "c",
    end_date_name = "c",
    concept_id_name = "c",
    source_concept_id_name = "c"
  )
)

use_data(
  patternfile, domainInformation, mockDrugStrength, mockConcept, namesTable,
  mockConceptAncestor, internal = TRUE, overwrite = TRUE
)

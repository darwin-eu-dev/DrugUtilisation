library(here)
library(readr)
library(usethis)

domainInformation <- read.csv(here("extras", "domain_information.csv"))

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

use_data(patternfile, domainInformation, internal = TRUE, overwrite = TRUE)

#x <- list(drugSummary = list(col = c("a", "gahd", "jskd"), colType = c("chr", "chr", "chr"), fun = c("CDMConnector::plotAttrition")))
#use_data(x, overwrite = TRUE)


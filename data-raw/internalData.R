# Copyright 2022 DARWIN EU (C)
#
# This file is part of DrugUtilisation
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

library(dplyr)

# add the mock vocabulary data
mockDrugStrength <- readr::read_csv(
  here::here("data-raw", "drug_strength.csv"), show_col_types = FALSE
)
mockConcept <- readr::read_csv(
  here::here("data-raw", "concept.csv"), show_col_types = FALSE
)
mockConceptAncestor <- readr::read_csv(
  here::here("data-raw", "concept_ancestor.csv"), show_col_types = FALSE
)

# add the information related to the different domains
domainInformation <- readr::read_csv(
  here::here("data-raw", "domain_information.csv"), show_col_types = FALSE
)

# add the current pattern file (of patterns for which we can calculate dose)
patternfile <- readr::read_csv(
  here::here("data-raw", "pattern_drug_strength.csv"),
  col_types = list(
    pattern_id = "numeric",
    amount_numeric = "numeric",
    amount_unit = "character",
    amount_unit_concept_id = "numeric",
    numerator_numeric = "numeric",
    numerator_unit = "character",
    numerator_unit_concept_id = "numeric",
    denominator_numeric = "numeric",
    denominator_unit = "character",
    denominator_unit_concept_id = "numeric",
    valid = "logical",
    pattern_name = "character",
    unit = "character"
  )
) %>%
  dplyr::select(-c(
    "valid", "pattern_name", "amount_unit", "numerator_unit", "denominator_unit"
  ))

formulafile <- read_csv(
  here::here("data-raw", "pattern_assessment_for_dose_final.csv"),
  col_types = list(
    numerator = "character",
    numerator_unit = "character",
    numerator_unit_concept_id = "numeric",
    denominator = "character",
    denominator_unit = "character",
    denominator_unit_concept_id = "numeric",
    pattern_meaning = "character",
    formula_id = "numeric",
    pattern_meaning = "character",
    route = "character",
    unit = "character"
  )
)

decisiontable <- read_csv(
  here::here("data-raw", "doseform_final.csv"),
  comment = "",
  col_types = list(
    route = "character",
    source_concept_id = "numeric",
    source_code_id = "character",
    source_name = "character",
    class = "character",
    concept = "character",
    domain = "character",
    validity = "character",
    vocabulary = "character"
  )
)

# add all rows in patternfile for the "any" dose patterns with all the possibilities
all_routes <- decisiontable %>%
  dplyr::select(route) %>%
  dplyr::distinct() %>%
  dplyr::pull()

formulafile_add <- formulafile %>%
  dplyr::filter(route == "any")

for(route_n in all_routes) {
  formulafile_add <- formulafile_add %>%
    dplyr::union_all(
      formulafile %>%
        dplyr::filter(route == "any") %>%
        dplyr::mutate(route = route_n)
    )
}

formulafile <- formulafile %>%
  dplyr::union_all(formulafile_add) %>%
  dplyr::mutate(route = dplyr::if_else(
    route == "any", as.character(NA), route
  ))


usethis::use_data(
  mockDrugStrength, mockConcept, mockConceptAncestor, domainInformation,
  patternfile, formulafile, decisiontable, internal = TRUE, overwrite = TRUE
)


# Copyright 2024 DARWIN EU (C)
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

formulas <- readr::read_csv(
  here::here("data-raw", "pattern_assessment_for_dose_final.csv"),
  col_types = list(
    pattern_id = "numeric",
    amount = "character",
    amount_unit = "character",
    amount_unit_concept_id = "numeric",
    numerator = "character",
    numerator_unit = "character",
    numerator_unit_concept_id = "numeric",
    denominator = "character",
    denominator_unit = "character",
    denominator_unit_concept_id = "numeric",
    formula_name = "character",
    unit = "character"
  )
) %>%
  dplyr::mutate(
    amount_numeric = dplyr::if_else(!is.na(.data$amount), 1, 0),
    numerator_numeric = dplyr::if_else(!is.na(.data$numerator), 1, 0),
    denominator_numeric = dplyr::if_else(!is.na(.data$denominator), 1, 0)
  ) |>
  dplyr::select(-c("amount", "numerator", "denominator"))


routes <- readr::read_csv(
  here::here("data-raw", "dose_form_final.csv"),
  comment = "",
  col_types = list(
    route = "character",
    id = "numeric"
  )
) %>%
  dplyr::select("dose_form_concept_id" = "id", "route")

patternsWithFormula <- formulas %>%
  dplyr::mutate(
    amount = dplyr::if_else(.data$amount_numeric == 1, "number", NA_character_),
    numerator = dplyr::if_else(.data$numerator_numeric == 1, "number", NA_character_),
    denominator = dplyr::if_else(.data$denominator_numeric == 1, "number", NA_character_)
  ) %>%
  dplyr::select(
    "pattern_id", "amount", "amount_unit", "numerator", "numerator_unit",
    "denominator", "denominator_unit", "formula_name"
  ) %>%
  dplyr::left_join(
    dplyr::tibble(
      formula_name = c("concentration formulation", "fixed amount formulation", "time based with denominator", "time based no denominator"),
      formula = c("quantity * numerator / days exposed", "quantity * amount / days exposed", "if (denominator>24) {numerator * 24 / denominator} else {numerator}", "24 * numerator")
    ),
    by = "formula_name"
  )

usethis::use_data(patternsWithFormula, internal = FALSE, overwrite = TRUE)

patterns <- formulas %>%
  dplyr::select(
    "pattern_id", "amount_numeric", "amount_unit_concept_id",
    "numerator_numeric", "numerator_unit_concept_id", "denominator_numeric",
    "denominator_unit_concept_id", "formula_name", "unit"
  )

usethis::use_data(
  mockDrugStrength, mockConcept, mockConceptAncestor, domainInformation,
  patterns, routes, internal = TRUE, overwrite = TRUE
)

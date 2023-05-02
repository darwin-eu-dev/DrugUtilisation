# Copyright 2022 DARWIN EU (C)
#
# This file is part of DrugUtilizationCharacteristics
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

#' Put concept ids from all cohorts of interest in the required list format
#'
#' @param conceptList table with all the concept ids read, with their respective
#' cohort, exclusion and descendant information
#'
#' @return list of concept_ids and respective cohort_definition_ids of interest
#' @noRd
formatConceptList <- function(cdm, conceptList) {
  conceptList <- conceptList %>%
    dplyr::filter(.data$include_descendants == FALSE) %>%
    dplyr::union(
      cdm[["concept_ancestor"]] %>%
        dplyr::select(
          "concept_id" = "ancestor_concept_id",
          "descendant_concept_id"
        ) %>%
        dplyr::inner_join(
          conceptList %>%
            dplyr::filter(.data$include_descendants == TRUE),
          copy = TRUE,
          by = "concept_id"
        ) %>%
        dplyr::select(-"concept_id") %>%
        dplyr::rename("concept_id" = "descendant_concept_id") %>%
        dplyr::collect()
    ) %>%
    dplyr::select(-"include_descendants") %>%
    dplyr::rename("drug_concept_id" = "concept_id")
  # eliminate the ones that is_excluded = TRUE
  conceptList <- conceptList %>%
    dplyr::filter(.data$is_excluded == FALSE) %>%
    dplyr::select("cohort_name", "drug_concept_id") %>%
    dplyr::anti_join(
      conceptList %>%
        dplyr::filter(.data$is_excluded == TRUE),
      by = c("cohort_name","drug_concept_id")
    )
  conceptFinalList <- list()
  for(n in conceptList$cohort_name %>% unique()) {
    conceptFinalList[[n]] <- conceptList %>% dplyr::filter(cohort_name == n) %>% dplyr::select(drug_concept_id) %>% dplyr::pull()
  }
  return(conceptFinalList)
  }

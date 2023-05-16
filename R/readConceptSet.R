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

#' Get concept ids and information from a list of json files provided
#'
#' @param conceptSets paths and names of all jsons to be read
#'
#' @return raw table with all the concept_ids read, and their information
#' @noRd
readConceptSet <- function(conceptSets) {
  names <- c("CONCEPT_CLASS_ID",
             "CONCEPT_CODE",
             "CONCEPT_ID",
             "CONCEPT_NAME",
             "DOMAIN_ID",
             "INVALID_REASON",
             "INVALID_REASON_CAPTION",
             "STANDARD_CONCEPT",
             "STANDARD_CONCEPT_CAPTION",
             "VOCABULARY_ID",
             "isExcluded",
             "includeMapped",
             "includeDescendants")

  for (k in 1:nrow(conceptSets)) {
    conceptSetName <- conceptSets$concept_set_name[k]
    conceptSet <- RJSONIO::fromJSON(conceptSets$concept_set_path[k])
    conceptSet <- lapply(conceptSet$items, function(x) {
      x <- append(x, x[["concept"]])
      x[["concept"]] <- NULL
      return(x)
    }) %>%
      dplyr::bind_rows() %>%
      dplyr::mutate(
        cohort_name = conceptSetName
      )
    # Add columns missing from the read file with default values
    conceptSet[setdiff(names, names(conceptSet))] <- as.character(NA)
    conceptSet <- conceptSet %>%
      dplyr::mutate(isExcluded = ifelse(is.na(.data$isExcluded), FALSE, .data$isExcluded),
                    includeMapped = ifelse(is.na(.data$includeMapped), FALSE, .data$includeMapped),
                    includeDescendants = ifelse(is.na(.data$includeDescendants), FALSE, .data$includeDescendants),)

    if (k == 1) {
      conceptList <- conceptSet
    } else {
      conceptList <- rbind(conceptList, conceptSet)
    }
  }
  conceptList <- conceptList %>%
    dplyr::select(
      "cohort_name",
      "concept_id" = "CONCEPT_ID",
      "is_excluded" = "isExcluded",
      "include_descendants" = "includeDescendants"
    )
  return(conceptList)
  }

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

#' Get concept ids from a provided path to json files
#'
#' @param path path to a file or folder containing jsons to be read
#' @param cdm object created with CDMConnector::cdm_from_con.
#'
#' @return list of concept_ids and respective concept_ids of interest
#' @export
#'
#' @examples
#'
readConceptList <- function(path, cdm) {
  # check path
  conceptSets <- checkPath(path)
  # check cdm
  checkCdm(cdm)

  # first part: read jsons
  if (!is.null(path)) {
    tryCatch(
      expr = conceptList <- readConceptSet(conceptSets),
      error = function(e) {
        stop("The json file is not a properly formated OMOP concept set.")
      }
    )
  }

  # second part: produce output list
  conceptFinalList <- formatConceptList(cdm, conceptList)

  # return list
  return(conceptFinalList)
}

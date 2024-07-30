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

#' Get concept ids from a provided path to json files
#'
#' `r lifecycle::badge("deprecated")`
#'
#' @param path path to a file or folder containing jsons to be read
#' @param cdm A cdm reference created with CDMConnector
#'
#' @return list of concept_ids and respective concept_ids of interest
#' @export
#'
#' @examples
#' \donttest{
#' library(DrugUtilisation)
#'
#' cdm <- mockDrugUtilisation()
#'
#' codelist <- readConceptList(
#'   path = system.file("concepts", package = "DrugUtilisation"), cdm = cdm
#' )
#' }
#'
readConceptList <- function(path, cdm) {
  lifecycle::deprecate_warn(
    when = "0.5.0",
    what = "readConceptList()",
    with = "CodelistGenerator::codesFromConceptSet()"
  )
  CodelistGenerator::codesFromConceptSet(path = path, cdm = cdm)
}

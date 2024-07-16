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

#' This function is used to summarise the indication table over multiple
#' cohorts.
#'
#' @param cohort Cohort with indications and strata.
#' @param strata Stratification list.
#' @param indicationCohortName Name of indication cohort table
#' @param indicationCohortId target cohort Id to add indication
#' @param indicationWindow time window of interests
#' @param unknownIndicationTable Tables to search unknown indications
#' @param indexDate Date of the indication
#'
#' @return A summarise result object
#'
#' @export
#'
#' @examples
#' \donttest{
#' library(DrugUtilisation)
#' library(PatientProfiles)
#' library(CDMConnector)
#'
#' cdm <- mockDrugUtilisation()
#' indications <- list("headache" = 378253, "asthma" = 317009)
#' cdm <- generateConceptCohortSet(cdm, indications, "indication_cohorts")
#' acetaminophen <- getDrugIngredientCodes(cdm, "acetaminophen")
#' cdm <- generateDrugUtilisationCohortSet(cdm, "drug_cohort", acetaminophen)
#' cdm[["drug_cohort"]] <- cdm[["drug_cohort"]] |>
#'   addIndication(
#'     indicationCohortName = "indication_cohorts",
#'     indicationWindow = list(c(0,0),c(-30,0),c(-365,0))
#'   )
#'
#' }
#'
summariseIndication <- function(cohort,
                                strata = list(),
                                indicationCohortName,
                                indicationCohortId = NULL,
                                indicationWindow = list(c(0,0)),
                                unknownIndicationTable = NULL) {
    # initialChecks
  cdm <- omopgenerics::cdmReference(cohort)
  checkInputs(cohort = cohort, cdm = cdm, strata = strata)

  cohort <-
    cohort |> DrugUtilisation::addIndication(
      indicationCohortName = indicationCohortName,
      indicationCohortId = indicationCohortId,
      indicationWindow = indicationWindow,
      unknownIndicationTable = unknownIndicationTable,
      name = NULL
    )
  indicationVariables <- indicationColumns(cohort)

  # update cohort_names
  cohort <- cohort |> PatientProfiles::addCohortName() |> dplyr::collect()

  # summarise indication columns
  result <- PatientProfiles::summariseResult(
    table = cohort, group = list("cohort_name"),
    includeOverallGroup = FALSE, includeOverallStrata = TRUE,
    strata = strata, variables = indicationVariables,
    estimates = c("count", "percentage")
  ) |>
    PatientProfiles::addCdmName(cdm = cdm) |>
    dplyr::mutate(
      variable_name = dplyr::if_else(
        substr(.data$variable_name, 1, 11) == "indication_",
        lapply(strsplit(.data$variable_name, "_"), function(x) {
          x <- paste0(x[1:min(4, length(x))], collapse = "_")
          x <- indicationColumnName(x)
        }) |>
          unlist(),
        .data$variable_name
      )
    )

  result <- result |>
    omopgenerics::newSummarisedResult(settings = dplyr::tibble(
      result_id = unique(result$result_id),
      result_type = "summarised_indication",
      package_name = "DrugUtilisation",
      package_version = as.character(utils::packageVersion("DrugUtilisation"))
    ))

  return(result)
}

#' Obtain automatically the indication columns
#'
#' @param x Tibble
#'
#' @return Name of the indication columns
#'
#' @noRd
#'
indicationColumns <- function(x) {
  names <- colnames(x)[substr(colnames(x), 1, 11) == "indication_"]
  return(names)
}

indicationColumnName <- function(x) {
  x[x == "indication_0_to_0"] <- "Indication on index date"
  x[x == "indication_minf_to_0"] <- "Indication any time prior"
  x[x == "indication_0_to_inf"] <- "Indication any time after"
  id <- substr(x, 1, 11) == "indication_"
  y <- x[id]
  x[id] <- gsub("_"," ",paste(
    "Indication time window", substr(x[id], 12, nchar(x[id])), "days")
  )
  return(x)
}


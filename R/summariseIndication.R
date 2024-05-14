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

#' This function is used to summarise the indication table over multiple
#' cohorts.
#'
#' @param cohort Cohort with indications and strata
#' @param cdm cdm_reference created by CDMConnector
#' @param strata Stratification list
#' @param minCellCount Minimum counts that a group can have. Cohorts with
#' less counts than this value are obscured.
#'
#' @return A Tibble with 4 columns: cohort_definition_id, variable, estimate and
#' value. There will be one row for each cohort, variable and cohort
#' combination.
#'
#' @export
#'
#' @examples
#' \donttest{
#' library(DrugUtilisation)
#' library(PatientProfiles)
#' library(CodelistGenerator)
#'
#' cdm <- mockDrugUtilisation()
#' indications <- list("headache" = 378253, "asthma" = 317009)
#' cdm <- generateConceptCohortSet(cdm, indications, "indication_cohorts")
#' acetaminophen <- getDrugIngredientCodes(cdm, "acetaminophen")
#' cdm <- generateDrugUtilisationCohortSet(cdm, "drug_cohort", acetaminophen)
#' cdm[["drug_cohort"]] <- cdm[["drug_cohort"]] %>%
#'   addIndication(
#'     indicationCohortName = "indication_cohorts",
#'     indicationGap = c(0, 30, 365)
#'   )
#'
#' summariseIndication(cdm[["drug_cohort"]])
#'
#' cdm[["drug_cohort"]] <- cdm[["drug_cohort"]] %>%
#'   addAge(ageGroup = list("<40" = c(0, 39), ">=40" = c(40, 150))) %>%
#'   addSex()
#'
#' summariseIndication(
#'   cdm[["drug_cohort"]], strata = list(
#'     "age_group" = "age_group", "age_group and sex" = c("age_group", "sex")
#'   )
#' )
#' }
#'
summariseIndication <- function(cohort,
                                cdm = lifecycle::deprecated(),
                                strata = list(),
                                minCellCount = lifecycle::deprecated()) {
  if (lifecycle::is_present()) {
    lifecycle::deprecate_soft(
      when = "0.5.0", what = "summariseIndication(minCellCount = )"
    )
  }
  if (lifecycle::is_present()) {
    lifecycle::deprecate_soft(
      when = "0.5.0", what = "summariseIndication(cdm = )"
    )
  }
  cdm <- omopgenerics::cdmReference(cohort)
  # initialChecks
  checkInputs(cohort = cohort, cdm = cdm, strata = strata)

  indicationVariables <- indicationColumns(cohort)

  # update cohort_names
  cohort <- cohort %>% PatientProfiles::addCohortName() %>% dplyr::collect()

  # summarise indication columns
  result <- PatientProfiles::summariseResult(
    table = cohort, group = list("cohort_name"),
    includeOverallGroup = FALSE, includeOverallStrata = TRUE,
    strata = strata, variables = indicationVariables,
    estimates = c("count", "percentage")
  ) %>%
    PatientProfiles::addCdmName(cdm = cdm) |>
    dplyr::mutate(
      variable_level = dplyr::if_else(
        substr(.data$variable_name, 1, 15) == "indication_gap_",
        lapply(strsplit(.data$variable_name, "_"), function(x) {
          if (length(x) > 3) {
            x <- paste0(x[-c(1:3)], collapse = "_")
            x <- paste0(toupper(substr(x, 1, 1)), substr(x, 2, nchar(x)))
          } else {
            x<- as.character(NA)
          }
          return(x)
        }) %>%
          unlist(),
        as.character(NA)
      ),
      variable_name = dplyr::if_else(
        substr(.data$variable_name, 1, 15) == "indication_gap_",
        lapply(strsplit(.data$variable_name, "_"), function(x) {
          x <- paste0(x[1:min(3, length(x))], collapse = "_")
          x <- indicationColumnName(x)
        }) %>%
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
    )) |>
    omopgenerics::suppress(minCellCount = 5)

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
  names <- colnames(x)[substr(colnames(x), 1, 15) == "indication_gap_"]
  return(names)
}

indicationColumnName <- function(x) {
  x[x == "indication_gap_0"] <- "Indication on index date"
  x[x == "indication_gap_inf"] <- "Indication any time prior"
  id <- substr(x, 1, 15) == "indication_gap_"
  y <- x[id]
  x[id] <- paste(
    "Indication during prior", substr(x[id], 16, nchar(x[id])), "days"
  )
  return(x)
}

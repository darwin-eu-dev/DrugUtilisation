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

#' Explain function
#'
#' @param cdm cdm
#' @param personSummaryName personSummaryName
#' @param genderAggregation genderAggregation
#' @param ageGroupsAgregation ageGroupsAgregation
#' @param ageGroups ageGroups the input is a list of user define age group
#' @param indexYearAggregation indexYearAggregation
#' @param indexYearMonthAggregation indexYearMonthAggregation
#' @param initialDoseAggregation initialDoseAggregation
#' @param meanDoseAggregation meanDoseAggregation
#' @param indicationAggregation indicationAggregation
#' @param indicationTableName indicationTableName
#' @param aggregationTableName aggregationTableName
#' @param verbose verbose
#'
#' @return
#' @export
#'
#' @examples
computeAggregation <- function(cdm,
                               personSummaryName,
                               genderAggregation = TRUE,
                               ageGroupsAgregation = TRUE,
                               ageGroups = list(c(0,19),c(20,39),c(40,59),c(60,79),c(80,150)) ,
                               indexYearAggregation = TRUE,
                               indexYearMonthAggregation = TRUE,
                               initialDoseAggregation = TRUE,
                               meanDoseAggregation = TRUE,
                               cohortid = NULL,
                              ## indicationAggregation,
                              ## indicationTableName,
                               aggregationTableName = "drug_utilisation_aggregation_table",
                              verbose) {
  #basic checks
  errorMessage <- checkmate::makeAssertCollection()
  checkmate::checkDbType(cdm, type = "cdm_reference", errorMessage)
  checkmate::checkLogical(genderAggregation, errorMessage, null.ok = FALSE)
  checkmate::checkLogical(ageGroupsAgregation, errorMessage, null.ok = FALSE)
  checkmate::checkLogical(indexYearAggregation, errorMessage, null.ok = FALSE)
  checkmate::checkLogical(indexYearMonthAggregation, errorMessage, null.ok = FALSE)
  checkmate::checkLogical(initialDoseAggregation, errorMessage, null.ok = FALSE)
  checkmate::checkLogical(meanDoseAggregation, errorMessage, null.ok = FALSE)
  checkmate::checkLogical(verbose, errorMessage, null.ok = FALSE)
  checkmate::checkCharacter(personSummaryName, errorMessage, null.ok = FALSE)
  checkmate::checkCharacter(aggregationTableName, errorMessage, null.ok = FALSE)
  checkmate::checkList(ageGroups, errorMessage, null.ok = TRUE)
  checkmate::checkCount(cohortid, errorMessage, null.ok = TRUE)
  checkmate::reportAssertions(collection = errorMessage)

  #check table inside cdm
  cdm_inherits_check <- inherits(cdm, "cdm_reference")
  checkmate::assertTRUE(cdm_inherits_check,
                        add = error_message)
  if (!isTRUE(cdm_inherits_check)) {
    error_message$push("- cdm must be a CDMConnector CDM reference object")
  }

  #check if personsummaryname table exist
  cdm_personSummaryName_exists <-
    inherits(cdm$personSummaryName, "tbl_dbi")
  checkmate::assertTRUE(cdm_personSummaryName_exists, add = error_message)
  if (!isTRUE(cdm_personSummaryName_exists)) {
    error_message$push("- person summary table is not found")
  }
  #check if patient table exist
  if (genderAggregation == TRUE | ageGroupsAgregation == TRUE) {
    cdm_person_exists <- inherits(cdm$person, "tbl_dbi")
    checkmate::assertTRUE(cdm_person_exists, add = error_message)
    if (!isTRUE(cdm_person_exists)) {
      error_message$push("- table `person` is not found")
    }
  }

  #check if aggregationTableName table exist in database

  cdm_aggregationTableName_exists <-
    inherits(cdm$aggregationTableName, "tbl_dbi")
  checkmate::assertTRUE(cdm_aggregationTableName_exists, add = error_message)
  if (!isFALSE(cdm_aggregationTableName_exists)) {
    error_message$push("output table already exist in database please specified a different name")
  }





  aggregationTable <- cdm[[personSummaryName]] %>%
    ##  dplyr::mutate(aggregation = "All") %>%
    ##  dplyr::mutate(value = as.character(NA)) %>%
    dplyr::select("subject_id", "cohort_start_date", "cohort_end_date")


  if (genderAggregation == TRUE) {
    #get gender from cohortprofile
    get_gender <-
      CohortProfiles::getSex(cdm = cdm,
                             cohortId = cohortid,
                             cohortTable = personSummaryName)


    #join gender to input table
    get_gender <-
      cdm[[personSummaryName]] %>% dplyr::left_join(get_gender %>% dplyr::select("subject_id", "sex"),
                                                    by = c("subject_id" = "person_id"))


    aggregationTable <- aggregationTable %>%
      dplyr::left_join(
        get_gender %>%
          ##dplyr::mutate(aggregation = "Gender") %>%
          dplyr::mutate(gender = .data$sex) %>%
          dplyr::select("subject_id", "gender")
      )
  }

  if (ageGroupsAgregation == TRUE) {
    #get age from cohortprofile
    get_age <-
      CohortProfiles::getAge(cdm = cdm,
                             cohortId = cohortid,
                             cohortTable = personSummaryName)


    #join age to input table
    get_age <-
      cdm[[personSummaryName]] %>% dplyr::left_join(get_age %>% dplyr::select("subject_id", "age"),
                                                    by = c("subject_id" = "person_id"))

    aggregationTableAge <- lapply(ageGroups, function(x) {
      groupName <- paste0(x[1], ";", x[2])
      return(
        get_age %>%
          dplyr::filter(.data$age >= .env$x[1]) %>%
          dplyr::filter(.data$age <= .env$x[2]) %>%
          ##dplyr::mutate(aggregation = "Age groups") %>%
          dplyr::mutate(age_groups = groupName) %>%
          dplyr::select("subject_id", "age_groups") %>%
          dplyr::compute()
      )
    })

    aggregationTable <- aggregationTable %>%
      dplyr::left_join(aggregationTableAge %>% purrr::reduce(union)) %>%
      dplyr::compute()
  }

  if (indexYearAggregation == TRUE) {
    aggregationTable <- aggregationTable %>%
      dplyr::left_join(
        cdm[[personSummaryName]] %>%
          ## dplyr::mutate(aggregation = "Index year") %>%
          dplyr::mutate(index_year = as.character(
            base::format(.data$cohort_start_date, format = "%Y")
          )) %>%
          dplyr::select("subject_id", "index_year")
      )
  }

  if (indexYearMonthAggregation == TRUE) {
    aggregationTable <- aggregationTable %>%
      dplyr::left_join(
        cdm[[personSummaryName]] %>%
          ## dplyr::mutate(aggregation = "Index month-year") %>%
          dplyr::mutate(index_month_year = as.character(
            paste0(
              base::format(.data$cohort_start_date, format = "%m"),
              "_",
              base::format(.data$cohort_start_date, format = "%Y")
            )
          )) %>%
          dplyr::select("subject_id", "index_month_year")
      )
  }

  if (initialDoseAggregation == TRUE) {
    aggregationTable <- aggregationTable %>%
      dplyr::left_join(
        cdm[[personSummaryName]] %>%
          ##dplyr::mutate(aggregation = "Initial dose") %>%
          dplyr::mutate(initial_dose = as.character(round(
            .data$initial_dose
          ))) %>%
          dplyr::select("subject_id", "initial_dose")
      )
  }

  if (meanDoseAggregation == TRUE) {
    aggregationTable <- aggregationTable %>%
      dplyr::left_join(
        cdm[[personSummaryName]] %>%
          ##  dplyr::mutate(aggregation = "Mean dose") %>%
          dplyr::mutate(mean_dose = as.character(
            round(.data$cumulative_dose / .data$exposed_days)
          )) %>%
          dplyr::select("subject_id", "mean_dose")
      )
  }

  # if (indicationAggregation == TRUE){
  #   aggregationTable <- aggregationTable %>%
  #     dplyr::union(
  #       cdm[[indicationTableName]] %>%
  #         dplyr::mutate(aggregation = "Indication") %>%
  #         dplyr::rename("value" = "indication_name") %>%
  #         dplyr::select("subject_id", "aggregation", "value")
  #     ) %>%
  #     dplyr::compute()

  #filter minimum counts
  cdm[[aggregationTableName]] <- aggregationTable %>%
    dplyr::compute()

  return(cdm)
}

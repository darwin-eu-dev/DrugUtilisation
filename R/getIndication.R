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

#' Get indication for a target cohort
#'
#' @param cdm object created with CDMConnector::cdm_from_con.
#' It must contain the targetCohortName and indicationCohortName table in it.
#' @param targetCohortName cohort table in the cdm contain list of target cohort for patients
#' @param targetCohortDefinitionId cohort definition ids to include to generate indication
#' @param indicationCohortName indication table that contain list of indication
#' @param indicationDefinitionSet definition of indication of interest,
#' this function will only get indication for indication contain in this set
#' @param indicationGap the maximum gap between the cohort start date and indication start date
#' @param unknownIndicationTable tables to get extra indication from the default is to include
#' condition_occurence and obersvation table in the cdm
#' @param verbose Whether the code should print the process.
#'
#'
#' @return
#' @export
#'
#' @examples
getIndication <- function(cdm,
                          targetCohortName,
                          targetCohortDefinitionId,
                          indicationCohortName,
                          indicationDefinitionSet,
                          indicationGap,
                          unknownIndicationTable = c("condition_occurrence", "observation"),
                          verbose = FALSE) {

  get_start_date <- list(
    "observation_period" = "observation_period_start_date",
    "visit_occurrence" = "visit_start_date",
    "condition_occurrence" = "condition_start_date",
    "drug_exposure" = "drug_exposure_start_date",
    "procedure_occurrence" = "procedure_date",
    "device_exposure" = "device_exposure_start_date",
    "measurement" = "measurement_date",
    "observation" = "observation_date",
    "drug_era" = "drug_era_start_date",
    "condition_era" = "condition_era_start_date",
    "specimen" = "specimen_date"
  )

  ## checks
  messageStore <- checkmate::makeAssertCollection()
  # check for cdm have the correct classes
  checkmate::assertClass(cdm, classes = "cdm_reference", add = messageStore)

  # check targetCohortName type is character of length 1
  checkmate::assertCharacter(targetCohortName,
    len = 1,
    add = messageStore
  )

  # check targetCohortName table exist
  cdm_targetCohortName_exists <-
    inherits(cdm[[targetCohortName]], "tbl_dbi")


  checkmate::assertTRUE(cdm_targetCohortName_exists, add = messageStore)
  if (!isTRUE(cdm_targetCohortName_exists)) {
    messageStore$push("- table `targetCohortName` is not found")
  }

  #check targetCohortName is not empty
  targetCohortNameEmpty <- cdm[[targetCohortName]] %>% dplyr::tally() %>%
    dplyr::pull()

  if (targetCohortNameEmpty == 0) {
    messageStore$push("- table `targetCohortName` contains 0 row")
  }

  # check targetCohortDefinitionId is a vector of integers
  checkmate::assertIntegerish(targetCohortDefinitionId,
    null.ok = TRUE,
    add = messageStore
  )

  # check indicationCohortName type is character of length 1
  checkmate::assertCharacter(indicationCohortName,
    len = 1,
    add = messageStore
  )

  # check indicationCohortName table exist
  cdm_indicationCohortName_exists <-
    inherits(cdm[[indicationCohortName]], "tbl_dbi")

  checkmate::assertTRUE(cdm_indicationCohortName_exists, add = messageStore)
  if (!isTRUE(cdm_indicationCohortName_exists)) {
    messageStore$push("- table `indicationCohortName` is not found")
  }

  # indicationDefinitionSet check is a tibble of contain two columns indication id and indication name
  checkmate::assertTibble(indicationDefinitionSet, add = messageStore)
  checkmate::assertSubset(
    c("cohortId", "cohortName"),
    colnames(indicationDefinitionSet),
    add = messageStore
  )

  # indicationGap check if is a vector of integer or NA
  if (length(indicationGap) == 1) {
    checkmate::assertCount(indicationGap, na.ok = TRUE, add = messageStore)
  } else {
    checkmate::assertIntegerish(
      indicationGap,
      unique = TRUE, add = messageStore
    )
  }

  # unknownIndicationTable is vector of characters
  checkmate::assertCharacter(unknownIndicationTable,
    null.ok = TRUE, add = messageStore
  )

  # unknownIndicationTable is only contain elements in get_start_date
  if (is.null(unknownIndicationTable) != TRUE) {
    checkmate::assertSubset(unknownIndicationTable, names(get_start_date), add = messageStore)
  }

  checkmate::reportAssertions(collection = messageStore)

  indicationDefinitionSet <- indicationDefinitionSet %>%
    dplyr::select(
      "indication_id" = "cohortId", "indication_name" = "cohortName"
    )

  # warning for different cohort start and end date in indicationCohortName
  if (cdm[[indicationCohortName]] %>%
    dplyr::mutate(equal = dplyr::if_else(.data$cohort_start_date == .data$cohort_end_date, 0, 1)) %>%
    dplyr::pull("equal") %>%
    sum() > 0) {
    warning("only cohort_start_date will be taken into account to compute indication")
  }

  # define result as a list
  result <- list()

  targetCohort <- cdm[[targetCohortName]]

  indicationCohort <- cdm[[indicationCohortName]] %>%
    dplyr::filter(
      .data$cohort_definition_id %in% !!indicationDefinitionSet$indication_id
    )

  if (!is.null(targetCohortDefinitionId)) {
    targetCohort <- targetCohort %>%
      dplyr::filter(
        .data$cohort_definition_id %in% .env$targetCohortDefinitionId
      )
  } else {
    targetCohortDefinitionId <- targetCohort %>%
      dplyr::select("cohort_definition_id") %>%
      dplyr::distinct() %>%
      dplyr::pull()
  }

  targetCohort <- targetCohort %>%
    dplyr::select("cohort_definition_id","subject_id", "cohort_start_date", "cohort_end_date") %>%
    dplyr::distinct()

  #create window from inidcationGap
    windowList <- list()

    for (i in 1:length(indicationGap)) {
      if (is.na(indicationGap[i])) {
        windowList[[i]] <- c(-Inf, 0)
      } else {
        windowList[[i]] <- c(-indicationGap[i], 0)
      }

    }

  for (j in 1:length(windowList)) {

    for (i in 1:length(indicationDefinitionSet$indication_id)) {
      ind <- indicationDefinitionSet$indication_id[i]


      intCohort <- targetCohort %>%
        PatientProfiles::addCohortIntersectFlag(
          cdm = cdm,
          targetCohortTable = indicationCohortName,
          targetCohortId = ind,
          indexDate = "cohort_start_date",
          targetStartDate = "cohort_start_date",
          targetEndDate = "cohort_end_date",
          window =  windowList[j],
          nameStyle = "{cohort_name}_{window_name}",
          tablePrefix = NULL
        ) %>% dplyr::rename("indication_id" = ncol(.env$targetCohort) + 1) %>%
        dplyr::mutate("indication_id" = dplyr::if_else(.data$indication_id > 0,
                                                       .env$ind, -1)) %>%
        dplyr::filter(.data$indication_id > 0)



      if (i == 1) {
        intersectCohort <- intCohort
      } else
      {
        intersectCohort <- intersectCohort %>% dplyr::union(intCohort)
      }

    }


    intersectCohort <- targetCohort %>%
      dplyr::left_join(intersectCohort,
                       by = c("cohort_definition_id","subject_id", "cohort_start_date", "cohort_end_date")) %>%
      dplyr::mutate("indication_id" = dplyr::if_else(is.na(.data$indication_id), -1,
                                                     .data$indication_id))
# look for unknownIndication
      if (!is.null(unknownIndicationTable)) {

        for (t in 1:length(unknownIndicationTable)) {

          subjectsUnknownIndication <- intersectCohort %>%
            dplyr::filter(.data$indication_id < 0)

          unknownIndicationTableName <- unknownIndicationTable[t]

          unknownIndication.k <- cdm[[unknownIndicationTableName]] %>%
            dplyr::select(
              "subject_id" = "person_id",
              "unknown_indication_start_date" =
                get_start_date[[unknownIndicationTableName]]
            ) %>%
            dplyr::inner_join(
              subjectsUnknownIndication,
              by = "subject_id"
            ) %>%
            dplyr::mutate(dif_time_unknown_indication = !!CDMConnector::datediff(
              start = "unknown_indication_start_date", end = "cohort_start_date"
            )) %>%
            dplyr::group_by(
              .data$subject_id, .data$cohort_start_date, .data$cohort_end_date
            ) %>%
            dplyr::summarise(
              dif_time_unknown_indication = min(
                .data$dif_time_unknown_indication,
                na.rm = TRUE
              ),
              .groups = "drop"
            ) %>%
            dplyr::filter(.data$dif_time_unknown_indication >= 0) %>%
            CDMConnector::computeQuery()


          if (t == 1) {
            unknownIndication <- unknownIndication.k
          } else {
            unknownIndication <- unknownIndication %>%
              dplyr::union_all(unknownIndication.k)
          }
        }

        if (!is.na(indicationGap[j])){

          gap <- indicationGap[j]

        unknownIndication <- unknownIndication %>%
          dplyr::filter(.data$dif_time_unknown_indication <= .env$gap)

        }

        intersectCohort <- intersectCohort %>% dplyr::left_join(unknownIndication,
                                             by = c("subject_id", "cohort_start_date", "cohort_end_date")) %>%
          dplyr::mutate("indication_id" = dplyr::if_else(
            !is.na(.data$dif_time_unknown_indication),
            0,
            .data$indication_id
          )) %>% dplyr::select(-"dif_time_unknown_indication")


      }

    if (is.na(indicationGap[j])) {
      result[["Any"]] <- intersectCohort
    }

    else {
      result[[as.character(indicationGap[j])]] <- intersectCohort

    }




}


  # define indication definition set
  if (!is.null(unknownIndicationTable)) {
    indicationDefinitionSet <- indicationDefinitionSet %>%
      dplyr::select("indication_id", "indication_name") %>%
      rbind(dplyr::tibble(
        indication_id = c(0, -1),
        indication_name = c("Unknown indication", "No indication")
      ))
  } else {
    indicationDefinitionSet <- indicationDefinitionSet %>%
      dplyr::select("indication_id", "indication_name") %>%
      rbind(
        dplyr::tibble(indication_id = -1, indication_name = "No indication")
      )
  }

  attr(result, "indicationDefinitionSet") <- indicationDefinitionSet

  return(result)
}

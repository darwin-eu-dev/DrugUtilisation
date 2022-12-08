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

#' Get indication for a target cohort
#'
#' @param cdm object created with CDMConnector::cdm_from_con.
#' It must contain the targetCohortName and indicationCohortName table in it.
#' @param targetCohortName cohort table in the cdm contain list of target cohort for patients
#' @param targetCohortDefinitionIds cohort definition ids to include to generate indication
#' @param indicationCohortName indication table that contain list of indication
#' @param indicationDefinitionSet definition of indication of interest,
#' this function will only get indication for indication contain in this set
#' @param indicationGap the maximum gap between the cohort start date and indication start date
#' @param unknownIndicationTables tables to get extra indication from the default is to include
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
                          targetCohortDefinitionIds,
                          indicationCohortName,
                          indicationDefinitionSet,
                          indicationGap,
                          unknownIndicationTables = c("condition_occurrence", "observation"),
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

  # check targetCohortDefinitionIds is a vector of integers
  checkmate::assertIntegerish(targetCohortDefinitionIds,
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
  checkmate::assertTibble(indicationDefinitionSet,
    ncols = 2, add = messageStore
  )
  checkmate::assertSubset(
    colnames(indicationDefinitionSet),
    c("indication_id", "indication_name"),
    add = messageStore
  )

  # indicationGap check if is a vector of integer or NA
  if (length(indicationGap) == 1) {
    checkmate::assertCount(indicationGap, na.ok = TRUE, add = messageStore)
  } else {

    checkmate::assertIntegerish(indicationGap[!is.na(indicationGap)], add = messageStore)
  }

  # unknownIndicationTables is vector of characters
  checkmate::assertCharacter(unknownIndicationTables,
    null.ok = TRUE, add = messageStore
  )

  # unknownIndicationTables is only contain elements in get_start_date
  if (is.null(unknownIndicationTables) != TRUE) {
    checkmate::assertSubset(unknownIndicationTables, names(get_start_date), add = messageStore)
  }

  checkmate::reportAssertions(collection = messageStore)


  # warning for different cohort start and end date in indicationCohortName
  if (cdm[[indicationCohortName]] %>%
    dplyr::mutate(equal = dplyr::if_else(.data$cohort_start_date == .data$cohort_end_date, 0, 1)) %>%
    dplyr::pull("equal") %>%
    sum() > 0) {
    warning("only cohort_start_date will be taken into account to compute indication")
  }

  # define result as a list
  result <- list()


  # get all indications if indicationGap = NA return all indication, if not return indication within indicationGap
  if (any(is.na(indicationGap))) {
    # define variable in the function from inputs
    target_db <- cdm[[targetCohortName]]
    indication_db <- cdm[[indicationCohortName]]

    # Filter indication_db by id in indicationDefinitionSet
    indicationDefinitionSet_id <- indicationDefinitionSet$indication_id
    indication_db <- indication_db %>%
      dplyr::filter(.data$cohort_definition_id %in% .env$indicationDefinitionSet_id)

    if (!is.null(targetCohortDefinitionIds)) {
      target_db <- target_db %>%
        dplyr::filter(.data$cohort_definition_id %in% .env$targetCohortDefinitionIds)
    }

    target_db <- target_db %>%
      dplyr::left_join(
        target_db %>%
          dplyr::inner_join(
            indication_db %>%
              dplyr::rename("indication_id" = "cohort_definition_id") %>%
              dplyr::rename("indication_start_date" = "cohort_start_date") %>%
              dplyr::select("indication_id", "subject_id", "indication_start_date"),
            by = c("subject_id")
          ) %>% dplyr::mutate(dif_time_indication = dbplyr::sql(
            CDMConnector::datediff(
              start = "cohort_start_date",
              end = "indication_start_date"
            )
          )) %>%
          dplyr::filter(
            .data$dif_time_indication <= 0
          ) %>%
          dplyr::select(-"dif_time_indication", -"indication_start_date") %>%
          dplyr::distinct(),
        by = c(
          "cohort_definition_id", "subject_id", "cohort_start_date",
          "cohort_end_date"
        )
      ) %>%
      dplyr::compute()


    # unknown indication
    if (!is.null(unknownIndicationTables)) {
      for (table in unknownIndicationTables) {
        target_db_unknown <- target_db %>%
          dplyr::filter(is.na(.data$indication_id))

        if (target_db_unknown %>% dplyr::tally() %>% dplyr::pull() > 0) {
          table_indication <- cdm[[table]] %>%
            dplyr::rename("unknown_indication_start_date" = get_start_date[[table]]) %>%
            dplyr::select("person_id", "unknown_indication_start_date") %>%
            dplyr::distinct()

          target_db_unknown <- target_db_unknown %>%
            dplyr::left_join(table_indication,
              by = c("subject_id" = "person_id")
            ) %>%
            dplyr::mutate(dif_time_unknown_indication = dbplyr::sql(
              CDMConnector::datediff(
                start = "cohort_start_date",
                end = "unknown_indication_start_date"
              )
            )) %>%
            dplyr::filter(.data$dif_time_unknown_indication <= 0) %>%
            dplyr::select(
              -"dif_time_unknown_indication",
              -"unknown_indication_start_date"
            ) %>%
            dplyr::distinct() %>%
            dplyr::compute()


          target_db <- target_db %>%
            dplyr::anti_join(target_db_unknown,
              by = c("subject_id", "cohort_start_date")
            ) %>%
            dplyr::union(target_db_unknown %>%
              dplyr::mutate(indication_id = 0)) %>%
            dplyr::compute()
        }
      }
    }


    target_db <-
      target_db %>%
      dplyr::mutate(indication_id = ifelse(is.na(.data$indication_id), -1, .data$indication_id)) %>%
      dplyr::inner_join(
        indicationDefinitionSet %>%
          dplyr::select("indication_id") %>%
          rbind(dplyr::tibble(indication_id = c(0, -1))) %>%
          dplyr::distinct(),
        by = "indication_id",
        copy = TRUE
      )


    result[["Any"]] <- target_db

  }

  if (length(indicationGap[!is.na(indicationGap)])>0){
  # exclude NA in indicatioGap
    indicationGapExcludeNA <- indicationGap[!is.na(indicationGap)]

    for (gap in indicationGapExcludeNA) {
      # define variable in the function from inputs
      target_db <- cdm[[targetCohortName]]
      indication_db <- cdm[[indicationCohortName]]
      # Filter indication_db by id in indicationDefinitionSet
      indicationDefinitionSet_id <- indicationDefinitionSet$indication_id
      indication_db <- indication_db %>%
        dplyr::filter(.data$cohort_definition_id %in% .env$indicationDefinitionSet_id)

      if (!is.null(targetCohortDefinitionIds)) {
        target_db <- target_db %>%
          dplyr::filter(.data$cohort_definition_id %in% .env$targetCohortDefinitionIds)
      }


      target_db <- target_db %>%
        dplyr::left_join(
          target_db %>%
            dplyr::inner_join(
              indication_db %>%
                dplyr::rename("indication_id" = "cohort_definition_id") %>%
                dplyr::rename("indication_start_date" = "cohort_start_date") %>%
                dplyr::select("indication_id", "subject_id", "indication_start_date"),
              by = c("subject_id")
            ) %>% dplyr::mutate(dif_time_indication = dbplyr::sql(
              CDMConnector::datediff(
                start = "indication_start_date",
                end = "cohort_start_date"
              )
            )) %>%
            dplyr::filter(
              .data$dif_time_indication <= .env$gap
            ) %>%
            dplyr::filter(
              .data$dif_time_indication >= 0
            ) %>%
            dplyr::select(-"dif_time_indication", -"indication_start_date") %>%
            dplyr::distinct(),
          by = c(
            "cohort_definition_id", "subject_id", "cohort_start_date",
            "cohort_end_date"
          )
        ) %>%
        dplyr::compute()


      # unknown indication
      if (!is.null(unknownIndicationTables)) {
        for (table in unknownIndicationTables) {
          target_db_unknown <- target_db %>%
            dplyr::filter(is.na(.data$indication_id))

          if (target_db_unknown %>% dplyr::tally() %>% dplyr::pull() > 0) {
            table_indication <- cdm[[table]] %>%
              dplyr::rename("unknown_indication_start_date" = get_start_date[[table]]) %>%
              dplyr::select("person_id", "unknown_indication_start_date") %>%
              dplyr::distinct()

            target_db_unknown <- target_db_unknown %>%
              dplyr::left_join(table_indication,
                by = c("subject_id" = "person_id")
              ) %>%
              dplyr::mutate(dif_time_unknown_indication = dbplyr::sql(
                CDMConnector::datediff(
                  start = "unknown_indication_start_date",
                  end = "cohort_start_date"
                )
              )) %>%
              dplyr::filter(
                .data$dif_time_unknown_indication <= .env$gap
              ) %>%
              dplyr::filter(
                .data$dif_time_unknown_indication >= 0
              ) %>%
              dplyr::select(
                -"dif_time_unknown_indication",
                -"unknown_indication_start_date"
              ) %>%
              dplyr::distinct() %>%
              dplyr::compute()


            target_db <- target_db %>%
              dplyr::anti_join(target_db_unknown,
                by = c("subject_id", "cohort_start_date")
              ) %>%
              dplyr::union(target_db_unknown %>%
                dplyr::mutate(indication_id = 0)) %>%
              dplyr::compute()
          }
        }
      }

      target_db <-
        target_db %>%
        dplyr::mutate(indication_id = ifelse(is.na(.data$indication_id), -1, .data$indication_id)) %>%
        dplyr::inner_join(
          indicationDefinitionSet %>%
            dplyr::select("indication_id") %>%
            rbind(dplyr::tibble(indication_id = c(0, -1))) %>%
            dplyr::distinct(),
          by = "indication_id",
          copy = TRUE
        )


      result[[as.character(gap)]] <- target_db
    }
  }

  # define indication definition set
  indicationDefinitionSet <- indicationDefinitionSet %>%
    dplyr::select("indication_id", "indication_name") %>%
    rbind(dplyr::tibble(
      indication_id = c(0, -1),
      indication_name = c("Unkown indication", "No indication")
    ))

  attr(result, "indicationDefinitionSet") <- indicationDefinitionSet

  # #save indication
  #  cdm[[indicationTableName]] <-
  #    result$indication
  # #save indication definition set
  #  cdm[["indicationDefinitionSet"]] <-
  #    target_db %>%  dplyr::mutate(indication_id = ifelse(is.na(.data$indication_id), -1, .data$indication_id))


  return(result)
}

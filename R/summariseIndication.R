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
#' @param cdm 'cdm' object created with CDMConnector::cdm_from_con(). It must
#' must contain the 'doseTableName' table and  also 'strataCohortName' and
#' 'indicationList' if specified. It is a compulsory input, no default
#' value is provided.
#' @param cohortId cohort definition id of the cohorts in the strata cohort
#' table that you want to summarise. By default cohortId = NULL, then all
#' cohorts in strataCohort will be summarised.
#' @param indicationList is a list of tables that contains the indication
#' information. You must use the getIndication function to obtain this list. No
#' default value is provided.
#' @param minimumCellCount Minimum counts that a group can have. Cohorts with
#' less counts than this value are obscured. By default: 5.
#'
#' @return A Tibble with 4 columns: cohort_definition_id, variable, estimate and
#' value. There will be one row for each cohort, variable and cohort
#' combination.
#'
#' @export
#'
#' @examples
summariseIndication <- function(cdm,
                                cohortId = NULL,
                                indicationList = NULL,
                                minimumCellCount = 5) {
  # first round of assertions CLASS
  # start checks
  errorMessage <- checkmate::makeAssertCollection()
  # check cdm
  checkmate::assertClass(cdm,
                         "cdm_reference",
                         add = errorMessage
  )
  # check cohortId
  checkmate::assertIntegerish(
    cohortId,
    unique = TRUE,
    null.ok = TRUE,
    any.missing = FALSE,
    add = errorMessage
  )
  # check indicationList
  checkmate::assertList(indicationList,
                        any.missing = FALSE,
                        add = errorMessage
  )
  # minimum cell counts
  checkmate::assertCount(minimumCellCount,
                         add = errorMessage
  )
  # report collection of errors
  checkmate::reportAssertions(collection = errorMessage)

  # checks if indication summary
  checkmate::assertTRUE(!is.null(attr(indicationList, which = "indicationDefinitionSet")),
                        add = errorMessage
  )
  for (k in names(indicationList)) {
    checkmate::assertTRUE(all(
      colnames(indicationList[[k]]) == c(
        "cohort_definition_id",
        "subject_id",
        "cohort_start_date",
        "cohort_end_date",
        "indication_id"
      )
    ))
  }

  checkmate::reportAssertions(collection = errorMessage)

  # define empty cohort table from indicationGap table
  cohort <- dplyr::tibble()

  for (k in names(indicationList)) {
    cohort <- rbind(cohort, indicationList[[k]] %>% dplyr::select(
      "cohort_definition_id",
      "subject_id",
      "cohort_start_date",
      "cohort_end_date"
    ) %>% dplyr::collect())
  }
  cohort <- cohort %>% dplyr::distinct()


  # Stop if indication cohort don't contain any observation with Id in cohortId
  if (nrow(cohort %>% dplyr::filter(.data$cohort_definition_id %in% .env$cohortId)) == 0 &
      !is.null(cohortId)) {
    stop("The indication cohort don't contain any observations with Id in cohortId")
  }

  if (is.null(cohortId)) {
    cohortId <- cohort %>%
      dplyr::select("cohort_definition_id") %>%
      dplyr::distinct() %>%
      dplyr::pull()
  }

  # define empty result table
  result <- dplyr::tibble()
  # get basic data of each cohort
  for (k in cohortId) {
    result <- rbind(result, cohort %>%
                      dplyr::filter(.data$cohort_definition_id == .env$k) %>%
                      dplyr::summarise(
                        number_observations.count = as.character(dplyr::n()),
                        cohort_start_date.min = as.character(min(.data$cohort_start_date,
                                                                 na.rm = TRUE
                        )),
                        cohort_start_date.max = as.character(max(.data$cohort_start_date,
                                                                 na.rm = TRUE
                        )),
                        cohort_end_date.min = as.character(min(.data$cohort_end_date,
                                                               na.rm = TRUE
                        )),
                        cohort_end_date.max = as.character(max(.data$cohort_end_date,
                                                               na.rm = TRUE
                        ))
                      ) %>%
                      dplyr::collect() %>%
                      tidyr::pivot_longer(
                        cols = dplyr::everything(),
                        names_to = c("variable", "estimate"),
                        names_pattern = "([[:alnum:]_]+).([[:alnum:]_]+)"
                      ) %>%
                      dplyr::mutate(cohort_definition_id = .env$k) %>%
                      dplyr::select("cohort_definition_id", "variable", "estimate", "value"))
  }
  # get indication data of each cohort
  indicationDefinitionSet <-
    attr(indicationList, "indicationDefinitionSet")


  for (i in names(indicationList)) {

    # subset indicationList to indication of interests from user
    indicationList_sub <- indicationList[[i]] %>%
      dplyr::filter(.data$cohort_definition_id %in% .env$cohortId)
    # select unique cohortId in the indicationList
    unique_cohortId <- indicationList_sub %>%
      dplyr::select("cohort_definition_id") %>%
      dplyr::distinct() %>%
      dplyr::pull()

    for (Id in unique_cohortId) {
      result <- rbind(
        result,
        indicationList_sub %>% dplyr::filter(.data$cohort_definition_id %in% .env$Id) %>%
          dplyr::group_by(.data$cohort_definition_id, .data$indication_id) %>%
          dplyr::tally() %>%
          dplyr::ungroup() %>%
          dplyr::collect() %>%
          dplyr::right_join(indicationDefinitionSet, by = "indication_id") %>%
          dplyr::mutate(cohort_definition_id = dplyr::if_else(is.na(.data$cohort_definition_id), .env$Id, .data$cohort_definition_id)) %>%
          dplyr::mutate(estimate = "count") %>%
          dplyr::mutate(n = dplyr::if_else(is.na(.data$n), 0, .data$n)) %>%
          dplyr::mutate(
            value = dplyr::if_else(
              .data$n > 0 & .data$n < .env$minimumCellCount,
              paste0("<", .env$minimumCellCount),
              as.character(.data$n)
            )
          ) %>%
          dplyr::mutate(
            variable = paste0(
              "indication_gap_",
              .env$i,
              "_",
              .data$indication_name
            )
          ) %>%
          dplyr::select("cohort_definition_id", "variable", "estimate", "value")
      )
    }
  }

  result <- obscureSummary(result, minimumCellCount = minimumCellCount)

  result <- result %>% dplyr::arrange(.data$cohort_definition_id)

  return(result)
}

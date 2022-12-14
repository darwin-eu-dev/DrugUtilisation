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

#' Get the cohorts to compute incidence and prevalence. WARNING: only Standard
#' codes are considered from the concept sets
#'
#' @param cdm 'cdm' object created with CDMConnector::cdm_from_con(). It must
#' must contain at least 'drug_exposure' and 'concept_ancestor' table. The 'cdm'
#' object must contain the 'write_schema' as attribute and  the user should have
#' permission to write on it. It is a compulsory input, no default value is
#' provided.
#' @param conceptIds List or vector of drug concept ids that form the concept
#' set(s). Each element of the list is instantiated as as different cohort.
#' 'conceptIds' or 'conceptSetFolder' are compulsory inputs and one of them
#' should be provided to instantiate the cohort(s).
#' @param conceptSetFolder Relative path from the root of the project to the
#' folder where the concept set .json files are contained. It will be
#' instantiated one cohort for each .json file. 'conceptIds' or
#' 'conceptSetFolder' are compulsory inputs and one of them should be provided
#' to instantiate the cohort(s).
#' @param gapEra Gap threshold between two consecutive exposures. If the number
#' of non exposed days between two exposures is smaller or equal to 'gapEra'
#' this non exposed days in between will be considered exposed. The input can be
#' a vector then cohorts with the combinations of all concept sets and gaps will
#' be instantiated. By default: 0.
#' @param incidencePrevalenceCohortName Name of the table to be instantiated in
#' the 'write_schema' of the database. By default:
#' "incidence_prevalence_dus_tables".
#' @param overwrite Weather or not overwrite the table if something is written
#' there. By default: FALSE.
#' @param verbose Weather or not the code should print the steps of the process.
#' By default: FALSE.
#'
#' @return This function return the reference of the cdm with the instantiated
#' table. The instantiated table is a cohort table in OMOP format
#' (cohort_definition_id, subject_id, cohort_start_date, cohort_end_date). The
#' individuals will be in the cohort while considered exposed by the concept
#' set used to create that cohort. This function also writes a csv file with
#' the relation between the different cohort_definition_id, the name of the
#' concept sets used and the gap between exposures considered. The file is named
#' "equivalences.csv" and is written in the project origin (if no
#' 'conceptSetFolder' is provided) or in the same folder where the concept sets
#'  are.
#'
#' @export
#'
#' @examples
instantiateIncidencePrevalenceCohorts <- function(cdm,
                                                  conceptIds = NULL,
                                                  conceptSetFolder = NULL,
                                                  gapEra = 0,
                                                  incidencePrevalenceCohortName = "incidence_prevalence_dus_tables",
                                                  overwrite = FALSE,
                                                  verbose = FALSE) {
  # CHECKS
  errorMessage <- checkmate::makeAssertCollection()
  checkmate::assertClass(cdm, classes = "cdm_reference", add = errorMessage)
  # check that drug_exposure and concept tables exists is missing, to be
  # implemented when https://github.com/darwin-eu/CDMConnector/issues/28 is
  # adressed.
  if (is.null(conceptSetFolder) && !is.null(conceptIds)) {
    if (!is.list(conceptIds)) {
      conceptIds <- list(conceptIds)
    }
    checkmate::assertList(
      conceptIds,
      min.len = 1,
      add = errorMessage
    )
    checkmate::assertTRUE(
      all(sapply(conceptIds, is.numeric)),
      add = errorMessage
    )
    checkmate::assertTRUE(
      all(sapply(conceptIds, function(x) {
        sum(is.na(x)) == 0
      })),
      add = errorMessage
    )
  } else if (!is.null(conceptSetFolder) && is.null(conceptIds)) {
    checkmate::assertPathForOutput(
      here::here(conceptSetFolder, "equivalence.csv"),
      overwrite = TRUE,
      add = errorMessage
    )
  } else if (is.null(conceptSetFolder) && is.null(conceptIds)) {
    stop("'conceptSetFolder' or 'conceptIds' are compulsory inputs.")
  } else {
    stop("Use only 'conceptSetFolder' or 'conceptIds' as concept input.")
  }
  checkmate::assertCount(gapEra, add = errorMessage)
  checkmate::assertCharacter(
    incidencePrevalenceCohortName,
    min.chars = 1, min.len = 1,
    add = errorMessage
  )
  checkmate::assertLogical(overwrite, len = 1, add = errorMessage)
  checkmate::assertLogical(verbose, len = 1, add = errorMessage)
  checkmate::reportAssertions(collection = errorMessage)

  # check that you can write in the database and schema provided

  if (!is.null(conceptSetFolder)) {
    # read the complete file name of the concepts in the specified folder
    conceptSets <- dplyr::tibble(concept_set_path = list.files(
      path = here::here(conceptSetFolder), full.names = TRUE
    ))
    # only consider the files that end in .json file
    conceptSets <- conceptSets %>%
      dplyr::filter(tools::file_ext(.data$concept_set_path) == "json")
    # obtain the name of the files without the extension
    conceptSets <- conceptSets %>%
      dplyr::mutate(
        concept_set_name =
          tools::file_path_sans_ext(basename(.data$concept_set_path))
      )
    # add the cohort_concept_id number
    conceptSets <- conceptSets %>%
      dplyr::mutate(cohort_definition_id = as.character(dplyr::row_number()))

    # read concept sets and extract the list of included/excluded codes and
    # with or without descendants
    tryCatch(
      expr = conceptList <- readConceptSets(conceptSets),
      error = function(e) {
        stop("The json files are not properly formated OMOP concept sets.")
      }
    )
    # add descendants to the ones that they have include_descendants = TRUE
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
      dplyr::select("cohort_definition_id", "drug_concept_id") %>%
      dplyr::anti_join(
        conceptList %>%
          dplyr::filter(.data$is_excluded == TRUE),
        by = "drug_concept_id"
      )
  } else {
    conceptList <- dplyr::bind_rows(
      lapply(
        conceptIds,
        function(x) {
          dplyr::tibble(drug_concept_id = x)
        }
      ),
      .id = "cohort_definition_id"
    )
  }

  # select only the variables of interest
  incidencePrevalenceCohort <- cdm[["drug_exposure"]] %>%
    dplyr::select(
      "person_id",
      "drug_concept_id",
      "drug_exposure_start_date",
      "drug_exposure_end_date"
    )
  # subset the drug exposure table
  incidencePrevalenceCohort <- incidencePrevalenceCohort %>%
    dplyr::inner_join(conceptList,
      by = "drug_concept_id",
      copy = TRUE
    ) %>%
    dplyr::select(-"drug_concept_id") %>%
    dplyr::compute()

  # Obtain the start of the intervals where the person may be exposed. An
  # interval starts at the start of an exposure
  incidencePrevalenceStart <- incidencePrevalenceCohort %>%
    dplyr::select("cohort_definition_id", "person_id",
      "interval_start_date" = "drug_exposure_start_date"
    )
  # or at the next day of the end of an exposure
  incidencePrevalenceStart <- incidencePrevalenceStart %>%
    dplyr::union(
      incidencePrevalenceCohort %>%
        dplyr::select(
          "cohort_definition_id", "person_id",
          "drug_exposure_end_date"
        ) %>%
        dplyr::distinct() %>%
        dplyr::mutate(interval_start_date = as.Date(dbplyr::sql(
          CDMConnector::dateadd(date = "drug_exposure_end_date", number = 1)
        ))) %>%
        dplyr::select(-"drug_exposure_end_date")
    )
  # we eliminate repeated starts:
  incidencePrevalenceStart <- incidencePrevalenceStart %>%
    dplyr::distinct()
  # now we have the starts of the intervals we eliminate the last start (end of
  # the last exposure) for each person in each cohort
  incidencePrevalenceStart <- incidencePrevalenceStart %>%
    dplyr::group_by(.data$cohort_definition_id, .data$person_id) %>%
    dplyr::filter(
      .data$interval_start_date < max(.data$interval_start_date, na.rm = TRUE)
    )
  # we sort and number the starts of the intervals
  incidencePrevalenceStart <- incidencePrevalenceStart %>%
    dbplyr::window_order(.data$interval_start_date) %>%
    dplyr::mutate(index = dplyr::row_number()) %>%
    dplyr::ungroup() %>%
    dplyr::compute()

  # Obtain the end of the intervals where the person may be exposed. An interval
  # ends at the previous day when an exposure starts
  incidencePrevalenceEnd <- incidencePrevalenceCohort %>%
    dplyr::select(
      "cohort_definition_id", "person_id",
      "drug_exposure_start_date"
    ) %>%
    dplyr::distinct() %>%
    dplyr::mutate(interval_end_date = as.Date(dbplyr::sql(
      CDMConnector::dateadd(date = "drug_exposure_start_date", number = -1)
    ))) %>%
    dplyr::select(-"drug_exposure_start_date")
  # or at the end of an exposure
  incidencePrevalenceEnd <- incidencePrevalenceEnd %>%
    dplyr::union(
      incidencePrevalenceCohort %>%
        dplyr::select("cohort_definition_id", "person_id",
          "interval_end_date" = "drug_exposure_end_date"
        )
    )
  # we eliminate repeated ends:
  incidencePrevalenceEnd <- incidencePrevalenceEnd %>%
    dplyr::distinct()
  # now we have the starts of the intervals we eliminate the first end
  # (previous day of the first exposure) for each person in each cohort
  incidencePrevalenceEnd <- incidencePrevalenceEnd %>%
    dplyr::group_by(.data$cohort_definition_id, .data$person_id) %>%
    dplyr::filter(
      .data$interval_end_date > min(.data$interval_end_date, na.rm = TRUE)
    )
  # we sort and number the ends of the intervals
  incidencePrevalenceEnd <- incidencePrevalenceEnd %>%
    dbplyr::window_order(.data$interval_end_date) %>%
    dplyr::mutate(index = dplyr::row_number()) %>%
    dplyr::ungroup() %>%
    dplyr::compute()

  # Start and end dates are joined with the exposures
  incidencePrevalenceCohort <- incidencePrevalenceStart %>%
    dplyr::inner_join(incidencePrevalenceEnd,
      by = c("cohort_definition_id", "person_id", "index")
    ) %>%
    dplyr::inner_join(incidencePrevalenceCohort,
      by = c("cohort_definition_id", "person_id")
    )
  # we mark the empty intervals as gaps
  incidencePrevalenceCohort <- incidencePrevalenceCohort %>%
    dplyr::mutate(inside = dplyr::if_else(
      .data$drug_exposure_start_date <= .data$interval_start_date &
        .data$drug_exposure_end_date >= .data$interval_end_date,
      1,
      0
    )) %>%
    dplyr::group_by(
      .data$cohort_definition_id, .data$person_id, .data$index,
      .data$interval_start_date, .data$interval_end_date
    ) %>%
    dplyr::summarise(
      number_exposures = sum(.data$inside, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(gap = dplyr::if_else(.data$number_exposures > 0, 0, 1)) %>%
    dplyr::select(-"number_exposures") %>%
    dplyr::compute()
  # gaps smaller than gapEra are not marked as gaps
  incidencePrevalenceCohort <- incidencePrevalenceCohort %>%
    dplyr::filter(.data$gap == 0) %>%
    dplyr::union_all(
      incidencePrevalenceCohort %>%
        dplyr::filter(.data$gap == 1) %>%
        dplyr::mutate(gap = dplyr::if_else(
          dbplyr::sql(CDMConnector::datediff(
            start = "interval_start_date",
            end = "interval_end_date"
          )) + 1 <= .env$gapEra,
          0,
          1
        ))
    )
  # We group by person_id and cohort_definition_id
  incidencePrevalenceCohort <- incidencePrevalenceCohort %>%
    dplyr::group_by(.data$person_id, .data$cohort_definition_id)
  # We arrange by interval_start_date and compute the era_id of each exposure
  incidencePrevalenceCohort <- incidencePrevalenceCohort %>%
    dbplyr::window_order(.data$index) %>%
    dplyr::mutate(era_id = cumsum(.data$gap)) %>%
    dplyr::filter(.data$gap == 0)
  # We summarize each era to obtain cohort_start_date and cohort_end_date
  incidencePrevalenceCohort <- incidencePrevalenceCohort %>%
    dplyr::group_by(
      .data$person_id, .data$cohort_definition_id, .data$era_id
    ) %>%
    dplyr::summarise(
      cohort_start_date = min(.data$interval_start_date, na.rm = TRUE),
      cohort_end_date = max(.data$interval_end_date, na.rm = TRUE),
      .groups = "drop"
    )
  # Select the final variables and compute
  incidencePrevalenceCohort <- incidencePrevalenceCohort %>%
    dplyr::select("cohort_definition_id",
      "subject_id" = "person_id",
      "cohort_start_date", "cohort_end_date"
    ) %>%
    dplyr::compute()

  # Instantiate the cohorts in the database as permanent tables
  cdm[[incidencePrevalenceCohortName]] <- incidencePrevalenceCohort
  CDMConnector::computePermanent(
    incidencePrevalenceCohort,
    incidencePrevalenceCohortName,
    schema = attr(cdm, "write_schema"),
    overwrite = overwrite
  )

  # Write the equivalence.csv table in the output folder
  if (is.null(conceptSetFolder)) {
    fileName <- here::here("equivalence.csv")
  } else {
    fileName <- here::here(conceptSetFolder, "equivalence.csv")
  }
  utils::write.csv(
    conceptList,
    file = fileName,
    row.names = FALSE
  )

  # return cdm
  return(cdm)
}


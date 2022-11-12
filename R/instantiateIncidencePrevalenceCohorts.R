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
#' must contain at least 'drug_exposure' and 'concept' table. The 'cdm' object
#' must contain the 'write_schema' as attribute and  the user should have
#' permission to write on it. It is a compulsory input, no default value is
#' provided.
#' @param conceptSetFolder Relative path from the root of the project to the
#' folder where the concept set .json files are contained. It will be
#' instantiated one cohort for each .json file. It is a compulsory input, no
#' default value is provided.
#' @param gapEra Gap threshold between two consecutive exposures. If the number
#' of non exposed days between two exposures is smaller or equal to 'gapEra'
#' this non exposed days in between will be considered exposed. The input can be
#' a vector then cohorts with the combinations of all concept sets and gaps will
#' be instantiated. By default: 0.
#' @param incidencePrevalenceCohortName Name of the table to be instantiated in
#' the 'write_schema' of the database. By default:
#' "incidence_prevalence_dus_tables".
#' @param overWrite Weather or not overwrite the table if something is written
#' there. By default: FALSE.
#' @param verbose Weather or not the code should print the steps of the process.
#' By default: FALSE.
#'
#' @return This function return the reference of the cdm with the instantiated
#' table. The instantiated table is a cohort table in OMOP format
#' (cohort_definition_id, subject_id, cohort_start_date, cohort_end_date). The
#' individuals will be in the cohort while considered exposed by the concept
#' set used to create that cohort. This function also provides a csv output with
#' the relation between the different cohort_definition_id, the name of the
#' concept sets used and the gap between exposures considered. The file is named
#' "equivalences.csv" in the same folder where the concept sets are.
#'
#' @export
#'
#' @examples
instantiateIncidencePrevalenceCohorts <- function(cdm,
                                                  conceptSetFolder,
                                                  gapEra = 0,
                                                  incidencePrevalenceCohortName = "incidence_prevalence_dus_tables",
                                                  overWrite = FALSE,
                                                  verbose = FALSE) {
  # CHECKS
  errorMessage <- checkmate::makeAssertCollection()
  checkmate::assertClass(cdm, classes = "cdm_ref", add = errorMessage)
  # check that drug_exposure and concept tables exists is missing, to be
  # implemented when https://github.com/darwin-eu/CDMConnector/issues/28 is
  # adressed.
  checkmate::assertPathForOutput(
    here::here(conceptSetFolder, "equivalence.csv"),
    overwrite = TRUE,
    add = errorMessage
  )
  checkmate::assertCount(gapEra, add = errorMessage)
  checkmate::assertCharacter(
    incidencePrevalenceCohortName,
    min.chars = 1, min.len = 1,
    add = errorMessage
  )
  checkmate::assertLogical(overWrite, len = 1, add = errorMessage)
  checkmate::assertLogical(verbose, len = 1, add = errorMessage)
  checkmate::reportAssertions(collection = errorMessage)

  # BLOCK 1 Obtain concept_set_names and cohort_definition_id
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
    dplyr::mutate(cohort_definition_id = dplyr::row_number())

  # BLOC 2 read the concept sets and obtain the drug_concept_id#
  # read concept sets and extract the list of included/excluded codes and
  # with or without descendants
  tryCatch(
    expr = conceptList <- readConceptSets(conceptSets),
    error = function(e){
      stop("The json files are not properly formated OMOP concept sets.")
    }
  )
  # add descendants to the ones that they have include_descendants = TRUE
  conceptList <- conceptList %>%
    dplyr::filter(include_descedants == FALSE) %>%
    dplyr::union(
      conceptList %>%
        dplyr::filter(include_descedants == TRUE) %>%
        dplyr::inner_join(
          cdm[["concept"]] %>%
            dplyr::select(
              "concept_id" = "ancestor_concept_id",
              "descendant_concept_id"
            ),
          by = "concept_id"
        ) %>%
        dplyr::select(-"concept_id") %>%
        dplyr::rename("concept_id" = "descendant_concept_id")
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

  # BLOC 3 subset drug exposure table
  # select only the variables of interest
  incidencePrevalenceCohort <- cdm[["drug_exposure"]] %>%
    dplyr::select(
      "person_id",
      "drug_concept_id",
      "drug_exposure_start_date",
      "drug_exposure_end_date"
    )
  # subset the drug exposure table
  incidencePrevalenceCohort <- incidencePrevalenceCohort
    dplyr::inner_join(conceptList,
      by = "drug_concept_id",
      copy = TRUE
    ) %>%
    dplyr::select(-"drug_concept_id") %>%
    dplyr::compute()

  # BLOC 4 obtain the start of the periods where the person may be exposed
  # A period starts at the start of an exposure
  incidencePrevalenceStart <- incidencePrevalenceCohort %>%
    dplyr::select("cohort_definition_id", "person_id",
      "period_start_date" = "drug_exposure_start_date"
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
        dplyr::mutate(period_start_date = as.Date(dbplyr::sql(
          SqlUtilities::sql_add_days(
            CDMConnector::dbms(attr(cdm, "dbcon")),
            1,
            "drug_exposure_end_date"
          )
        ))) %>%
        dplyr::select(-"drug_exposure_end_date")
    )
  # we eliminate repeated starts:
  incidencePrevalenceStart <- incidencePrevalenceStart %>%
    dplyr::distinct()
  # now we have the starts of the periods we eliminate the last start (end of
  # the last exposure) for each person in each cohort
  incidencePrevalenceStart <- incidencePrevalenceStart %>%
    dplyr::group_by(.data$cohort_definition_id, .data$person_id) %>%
    dplyr::filter(
      .data$period_start_date < max(.data$period_start_date, na.rm = TRUE))
  # we sort and number the starts of the periods
  incidencePrevalenceStart <- incidencePrevalenceStart %>%
    dbplyr::window_order(.data$period_start_date) %>%
    dplyr::mutate(index = dplyr::row_number()) %>%
    dplyr::ungroup() %>%
    dplyr::compute()

  # BLOC 5 obtain the end of the periods where the person may be exposed
  # A period ends at the previous day when an exposure starts
  incidencePrevalenceEnd <- incidencePrevalenceCohort %>%
    dplyr::select(
      "cohort_definition_id", "person_id",
      "drug_exposure_start_date"
    ) %>%
    dplyr::distinct() %>%
    dplyr::mutate(cohort_end_date = as.Date(dbplyr::sql(
      sql_add_days(
        CDMConnector::dbms(attr(cdm, "dbcon")), -1,
        "drug_exposure_start_date"
      )
    ))) %>%
    dplyr::select(-"drug_exposure_start_date") %>%
  # or at the end of an exposure
    incidencePrevalenceEnd <- incidencePrevalenceEnd %>%
    dplyr::union(
      incidencePrevalenceCohort %>%
        dplyr::select("cohort_definition_id", "person_id",
                      "period_end_date" = "drug_exposure_end_date"
        )
    )
  # we eliminate repeated ends:
  incidencePrevalenceEnd <- incidencePrevalenceEnd %>%
    dplyr::distinct()
  # now we have the starts of the periods we eliminate the first end
  # (previous day of the first exposure) for each person in each cohort
  incidencePrevalenceEnd <- incidencePrevalenceEnd %>%
    dplyr::group_by(.data$cohort_definition_id, .data$person_id) %>%
    dplyr::filter(
      .data$period_end_date > min(.data$period_end_date, na.rm = TRUE))
  # we sort and number the ends of the periods
  incidencePrevalenceEnd <- incidencePrevalenceEnd %>%
    dbplyr::window_order(.data$period_end_date) %>%
    dplyr::mutate(index = dplyr::row_number()) %>%
    dplyr::ungroup() %>%
    dplyr::compute()

  # BLOC 6 obtain the exposed periods
  # Start and end dates are joined with the exposures
  incidencePrevalenceCohort <- incidencePrevalenceStart %>%
    dplyr::inner_join(incidencePrevalenceEnd,
      by = c("cohort_definition_id", "person_id", "index")
    ) %>%
    dplyr::inner_join(incidencePrevalenceCohort,
      by = c("cohort_definition_id", "person_id")
    )
  # we mark the empty periods as gaps
  incidencePrevalenceCohort <- incidencePrevalenceCohort %>%
    dplyr::group_by(.data$cohort_definition_id, .data$person_id, .data$index) %>%
    dplyr::mutate(gap = dplyr::if_else(sum(
      .data$drug_exposure_start_date <= .data$period_start_date &
        .data$drug_exposure_end_date >= .data$period_end_date
    ) == 0, 1, 0)) %>%
    dplyr::ungroup() %>%
    dplyr::compute()
  # gaps smaller than gapEra are not marked as gaps
  # I AM HERE!
  incidencePrevalenceCohort <- incidencePrevalenceCohort %>%
    dplyr::filter(.data$gap == 0) %>%
    dplyr::union_all(
      incidencePrevalenceCohort %>%
        dplyr::filter(.data$gap == 1) %>%
        dplyr::mutate(gap = dplyr::if_else(
          dbplyr::sql(SqlUtilities::sqlDiffDays())
        ))
    )
    dplyr::filter() %>%
    dplyr::filter() %>%
    dplyr::select(
      "cohort_definition_id", "person_id", "cohort_start_date",
      "cohort_end_date"
    ) %>%
    dplyr::group_by(.data$cohort_definition_id, .data$person_id) %>%
    dbplyr::window_order(.data$cohort_end_date) %>%
    dplyr::mutate(index = dplyr::row_number()) %>%
    dplyr::ungroup() %>%
    dplyr::compute()

  # Now we want to compute the different eras
  incidencePrevalenceCohort <- incidencePrevalenceCohort %>%
    dplyr::left_join(
      incidencePrevalenceCohort %>%
        dplyr::mutate(index = .data$index + 1) %>%
        dplyr::rename("prev_exposure_end" = "cohort_end_date") %>%
        dplyr::select(
          "cohort_definition_id", "person_id", "index",
          "prev_exposure_end"
        ),
      by = c("cohort_definition_id", "person_id", "index")
    ) %>%
    dplyr::mutate(era_index = dplyr::if_else(
      is.na(.data$prev_exposure_end),
      1,
      dplyr::if_else(
        dbplyr::sql(SqlUtilities::datediff(
          "prev_exposure_end", "cohort_start_date"
        )) - 1 <= .env$gapEra,
        0,
        1
      )
    )) %>%
    dplyr::group_by(.data$cohort_definition_id, .data$person_id) %>%
    dbplyr::window_order(.data$index) %>%
    dplyr::mutate(era_group = cumsum(.data$era_index)) %>%
    dplyr::group_by(
      .data$cohort_definition_id, .data$person_id,
      .data$era_group
    ) %>%
    dplyr::summarise(
      cohort_start_date = min(.data$cohort_start_date, na.rm = TRUE),
      cohort_end_date = max(.data$cohort_end_date, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::select("cohort_definition_id",
      "subject_id" = "person_id",
      "cohort_start_date", "cohort_end_date"
    ) %>%
    dplyr::compute()

  # instantiate the cohorts
  cdm <-
    SqlUtilities::computePermanent(
      incidencePrevalenceCohort,
      incidencePrevalenceCohortName,
      schema = attr(cdm, "write_schema"),
      overwrite = overWrite
    )

  # write the equivalence csv
  write.csv(
    conceptSets,
    file = here::here(conceptSetFolder, "equivalence.csv"),
    row.names = FALSE
  )

  # return cdm
  return(cdm)
}

#' Function to read the concept sets and export a tibble with
#' cohort_definition_id and drug_concept_id with the list of drug_concept_id
#' included in each concept set
#' @noRd
readConceptSets <- function(conceptSets) {
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
        cohort_definition_id = .env$conceptSets$cohort_definition_id[k]
      )
    if (k == 1) {
      conceptList <- conceptSet
    } else {
      conceptList <- rbind(conceptList, conceptSet)
    }
  }
  conceptList <- conceptList %>%
    dplyr::select(
      "cohort_definition_id",
      "concept_id" = "CONCEPT_ID",
      "is_excluded" = "isExcluded",
      "include_descendants" = "includeDescendants"
    )
  return(conceptList)
}

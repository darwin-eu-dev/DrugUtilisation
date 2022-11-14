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

#' It instantiates the cohorts and their supplementary information
#' (cohorts_info) for the DUS study
#'
#' @param cdm 'cdm' object created with CDMConnector::cdm_from_con(). It must
#' must contain at least 'drug_exposure', 'drug_strength' and
#' observation_period' tables. The 'cdm' object must contain the
#' 'write_schema' as attribute and  the user should have permission to write on
#' it. It is a compulsory input, no default value is provided.
#' @param ingredientConceptId Ingredient OMOP concept that we are interested for
#' the study. It is a compulsory input, no default value is provided.
#' @param specifications Tibble with one to three columns. First column
#' (drug_concept_id) contains the OMOP drug concept id of the drugs of interest.
#' The first column is compulsory if the argument is provided. Second column
#' (default_duration) is only required if 'imputeDuration' = TRUE, then zero,
#' negative or outside range ('durationRange') durations are imputed by the
#' provided value. Third column (default_daily_dose) is only required if
#' 'imputeDailyDose' = TRUE, then negative or outside range ('dailyDoseRange)
#' daily doses are imputed by the provided value. DrugExposureDiagnostics is a
#' package that can be used to generate the specifications file. If it is NULL
#' all drug concept ids that contain the provided ingredientConceptId are
#' considered, no imputation is allowed in this case. 'drug_strength' table is
#' used to determine which are the drug_concept_id that contain a certain
#' ingredient. By default: NULL.
#' @param studyStartDate Minimum date where the incident exposed eras should
#' start to be considered. Only incident exposed eras larger than StudyStartDate
#' are allowed. If it is NULL no restriction is applied. By default: NULL.
#' @param studyEndDate Maximum date where the incident exposed eras should
#' start to be considered. Only incident exposed eras before StudyEndDate
#' are allowed. If it is NULL no restriction is applied. By default: NULL.
#' @param summarizeMode Choice on how to summarize the exposures. There are
#' three options:
#' "FixedTime" each individual is followed the exact same number of days
#' specified in 'studyTime' argument.
#' "AllEras" we summarize the output will be a summary of the exposed eras of
#' each individual. Each individual can contribute multiple times.
#' "FirstEra" we only consider the first observable era of each individual. In
#' this case each individual can not contribute with multiple rows.
#' By default: "AllEras".
#' @param studyTime Time period after first exposure where we summarize the
#' ingredient of interest. Argument only considered if 'summarizeMode' =
#' "FixedTime". No default value is provided.
#' @param cohortEntryPriorHistory Minimum number of days of prior history
#' (observation time) required for the incident eras to be considered. By
#' default: 180.
#' @param gapEra Number of days between two continuous exposures to be
#' considered in the same era. By default: 180.
#' @param eraJoinMode How two different continuous exposures are joined in an
#' era. There are four options:
#' "Zero" the exposures are joined considering that the period between both
#' continuous exposures the subject is treated with a daily dose of zero. The
#' time between both exposures contributes to the total exposed time.
#' "Join" the exposures are joined considering that the period between both
#' continuous exposures the subject is treated with a daily dose of zero. The
#' time between both exposures does not contribute to the total exposed time.
#' "Previous" the exposures are joined considering that the period between both
#' continuous exposures the subject is treated with the daily dose of the
#' previous subexposure. The time between both exposures contributes to the
#' total exposed time.
#' "Subsequent" the exposures are joined considering that the period between
#' both continuous exposures the subject is treated with the daily dose of the
#' subsequent subexposure. The time between both exposures contributes to the
#' total exposed time.
#' By default: "Previous".
#' @param overlapMode How the overlapping between two exposures that do not
#' start on the same day is solved inside a subexposure. There are five possible
#'  options:
#' "Previous" the considered daily_dose is the one of the earliest exposure.
#' "Current" the considered daily_dose is the one of the new exposure that
#' starts in that subexposure.
#' "Minimum" the considered daily_dose is the minimum of all of the exposures in
#' the subexposure.
#' "Maximum" the considered daily_dose is the maximum of all of the exposures in
#' the subexposure.
#' "Sum" the considered daily_dose is the sum of all the exposures present in
#' the subexposure.
#' By default: "Previous".
#' @param sameIndexMode How the overlapping between two exposures that start on
#' the same day is solved inside a subexposure. There are five possible options:
#' "Minimum" the considered daily_dose is the minimum of all of the exposures in
#' the subexposure.
#' "Maximum" the considered daily_dose is the maximum of all of the exposures in
#' the subexposure.
#' "Sum" the considered daily_dose is the sum of all the exposures present in
#' the subexposure.
#' By default: "Sum".
#' @param imputeDuration Whether the duration should be imputed (imputeDuration
#' = TRUE) or the exposure should be dismissed (imputeDuration = FALSE) when the
#' duration is negative, zero or outside durationRange. By default: FALSE.
#' @param imputeDailyDose Whether the daily_dose should be imputed
#' (imputeDailyDose = TRUE) or the exposure should be dismissed
#' (imputeDailyDose = FALSE) when the daily_dose is negative or outside
#' dailyDoseRange. By default: FALSE.
#' @param durationRange Range between the duration must be comprised. It should
#' be a numeric vector of length two, with no NAs and the first value should be
#' equal or smaller than the second one. It is only required if imputeDuration
#' = TRUE. If NULL no restrictions are applied. By default: NULL.
#' @param dailyDoseRange Range between the daily_dose must be comprised. It
#' should be a numeric vector of length two, with no NAs and the first value
#' should be equal or smaller than the second one. It is only required if
#' imputeDailyDose = TRUE. If NULL no restrictions are applied. By default:
#' NULL.
#' @param drugUtilisationCohortName Name of the table that will be instantiated
#' in the database in the 'write_schema'. By default: "dus_cohort".
#' @param overwrite Whether the cohort overwrites the table if it already
#' exists. By default: TRUE.
#' @param instantiateInfo Whether the dosage summary is instantiated in the
#' database or it is maintained as temporal table. The name of this table is the
#' one provided in drugUtilisationCohortName adding "_dose_info" at the end, so by
#' default its name is going to be "dus_cohort_dose_info". By default: FALSE.
#' @param verbose Whether the code should print the process.
#'
#' TERMINOLOGY
#' - exposure: we refer to exposure to a row in the drug_exposure table of the
#' cdm object.
#' - subexposure: period of time were the number of exposures that a person is
#' exposed to does not change.
#' - continuous exposure: period of time where the individual is exposed with no
#' interruption. This period can be comprised by multiple subexposures and
#' exposure.
#' - exposed gap: period of time that the individual is not exposed (there are
#' no exposures in the time period), but its length is smaller or equal than
#' gapEra parameter value so the individual is considered to be exposed.
#' - non exposed gap: period of time that the individual is not exposed (there are
#' no exposures in the time period) and its length is larger than gapEra
#' parameter value
#' - era: period of time the individual is considered exposed. It is formed by
#' continuous exposures and exposed gaps.
#' See this picture for a descriptive visualization of the terms previously
#' described:
#' \if{html}{
#'   \out{<div style="text-align: center">}\figure{exposures_definitions.png}{options: style="width:750px;max-width:75\%;"}\out{</div>}
#' }
#' \if{latex}{
#'   \out{\begin{center}}\figure{myfigure.png}\out{\end{center}}
#' }
#'
#' @return The function returns the 'cdm' object with the created tables as
#' references of the object.
#' @export
#'
#' @examples
instantiateDrugUtilisationCohorts <- function(cdm,
                                              ingredientConceptId,
                                              specifications = NULL,
                                              studyStartDate = NULL,
                                              studyEndDate = NULL,
                                              summarizeMode = "AllEras",
                                              studyTime,
                                              cohortEntryPriorHistory = 180,
                                              gapEra = 30,
                                              eraJoinMode = "Previous",
                                              overlapMode = "Previous",
                                              sameIndexMode = "Sum",
                                              imputeDuration = FALSE,
                                              imputeDailyDose = FALSE,
                                              durationRange = NULL,
                                              dailyDoseRange = NULL,
                                              drugUtilisationCohortName = "dus_cohort",
                                              overwrite = TRUE,
                                              instantiateInfo = FALSE,
                                              verbose = FALSE) {
  errorMessage <- checkmate::makeAssertCollection()

  # Check cdm
  checkmate::assertClass(cdm, classes = "cdm_reference", add = errorMessage)

  # To check that cdm contains the desired tables in the desired format

  # check ingredient concept id is a count
  checkmate::assertCount(
    ingredientConceptId,
    add = errorMessage
  )

  # try to put specification as tibble
  if (!is.null(specifications)) {
    try(specifications <- dplyr::as_tibble(specifications))
  }

  # check specifications
  checkmate::assertTibble(
    specifications,
    min.cols = 1,
    min.rows = 1,
    null.ok = TRUE,
    add = errorMessage
  )
  if (!is.null(specifications)) {
    checkmate::assertTRUE(
      "drug_concept_id" %in% colnames(specifications),
      add = errorMessage
    )
  }
  if (imputeDuration == TRUE) {
    checkmate::assertTRUE(
      "default_duration" %in% colnames(specifications),
      add = errorMessage
    )
    if ("default_duration" %in% colnames(specifications)) {
      checkmate::assertTRUE(
        all(!is.na(specifications$default_duration)),
        add = errorMessage
      )
    }
  }
  if (imputeDailyDose == TRUE) {
    checkmate::assertTRUE(
      "default_daily_dose" %in% colnames(specifications),
      add = errorMessage
    )
    if ("default_daily_dose" %in% colnames(specifications)) {
      checkmate::assertTRUE(
        all(!is.na(specifications$default_daily_dose)),
        add = errorMessage
      )
    }
  }

  # check studyStartDate is an integer
  checkmate::assertDate(
    studyStartDate,
    add = messageStore,
    null.ok = TRUE
  )

  # check studyEndDate is an integer
  checkmate::assertDate(
    studyEndDate,
    add = messageStore,
    null.ok = TRUE
  )

  # check summarize mode
  checkmate::assertChoice(
    summarizeMode,
    choices = c("AllEras", "FirstEra", "FixedTime")
  )

  # check studyTime
  if (summarizeMode == "FixedTime") {
    checkmate::assertCount(
      studyTime,
      positive = TRUE,
      add = errorMessage
    )
  }

  # check cohortEntryPriorHistory is an integer
  checkmate::assertCount(
    cohortEntryPriorHistory,
    add = errorMessage
  )

  # check gapEra is an integer
  checkmate::assertCount(
    gapEra,
    add = errorMessage
  )

  # check eraJoinMode
  checkmate::assertChoice(
    eraJoinMode,
    choices = c("Previous", "Subsequent", "Zero", "Join"),
    add = errorMessage
  )

  # check overlapMode
  checkmate::assertChoice(
    overlapMode,
    choices = c("Previous", "Current", "Minimum", "Maximum", "Sum"),
    add = errorMessage
  )

  # check sameIndexMode
  checkmate::assertChoice(
    sameIndexMode,
    choices = c("Minimum", "Maximum", "Sum"),
    add = errorMessage
  )

  # check imputeDuration is a character
  checkmate::assertLogical(
    imputeDuration,
    add = messageStore
  )

  # check imputeDailyDose is a character
  checkmate::assertLogical(
    imputeDailyDose,
    add = messageStore
  )

  # check durationRange
  checkmate::assertNumeric(
    durationRange,
    len = 2,
    null.ok = TRUE,
    add = errorMessage
  )
  if (is.null(durationRange)) {
    durationRange <- c(NA, NA)
  }
  if (sum(is.na(durationRange)) == 0) {
    checkmate::assertTRUE(
      durationRange[1] <= durationRange[2],
      add = errorMessage
    )
  }

  # check dailyDoseRange
  checkmate::assertNumeric(
    dailyDoseRange,
    len = 2,
    null.ok = TRUE,
    add = errorMessage
  )
  if (is.null(dailyDoseRange)) {
    dailyDoseRange <- c(NA, NA)
  }
  if (sum(is.na(dailyDoseRange)) == 0) {
    checkmate::assertTRUE(
      dailyDoseRange[1] <= dailyDoseRange[2],
      add = errorMessage
    )
  }

  # check drugUtilisationCohortName is a character
  checkmate::assertCharacter(
    drugUtilisationCohortName,
    add = messageStore
  )

  # check overwrite
  checkmate::assertLogical(
    overwrite,
    add = errorMessage
  )

  # assert that the cohort can be instantiated (warning if overwrite = TRUE,
  # error if overwrite = FALSE)

  # check instantiateInfo
  checkmate::assertLogical(
    instantiateInfo,
    add = errorMessage
  )
  if (isTRUE(instantiateInfo)) {
    # assert that the second table can be instantiated (warning if
    # overwrite = TRUE, error if overwrite = FALSE)
  }

  # check verbose
  checkmate::assertLogical(verbose, add = errorMessage)

  # THIS CHECKS SHOULD BE SIMPLIFIED WHEN CHECKS IN CDMConnector:: are allowed
  # check drug exposure table exist
  cdm_drug_exp_exists <- inherits(cdm$drug_exposure, "tbl_dbi")
  checkmate::assertTRUE(cdm_drug_exp_exists, add = errorMessage)
  if (!isTRUE(cdm_drug_exp_exists)) {
    errorMessage$push(
      "- table `drug exposure` is not found"
    )
  }
  # check drug strength table exist
  cdm_drug_str_exists <- inherits(cdm$drug_strength, "tbl_dbi")
  checkmate::assertTRUE(cdm_drug_str_exists, add = errorMessage)
  if (!isTRUE(cdm_drug_str_exists)) {
    errorMessage$push(
      "- table `drug strength` is not found"
    )
  }

  checkmate::reportAssertions(collection = errorMessage)

  if (is.null(specifications)) {
    specifications <- cdm[["drug_strength"]] %>%
      dplyr::filter(.data$ingredient_concept_id == .env$ingredientConceptId) %>%
      dplyr::select("drug_concept_id") %>%
      dplyr::collect()
  }

  # get sql dialect of the database
  dialect <- CDMConnector::dbms(attr(cdm, "dbcon"))
  # get the name of the info table
  drugUtilisationTableDataName <- paste0(drugUtilisationCohortName, "_info")

  # if specifications is not specified get all drug concept ids that contain a
  # certain ingredient in drug_strangth table
  if (is.null(specifications)) {
    specifications <- cdm[["drug_strength"]] %>%
      dplyr::filter(
        ingredient_concept_id == ingredientConceptId
      ) %>%
      select("drug_concept_id") %>%
      collect()
  }

  # select the specification variables that we are interested in
  specifications <- specifications %>%
    dplyr::select(
      "drug_concept_id",
      tidyselect::matches("default_duration"),
      tidyselect::matches("default_daily_dose")
    )

  # subset drug_exposure and only get the drug concept ids that we are
  # interested in.
  drugUtilisationCohort <- cdm[["drug_exposure"]] %>%
    dplyr::select(
      "person_id", "drug_concept_id", "drug_exposure_start_date",
      "drug_exposure_end_date", "quantity", "drug_exposure_id"
    ) %>%
    dplyr::inner_join(
      specifications,
      by = "drug_concept_id",
      copy = TRUE
    ) %>%
    dplyr::compute()

  # if the summarize mode is 'FixedTime' get only the exposures in the certain
  # period of time after the first incident case.
  if (summarizeMode == "FixedTime") {
    # first get the first exposure for each person
    drugUtilisationCohort <- drugUtilisationCohort %>%
      dplyr::group_by(.data$person_id) %>%
      dplyr::mutate(
        drug_exposure_end_date_max = min(
          .data$drug_exposure_start_date,
          na.rm = TRUE
        )
      ) %>%
      dplyr::ungroup() %>%
      # compute its maximum possible end date
      dplyr::mutate(
        drug_exposure_end_date_max = as.Date(dbplyr::sql(sql_add_days(
          CDMConnector::dbms(attr(cdm, "dbcon")),
          studyTime - 1,
          "drug_exposure_end_date_max"
        )))
      ) %>%
      # get only the exposures that start before the end date
      dplyr::filter(
        .data$drug_exposure_start_date <= .data$drug_exposure_end_date_max
      )
  }

  # compute the number of days exposed according to:
  # days_exposed = end - start + 1
  drugUtilisationCohort <- drugUtilisationCohort %>%
    dplyr::mutate(days_exposed = dbplyr::sql(sqlDiffDays(
      CDMConnector::dbms(attr(cdm, "dbcon")),
      "drug_exposure_start_date",
      "drug_exposure_end_date"
    )) + 1)

  # impute or eliminate the exposures that duration does not fulfill the
  # conditions ( <=0; <durationRange[1]; >durationRange[2])
  drugUtilisationCohort <- imputeVariable(
    x = drugUtilisationCohort,
    variableName = "days_exposed",
    impute = imputeDuration,
    lowerBound = durationRange[1],
    upperBound = durationRange[2],
    imputeValueName = "default_duration",
    allowZero = FALSE
  )

  # Compute the new drug_exposure_end_date based on the imputed new duration
  if (imputeDuration == TRUE) {
    drugUtilisationCohort <- drugUtilisationCohort %>%
      dplyr::mutate(
        drug_exposure_end_date = .data$drug_exposure_start_date +
          .data$days_exposed - 1
      ) %>%
      dplyr::compute()
  }

  # We compute the daily dose using drugExposureDiagnostics function (to be
  # updated in the next interation as drugExposureDiagnostics does not work as
  # we want)
  drugUtilisationCohort <- computeDailyDose(
    table = drugUtilisationCohort,
    cdm = cdm,
    ingredient_concept_id = ingredientConceptId,
    verbose = verbose
  )

  # if summarizeMode == "FixedTime" we cut the exposures that end later than the
  # first_exposure_date + studyTime - 1 (drug_exposure_end_max)
  if (summarizeMode == "FixedTime") {
    drugUtilisationCohort <- drugUtilisationCohort %>%
      dplyr::mutate(
        drug_exposure_end_date = dplyr::if_else(
          .data$drug_exposure_end_date > .data$drug_exposure_end_date_max,
          .data$drug_exposure_end_date_max,
          .data$drug_exposure_end_date,
        )
      )
  }

  # impute or eliminate the exposures that daily_dose does not fulfill the
  # conditions ( <0; <dailyDoseRange[1]; >dailyDoseRange[2])
  drugUtilisationCohort <- imputeVariable(
    x = drugUtilisationCohort,
    variableName = "daily_dose",
    impute = imputeDuration,
    lowerBound = dailyDoseRange[1],
    upperBound = dailyDoseRange[2],
    imputeValueName = "default_daily_dose",
    allowZero = TRUE
  )

  # get only the variables that we are interested in
  drugUtilisationCohort <- drugUtilisationCohort %>%
    dplyr::select(
      "person_id", "drug_exposure_id", "drug_exposure_start_date",
      "drug_exposure_end_date", "daily_dose"
    ) %>%
    dplyr::compute()

  # split the exposures into subexposures (a subexposure is a period where the
  # number of exposures that the person is exposed to does not change over time)
  # it also adds the continuous_subexposure_id to each of the subexposures
  drugUtilisationCohort <- getPeriods(
    x = drugUtilisationCohort,
    dialect = dialect,
    verbose = verbose
  )

  # joinExposures function will compute the gaps and see if they are exposed or
  # unexposed. Only exposed gaps are going to be shown. It will add two
  # variables era_id and gap. Gap = 1 will identify that subexposure as an
  # exposed gap. Era_id will identify all the subexposures that form an era
  drugUtilisationCohort <- joinExposures(
    x = drugUtilisationCohort,
    gapEra = gapEra,
    eraJoinMode = eraJoinMode,
    sameIndexMode = sameIndexMode,
    dialect = dialect,
    verbose = verbose
  )

  # continuousExposures function gets the considered dose for each of the
  # subexposures, solving the overlap between different exposures if multiple
  # exposures contribute to the same subexposure. This function also summarizes
  # the data at summarizeMode level.
  drugUtilisationCohort <- continuousExposures(
    x = drugUtilisationCohort,
    overlapMode = overlapMode,
    sameIndexMode = sameIndexMode,
    summarizeMode = summarizeMode,
    dialect = dialect,
    verbose = verbose
  )

  drugUtilisationCohort <- drugUtilisationCohort %>%
    dplyr::rename("subject_id" = "person_id") %>%
    dplyr::mutate(cohort_definition_id = 1)


  if (!is.null(studyStartDate)) {
    drugUtilisationCohort <- drugUtilisationCohort %>%
      dplyr::filter(
        .data$cohort_start_date >= studyStartDate
      ) %>%
      dplyr::compute()
  }

  if (!is.null(studyEndDate)) {
    drugUtilisationCohort <- drugUtilisationCohort %>%
      dplyr::filter(
        .data$cohort_start_date <= studyEndDate
      ) %>%
      dplyr::compute()
  }

  # priorDaysCohort <- CohortProfiles::getPriorHistoryCohortEntry(
  #   cdm,
  #   drugUtilisationCohortName
  # )
  # drugUtilisationCohort <- drugUtilisationCohort %>%
  #   dplyr::left_join(
  #     priorDaysCohort,
  #     by = c(
  #       "subject_id", "cohort_definition_id",
  #       "cohort_start_date", "cohort_end_date"
  #     )
  #   ) %>%
  #   dplyr::filter(.data$number_of_days >= .env$cohortEntryPriorHistory) %>%
  #   dplyr::compute()

  cdm[[drugUtilisationCohortName]] <- SqlUtilities::computePermanent(
    drugUtilisationCohort %>%
      dplyr::select(
        "cohort_definition_id", "subject_id", "cohort_start_date",
        "cohort_end_date"
      ),
    drugUtilisationCohortName,
    schema = attr(cdm, "write_schema"),
    overwrite = overwrite
  )

  if (instantiateInfo == TRUE) {
    cdm[[drugUtilisationTableDataName]] <- SqlUtilities::computePermanent(
      drugUtilisationCohort,
      drugUtilisationTableDataName,
      schema = attr(cdm, "write_schema"),
      overwrite = overwrite
    )
  } else {
    cdm[[drugUtilisationTableDataName]] <- drugUtilisationCohort
  }

  return(cdm)
}

#' Impute or eliminate values under a certain conditions
#'
#' @param x x
#' @param variableName variableName
#' @param impute impute
#' @param lowerBound lowerBound
#' @param upperBound upperBound
#' @param imputeValueName imputeValueName
#' @param allowZero allowZero
#'
#' @noRd
imputeVariable <- function(x,
                           variableName,
                           impute,
                           lowerBound,
                           upperBound,
                           imputeValueName,
                           allowZero) {
  # rename the variable of interest to variable
  x <- x %>%
    dplyr::rename("variable" = .env$variableName)
  # identify (as impute = 1) the values that are smaller than zero (smaller or
  # equal than zero when allowZero = FALSE)
  if (isTRUE(allowZero)) {
    x <- x %>%
      dplyr::mutate(impute = dplyr::if_else(
        is.na(.data$variable) | .data$variable < 0,
        1,
        0
      ))
  } else {
    x <- x %>%
      dplyr::mutate(impute = dplyr::if_else(
        is.na(.data$variable) | .data$variable <= 0,
        1,
        0
      ))
  }
  # identify (as impute = 1) the values smaller than lower bound
  if (!is.na(lowerBound)) {
    x <- x %>%
      dplyr::mutate(impute = dplyr::if_else(
        .data$variable < .env$lowerBound,
        1,
        .data$impute
      ))
  }
  # identify (as impute = 1) the values greater than upper bound
  if (!is.na(upperBound)) {
    x <- x %>%
      dplyr::mutate(impute = dplyr::if_else(
        .data$variable > .env$upperBound,
        1,
        .data$impute
      ))
  }
  # if impute is false then all values with impute = 1 are not considered
  if (isFALSE(impute)) {
    x <- x %>%
      dplyr::filter(.data$impute == 0)
  } else {
    # if impute id true then all values with impute = 1 are imputed according to
    # the imputeValue column
    x <- x %>%
      dplyr::rename("imputeValue" = .env$imputeValueName) %>%
      dplyr::mutate(variable = dplyr::if_else(
        .data$impute == 1,
        .data$imputeValue,
        .data$variable
      )) %>%
      dplyr::rename(!!imputeValueName := "imputeValue")
  }
  # impute value id eliminated and variable name is renamed to its original name
  x <- x %>%
    dplyr::select(-"impute") %>%
    dplyr::rename(!!variableName := "variable")

  return(x)
}


#' Explain function
#'
#' @param x table
#' @param dialect dialect
#' @param verbose verbose
#'
#' @noRd
getPeriods <- function(x, dialect, verbose) {
  # compute the start of subexposures
  x_start <- x %>%
    # the start of a subexposure can be the start of an exposure
    dplyr::select("person_id", "drug_exposure_start_date") %>%
    dplyr::distinct() %>%
    dplyr::rename("start_interval" = "drug_exposure_start_date") %>%
    dplyr::union(
      # or the day after that an exposure ends
      x %>%
        dplyr::select("person_id", "drug_exposure_end_date") %>%
        dplyr::distinct() %>%
        dplyr::mutate(
          start_interval = as.Date(dbplyr::sql(sql_add_days(
            dialect,
            1,
            "drug_exposure_end_date"
          )))
        ) %>%
        dplyr::select(-"drug_exposure_end_date")
    ) %>%
    # we eliminate the last start_interval per person as it corresponds to the
    # day after the end of the last subexposure
    dplyr::group_by(.data$person_id) %>%
    dplyr::filter(
      .data$start_interval < max(.data$start_interval, na.rm = TRUE)
    ) %>%
    # sort the subexposures by start date
    dbplyr::window_order(.data$start_interval) %>%
    # number the sorted subexsposures within each person
    dplyr::mutate(subexposure_id = dplyr::row_number()) %>%
    dplyr::ungroup() %>%
    dplyr::compute()

  # compute the end of subexposures
  x_end <- x %>%
    # the end of a subexposure can be the day before the start of an exposure
    dplyr::select("person_id", "drug_exposure_start_date") %>%
    dplyr::distinct() %>%
    dplyr::mutate(
      end_interval = as.Date(dbplyr::sql(sql_add_days(
        dialect,
        -1,
        "drug_exposure_start_date"
      )))
    ) %>%
    dplyr::select(-"drug_exposure_start_date") %>%
    # or the day that an exposure ends
    dplyr::union(
      x %>%
        dplyr::select("person_id", "drug_exposure_end_date") %>%
        dplyr::distinct() %>%
        dplyr::rename("end_interval" = "drug_exposure_end_date")
    ) %>%
    # we eliminate the first end per person as it is the day before the start of
    # the first exposure
    dplyr::group_by(.data$person_id) %>%
    dplyr::filter(
      .data$end_interval > min(.data$end_interval, na.rm = TRUE)
    ) %>%
    # sort the subexposures by end date
    dbplyr::window_order(.data$end_interval) %>%
    # number the sorted subexsposures within each person
    dplyr::mutate(subexposure_id = dplyr::row_number()) %>%
    dplyr::ungroup() %>%
    dplyr::compute()

  # compute the subexposure intervals joining start and end dates
  x_intervals <- x_start %>%
    dplyr::inner_join(
      x_end,
      by = c("person_id", "subexposure_id")
    ) %>%
    # compute the number of days in each subexposure
    dplyr::mutate(days_exposed = dbplyr::sql(sqlDiffDays(
      CDMConnector::dbms(attr(cdm, "dbcon")),
      "start_interval",
      "end_interval"
    )) + 1)

  # we join the exposures with the overlapping periods and we only consider the
  # exposures that contribute to each overlapping period
  x_intervals <- x_intervals %>%
    dplyr::inner_join(x, by = "person_id") %>%
    dplyr::filter(.data$drug_exposure_start_date <= .data$start_interval) %>%
    dplyr::filter(.data$drug_exposure_end_date >= .data$end_interval) %>%
    dplyr::compute()

  # the overlapping subgroups are grouped into groups of continuous exposure,
  # see the documentation for a more detailed explanation of the difference
  # between overlapping group and subgroups
  x_intervals_ids <- x_intervals %>%
    # select distinct person and subexposure_id
    dplyr::select("person_id", "subexposure_id") %>%
    dplyr::distinct() %>%
    # join with the subsequent one, the ones that they have a previous
    # subexposure will have index_continuous_exposure = 0, the other ones that
    # not will have index_continuous_exposure = NA
    dplyr::left_join(
      x_intervals %>%
        dplyr::select("person_id", "subexposure_id") %>%
        dplyr::distinct() %>%
        dplyr::mutate(subexposure_id = .data$subexposure_id + 1) %>%
        dplyr::mutate(index_continuous_exposure = 0),
      by = c("person_id", "subexposure_id")
    ) %>%
    # if index_continuous exposure = NA will be turn to 1
    dplyr::mutate(index_continuous_exposure = dplyr::if_else(
      is.na(.data$index_continuous_exposure),
      1,
      .data$index_continuous_exposure
    )) %>%
    # we group by person
    dplyr::group_by(.data$person_id) %>%
    # we sort by subexposure_id
    dbplyr::window_order(.data$subexposure_id) %>%
    # index_continuous_exposure is 0 if they have a previous subexposure or 1 if
    # they do not have so doing cumsum all of the exposures in the same
    # continuous exposure wull have the same continuous_exposure_id
    dplyr::mutate(
      continuous_exposure_id = cumsum(.data$index_continuous_exposure)
    ) %>%
    dplyr::ungroup() %>%
    # x_intervals_id will contain the continuous_exposure_id for each person_id
    # and subexposure_id
    dplyr::select("person_id", "subexposure_id", "continuous_exposure_id") %>%
    dplyr::compute()

  # we add the continuous_exposure_id to the periods
  x_intervals <- x_intervals %>%
    dplyr::left_join(
      x_intervals_ids,
      by = c("person_id", "subexposure_id")
    ) %>%
    dplyr::compute()

  return(x_intervals)
}

#' Explain function
#'
#' @param x table
#' @param gapEra gapEra
#' @param eraJoinMode eraJoinMode
#' @param sameIndexMode sameIndexMode
#' @param dialect dialect
#' @param verbose verbose
#'
#' @noRd
joinExposures <- function(x,
                          gapEra,
                          eraJoinMode,
                          sameIndexMode,
                          dialect,
                          verbose) {
  # Get the start of gaps. The starts of gaps is the day after the end of a
  # continuous exposure.
  gap_start <- x %>%
    dplyr::group_by(.data$person_id) %>%
    # we do not consider the last continuous exposure as this wont be a gap
    dplyr::filter(
      .data$continuous_exposure_id < max(
        .data$continuous_exposure_id,
        na.rm = TRUE
      )
    ) %>%
    # get the end of the continuous_exposure_id
    dplyr::group_by(.data$person_id, .data$continuous_exposure_id) %>%
    dplyr::summarise(
      start_interval = max(.data$end_interval, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::ungroup() %>%
    # we compute the day after the end of the continuous_exposure_id
    dplyr::mutate(
      start_interval = as.Date(dbplyr::sql(sql_add_days(
        dialect,
        1,
        "start_interval"
      )))
    ) %>%
    dplyr::compute()

  # Get the end of gaps. The ends of gaps is the day before the start of a
  # continuous exposure.
  gap_end <- x %>%
    dplyr::group_by(.data$person_id) %>%
    # we do not consider the first continuous exposure as this wont be a gap
    dplyr::filter(
      .data$continuous_exposure_id > min(
        .data$continuous_exposure_id,
        na.rm = TRUE
      )
    ) %>%
    # get the start of the continuous_exposure_id
    dplyr::group_by(.data$person_id, .data$continuous_exposure_id) %>%
    dplyr::summarise(
      end_interval = min(.data$start_interval, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::ungroup() %>%
    # we compute the day before the start of the continuous_exposure_id
    dplyr::mutate(
      end_interval = as.Date(dbplyr::sql(sql_add_days(
        dialect,
        -1,
        "end_interval"
      )))
    ) %>%
    # we substract one from continuous exposure id as the end of a gaps will be
    # the day before the start of the subsequent continuous exposure id
    dplyr::mutate(continuous_exposure_id = .data$continuous_exposure_id - 1) %>%
    dplyr::compute()

  # Get the gaps joining the end and start dates
  gap_period <- gap_start %>%
    dplyr::inner_join(
      gap_end,
      by = c("person_id", "continuous_exposure_id")
    ) %>%
    # compute the length of the gap
    dplyr::mutate(days_exposed = dbplyr::sql(sqlDiffDays(
      dialect,
      "start_interval",
      "end_interval"
    )) + 1) %>%
    # if the gap is larger than gapEra remove that gaps
    dplyr::filter(.data$days_exposed <= .env$gapEra) %>%
    # the other gaps are identified as exposed gaps
    dplyr::mutate(gap = 1)

  # Now we have to decide the daily_dose and days_exposed of the exposed gaps,
  # this will be decided according the eraJoinMode
  if (eraJoinMode == "Zero") {
    # if eraJoinMode is "zero" the daily dose is 0, days_exposed remain the gap
    # length
    gap_period <- gap_period %>%
      dplyr::mutate(daily_dose = 0)
  } else if (eraJoinMode == "Join") {
    # if eraJoinMode is "join" the daily_dose and the days_exposed are 0
    gap_period <- gap_period %>%
      dplyr::mutate(daily_dose = 0) %>%
      dplyr::mutate(days_exposed = 0)
  }

  # we have to get each gap its corresponding subexposure_id which will be the
  # number between the previous and the subsequent subexposure. So we will
  # compute the subexposure_id of the gaps as the previous subexposure id + 1.
  # We can join them because (for the moment) each gap has the
  # continuous_exposure_id of the previous continuous exposure_id.
  # subexposure_id table will contain the subexposure_id for each gap.
  subexposure_id <- gap_period %>%
    dplyr::select("person_id", "continuous_exposure_id") %>%
    dplyr::inner_join(
      # for each continuous exposure id
      x %>%
        dplyr::group_by(.data$person_id, .data$continuous_exposure_id) %>%
        # get the last subexposure_id
        dplyr::summarise(
          subexposure_id = max(.data$subexposure_id, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        # add one
        dplyr::mutate(subexposure_id = .data$subexposure_id + 1),
      by = c("person_id", "continuous_exposure_id")
    ) %>%
    dplyr::compute()
  # we add the subexposure id to each of the gaps
  gap_period <- gap_period %>%
    dplyr::inner_join(
      subexposure_id,
      by = c("person_id", "continuous_exposure_id")
    ) %>%
    # we remove continuous_exposure_id as the gaps are not goint to be part of a
    # continuous_exposure_id
    dplyr::select(-"continuous_exposure_id") %>%
    dplyr::compute()

  # if eraJoinMode is "Previous" or "Subsequent" we have to compute the daily
  # doses of the previous or subsequent exposures
  if (eraJoinMode == "Previous" | eraJoinMode == "Subsequent") {
    # if it is "Previous" we compute the daily_dose of the exposures in the
    # previous subexposure
    if (eraJoinMode == "Previous") {
      daily_dose <- gap_period %>%
        # get the exposures in the previous subexposure
        dplyr::select("person_id", "subexposure_id") %>%
        dplyr::inner_join(
          x %>%
            dplyr::select(
              "person_id", "subexposure_id", "drug_exposure_start_date",
              "daily_dose"
            ) %>%
            dplyr::mutate(subexposure_id = .data$subexposure_id + 1),
          by = c("person_id", "subexposure_id")
        ) %>%
        # compute the number of exposures in that subexposure
        dplyr::group_by(.data$person_id, .data$subexposure_id) %>%
        dplyr::mutate(number_in_group = dplyr::n()) %>%
        dplyr::ungroup() %>%
        dplyr::select(-"drug_exposure_start_date") %>%
        dplyr::compute()
    } else {
      # get the exposures in the subsequent subexposure
      daily_dose <- gap_period %>%
        dplyr::select("person_id", "subexposure_id") %>%
        dplyr::inner_join(
          x %>%
            dplyr::select(
              "person_id", "subexposure_id", "daily_dose"
            ) %>%
            dplyr::mutate(subexposure_id = .data$subexposure_id - 1),
          by = c("person_id", "subexposure_id")
        ) %>%
        # compute the number of exposures in that subexposure
        dplyr::group_by(.data$person_id, .data$subexposure_id) %>%
        dplyr::mutate(number_in_group = dplyr::n()) %>%
        dplyr::ungroup() %>%
        dplyr::compute()
    }
    # get the subexposures with multiple exposures (number_in_group > 1)
    daily_dose_multiple <- daily_dose %>%
      dplyr::filter(.data$number_in_group > 1) %>%
      dplyr::select(-"number_in_group") %>%
      dplyr::compute()
    # if there are subexposures with multiple exposures solve the conflict
    if (daily_dose_multiple %>% dplyr::tally() %>% dplyr::pull() > 0) {
      daily_dose_multiple <- daily_dose_multiple %>%
        dplyr::group_by(.data$person_id, .data$subexposure_id)
      # We compute the daily_dose based on the rules dictated by sameIndexMode
      if (sameIndexMode == "Sum") {
        # if sameIndexMode = "Sum" the daily dose is the sum of all the daily
        # doses in the subexposure
        daily_dose_multiple <- daily_dose_multiple %>%
          dplyr::summarise(
            daily_dose = sum(.data$daily_dose, na.rm = TRUE),
            .groups = "drop"
          )
      } else if (sameIndexMode == "Maximum") {
        # if sameIndexMode = "Maximum" the daily dose is the maximum of all the
        # daily doses in the subexposure
        daily_dose_multiple <- daily_dose_multiple %>%
          dplyr::summarise(
            daily_dose = max(.data$daily_dose, na.rm = TRUE),
            .groups = "drop"
          )
      } else if (sameIndexMode == "Minimum") {
        # if sameIndexMode = "Minimum" the daily dose is the minimum of all the
        # daily doses in the subexposure
        daily_dose_multiple <- daily_dose_multiple %>%
          dplyr::summarise(
            daily_dose = min(.data$daily_dose, na.rm = TRUE),
            .groups = "drop"
          )
      }
      daily_dose_multiple <- daily_dose_multiple %>%
        dplyr::compute()

      daily_dose <- daily_dose %>%
        dplyr::select(-"number_in_group") %>%
        # We eliminate the daily doses with multiple exposures
        dplyr::anti_join(
          daily_dose_multiple,
          by = c("person_id", "subexposure_id")
        ) %>%
        # We add the daily doses with the selected exposure from the multiple
        # exposures
        dplyr::union_all(daily_dose_multiple) %>%
        dplyr::compute()
    } else {
      daily_dose <- daily_dose %>%
        dplyr::select(-"number_in_group")
    }
    # We add the computed gap daily doses to the exposed gaps table
    gap_period <- gap_period %>%
      dplyr::inner_join(
        daily_dose,
        by = c("person_id", "subexposure_id")
      ) %>%
      dplyr::compute()
  }

  # We add  the gaps to the exposures table
  x <- x %>%
    dplyr::union_all(gap_period) %>%
    # A varibale to identify gap periods is added
    dplyr::mutate(gap = dplyr::if_else(is.na(.data$gap), 0, 1)) %>%
    dplyr::compute()

  # To add the era id in each subexposure_id we need to compute all the
  # subexposures_id and we are going to identify the eras as the consecutive
  # subexposures (the previous subexposure_id exist)
  # compute distinct person id and subexposure id
  era_id <- x %>%
    dplyr::select("person_id", "subexposure_id") %>%
    dplyr::distinct() %>%
    dplyr::compute()
  # left join with the subsequent subexposure
  era_id <- era_id %>%
    dplyr::left_join(
      era_id %>%
        dplyr::mutate(subexposure_id = .data$subexposure_id + 1) %>%
        dplyr::mutate(era_id = 0),
      by = c("person_id", "subexposure_id")
    ) %>%
    # era id is 1 if the previous subexposure does not exist
    dplyr::mutate(era_id = dplyr::if_else(is.na(era_id), 1, 0)) %>%
    dplyr::group_by(.data$person_id) %>%
    dbplyr::window_order(.data$subexposure_id) %>%
    # we compute the era id as the cumsum of the consecutive era_id
    dplyr::mutate(era_id = cumsum(.data$era_id)) %>%
    dplyr::ungroup() %>%
    dplyr::compute()
  # we add the era id to the current table
  x <- x %>%
    dplyr::inner_join(
      era_id,
      by = c("person_id", "subexposure_id")
    ) %>%
    dplyr::compute()

  return(x)
}

#' Explain function
#'
#' @param x table
#' @param overlapMode overlapMode
#' @param sameIndexMode sameIndexMode
#' @param verbose verbose
#'
#' @noRd
continuousExposures <- function(x,
                                overlapMode,
                                sameIndexMode,
                                summarizeMode,
                                dialect,
                                verbose) {

  # if we are only interested in the first era we only keep the first era
  if (summarizeMode == "FirstEra") {
    x <- x %>%
      dplyr::filter(.data$era_id == 1)
  }
  # get the groups variable to know at which level we are grouping
  if (summarizeMode == "FizedTime") {
    groups <- c("person_id")
  } else {
    groups <- c("person_id", "era_id")
  }

  # compute the number of exposures in each interval
  x <- x %>%
    dplyr::select(-"drug_exposure_end_date") %>%
    dplyr::group_by(.data$person_id, .data$subexposure_id) %>%
    dplyr::mutate(number_exposures_interval = dplyr::n()) %>%
    dplyr::ungroup() %>%
    dplyr::compute()
  # save number of exposures, number of subexposures, number of groups...
  exposureCounts <- x %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(groups))) %>%
    dplyr::summarise(
      number_exposures = dplyr::n_distinct(.data$drug_exposure_id),
      number_subexposures = dplyr::n_distinct(.data$subexposure_id),
      number_subexposures_with_overlap = dplyr::n_distinct(
        .data$subexposure_id[.data$number_exposures_interval > 1]
      ),
      number_continuous_exposures = dplyr::n_distinct(
        .data$continuous_exposure_id[!is.na(.data$continuous_exposure_id)]
      ),
      number_continuous_exposures_with_overlap = dplyr::n_distinct(
        .data$continuous_exposure_id[.data$number_exposures_interval > 1 &
          !is.na(.data$continuous_exposure_id)]
      ),
      number_eras = dplyr::n_distinct(.data$era_id),
      number_eras_with_overlap = dplyr::n_distinct(
        .data$era_id[.data$number_exposures_interval > 1]
      ),
      number_non_exposed_periods = dplyr::n_distinct(.data$era_id),
      number_gaps = sum(.data$gap, na.rm = TRUE),
      number_days_gap = sum(.data$gap * .data$days_exposed, na.rm = TRUE),
      cumulative_gap_dose = sum(
        .data$gap * .data$days_exposed * daily_dose,
        na.rm = TRUE
      ),
      all_dose = sum(
        .data$days_exposed * daily_dose,
        na.rm = TRUE
      ),
      all_exposed_days = sum(
        .data$days_exposed,
        na.rm = TRUE
      ),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      number_subexposures_no_overlap =
        .data$number_subexposures - .data$number_subexposures_with_overlap
    ) %>%
    dplyr::mutate(
      number_continuous_exposures_no_overlap =
        .data$number_continuous_exposures -
          .data$number_continuous_exposures_with_overlap
    ) %>%
    dplyr::mutate(
      number_eras_no_overlap =
        .data$number_eras - .data$number_eras_with_overlap
    ) %>%
    dplyr::compute()

  # get the subexposures without overlaping
  uniqueExposures <- x %>%
    dplyr::filter(.data$number_exposures_interval == 1) %>%
    dplyr::select(
      "person_id", "subexposure_id", "daily_dose", "days_exposed",
      "start_interval", "end_interval"
    ) %>%
    dplyr::compute()

  # get the overlapped subexposures
  multipleExposures <- x %>%
    dplyr::filter(.data$number_exposures_interval > 1) %>%
    dplyr::select(
      "person_id", "subexposure_id", "daily_dose", "days_exposed",
      "start_interval", "end_interval", "drug_exposure_start_date"
    ) %>%
    dplyr::compute()

  if (multipleExposures %>% dplyr::tally() %>% dplyr::pull() > 0) {

    # search individuals with same index date in subgroups
    multipleExposuresSameIndex <- multipleExposures %>%
      dplyr::group_by(
        .data$person_id, .data$subexposure_id, .data$drug_exposure_start_date
      ) %>%
      dplyr::mutate(number_same_index = dplyr::n()) %>%
      dplyr::ungroup() %>%
      dplyr::filter(.data$number_same_index > 1) %>%
      dplyr::select(-"number_same_index") %>%
      dplyr::compute()

    # get the overlaps with different index date
    multipleExposures <- multipleExposures %>%
      dplyr::anti_join(
        multipleExposuresSameIndex,
        by = c("person_id", "subexposure_id", "drug_exposure_start_date")
      ) %>%
      dplyr::compute()

    # solve the overlap with same index date
    # if criteria is to pick maximum
    if (sameIndexMode == "Maximum") {
      multipleExposuresSameIndex <- multipleExposuresSameIndex %>%
        dplyr::group_by(
          .data$person_id, .data$subexposure_id, .data$drug_exposure_start_date
        ) %>%
        dplyr::filter(daily_dose == max(.data$daily_dose, na.rm = TRUE)) %>%
        dplyr::distinct() %>%
        dplyr::ungroup() %>%
        dplyr::compute()
      # if criteria is to pick the Sum
    } else if (sameIndexMode == "Sum") {
      multipleExposuresSameIndex <- multipleExposuresSameIndex %>%
        dplyr::group_by(
          .data$person_id, .data$subexposure_id, .data$drug_exposure_start_date
        ) %>%
        dplyr::mutate(daily_dose = sum(.data$daily_dose, na.rm = TRUE)) %>%
        dplyr::distinct() %>%
        dplyr::ungroup() %>%
        dplyr::compute()
      # if criteria is to pick the minimum
    } else if (sameIndexMode == "Minimum") {
      multipleExposuresSameIndex <- multipleExposuresSameIndex %>%
        dplyr::group_by(
          .data$person_id, .data$subexposure_id, .data$drug_exposure_start_date
        ) %>%
        dplyr::filter(daily_dose == min(.data$daily_dose, na.rm = TRUE)) %>%
        dplyr::distinct() %>%
        dplyr::ungroup() %>%
        dplyr::compute()
    }
    # When we add back the multipleExposuresSameIndex all exposures should have
    # different drug_exposure_start_date
    multipleExposures <- multipleExposures %>%
      dplyr::union_all(multipleExposuresSameIndex) %>%
      dplyr::compute()
    # solve the overlap with different index dates
    multipleExposures <- multipleExposures %>%
      dplyr::group_by(.data$person_id, .data$subexposure_id)
    # if the overlapMode is "Previous" (earliest exposure prevails)
    if (overlapMode == "Previous") {
      multipleExposures <- multipleExposures %>%
        dplyr::filter(
          .data$drug_exposure_start_date == min(
            .data$drug_exposure_start_date,
            na.rm = TRUE
          )
        )
      # if the overlapMode is "Subsequent" (latest exposure prevails)
    } else if (overlapMode == "Subsequent") {
      multipleExposures <- multipleExposures %>%
        dplyr::filter(
          .data$drug_exposure_start_date == max(
            .data$drug_exposure_start_date,
            na.rm = TRUE
          )
        )
      # if the overlapMode is "Maximum" (exposure with more daily dose prevails)
    } else if (overlapMode == "Maximum") {
      multipleExposures <- multipleExposures %>%
        dplyr::select(-"drug_exposure_start_date") %>%
        dplyr::filter(
          .data$daily_dose == max(.data$daily_dose, na.rm = TRUE)
        ) %>%
        dplyr::distinct()
      # if the overlapMode is "Sum" (all exposure are considered)
    } else if (overlapMode == "Sum") {
      multipleExposures <- multipleExposures %>%
        dplyr::select(-"drug_exposure_start_date") %>%
        dplyr::mutate(daily_dose = sum(.data$daily_dose, na.rm = TRUE)) %>%
        dplyr::distinct()
      # if the overlapMode is "Minimum" (exposure with less daily dose prevails)
    } else if (overlapMode == "Minimum") {
      multipleExposures <- multipleExposures %>%
        dplyr::select(-"drug_exposure_start_date") %>%
        dplyr::filter(
          .data$daily_dose == min(.data$daily_dose, na.rm = TRUE)
        ) %>%
        dplyr::distinct()
    }
    multipleExposures <- multipleExposures %>%
      dplyr::ungroup() %>%
      dplyr::select(
        "person_id", "subexposure_id", "daily_dose", "days_exposed",
        "start_interval", "end_interval"
      ) %>%
      dplyr::compute()

    if (uniqueExposures %>% dplyr::tally() %>% dplyr::pull() > 0) {
      uniqueExposures <- multipleExposures %>%
        dplyr::union(uniqueExposures) %>%
        dplyr::compute()
    } else {
      uniqueExposures <- multipleExposures
    }
  }

  personSummary <- uniqueExposures %>%
    dplyr::group_by(.data$person_id) %>%
    dplyr::summarise(
      cohort_start_date = min(.data$start_interval, na.rm = TRUE),
      cohort_end_date = max(.data$end_interval, na.rm = TRUE),
      exposed_days = sum(.data$days_exposed, na.rm = TRUE),
      cumulative_dose = sum(.data$daily_dose * .data$days_exposed, na.rm = TRUE),
      subexposure_id = min(.data$subexposure_id, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(study_days = dbplyr::sql(sqlDiffDays(
      dialect,
      "cohort_start_date",
      "cohort_end_date"
    )) + 1) %>%
    dplyr::mutate(not_exposed_days = .data$study_days - .data$exposed_days) %>%
    dplyr::inner_join(uniqueExposures %>%
      dplyr::rename("initial_dose" = "daily_dose") %>%
      dplyr::select("person_id", "subexposure_id", "initial_dose"),
    by = c("person_id", "subexposure_id")
    ) %>%
    dplyr::select(-"subexposure_id") %>%
    dplyr::left_join(exposureCounts,
      by = c("person_id")
    ) %>%
    dplyr::mutate(
      not_considered_dose = .data$all_dose - .data$cumulative_dose,
      not_considered_exposed_days = .data$all_exposed_days - .data$exposed_days,
      prop_cum_gap_dose = .data$cumulative_gap_dose / .data$cumulative_dose,
      prop_not_considered_exp_days = .data$not_considered_exposed_days / .data$all_exposed_days
    ) %>%
    dplyr::compute()

  return(personSummary)
}

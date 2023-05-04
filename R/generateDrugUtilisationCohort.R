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

#' Generates a cohort of the drug use of a certain list of concepts.
#'
#' @param cdm 'cdm' object created with CDMConnector::cdm_from_con(). It must
#' must contain at least 'drug_exposure', 'drug_strength' and
#' observation_period' tables. The 'cdm' object must contain the
#' 'write_schema' as attribute and  the user should have permission to write on
#' it. It is a compulsory input, no default value is provided.
#' @param conceptSetList Names list of concept sets.
#' @param studyStartDate Minimum date where the incident exposed eras should
#' start to be considered. Only incident exposed eras larger than StudyStartDate
#' are allowed. If it is NULL no restriction is applied. By default: NULL.
#' @param studyEndDate Maximum date where the incident exposed eras should
#' start to be considered. Only incident exposed eras before StudyEndDate
#' are allowed. If it is NULL no restriction is applied. By default: NULL.
#' @param summariseMode Choice on how to summarize the exposures. There are
#' three options:
#' "FixedTime" each individual is followed the exact same number of days
#' specified in 'fixedTime' argument.
#' "AllEras" we summarize the output will be a summary of the exposed eras of
#' each individual. Each individual can contribute multiple times.
#' "FirstEra" we only consider the first observable era of each individual. In
#' this case each individual can not contribute with multiple rows.
#' By default: "AllEras".
#' @param fixedTime Time period after first exposure where we summarize the
#' ingredient of interest. Argument only considered if 'summariseMode' =
#' "FixedTime". No default value is provided.
#' @param daysPriorHistory Minimum number of days of prior history
#' (observation time) required for the incident eras to be considered. By
#' default: 0, meaning it has to be in observation_period table.
#' When Null, we do not check if in observation_period table.
#' @param gapEra Number of days between two continuous exposures to be
#' considered in the same era. By default: 180.
#' @param priorUseWashout Prior days without exposure. By default: NULL.
#' @param imputeDuration Whether/how the duration should be imputed
#' "eliminate", "median", "mean", "quantile25", "quantile75".
#' . By default: eliminate
#' @param durationRange Range between the duration must be comprised. It should
#' be a numeric vector of length two, with no NAs and the first value should be
#' equal or smaller than the second one. It is only required if imputeDuration
#' = TRUE. If NULL no restrictions are applied. By default: NULL.
#'
#' @param tablePrefix The stem for the permanent tables that will
#' be created. If NULL, temporary tables will be used throughout.
#'
#' @return The function returns the 'cdm' object with the created tables as
#' references of the object.
#' @export
#'
#' @examples
generateDrugUtilisationCohort <- function(cdm,
                                          conceptSetList,
                                          name,
                                          temporary = TRUE,
                                          studyPeriod = NULL,
                                          summariseMode = "AllEras",
                                          fixedTime = 365,
                                          daysPriorHistory = 0,
                                          gapEra = 30,
                                          priorUseWashout = 0,
                                          imputeDuration = "eliminate",
                                          daysExposedRange = c(1, Inf)) {
  checkCdm(
    cdm, c("drug_exposure", "observation_period", "person", "drug_strength")
  )
  checkConceptSetList(conceptSetList)
  checkCohortName(name, names(cdm))
  checkmate::assertLogical(temporary, any.missing = F, len = 1)
  checkStudyPeriod(studyPeriod)
  checkSummariseMode(summariseMode, fixedTime)
  checkmate::assertIntegerish(daysPriorHistory, lower = 0, len = 1, null.ok = T)
  checkmate::assertIntegerish(gapEra, lower = 0, len = 1)
  checkmate::assertIntegerish(priorUseWashout, lower = 0, len = 1)
  checkImputeDuration(imputeDuration)
  checkRange(daysExposedRange)

  if (is.null(priorUseWashout)) {priorUseWashout <- NA}

  # attr(cdm, "temporary") <- temporary
  # compute <- function(x, cdm) {
  #   CDMConnector::computeQuery(
  #     x,
  #     name = paste0(attr(cdm, "write_prefix"), CDMConnector:::uniqueTableName()),
  #     temporary = attr(cdm, "temporary"),
  #     schema = attr(cdm, "write_schema"),
  #     overwrite = TRUE
  #   )
  # }

  # subset drug_exposure and only get the drug concept ids that we are
  # interested in.
  cohort <- subsetTable(cdm, conceptList, "drug_exposure")
  if (cohort %>% dplyr::tally() %>% dplyr::pull("n") == 0) {
    cli::cli_abort("No record found with the current specifications in
    drug_exposure table")
  }
  attrition <- addAttritionLine(NULL, cohort, "Initial Exposures")

  # get cohort set
  cohortSet <- attr(cohort, "cohortSet") %>%
    dplyr::mutate(
      study_period_start = .env$studyPeriod[1],
      study_period_end = .env$studyPeriod[2],
      summarise_mode = .env$summariseMode,
      fixed_time = .env$fixedTime,
      days_prior_history = .env$daysPriorHistory,
      gap_era = .env$gapEra,
      prior_use_washout = .env$priorUseWashout,
      impute_duration = .env$imputeDuration,
      days_exposed_range_min = .env$daysExposedRange[1],
      days_exposed_range_max = .env$daysExposedRange[2]
    )

  # correct days exposed
  cohort <- correctDaysExposed(cohort, cdm)
  reason <- paste(
    "Days exposed imputation; affected rows:", attr(cohort, "numberImputations")
  )
  attrition <- addAttritionLine(attrition, cohort, reason)

  cohort <- unionCohort(cohort, gapEra)
  attrition <- addAttritionLine(attrition, cohort, "Join eras")

  cohort <- applyDaysPriorHistory(cohort, daysPriorHistory)
  attrition <- addAttritionLine(attrition, cohort, "daysPriorHistory applied")

  cohort <- applyPriorUseWashout(cohort, priorUseWashout)
  attrition <- addAttritionLine(attrition, cohort, "priorUseWashout applied")

  if (!is.na(studyPeriod[1])) {
    cohort <- cohort %>%
      dplyr::filter(.data$cohort_start_date >= !!studyPeriod[1]) %>%
      CDMConnector::computeQuery()
    attrition <- addAttritionLine(
      attrition, cohort, paste0("priorUseWashout applied")
    )
  }

  if (!is.na(studyPeriod[2])) {
    cohort <- cohort %>%
      dplyr::filter(.data$cohort_start_date <= !!studyPeriod[2]) %>%
      CDMConnector::computeQuery()
    attrition <- addAttritionLine(
      attrition, cohort, paste0("priorUseWashout applied")
    )
  }


  if (summariseMode == "FirstEra") {
    cohort <- cohort %>%
      dplyr::group_by(.data$cohort_definition_id, .data$subject_id) %>%
      dplyr::filter(
        .data$cohort_start_date == min(.data$cohort_start_date, na.rm = TRUE)
      ) %>%
      dplyr::ungroup()
    attrition <- attrition %>%
      dplyr::union_all(addattritionLine(cohort, "Only first era"))

  } else if (summariseMode == "FixedTime") {
    cohort <- cohort %>%
      dplyr::group_by(.data$cohort_definition_id, .data$subject_id) %>%
      dplyr::summarise(
        cohort_start_date = min(.data$cohort_start_date, na.rm = TRUE)
      ) %>%
      dplyr::mutate(cohort_end_date = !!CDMConnector::dateadd(
        "cohort_start_date",
        fixedTime - 1
      )) %>%
      dplyr::ungroup()
    attrition <- attrition %>%
      dplyr::union_all(addattritionLine(
        cohort,
        paste0("Only first era; fixedTime = ", fixedTime, " days")
      ))
  }

  cohort <- cohort %>%
    dplyr::select(
      "cohort_definition_id", "subject_id", "cohort_start_date",
      "cohort_end_date"
    )

  attrition <- attrition %>%
    dplyr::left_join(
      dplyr::tibble(
        order_id = c(1:10),
        reason = c(
          "Initial Exposures",
          "Imputation",
          "Eras",
          paste0("Prior washout of ", priorUseWashout, " days"),
          paste0("Start after or at ", studyStartDate),
          paste0("Start before or at ", studyEndDate),
          "In observation on cohort_start_date",
          paste0("At least ", daysPriorHistory, " days of prior history"),
          "Only first era",
          paste0("Only first era; fixedTime = ", fixedTime, " days")
        )
      ),
      by = "reason"
    ) %>%
    dplyr::arrange(.data$cohort_definition_id, .data$order_id) %>%
    dplyr::select(-"order_id")

  cohortCount <- cohort %>%
    dplyr::group_by(.data$cohort_definition_id) %>%
    dplyr::summarise(
      number_records = dplyr::n(),
      number_subjects = dplyr::n_distinct(.data$subject_id),
      .groups = "drop"
    ) %>%
    dplyr::compute()

  con <- attr(cdm, "dbcon")
  if(is.null(tablePrefix)){
    cohortRef <- cohort %>% CDMConnector::computeQuery()
    cohortSetTableName <- CDMConnector::uniqueTableName()
    DBI::dbWriteTable(con, cohortSetTableName, conceptSets, temporary = TRUE)
    cohortSetRef <- dplyr::tbl(con, cohortSetTableName)
    cohortAttritionTableName <- CDMConnector::uniqueTableName()
    DBI::dbWriteTable(con, cohortAttritionTableName, attrition, temporary = TRUE)
    cohortAttritionRef <- dplyr::tbl(con, cohortAttritionTableName)
    cohortCountRef <- cohortCount
  } else {
    writeSchema <- attr(cdm, "write_schema")
    cohortRef <- CDMConnector::computeQuery(
      x = cohort, name = tablePrefix, temporary = FALSE,
      schema = writeSchema, overwrite = TRUE
    )
    DBI::dbWriteTable(
      conn = con, name = CDMConnector::inSchema(writeSchema, paste0(tablePrefix, "_set"), CDMConnector::dbms(con)),
      value = as.data.frame(conceptSets),
      overwrite = TRUE
    )
    cohortSetRef <- dplyr::tbl(con, CDMConnector::inSchema(writeSchema, paste0(tablePrefix, "_set"), CDMConnector::dbms(con)))
    DBI::dbWriteTable(
      conn = con, name = CDMConnector::inSchema(writeSchema, paste0(tablePrefix, "_attrition"), CDMConnector::dbms(con)),
      value = as.data.frame(attrition),
      overwrite = TRUE
    )
    cohortAttritionRef <- dplyr::tbl(con, CDMConnector::inSchema(writeSchema, paste0(tablePrefix, "_attrition"), CDMConnector::dbms(con)))
    cohortCountRef <- CDMConnector::computeQuery(
      x = cohortCount, name = paste0(tablePrefix, "_count"), temporary = FALSE,
      schema = writeSchema, overwrite = TRUE
    )
  }

  cohort <- CDMConnector::newGeneratedCohortSet(
    cohortRef = cohortRef,
    cohortSetRef = cohortSetRef,
    cohortAttritionRef = cohortAttritionRef,
    cohortCountRef = cohortCountRef
  )

  return(cohort)
}

#' Impute or eliminate values under a certain conditions
#' @noRd
imputeVariable <- function(x, column, impute, range, imputeRound = FALSE) {
  # identify NA
  x <- x %>%
    dplyr::mutate(impute = dplyr::if_else(is.na(.data[[column]]), 1, 0))

  # identify < range[1]
  if (!is.na(range[1])) {
    x <- x %>%
      dplyr::mutate(impute = dplyr::if_else(
        .data$impute == 0, dplyr::if_else(.data[column] < !!range[1], 1, 0), 1
      ))
  }
  # identify > range[2]
  if (!is.na(range[2])) {
    x <- x %>%
      dplyr::mutate(impute = dplyr::if_else(
        .data$impute == 0, dplyr::if_else(.data[column] > !!range[2], 1, 0), 1
      ))
  }
  numberImputations <- x %>%
    dplyr::filter(.data$impute == 1) %>%
    dplyr::summarise(n = as.numeric(dplyr::n())) %>%
    dplyr::pull()
  if (numberImputations > 0) {
    # if impute is false then all values with impute = 1 are not considered
    if (impute == "eliminate") {
      x <- x %>%
        dplyr::filter(.data$impute == 0)
    } else {
      if (is.character(impute)) {
        values <- x %>%
          dplyr::filter(.data$impute == 0) %>%
          dplyr::pull(dplyr::all_of(column))
        impute <- switch(
          impute,
          "median" = stats::median(values),
          "mean" = mean(values),
          "quantile25" = stats::quantile(values, 0.25),
          "quantile75" = stats::quantile(values, 0.75),
        )
        if (imputeRound) {
          impute <- round(impute)
        }
      }
      x <- x %>%
        dplyr::mutate(!!column := dplyr::if_else(
          .data$impute == 1, .env$impute, .data[[column]]
        ))
    }
  }
  x <- x %>%
    dplyr::select(-"impute")
  attr(x, "numberImputations") <- numberImputations
  return(x)
}

#' @noRd
correctDaysExposed <- function(x, daysExposedRange, cdm) {
  # compute the number of days exposed according to:
  # days_exposed = end - start + 1
  x <- x %>%
    dplyr::mutate(days_exposed = !!CDMConnector::datediff(
      start = "cohort_start_date",
      end = "cohort_end_date"
    ) + 1)

  # impute or eliminate the exposures that duration does not fulfill the
  # conditions (<daysExposedRange[1]; >daysExposedRange[2])
  x <- imputeVariable(
    x = x,
    variableName = "days_exposed",
    impute = imputeDuration,
    lowerBound = daysExposedRange[1],
    upperBound = daysExposedRange[2],
    imputeValueName = "imputeDuration"
  ) %>%
    dplyr::mutate(days_to_add = as.integer(.data$days_exposed - 1)) %>%
    dplyr::compute() %>%
    dplyr::mutate(drug_exposure_end_date = as.Date(dbplyr::sql(
      CDMConnector::dateadd(
        date = "drug_exposure_start_date",
        number = "days_to_add"
      )
    ))) %>%
    compute(cdm)
  return(x)
}

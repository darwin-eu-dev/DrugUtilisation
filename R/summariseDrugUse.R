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

#' This function is used to summarise the dose table over multiple cohorts.
#'
#' @param cdm 'cdm' object created with CDMConnector::cdm_from_con(). It must
#' must contain the 'doseTableName' table and  also 'strataCohortName' and
#' 'indicationList' if specified. It is a compulsory input, no default
#' value is provided.
#' @param strataCohortName Name to point to the cdm table that contains the
#' strata information. To compute this cohort you can use the getStratification
#' function or use any other function that the output is a cohort. It is a
#' compulsory input, no default value is provided.
#' @param cohortId cohort definition id of the cohorts in the strata cohort
#' table that you want to summarise. By default cohortId = NULL, then all
#' cohorts in strataCohort will be summarised.
#' @param doseTableName Name to point to the cdm table that contains the dose
#' cohort information computed using getDoseInformationTable
#' @param variable Vector with the variable that we want to summarise. The
#' variable must be contained in doseTableName. If NULL all variable in
#' doseTableName are summarised. By defaukt: NULL.
#' @param estimates Vector with the names of the functions used to summarise the
#' variable. The possible estimates are: "min", "max", "mean", "median", "iqr",
#' "range", "q5", "q10", "q15", "q20", "q25", "q30", "q35", "q40", "q45", "q50",
#' "q55", "q60", "q65", "q70", "q75", "q80", "q85", "q90", "q95" and "std". By
#' default: c("min", "max", "mean", "std", "median", "iqr", "q25", "q75").
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
summariseDoseTable <- function(cdm,
                               strataCohortName,
                               cohortId = NULL,
                               doseTableName,
                               variable = NULL,
                               estimates = c(
                                 "min", "max", "mean", "std", "median", "iqr",
                                 "q25", "q75"
                               ),
                               minimumCellCount = 5) {
  # first round of assertions CLASS
  # start checks
  errorMessage <- checkmate::makeAssertCollection()
  # check cdm
  checkmate::assertClass(
    cdm,
    "cdm_reference",
    add = errorMessage
  )
  # check doseTableName
  checkmate::assertCharacter(
    doseTableName,
    len = 1,
    any.missing = FALSE,
    add = errorMessage
  )
  # check strataCohortName
  checkmate::assertCharacter(
    strataCohortName,
    len = 1,
    any.missing = FALSE,
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
  # check variable
  checkmate::assertCharacter(
    variable,
    min.len = 1,
    null.ok = TRUE,
    any.missing = FALSE,
    add = errorMessage
  )
  # check estimates
  checkmate::assertCharacter(
    estimates,
    any.missing = FALSE,
    add = errorMessage
  )
  # minimum cell counts
  checkmate::assertCount(
    minimumCellCount,
    add = errorMessage
  )
  # report collection of errors
  checkmate::reportAssertions(collection = errorMessage)

  checkmate::assertTRUE(doseTableName %in% names(cdm))
  checkmate::assertTRUE(strataCohortName %in% names(cdm))

  # check strataCohort
  strataCohort <- cdm[[strataCohortName]]
  checkmate::assertTRUE(
    all(c(
      "cohort_definition_id", "subject_id", "cohort_start_date",
      "cohort_end_date"
    ) %in% colnames(strataCohort)),
    add = errorMessage
  )
  #
  strataCohortNameEmpty <- cdm[[strataCohortName]] %>% dplyr::tally()%>%
    dplyr::pull()

  if (strataCohortNameEmpty == 0) {
    errorMessage$push("- table `strataCohortName` contains 0 row")
  }

  # checks dose summary
  doseTable <- cdm[[doseTableName]]
  if (is.null(variable)) {
    variable <- colnames(doseTable)
    variable <- variable[!(variable %in% c(
      "subject_id", "cohort_start_date", "cohort_end_date"
    ))]
  }
  checkmate::assertTRUE(
    all(variable %in% colnames(doseTable)),
    add = errorMessage
  )
  if (all(unlist(lapply(
    doseTable %>%
      dplyr::select(dplyr::all_of(.env$variable)) %>%
      utils::head(1) %>%
      dplyr::collect(),
    function(x) {
      is.numeric(x)
    }
  ))) == FALSE) {
    errorMessage$push("-All variable should be numeric")
  }
  checkmate::assertTRUE(
    all(
      estimates %in% c(
        "min", "max", "mean", "median", "iqr", "range", "q5", "q10", "q15",
        "q20", "q25", "q30", "q35", "q40", "q45", "q50", "q55", "q60", "q65",
        "q70", "q75", "q80", "q85", "q90", "q95", "std"
      )
    ),
    add = errorMessage
  )

  checkmate::reportAssertions(collection = errorMessage)

  estimates_func <- list(
    "min" = function(x) {
      base::min(x, na.rm = TRUE)
    },
    "max" = function(x) {
      base::max(x, na.rm = TRUE)
    },
    "mean" = function(x) {
      base::mean(x, na.rm = TRUE)
    },
    "median" = function(x) {
      stats::median(x, na.rm = TRUE)
    },
    "iqr" = function(x) {
      stats::IQR(x, na.rm = TRUE)
    },
    "range" = function(x) {
      base::diff(base::range(x, na.rm = TRUE))
    },
    "std" = function(x) {
      stats::sd(x, na.rm = TRUE)
    },
    "q5" = function(x) {
      stats::quantile(x, 0.05, na.rm = TRUE)
    },
    "q10" = function(x) {
      stats::quantile(x, 0.10, na.rm = TRUE)
    },
    "q15" = function(x) {
      stats::quantile(x, 0.15, na.rm = TRUE)
    },
    "q20" = function(x) {
      stats::quantile(x, 0.20, na.rm = TRUE)
    },
    "q25" = function(x) {
      stats::quantile(x, 0.25, na.rm = TRUE)
    },
    "q30" = function(x) {
      stats::quantile(x, 0.30, na.rm = TRUE)
    },
    "q35" = function(x) {
      stats::quantile(x, 0.35, na.rm = TRUE)
    },
    "q40" = function(x) {
      stats::quantile(x, 0.40, na.rm = TRUE)
    },
    "q45" = function(x) {
      stats::quantile(x, 0.45, na.rm = TRUE)
    },
    "q55" = function(x) {
      stats::quantile(x, 0.55, na.rm = TRUE)
    },
    "q60" = function(x) {
      stats::quantile(x, 0.60, na.rm = TRUE)
    },
    "q65" = function(x) {
      stats::quantile(x, 0.65, na.rm = TRUE)
    },
    "q70" = function(x) {
      stats::quantile(x, 0.70, na.rm = TRUE)
    },
    "q75" = function(x) {
      stats::quantile(x, 0.75, na.rm = TRUE)
    },
    "q80" = function(x) {
      stats::quantile(x, 0.80, na.rm = TRUE)
    },
    "q85" = function(x) {
      stats::quantile(x, 0.85, na.rm = TRUE)
    },
    "q90" = function(x) {
      stats::quantile(x, 0.90, na.rm = TRUE)
    },
    "q95" = function(x) {
      stats::quantile(x, 0.95, na.rm = TRUE)
    }
  )

  estimates_func <- estimates_func[estimates]

  if (is.null(cohortId)) {
    cohortId <- strataCohort %>%
      dplyr::select("cohort_definition_id") %>%
      dplyr::distinct() %>%
      dplyr::pull()
  }

  # get basic data of each cohort
  for (k in 1:length(cohortId)) {
    result.k <- strataCohort %>%
      dplyr::filter(.data$cohort_definition_id == !!cohortId[k]) %>%
      dplyr::summarise(
        number_observations.count = as.character(dplyr::n()),
        cohort_start_date.min = as.character(min(
          .data$cohort_start_date,
          na.rm = TRUE
        )),
        cohort_start_date.max = as.character(max(
          .data$cohort_start_date,
          na.rm = TRUE
        )),
        cohort_end_date.min = as.character(min(
          .data$cohort_end_date,
          na.rm = TRUE
        )),
        cohort_end_date.max = as.character(max(
          .data$cohort_end_date,
          na.rm = TRUE
        ))
      ) %>%
      dplyr::collect() %>%
      tidyr::pivot_longer(
        cols = dplyr::everything(),
        names_to = c("variable", "estimate"),
        names_pattern = "([[:alnum:]_]+).([[:alnum:]_]+)"
      ) %>%
      dplyr::mutate(cohort_definition_id = !!cohortId[k]) %>%
      dplyr::select(
        "cohort_definition_id", "variable", "estimate", "value"
      )
    if (k == 1) {
      result <- result.k
    } else {
      result <- rbind(result, result.k)
    }
  }

  # get dose data of each cohort
  for (k in 1:length(cohortId)) {
    result <- rbind(
      result,
      strataCohort %>%
        dplyr::filter(.data$cohort_definition_id == !!cohortId[k]) %>%
        dplyr::select(
          "subject_id", "cohort_start_date", "cohort_end_date"
        ) %>%
        dplyr::inner_join(
          doseTable,
          by = c("subject_id", "cohort_start_date", "cohort_end_date")
        ) %>%
        dplyr::select(dplyr::all_of(.env$variable)) %>%
        dplyr::collect() %>%
        dplyr::summarise(dplyr::across(
          .cols = dplyr::all_of(.env$variable),
          .fns = estimates_func,
          .names = "{.col}.{.fn}"
        )) %>%
        tidyr::pivot_longer(
          cols = dplyr::everything(),
          names_to = c("variable", "estimate"),
          names_pattern = "([[:alnum:]_]+).([[:alnum:]_]+)"
        ) %>%
        dplyr::mutate(value = as.character(.data$value)) %>%
        dplyr::mutate(cohort_definition_id = !!cohortId[k]) %>%
        dplyr::select(
          "cohort_definition_id", "variable", "estimate", "value"
        )
    )
  }

  result <- obscureSummary(result, minimumCellCount = minimumCellCount)

  result <- result %>% dplyr::arrange(.data$cohort_definition_id)

  return(result)
}

#' @noRd
obscureSummary <- function(result, minimumCellCount) {
  values_to_osbcure <- suppressWarnings(as.numeric(result$value)) <
    minimumCellCount &
    suppressWarnings(as.numeric(result$value)) > 0
  obscured_values <- result$estimate == "count" & values_to_osbcure
  obscured_cohort <- unique(result$cohort_definition_id[
    result$estimate == "count" &
      result$variable == "number_observations" &
      values_to_osbcure
  ])
  result$value[obscured_values] <- paste0("<", minimumCellCount)
  result$value[
    result$cohort_definition_id %in% obscured_cohort
  ] <- as.character(NA)
  result$value[
    result$cohort_definition_id %in% obscured_cohort &
      result$variable == "number_observations"
  ] <- paste0("<", minimumCellCount)
  return(result)
}
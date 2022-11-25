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
#' @param cdm 'cdm' object created with CDMConnector::cdm_from_con(). It must
#' must contain the 'doseCohortTable' table and  also 'aggegationCohort' if
#' specified. It is a compulsory input, no default value is provided.
#' @param doseCohortTable Name to point to the cdm table that contains the dose
#' cohort information computed using instantiateDrugUtilisationCohorts
#' (doseInformation = TRUE).
#' @param aggegationCohort Name to point to the cdm table that contains the
#' aggregation information. To compute this cohort you can use the
#' drugUtilisationSettings() function or use any other function that the output
#' is a cohort. A warning will be produced if
#' @param cohortId cohort definition id of the cohorts in the aggregation or
#' dose cohort table that we want to summarise.
#' @param variables Vector with the variables that we want to summarise. The
#' variables must be contained in doseCohortTable.
#' @param estimates Vector with the names of the functions used to summarise the
#' variables. The possible estimates are: "min", "max", "mean", "median", "iqr",
#' "range", "q5", "q10", "q15", "q20", "q25", "q30", "q35", "q40", "q45", "q50",
#' "q55", "q60", "q65", "q70", "q75", "q80", "q85", "q90", "q95" and "std".
#' @param minimumCellCounts Minimum counts that a group can have.
#'
#' @return
#' @export
#'
#' @examples
summariseDoseTable <- function(cdm,
                               doseCohortTable,
                               aggegationCohort = NULL,
                               cohortId = NULL,
                               variables = c(
                                 "cohort_start_date", "cohort_end_date",
                                 "exposed_days", "cumulative_dose",
                                 "initial_dose", "study_days",
                                 "number_exposures", "number_subexposures",
                                 "number_continuous_exposures",
                                 "cumulative_gap_dose", "prop_cum_gap_dose",
                                 "number_non_exposed_periods",
                                 "not_exposed_days", "number_days_gaps",
                                 "number_gaps", "not_considered_dose",
                                 "not_considered_exposed_days",
                                 "prop_not_considered_exp_days",
                                 "number_subexposures_with_overlap",
                                 "number_continuous_exposures_with_overlap",
                                 "number_eras_with_overlap",
                                 "number_subexposures_no_overlap",
                                 "number_continuous_exposures_no_overlap",
                                 "number_eras_no_overlap"
                               ),
                               estimates = c(
                                 "min", "max", "mean", "std", "median", "iqr",
                                 "q25", "q75"
                               ),
                               minimumCellCounts = 5) {
  # start checks
  errorMessage <- checkmate::makeAssertCollection()

  # check cdm
  checkmate::assertClass(cdm, "cdm_reference", add = errorMessage)

  # check doseCohortTable
  checkmate::assertCharacter(doseCohortTable, len = 1, add = errorMessage)

  # check aggegationCohort
  checkmate::assertCharacter(
    aggegationCohort,
    len = 1,
    null.ok = TRUE,
    add = errorMessage
  )

  # check cohortId
  checkmate::assertIntegerish(
    cohortId,
    unique = TRUE,
    null.ok = TRUE,
    add = errorMessage
  )

  # check variables
  checkmate::assertCharacter(variables, add = errorMessage)
  checkmate::assertTRUE(
    all(variables %in% colnames(cdm[[doseCohortTable]])),
    add = errorMessage
  )
  checkmate::assertTRUE(all(unlist(lapply(
    cdm[[doseCohortTable]] %>%
      dplyr::select(dplyr::all_of(.env$variables)) %>%
      head(1) %>%
      dplyr::collect(),
    function(x) {
      is.numeric(x)
    }
  ))))

  # check estimates
  checkmate::assertCharacter(estimates, add = errorMessage)
  checkmate::assertTRUE(
    all(estimates %in% c(
      "min", "max", "mean", "median", "iqr", "range", "q5", "q10", "q15", "q20",
      "q25", "q30", "q35", "q40", "q45", "q50", "q55", "q60", "q65", "q70",
      "q75", "q80", "q85", "q90", "q95", "std"
    )),
    add = errorMessage
  )

  # minimum cell counts
  checkmate::assertCount(minimumCellCounts, add = errorMessage)

  # report collection of errors
  checkmate::reportAssertions(collection = errorMessage)

  estimates_func <- list(
    "min" = base::min,
    "max" = base::max,
    "mean" = base::mean,
    "median" = stats::median,
    "iqr" = stats::IQR,
    "range" = function(x) {
      base::diff(base::range(x))
    },
    "std" = stats::sd,
    "q5" = function(x) {
      stats::quantile(x, 0.05)
    },
    "q10" = function(x) {
      stats::quantile(x, 0.10)
    },
    "q15" = function(x) {
      stats::quantile(x, 0.15)
    },
    "q20" = function(x) {
      stats::quantile(x, 0.20)
    },
    "q25" = function(x) {
      stats::quantile(x, 0.25)
    },
    "q30" = function(x) {
      stats::quantile(x, 0.30)
    },
    "q35" = function(x) {
      stats::quantile(x, 0.35)
    },
    "q40" = function(x) {
      stats::quantile(x, 0.40)
    },
    "q45" = function(x) {
      stats::quantile(x, 0.45)
    },
    "q50" = function(x) {
      stats::quantile(x, 0.50)
    },
    "q55" = function(x) {
      stats::quantile(x, 0.55)
    },
    "q60" = function(x) {
      stats::quantile(x, 0.60)
    },
    "q65" = function(x) {
      stats::quantile(x, 0.65)
    },
    "q70" = function(x) {
      stats::quantile(x, 0.70)
    },
    "q75" = function(x) {
      stats::quantile(x, 0.75)
    },
    "q80" = function(x) {
      stats::quantile(x, 0.80)
    },
    "q85" = function(x) {
      stats::quantile(x, 0.85)
    },
    "q90" = function(x) {
      stats::quantile(x, 0.90)
    },
    "q95" = function(x) {
      stats::quantile(x, 0.95)
    }
  )

  estimates_func <- estimates_func[estimates]

  if (is.null(aggegationCohort)) {
    if ("cohort_definition_id" %in% colnames(doseCohortTable)) {
      cohortId <- cdm[[doseCohortTable]] %>%
        dplyr::select("cohort_definition_id") %>%
        dplyr::distinct() %>%
        dplyr::pull()
      for (k in 1:length(cohortId)) {
        result.k <- cdm[[doseCohortTable]] %>%
          dplyr::filter(.data$cohort_definition_id == !!cohortId[k]) %>%
          dplyr::select(dplyr::all_of(.env$variables)) %>%
          dplyr::summarise_all(estimates_func) %>%
          dplyr::collect() %>%
          dplyr::mutate(cohort_definition_id = !!.env$cohortId[k])
        if (k == 1) {
          result <- result.k
        } else {
          result <- rbind(result, result.k)
        }
      }
    } else {
      result <- cdm[[doseCohortTable]] %>%
        dplyr::select(dplyr::all_of(.env$variables)) %>%
        dplyr::summarise_all(
          estimates_func
        ) %>%
        dplyr::collect()
    }
  } else {
    if (is.null(cohortId)) {
      cohortId <- cdm[[aggegationCohort]] %>%
        dplyr::select("cohort_definition_id") %>%
        dplyr::distinct() %>%
        dplyr::pull()
    }
    for (k in 1:length(cohortId)) {
      result.k <- cdm[[aggegationCohort]] %>%
        dplyr::filter(.data$cohort_definition_id == !!cohortId[k]) %>%
        dplyr::select(
          "subject_id",
          "cohort_start_date",
          "cohort_end_date"
        ) %>%
        dplyr::inner_join(
          cdm[[doseCohortTable]],
          by = c("subject_id", "cohort_start_date", "cohort_end_date")
        ) %>%
        dplyr::select(dplyr::all_of(.env$variables)) %>%
        dplyr::summarise_all(estimates_func) %>%
        dplyr::collect() %>%
        dplyr::mutate(cohort_definition_id = !!.env$cohortId[k])
      if (k == 1) {
        result <- result.k
      } else {
        result <- rbind(result, result.k)
      }
    }
  }

  return(result)
}

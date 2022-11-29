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
#' must contain the 'doseCohortName' table and  also 'aggegationCohortName' if
#' specified. It is a compulsory input, no default value is provided.
#' @param aggegationCohortName Name to point to the cdm table that contains the
#' aggregation information. To compute this cohort you can use the
#' drugUtilisationSettings() function or use any other function that the output
#' is a cohort. A warning will be produced if
#' @param cohortId cohort definition id of the cohorts in the aggregation or
#' dose cohort table that we want to summarise.
#' @param doseCohortName Name to point to the cdm table that contains the dose
#' cohort information computed using instantiateDrugUtilisationCohorts
#' (doseInformation = TRUE).
#' @param variables Vector with the variables that we want to summarise. The
#' variables must be contained in doseCohortName.
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
                               aggegationCohortName = NULL,
                               cohortId = NULL,
                               doseCohortName,
                               variables = NULL,
                               estimates = c(
                                 "min", "max", "mean", "std", "median", "iqr",
                                 "q25", "q75"
                               ),
                               minimumCellCounts = 5) {
  # start checks
  errorMessage <- checkmate::makeAssertCollection()

  # check cdm
  checkmate::assertClass(cdm, "cdm_reference", add = errorMessage)

  # check doseCohortName
  checkmate::assertCharacter(doseCohortName, len = 1, add = errorMessage)
  checkmate::assertTRUE(all(c(
    "subject_id", "cohort_start_date", "cohort_end_date"
  ) %in% colnames(cdm[[doseCohortName]])),
  add = errorMessage
  )

  # check aggegationCohortName
  checkmate::assertCharacter(
    aggegationCohortName,
    len = 1,
    null.ok = TRUE,
    add = errorMessage
  )
  if (!is.null(aggegationCohortName)) {
    checkmate::assertTRUE(all(c(
      "cohort_definition_id", "subject_id", "cohort_start_date",
      "cohort_end_date"
    ) %in% colnames(cdm[[aggegationCohortName]])),
    add = errorMessage
    )
  }

  # check cohortId
  checkmate::assertIntegerish(
    cohortId,
    unique = TRUE,
    null.ok = TRUE,
    add = errorMessage
  )

  # check variables
  if (is.null(variables)) {
    variables <- colnames(cdm[[doseCohortName]])
  }
  if (is.character(variables)) {
    variables <- variables[!(variables %in% c(
      "cohort_definition_id", "subject_id", "cohort_start_date",
      "cohort_end_date"
    ))]
  }
  checkmate::assertCharacter(variables, min.len = 1, add = errorMessage)
  checkmate::assertTRUE(
    all(variables %in% colnames(cdm[[doseCohortName]])),
    add = errorMessage
  )
  if (all(unlist(lapply(
    cdm[[doseCohortName]] %>%
      dplyr::select(dplyr::all_of(.env$variables)) %>%
      head(1) %>%
      dplyr::collect(),
    function(x) {
      is.numeric(x)
    }
  ))) == FALSE) {
    errorMessage$push("-All variables should be numeric")
  }

  # check estimates
  checkmate::assertCharacter(estimates, add = errorMessage)
  checkmate::assertTRUE(
    all(estimates %in% c(
      "min", "max", "mean", "median", "iqr", "range", "q5", "q10", "q15", "q20",
      "q25", "q30", "q35", "q40", "q45", "q55", "q60", "q65", "q70",
      "q75", "q80", "q85", "q90", "q95", "std"
    )),
    add = errorMessage
  )

  # minimum cell counts
  checkmate::assertCount(minimumCellCounts, add = errorMessage)

  # report collection of errors
  checkmate::reportAssertions(collection = errorMessage)

  estimates_func <- list(
    "min" = function(x){
      base::min(x, na.rm = TRUE)
    },
    "max" = function(x){
      base::max(x, na.rm = TRUE)
    },
    "mean" = function(x){
      base::mean(x, na.rm = TRUE)
    },
    "median" = function(x){
      stats::median(x, na.rm = TRUE)
    },
    "iqr" = function(x){
      stats::IQR(x, na.rm = TRUE)
    },
    "range" = function(x) {
      base::diff(base::range(x, na.rm = TRUE))
    },
    "std" = function(x){
      stats::sd(x, na.rm = TRUE)
    },
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

  if (is.null(aggegationCohortName)) {
    aggegationCohort <- cdm[[doseCohortName]] %>%
      dplyr::select(
        "cohort_definition_id", "subject_id", "cohort_start_date",
        "cohort_end_date"
      )
  } else {
    aggegationCohort <- cdm[[aggegationCohortName]]
    if ("cohort_definition_id" %in% colnames(cdm[[doseCohortName]])) {
      warning("'cohort_definition_id' of 'doseCohortName' will be dissmissed.")
    }
  }

  if (is.null(cohortId)) {
    cohortId <- aggegationCohort %>%
      dplyr::select("cohort_definition_id") %>%
      dplyr::distinct() %>%
      dplyr::pull()
  }

  for (k in 1:length(cohortId)) {
    result.k <- aggegationCohort %>%
      dplyr::filter(.data$cohort_definition_id == !!cohortId[k]) %>%
      dplyr::summarise(
        number_observations.counts = as.character(dplyr::n()),
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
      dplyr::union_all(
        aggegationCohort %>%
          dplyr::filter(.data$cohort_definition_id == !!cohortId[k]) %>%
          dplyr::select(
            "subject_id", "cohort_start_date", "cohort_end_date"
          ) %>%
          dplyr::inner_join(
            cdm[[doseCohortName]],
            by = c("subject_id", "cohort_start_date", "cohort_end_date")
          ) %>%
          dplyr::select(dplyr::all_of(.env$variables)) %>%
          dplyr::collect() %>%
          dplyr::summarise(dplyr::across(
            .cols = dplyr::all_of(.env$variables),
            .fns = estimates_func,
            .names = "{.col}.{.fn}"
          )) %>%
          tidyr::pivot_longer(
            cols = dplyr::everything(),
            names_to = c("variable", "estimate"),
            names_pattern = "([[:alnum:]_]+).([[:alnum:]_]+)"
          ) %>%
          dplyr::mutate(value = as.character(.data$value))
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

  to_obscure <- result %>%
    dplyr::filter(.data$variable == "number_observations" &
      .data$estimate == "counts") %>%
    dplyr::filter(as.numeric(.data$value) < .env$minimumCellCounts) %>%
    dplyr::pull("cohort_definition_id")

  result$value[result$cohort_definition_id %in% to_obscure] <- as.character(NA)
  result$value[result$cohort_definition_id %in% to_obscure &
    result$variable == "number_observations" &
    result$estimate == "counts"] <- paste0("<", minimumCellCounts)

  return(result)
}

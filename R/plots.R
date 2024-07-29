# Copyright 2024 DARWIN EU (C)
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

#' Generate a custom ggplot2 from a summarised_result object generated with
#' summariseTreatment function.
#'
#' @param result A summarised_result object with results from
#' summariseDrugRestart().
#' @param facetX Vector of variables to facet by horizontally. Allowed options
#' are: "cdm_name", "cohort_name", "strata", "variable_name"
#' @param facetY Vector of variables to facet by vertically Allowed options
#' are: "cdm_name", "cohort_name", "strata", "variable_name".
#' @param splitStrata Whether to split strata columns.
#' @param colour Vector of variables to distinct by colour. Allowed options
#' are: "cdm_name", "cohort_name", "strata", "variable_name", and
#' "variable_level".
#'
#' @return A ggplot2 object.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(DrugUtilisation)
#'
#' cdm <- mockDrugUtilisation()
#' result <- cdm$cohort1 |>
#'   summariseTreatment(
#'     treatmentCohortName = "cohort2",
#'     window = list(c(0, 30), c(31, 365))
#'   )
#'
#' plotTreatment(result)
#' }
#'
plotTreatment <- function(result,
                          facetX = "window_name",
                          facetY = c("cdm_name", "cohort_name", "strata"),
                          splitStrata = TRUE,
                          colour = "treatment") {
  rlang::check_installed("ggplot2")
  # check input
  assertChoice(
    facetX,
    choices = c("window_name", "cdm_name", "cohort_name", "strata"),
    null = TRUE, unique = T
  )
  assertChoice(
    facetY,
    choices = c("window_name", "cdm_name", "cohort_name", "strata"),
    null = TRUE, unique = T
  )
  assertLogical(splitStrata, length = 1)
  assertChoice(
    colour,
    choices = c("window_name", "cdm_name", "cohort_name", "strata", "treatment"),
    null = T,
    unique = T
  )

  result <- omopgenerics::newSummarisedResult(result) |>
    visOmopResults::filterSettings(
      .data$result_type == "summarise_treatment"
    ) |>
    dplyr::filter(.data$estimate_name == "percentage")

  if (nrow(result) == 0) {
    cli::cli_warn(c("!" = "There are no results to plot, returning empty plot"))
    return(ggplot2::ggplot())
  }

  # to display order accordingly
  lev <- result$variable_name |>
    unique() |>
    rev()

  warnDuplicatePoints(result, unique(c(facetX, facetY, colour)))

  if (splitStrata) {
    strata <- visOmopResults::strataColumns(result)
    result <- result |> visOmopResults::splitStrata()
    facetX <- substituteStrata(facetX, strata)
    facetY <- substituteStrata(facetY, strata)
    colour <- substituteStrata(colour, strata)
  } else {
    result <- result |> dplyr::rename("strata" = "strata_level")
    strata <- "strata"
  }

  result <- result |>
    dplyr::select(
      "cdm_name",
      "cohort_name" = "group_level", dplyr::all_of(strata),
      "treatment" = "variable_name", "estimate_value",
      "window_name" = "additional_level"
    ) |>
    dplyr::mutate("estimate_value" = as.numeric(.data$estimate_value)) |>
    dplyr::mutate("treatment" = factor(.data$treatment, levels = lev))

  if (length(colour) > 0) {
    cols <- colour
    colourLab <- gsub("_", " ", cols) |>
      paste0(collapse = ", ")
    colour <- omopgenerics::uniqueId(exclude = colnames(result))
    result <- result |>
      tidyr::unite(col = !!colour, dplyr::all_of(cols), remove = FALSE)
  } else {
    colour <- NULL
    colourLab <- NULL
  }

  if (length(facetX) == 0) facetX <- "."
  if (length(facetY) == 0) facetY <- "."
  form <- paste0(
    paste0(facetY, collapse = " + "), " ~ ", paste0(facetX, collapse = " + ")
  ) |>
    stats::as.formula()

  ggplot2::ggplot(data = result, mapping = ggplot2::aes(x = .data$estimate_value, y = .data$treatment, fill = .data[[colour]])) +
    ggplot2::geom_col() +
    ggplot2::facet_grid(form) +
    ggplot2::labs(fill = colourLab, x = "Percentage", y = "Treatment")
}

#' Generate a custom ggplot2 from a summarised_result object generated with
#' summariseDrugRestart() function.
#'
#' @param result A summarised_result object with results from
#' summariseDrugRestart().
#' @param facetX Vector of variables to facet by horizontally. Allowed options
#' are: "cdm_name", "cohort_name", "strata", "variable_name"
#' @param facetY Vector of variables to facet by vertically Allowed options
#' are: "cdm_name", "cohort_name", "strata", "variable_name".
#' @param colour Vector of variables to distinct by colour. Allowed options
#' are: "cdm_name", "cohort_name", "strata", "variable_name", and
#' "variable_level".
#' @param splitStrata Whether to split strata columns.
#'
#' @return A ggplot2 object.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(DrugUtilisation)
#'
#' cdm <- mockDrugUtilisation()
#'
#' conceptlist <- list("a" = 1125360, "b" = c(1503297, 1503327))
#' cdm <- generateDrugUtilisationCohortSet(
#'   cdm = cdm,
#'   name = "switch_cohort",
#'   conceptSet = conceptlist
#' )
#'
#' result <- cdm$cohort1 |>
#'   summariseDrugRestart(switchCohortTable = "switch_cohort")
#'
#' plotDrugRestart(result)
#'
#' CDMConnector::cdmDisconnect(cdm = cdm)
#' }
#'
plotDrugRestart <- function(result,
                            facetX = "variable_name",
                            facetY = c("cdm_name", "cohort_name", "strata"),
                            colour = "variable_level",
                            splitStrata = TRUE) {
  rlang::check_installed("ggplot2")
  # check input
  assertChoice(
    facetX,
    choices = c("cdm_name", "cohort_name", "strata", "variable_name"),
    null = TRUE, unique = TRUE
  )
  assertChoice(
    facetY,
    choices = c("cdm_name", "cohort_name", "strata", "variable_name"),
    null = TRUE, unique = TRUE
  )
  assertLogical(splitStrata, length = 1)
  assertChoice(
    colour,
    choices = c("cdm_name", "cohort_name", "strata", "variable_name", "variable_level"),
    null = TRUE,
    unique = TRUE
  )

  result <- omopgenerics::newSummarisedResult(result) |>
    visOmopResults::filterSettings(.data$result_type == "summarise_drug_restart") |>
    dplyr::filter(.data$estimate_name == "percentage")

  if (nrow(result) == 0) {
    cli::cli_warn(c("!" = "There are no results to plot, returning empty plot"))
    return(ggplot2::ggplot())
  }

  # to display order accordingly
  lev <- c("restart", "switch", "restart and switch", "not treated") |> rev()

  warnDuplicatePoints(result, unique(c(facetX, facetY, colour)))

  if (splitStrata) {
    strata <- visOmopResults::strataColumns(result)
    result <- result |> visOmopResults::splitStrata()
    facetX <- substituteStrata(facetX, strata)
    facetY <- substituteStrata(facetY, strata)
    colour <- substituteStrata(colour, strata)
  } else {
    result <- result |> dplyr::rename("strata" = "strata_level")
    strata <- "strata"
  }

  result <- result |>
    dplyr::select(
      "cdm_name",
      "cohort_name" = "group_level", dplyr::all_of(strata),
      "variable_name", "estimate_value", "variable_level"
    ) |>
    dplyr::mutate("estimate_value" = as.numeric(.data$estimate_value)) |>
    dplyr::mutate("variable_level" = factor(
      .data$variable_level,
      levels = lev, labels = stringr::str_to_sentence(lev)
    ))

  if (length(colour) > 0) {
    cols <- colour
    colourLab <- gsub("_", " ", cols) |>
      paste0(collapse = ", ")
    colour <- omopgenerics::uniqueId(exclude = colnames(result))
    result <- result |>
      tidyr::unite(col = !!colour, dplyr::all_of(cols), remove = FALSE)
  } else {
    colour <- NULL
    colourLab <- NULL
  }

  if (length(facetX) == 0) facetX <- "."
  if (length(facetY) == 0) facetY <- "."
  form <- paste0(
    paste0(facetY, collapse = " + "), " ~ ", paste0(facetX, collapse = " + ")
  ) |>
    stats::as.formula()

  ggplot2::ggplot(
    data = result,
    mapping = ggplot2::aes(
      x = .data$estimate_value,
      y = .data$variable_level,
      fill = .data[[colour]]
    )
  ) +
    ggplot2::geom_col() +
    ggplot2::facet_grid(form) +
    ggplot2::labs(fill = colourLab, x = "Percentage", y = "Event") +
    ggplot2::theme(legend.title = ggplot2::element_blank())
}

warnDuplicatePoints <- function(result, exclude) {
  result <- result |>
    dplyr::rename("cohort_name" = "group_level", "strata" = "strata_level", "treatment" = "variable_name")
  potential <- c("result_id", "cdm_name", "cohort_name", "strata", "treatment")
  potential <- potential[!potential %in% exclude]
  duplicates <- result |>
    dplyr::select(dplyr::all_of(potential)) |>
    dplyr::distinct()
  if (nrow(duplicates) > 1) {
    x <- list()
    for (col in colnames(duplicates)) {
      x[[col]] <- unique(duplicates[[col]])
    }
    x <- x[lengths(x) > 1]
    mes <- c("!" = "There are duplicated points, not included either in facetX, facetY or colour:")
    for (k in seq_along(x)) {
      col <- names(x)[k]
      values <- x[[k]]
      mes <- c(mes, "*" = paste0(col, ": ", paste0(values, collapse = ", "), "."))
    }
    cli::cli_inform(mes)
  }
  return(invisible(NULL))
}
substituteStrata <- function(x, strata) {
  id <- which(x == "strata")
  if (length(id) == 1) {
    len <- length(x)
    if (id == 1) {
      x <- c(strata, x[-1])
    } else if (id == len) {
      x <- c(x[1:(id - 1)], strata)
    } else {
      x <- c(x[1:(id - 1)], strata, x[(id + 1):len])
    }
  }
  return(x)
}

#' Generate a plot visualisation (ggplot2) from the output of
#' summariseIndication
#'
#' @param result A summarised_result object.
#' @param x Variables to be used in the x axis.
#' @param facet Variables to be used to facet the plot.
#' @param color Variables to be used to color the plot.
#' @param splitStrata Whether to split strata.
#'
#' @return A ggplot2 object
#'
#' @export
#'
#' @examples
#' \donttest{
#' library(DrugUtilisation)
#' library(CDMConnector)
#' library(dplyr)
#'
#' cdm <- mockDrugUtilisation()
#'
#' indications <- list("headache" = 378253, "asthma" = 317009)
#' cdm <- generateConceptCohortSet(cdm, indications, "indication_cohorts")
#'
#' cdm <- generateIngredientCohortSet(
#'   cdm = cdm, name = "drug_cohort", ingredient = "acetaminophen"
#' )
#'
#' result <- cdm$drug_cohort |>
#'   summariseIndication(
#'     indicationCohortName = "indication_cohorts",
#'     unknownIndicationTable = "condition_occurrence",
#'     indicationWindow = list(c(-Inf, 0), c(-365, 0))
#'   )
#'
#' plotIndication(result)
#' }
#'
plotIndication <- function(result,
                           x = "window",
                           facet = c("cdm_name", "cohort_name", "strata"),
                           color = c("indication"),
                           splitStrata = TRUE) {
  # initial checks
  assertClass(result, class = "summarised_result")
  result <- result |>
    visOmopResults::filterSettings(.data$result_type == "summarise_indication")
  if (nrow(result) == 0) {
    cli::cli_warn(c("!" = "No `summarise_indication` records found, returning empty plot."))
    return(ggplot2::ggplot())
  }
  assertLogical(splitStrata, length = 1)
  assertCharacter(x)
  assertCharacter(facet)
  assertCharacter(color)
  if (splitStrata) {
    strata <- visOmopResults::strataColumns(result)
  } else {
    strata <- "strata"
  }
  facet <- insertValue(facet, which(facet == "strata"), strata)
  x <- insertValue(x, which(x == "strata"), strata)
  color <- insertValue(color, which(color == "strata"), strata)
  facet <- insertValue(facet, which(facet == "group"), "cohort_name")
  x <- insertValue(x, which(x == "group"), "cohort_name")
  color <- insertValue(color, which(color == "group"), "cohort_name")
  opts <- c("cdm_name", "cohort_name", strata, "indication", "window")
  assertChoice(x, choices = opts, unique = TRUE)
  assertChoice(facet, choices = opts, unique = TRUE)
  assertChoice(color, choices = opts, unique = TRUE)
  intersection <- intersect(x, facet)
  if (length(intersection) > 0) {
    cli::cli_abort("{intersection} present in x and facet, no common values allowed")
  }
  if (length(x) == 0) cli::cli_abort("`x` can not be empty")

  if (splitStrata) {
    result <- result |>
      visOmopResults::splitStrata()
  } else {
    result <- result |>
      dplyr::mutate(
        "strata" = paste0(.data$strata_name, ": ", .data$strata_level)
      )
  }
  result <- result |>
    dplyr::filter(.data$estimate_name == "percentage") |>
    dplyr::select(
      "cdm_name",
      "cohort_name" = "group_level",
      "indication" = "variable_level", "window" = "variable_name",
      dplyr::all_of(strata), "estimate_value"
    ) |>
    dplyr::mutate("estimate_value" = as.numeric(.data$estimate_value)) |>
    dplyr::group_by(dplyr::across(!c("estimate_value", "indication"))) |>
    dplyr::mutate(
      estimate_value = 100 * .data$estimate_value / sum(.data$estimate_value)
    ) |>
    dplyr::ungroup() |>
    tidyr::unite(col = "x", dplyr::all_of(x), remove = FALSE)

  notPresent <- unique(c(x, facet, color))
  notPresent <- notPresent[!notPresent %in% opts]
  if (length(notPresent) > 0) {
    notPresentNoUnique <- character()
    for (k in notPresent) {
      if (length(result[[k]] |> unique()) > 0) {
        notPresentNoUnique <- c(notPresentNoUnique, k)
      }
    }
    if (length(notPresentNoUnique) > 0) {
      cli::cli_abort("{.var {notPresent}} must be in either 'x', 'facet' or 'color'")
    }
  }

  if (length(color) > 0) {
    result <- result |>
      tidyr::unite(col = "color", dplyr::all_of(color), remove = FALSE)
  } else {
    result <- result |> dplyr::mutate("color" = "percentage")
  }

  result <- result |>
    dplyr::select(dplyr::all_of(c("x", facet, "color", "estimate_value")))

  p <- result |>
    ggplot2::ggplot(mapping = ggplot2::aes(
      x = .data$x, y = .data$estimate_value, color = .data$color,
      fill = .data$color
    )) +
    ggplot2::geom_col() +
    ggplot2::xlab("") +
    ggplot2::ylab("Percentage") +
    ggplot2::ylim(c(0, 101))

  if (length(facet) > 0) {
    p <- p +
      ggplot2::facet_wrap(facets = facet)
  }

  return(p)
}

insertValue <- function(x, pos, value) {
  if (length(pos) == 1) {
    if (pos == 1) {
      x <- c(value, x[-1])
    } else if (pos == length(x)) {
      x <- c(x[-length(x)], value)
    } else {
      x <- c(x[1:(pos - 1)], value, x[(pos + 1):length(x)])
    }
  }
  return(x)
}

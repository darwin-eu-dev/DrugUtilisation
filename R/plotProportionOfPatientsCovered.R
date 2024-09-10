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


#' Plot proportion of patients covered
#'
#' @param result Output of summariseProportionOfPatientsCovered
#' @param ylim Limits for the Y axis
#' @param facet Variables to use for facets
#' @param colour Variables to use for colours
#' @param colour_name Colour legend name
#'
#' @return Plot of proportion Of patients covered over time
#' @export
#'
plotProportionOfPatientsCovered <- function(result,
                                            ylim = c(0, NA),
                                            facet = NULL,
                                            colour = NULL,
                                            colour_name = NULL) {
  rlang::check_installed("ggplot2", reason = "for plot functions")
  rlang::check_installed("scales", reason = "for plot functions")

  checkmate::assertTRUE(inherits(result, "summarised_result"))

  workingResult <- result |>
    dplyr::filter(.data$estimate_name == "ppc")

  if (nrow(workingResult) == 0) {
    cli::cli_warn("No PPC results found")
    return(ggplot2::ggplot() +
      ggplot2::theme_void())
  }

  plot_data <- getPlotData(
    estimates = workingResult,
    facetVars = facet,
    colourVars = colour
  ) |>
    dplyr::mutate(
      ppc = as.numeric(.data$ppc) / 100,
      time = as.numeric(.data$time)
    )

  if (is.null(colour)) {
    plot <- plot_data %>%
      ggplot2::ggplot(
        ggplot2::aes(
          x = .data$time,
          y = .data$ppc
        )
      )
  } else {
    plot <- plot_data %>%
      ggplot2::ggplot(
        ggplot2::aes(
          x = .data$time,
          y = .data$ppc,
          group = .data$colour_vars,
          colour = .data$colour_vars,
          fill = .data$colour_vars
        )
      ) +
      ggplot2::geom_line(linewidth = 0.25) +
      ggplot2::labs(
        fill = colour_name,
        colour = colour_name
      )
  }


  plot <- plot +
    ggplot2::geom_line(linewidth = 0.5)

  if (is.null(ylim)) {
    plot <- plot +
      ggplot2::scale_y_continuous(
        labels =
          scales::percent_format(accuracy = 0.1)
      )
  } else {
    plot <- addYLimits(plot = plot, ylim = ylim)
  }

  if (!is.null(facet)) {
    facetNcols <- NULL
    if ("facetNcols" %in% names(options)) {
      facetNcols <- options[["facetNcols"]]
    }
    facetScales <- "fixed"
    if ("facetScales" %in% names(options)) {
      facetScales <- options[["facetScales"]]
    }

    plot <- plot +
      ggplot2::facet_wrap(ggplot2::vars(.data$facet_var),
        ncol = facetNcols,
        scales = facetScales
      ) +
      ggplot2::theme_bw()
  } else {
    plot <- plot +
      ggplot2::theme_minimal()
  }

  plot <- plot +
    ggplot2::ylab("Proportion of patients covered (PPC)") +
    ggplot2::xlab("Days")

  return(plot)
}

getPlotData <- function(estimates, facetVars, colourVars) {
  plotData <- estimates

  plotData <- plotData |>
    visOmopResults::splitAdditional() |>
    visOmopResults::addSettings() |>
    dplyr::select(!"estimate_type") |>
    tidyr::pivot_wider(
      names_from = "estimate_name",
      values_from = "estimate_value"
    )


  if (!is.null(facetVars)) {
    plotData <- plotData %>%
      tidyr::unite("facet_var",
        c(dplyr::all_of(.env$facetVars)),
        remove = FALSE, sep = "; "
      )
  }
  if (!is.null(colourVars)) {
    plotData <- plotData %>%
      tidyr::unite("colour_vars",
        c(dplyr::all_of(.env$colourVars)),
        remove = FALSE, sep = "; "
      )
  }

  return(plotData)
}

addYLimits <- function(plot, ylim) {
  plot <- plot +
    ggplot2::scale_y_continuous(
      labels =
        scales::percent_format(accuracy = 0.1),
      limits = ylim
    )
  return(plot)
}

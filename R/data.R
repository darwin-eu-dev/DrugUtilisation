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

#' Patterns valid to compute daily dose with the associated formula.
#'
#' @format A data frame with eight variables: \code{pattern_id}, \code{amount},
#'   \code{amount_unit}, \code{numerator}, \code{numerator_unit},
#'   \code{denominator}, \code{denominator_unit}, \code{formula_name} and
#'   \code{formula}.
"patternsWithFormula"

# #' Formulas applied to compute daily dose.
# #'
# #' @export
# #'
# #' @examples
# #' \donttest{
# #' library(DrugUtilisation)
# #' seeFormulas()
# #' }
# #'
# seeFormulas <- function() {
#   mes <- c(
#     "Currenly 4 different formualas are applied:",
#     paste0(cli::style_bold("concentration formulation:   "), cli::col_silver("quantity * numerator / days exposed")),
#     paste0(cli::style_bold("fixed amount formulation:    "), cli::col_silver("quantity * amount / days exposed")),
#     paste0(cli::style_bold("time based with denominator: "), cli::col_silver("if (denominator>24) {numerator * 24 / denominator} else {numerator}")),
#     paste0(cli::style_bold("time based no denominator:   "), cli::col_silver("24 * numerator"))
#   )
#   cat(mes, sep = "\n")
#   invisible(TRUE)
# }

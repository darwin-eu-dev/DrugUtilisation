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

#' Patterns and routes valid to compute daily dose
#'
#' @format A data frame with eight variables: \code{amount}, \code{amount_unit},
#'   \code{numerator}, \code{numerator_unit}, \code{denominator},
#'   \code{denominator_unit}, \code{route} and \code{formula_id}. See the
#'   formulas using seeFormulas.
"patternsWithFormula"

#' Formulas applied to compute daily dose.
#'
#' @export
#'
#' @examples
#' \donttest{
#' library(DrugUtilisation)
#' seeFormulas()
#' }
#'
seeFormulas <- function() {
  mes <- c(
    "Currenly 4 different formualas are applied:",
    cli::style_bold("formula_id          ", cli::col_silver("formula")),
    paste0("     1         ", cli::col_silver("quantity * numerator / days exposed")),
    paste0("     2         ", cli::col_silver("quantity * amount / days exposed")),
    paste0("     3         ", cli::col_silver("24 * numerator / denominator")),
    paste0("     4         ", cli::col_silver("24 * numerator"))
  )
  cat(mes, sep = "\n")
  invisible(TRUE)
}

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
  x <- x %>%
    dplyr::rename("variable" = .env$variableName)
  # impute if allow zero
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
  # impute lower bound
  if (!is.null(lowerBound)) {
    x <- x %>%
      dplyr::mutate(impute = dplyr::if_else(
        .data$variable < .env$lowerBound,
        1,
        .data$impute
      ))
  }
  if (!is.null(upperBound)) {
    x <- x %>%
      dplyr::mutate(impute = dplyr::if_else(
        .data$variable > .env$upperBound,
        1,
        .data$impute
      ))
  }
  if (isFALSE(impute)) {
    x <- x %>%
      dplyr::filter(.data$impute == 0)
  } else {
    x <- x %>%
      dplyr::rename("imputeValue" = .env$imputeValueName) %>%
      dplyr::mutate(variable = dplyr::if_else(
        .data$impute == 1,
        .data$imputeValue,
        .data$variable
      )) %>%
      dplyr::rename(!!imputeValueName := "imputeValue")
  }
  x <- x %>%
    dplyr::select(-"impute") %>%
    dplyr::rename(!!variableName := "variable") %>%
    dplyr::compute()
  return(x)
}

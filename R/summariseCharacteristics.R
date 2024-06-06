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

#' This function has been deprecated please use CohortCharacteristics package
#'
#' @return An error pointing to the new function in CohortCharacteristics
#'
#' @export
#'
summariseCharacteristics <- function() {
  lifecycle::deprecate_stop(
    when = "0.6.1",
    what = "DrugUtilisation::summariseCharacteristics()",
    with = "CohortCharacteristics::summariseCharacteristics()"
  )
}

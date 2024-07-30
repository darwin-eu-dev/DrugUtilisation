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

assertCharacter <- function(x,
                            length = NULL,
                            na = FALSE,
                            null = FALSE,
                            unique = FALSE,
                            named = FALSE,
                            minNumCharacter = 0,
                            call = parent.frame(),
                            msg = NULL) {
  nm <- paste0(substitute(x), collapse = "")
  if (is.null(msg)) {
    msg <- errorMessage(
      nm = nm, object = "a character vector", length = length, na = na,
      null = null, unique = unique, named = named,
      minNumCharacter = minNumCharacter
    )
  }

  # assert null
  if (assertNull(x, nm, null, msg, call)) {
    # assert class
    if (!is.character(x)) {
      c("!" = "{.strong `{nm}` is not a character vector.}", msg) |>
        cli::cli_abort(call = call)
    }

    # assert length
    assertLength(x, nm, length, msg, call)

    # assert na
    assertNa(x, nm, na, msg, call)

    # assert unique
    assertUnique(x, nm, unique, msg, call)

    # assert named
    assertNamed(x, nm, named, msg, call)

    # minimum number of characters
    pos <- which(nchar(x) < minNumCharacter)
    if (length(pos) > 0) {
      c("!" = "{.strong `{nm}` has less than {minNumCharacter} character{?s} in position: {pos}.}", msg) |>
        cli::cli_abort(call = call)
    }
  }

  return(invisible(x))
}
assertChoice <- function(x,
                         choices,
                         length = NULL,
                         na = FALSE,
                         null = FALSE,
                         unique = FALSE,
                         named = FALSE,
                         call = parent.frame(),
                         msg = NULL) {
  nm <- paste0(substitute(x), collapse = "")
  if (is.null(msg)) {
    msg <- errorMessage(
      nm = nm,
      object = "a choice between {choices}" |>
        cli::cli_text() |>
        cli::cli_fmt() |>
        paste0(collapse = " "),
      length = length, na = na, null = null, unique = unique, named = named
    )
  }

  # assert null
  if (assertNull(x, nm, null, msg, call)) {
    # no NA vector
    xNoNa <- x[!is.na(x)]

    # assert class
    if (!identical(class(x), class(choices))) {
      c("!" = "{.strong class of `{nm}` is {class(x)} different from class of choices {class(choices)}.}", msg) |>
        cli::cli_abort(call = call)
    }

    # assert length
    assertLength(x, nm, length, msg, call)

    # assert na
    assertNa(x, nm, na, msg, call)

    # assert unique
    assertUnique(x, nm, unique, msg, call)

    # assert named
    assertNamed(x, nm, named, msg, call)

    # assert choices
    if (base::length(xNoNa) > 0) {
      if (!all(xNoNa %in% choices)) {
        c("!" = "{.strong `{nm}` is not a choice between {choices}.}", msg) |>
          cli::cli_abort(call = call)
      }
    }
  }

  return(invisible(x))
}
assertClass <- function(x,
                        class,
                        length = NULL,
                        null = TRUE,
                        all = FALSE,
                        extra = TRUE,
                        call = parent.frame(),
                        msg = NULL) {
  nm <- paste0(substitute(x), collapse = "")
  if (is.null(msg)) {
    if (all) {
      obj <- "an object with class: {class}"
    } else {
      obj <- "an object with at least one of these classes: {class}"
    }
    if (extra) {
      obj <- paste0(obj, "; it can contain extra classes")
    } else {
      obj <- paste0(obj, "; it can not contain extra classes")
    }
    msg <- errorMessage(nm = nm, object = obj, null = null, length = length)
  }

  # assert null
  if (assertNull(x, nm, null, msg, call)) {
    # class of the object
    cl <- base::class(x)

    # class
    if (isTRUE(all)) {
      if (!base::all(class %in% cl)) {
        c("!" = "{.strong `{nm}` has class {cl}, but must have {class}.}", msg) |>
          cli::cli_abort(call = call)
      }
    } else if (isFALSE(all)) {
      if (!base::any(class %in% cl)) {
        c("!" = "{.strong `{nm}` has class {cl}, but must have at least of the following: {class}.}", msg) |>
          cli::cli_abort(call = call)
      }
    }

    # extra classes
    if (isFALSE(extra)) {
      extraClasses <- cl[!cl %in% class]
      if (length(extraClasses) > 0) {
        c("!" = "{.strong `{nm}` extra class: {extraClasses} not allowed.}", msg) |>
          cli::cli_abort(call = call)
      }
    }

    # assert length
    assertLength(x, nm, length, msg, call)
  }
  invisible(x)
}
assertList <- function(x,
                       length = NULL,
                       na = FALSE,
                       null = FALSE,
                       unique = FALSE,
                       named = FALSE,
                       class = NULL,
                       call = parent.frame(),
                       msg = NULL) {
  nm <- paste0(substitute(x), collapse = "")
  if (is.null(msg)) {
    if (!is.null(class)) {
      obj <- "a list with objects of class {class}" |>
        cli::cli_text() |>
        cli::cli_fmt() |>
        paste0(collapse = " ")
    } else {
      obj <- "a list"
    }
    msg <- errorMessage(
      nm = nm, object = obj, length = length, na = na, null = null,
      unique = unique, named = named
    )
  }

  # assert null
  if (assertNull(x, nm, null, msg, call)) {
    # no NA vector
    xNoNa <- x[!is.na(x)]

    # assert class
    if (!is.list(x)) {
      c("!" = "{.strong `{nm}` is not a list.}", msg) |>
        cli::cli_abort(call = call)
    }

    # assert length
    assertLength(x, nm, length, msg, call)

    # assert na
    assertNa(x, nm, na, msg, call)

    # assert unique
    assertUnique(x, nm, unique, msg, call)

    # assert named
    assertNamed(x, nm, named, msg, call)

    # assert class
    if (!is.null(class) && length(xNoNa) > 0) {
      flag <- lapply(xNoNa, function(y) {
        any(class %in% base::class(y))
      }) |>
        unlist()
      pos <- which(!flag)
      if (length(pos) > 0) {
        c("!" = "{.strong Elements {pos} does not have class {class}.}", msg) |>
          cli::cli_abort(call = call)
      }
    }
  }

  return(invisible(x))
}
assertLogical <- function(x,
                          length = NULL,
                          na = FALSE,
                          null = FALSE,
                          named = FALSE,
                          call = parent.frame(),
                          msg = NULL) {
  nm <- paste0(substitute(x), collapse = "")
  if (is.null(msg)) {
    msg <- errorMessage(
      nm = nm, object = "a logical", length = length, na = na, null = null,
      named = named
    )
  }

  # assert null
  if (assertNull(x, nm, null, msg, call)) {
    # assert class
    if (!is.logical(x)) {
      c("!" = "{.strong `{nm}` is not logical.}", msg) |>
        cli::cli_abort(call = call)
    }

    # assert length
    assertLength(x, nm, length, msg, call)

    # assert na
    assertNa(x, nm, na, msg, call)

    # assert named
    assertNamed(x, nm, named, msg, call)
  }

  return(invisible(x))
}
assertNumeric <- function(x,
                          integerish = FALSE,
                          min = -Inf,
                          max = Inf,
                          length = NULL,
                          na = FALSE,
                          null = FALSE,
                          unique = FALSE,
                          named = FALSE,
                          call = parent.frame(),
                          msg = NULL) {
  nm <- substitute(x) |> utils::capture.output()
  if (is.null(msg)) {
    if (integerish) obj <- "an integerish numeric" else obj <- "a numeric"
    msg <- errorMessage(
      nm = nm, object = obj, min = min, max = max, length = length, na = na,
      null = null, unique = unique, named = named
    )
  }

  # assert null
  if (assertNull(x, nm, null, msg, call)) {
    # no NA vector
    xNoNa <- x[!is.na(x)]

    # assert class
    if (!is.numeric(x)) {
      c("!" = "{.strong `{nm}` is not numeric.}", msg) |>
        cli::cli_abort(call = call)
    }

    # assert integerish
    if (integerish & base::length(xNoNa) > 0 & !all(is.infinite(xNoNa))) {
      xInt <- xNoNa[!is.infinite(xNoNa)]
      err <- max(abs(xInt - round(xInt)))
      if (err > 0.0001) {
        c("!" = "{.strong `{nm}` is not integerish.}", msg) |>
          cli::cli_abort(call = call)
      }
    }

    # assert lower bound
    if (!is.infinite(min) & base::length(xNoNa) > 0) {
      if (base::min(xNoNa) < min) {
        c("!" = "{.strong `{nm}` is not bigger or equal to {min}.}", msg) |>
          cli::cli_abort(call = call)
      }
    }

    # assert upper bound
    if (!is.infinite(max) & base::length(xNoNa) > 0) {
      if (base::max(xNoNa) > max) {
        c("!" = "{.strong `{nm}` is not smaller or equal to {max}.}", msg) |>
          cli::cli_abort(call = call)
      }
    }

    # assert length
    assertLength(x, nm, length, msg, call)

    # assert na
    assertNa(x, nm, na, msg, call)

    # assert unique
    assertUnique(x, nm, unique, msg, call)

    # assert named
    assertNamed(x, nm, named, msg, call)
  }

  return(invisible(x))
}
assertTable <- function(x,
                        class = NULL,
                        numberColumns = NULL,
                        numberRows = NULL,
                        columns = character(),
                        allowExtraColumns = TRUE,
                        null = FALSE,
                        unique = FALSE,
                        call = parent.frame(),
                        msg = NULL) {
  nm <- paste0(substitute(x), collapse = "")
  if (is.null(msg)) {
    if (!is.null(class)) {
      obj <- "a table of class: {class}" |>
        cli::cli_text() |>
        cli::cli_fmt() |>
        paste0(collapse = " ")
    } else {
      obj <- "a table"
    }
    msg <- errorMessage(
      nm = nm, object = obj, numberColumns = numberColumns,
      numberRows = numberRows, columns = columns,
      allowExtraColumns = allowExtraColumns, null = null, unique = unique
    )
  }

  # assert null
  if (assertNull(x, nm, null, msg, call)) {
    # assert class
    if (!is.null(class) && !any(class %in% base::class(x))) {
      c("!" = "{.strong `{nm}` must be one of: {class}, but has class: {base::class(x)}.}", msg) |>
        cli::cli_abort(call = call)
    }

    # assert numberColumns
    if (!is.null(numberColumns)) {
      if (ncol(x) != numberColumns) {
        c("!" = "{.strong `{nm}` must have {numberColumns} columns, but has {ncol(x)}.}", msg) |>
          cli::cli_abort(call = call)
      }
    }

    # assert numberRows
    if (!is.null(numberRows)) {
      if (nrow(x) != numberRows) {
        c("!" = "{.strong `{nm}` must have {numberRows} rows, but has {nrow(x)}.}", msg) |>
          cli::cli_abort(call = call)
      }
    }

    # assert columns
    if (!is.null(columns)) {
      notPresent <- columns[!columns %in% colnames(x)]
      if (length(notPresent) > 0) {
        c("!" = "{.strong {notPresent} not present in `{nm}`.}", msg) |>
          cli::cli_abort(call = call)
      }
    }

    # allow extra columns
    if (!allowExtraColumns) {
      extraCols <- colnames(x)[!colnames(x) %in% columns]
      if (length(extraCols) > 0) {
        c("!" = "{.strong `{nm}` contains not extra columns that are not allowed: {extraCols}.}", msg) |>
          cli::cli_abort(call = call)
      }
    }

    # assert unique.
    if (unique) {
      if (nrow(x) != x |>
        dplyr::distinct() |>
        nrow()) {
        c("!" = "{.strong Rows are not unique.}", msg) |>
          cli::cli_abort(call = call)
      }
    }
  }

  return(invisible(x))
}
errorMessage <- function(nm,
                         object,
                         length = NULL,
                         na = NULL,
                         named = NULL,
                         unique = NULL,
                         null = NULL,
                         min = -Inf,
                         max = Inf,
                         minNumCharacter = 0,
                         numberColumns = NULL,
                         numberRows = NULL,
                         columns = NULL,
                         allowExtraColumns = NULL,
                         distinct = NULL) {
  paste0(
    c(
      "`{nm}` must be {object}",
      if (!is.null(length)) "with length = {length}",
      if (isFALSE(na)) "it can not contain NA",
      if (isTRUE(named)) "it has to be named",
      if (isTRUE(unique)) "it has to contain unique elements",
      if (isFALSE(null)) "it can not be NULL",
      if (!is.infinite(min)) "greater or equal to {min}",
      if (!is.infinite(max)) "smaller or equal to {max}",
      if (minNumCharacter > 0) "with at least {minNumCharacter} character per element",
      if (is.numeric(numberColumns)) "with exactly {numberColumns} columns",
      if (is.numeric(numberRows)) "with exactly {numberRows} rows",
      if (is.character(columns) && length(columns) > 0) "must contain {columns} as columns",
      if (isFALSE(allowExtraColumns)) "no extra columns are allowed",
      if (isTRUE(distinct)) "rows must be unique"
    ),
    collapse = "; "
  ) |>
    cli::cli_text() |>
    cli::cli_fmt() |>
    paste0(collapse = " ") |>
    paste0(".") |>
    as.character()
}
assertLength <- function(x, nm, length, msg, call) {
  len <- base::length(x)
  if (!is.null(length) && len != length) {
    c(
      "!" = "{.strong `{nm}` has length {len}, but must have length {length}.}",
      msg
    ) |>
      cli::cli_abort(call = call)
  }
  invisible(NULL)
}
assertNa <- function(x, nm, na, msg, call) {
  if (!na && length(x) > 0) {
    pos <- which(is.na(x))
    if (length(pos) > 0) {
      c("!" = "{.strong `{nm}` contains NA in position {pos}.}", msg) |>
        cli::cli_abort(call = call)
    }
  }
  invisible(NULL)
}
assertNamed <- function(x, nm, named, msg, call) {
  if (named && length(names(x)[names(x) != ""]) != length(x)) {
    c("!" = "{.strong `{nm}` must be named.}", msg) |>
      cli::cli_abort(call = call)
  }
  invisible(NULL)
}
assertUnique <- function(x, nm, unique, msg, call) {
  if (unique && length(unique(x)) != length(x)) {
    c("!" = "{.strong `{nm}` must be unique.}", msg) |>
      cli::cli_abort(call = call)
  }
  invisible(NULL)
}
assertNull <- function(x, nm, null, msg, call) {
  if (!null && is.null(x)) {
    c("!" = "{.strong `{nm}` can not be NULL.}", msg) |>
      cli::cli_abort(call = call)
  }
  return(!is.null(x))
}

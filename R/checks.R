#' @noRd
checkAgeGroup <- function(ageGroup) {
  checkmate::assertList(ageGroup, min.len = 1, null.ok = TRUE)
  if (!is.null(ageGroup)) {
    if (is.numeric(ageGroup[[1]])) {
      ageGroup <- list("age_group" = ageGroup)
    }
    for (k in seq_along(ageGroup)) {
      invisible(checkCategory(ageGroup[[k]]))
    }
    if (is.null(names(ageGroup))) {
      names(ageGroup) <- paste0("age_group_", 1:length(ageGroup))
    }
    if ("" %in% names(ageGroup)) {
      id <- which(names(ageGroup) == "")
      names(ageGroup)[id] <- paste0("age_group_", id)
    }
  }
  return(ageGroup)
}

#' @noRd
checkCategory <- function(category) {
  checkmate::assertList(
    category,
    types = "integerish", any.missing = FALSE, unique = TRUE,
    min.len = 1
  )

  if (is.null(names(category))) {
    names(category) <- rep("", length(category))
  }

  # check length
  category <- lapply(category, function(x) {
    if (length(x) == 1) {
      x <- c(x, x)
    } else if (length(x) > 2) {
      cli::cli_abort(
        paste0(
          "Categories should be formed by a lower bound and an upper bound, ",
          "no more than two elements should be provided."
        ),
        call. = FALSE
      )
    }
    return(x)
  })

  # check lower bound is smaller than upper bound
  checkLower <- unlist(lapply(category, function(x) {
    x[1] <= x[2]
  }))
  if (!(all(checkLower))) {
    cli::cli_abort("Lower bound should be equal or smaller than upper bound")
  }

  # built tibble
  result <- lapply(category, function(x) {
    dplyr::tibble(lower_bound = x[1], upper_bound = x[2])
  }) %>%
    dplyr::bind_rows() %>%
    dplyr::mutate(category_label = names(.env$category)) %>%
    dplyr::mutate(category_label = dplyr::if_else(
      .data$category_label == "",
      paste0(.data$lower_bound, " to ", .data$upper_bound),
      .data$category_label
    )) %>%
    dplyr::arrange(.data$lower_bound)

  # check overlap
  if (nrow(result) > 1) {
    lower <- result$lower_bound[2:nrow(result)]
    upper <- result$upper_bound[1:(nrow(result) - 1)]
    if (!all(lower > upper)) {
      cli::cli_abort("There can not be overlap between categories")
    }
  }
  return(result)
}

#' @noRd
checkCdm <- function(cdm, tables = NULL) {
  if (!isTRUE(inherits(cdm, "cdm_reference"))) {
    cli::cli_abort("cdm must be a CDMConnector CDM reference object")
  }
  if (!is.null(tables)) {
    tables <- tables[!(tables %in% names(cdm))]
    if (length(tables) > 0) {
      cli::cli_abort(paste0(
        "tables: ",
        paste0(tables, collapse = ", "),
        "are nor present in the cdm object"
      ))
    }
  }
  invisible(NULL)
}

#' @noRd

checkListTable <- function(listTables) {


  checkmate::assertTRUE(length(listTables) == length(unique(names(listTables))))

  namesTables <- names(listTables)

  namesTables <- lapply(stringr::str_split(namesTables, "[[:upper:]]"),
                        function(x) {
                          x[1]
                        }) %>%
    unlist() %>%
    unique()

  if (length(namesTables) > 0) {
    for (k in 1:length(namesTables)) {
      errorMessage <- checkmate::makeAssertCollection()
      name <- namesTables[k]
      tableName <- listTables[[paste0(name, "TableName")]]
      set <- listTables[[paste0(name, "Set")]]
      lookbackWindow <- listTables[[paste0(name, "Window")]]
      checkmate::assertTibble(set, add = errorMessage)
      checkmate::assertTRUE(all(c("cohortId", "cohortName") %in% colnames(set)),
                            add = errorMessage)
      checkmate::assertIntegerish(set$cohortId, add = errorMessage)
      checkmate::assertCharacter(set$cohortName,
                                 any.missing = FALSE, add = errorMessage)
      checkmate::assertIntegerish(
        lookbackWindow,
        min.len = 1,
        max.len = 2,
        null.ok = FALSE,
        add = errorMessage
      )
      checkmate::assertTRUE(tableName %in% names(cdm), add = errorMessage)
      checkmate::assertTRUE(all(
        colnames(cdm[[tableName]]) %in% c(
          "cohort_definition_id",
          "subject_id",
          "cohort_start_date",
          "cohort_end_date"
        )
      ),
      add = errorMessage)
      if (!errorMessage$isEmpty()) {
        errorMessage$push(paste0("- In ", name))
      }
      checkmate::reportAssertions(collection = errorMessage)
    }
  }

}

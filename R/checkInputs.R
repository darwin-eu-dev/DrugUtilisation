checkInputs <- function(...) {
  inputs <- list(...)
  lapply(names(inputs), function(x) {checkInput(inputs[[x]], x)})
  checkDependantVariables(inputs)
  invisible(NULL)
}

checkInput <- function(x, nam) {
  listChecks <- c(
    "cdm", "conceptSetList", "name", "summariseMode", "fixedTime",
    "daysPriorHistory", "gapEra", "priorUseWashout", "cohortDateRange",
    "imputeDuration", "durationRange", "attrition", "x", "reason", "tableRef",
    "targetCohortName"
  )
  if (!(nam %in% listChecks)) {
    cli::cli_abort(paste("Input parameter could not be checked:", nam))
  }
  eval(parse(text = paste0(
    "output <- check", toupper(substr(nam, 1, 1)), substr(nam, 2, nchar(nam)),
    "(x)"
  )))
  return(output)
}

checkDependantVariables <- function(inputs) {
  nam <- names(inputs)
  if (all(c("name", "cdm") %in% nam)) {
    if (inputs$name %in% names(inputs$cdm)) {
      cli::cli_alert_warning(
        "A cohort with this name already exist in the cdm object. It will be overwritten."
      )
    }
  }
  if (all(c("targetCohortName", "cdm") %in% nam)) {
    if (!(inputs$targetCohortName %in% names(inputs$cdm))) {
      cli::cli_abort("targetCohortName is not in the cdm reference")
    }
    numberRows <- cdm[[targetCohortName]] %>% dplyr::tally() %>% dplyr::pull()
    if (numberRows == 0) {
      cli::cli_abort("targetCohort is empty")
    }
  }
}

checkCdm <- function(cdm) {
  if (!("cdm_reference" %in% class(cdm))) {
    cli::cli_abort(
      "Argument cdm is not a valid cdm reference, please use
      CDMConnector::cdmFromCon() to create a valid cdm reference"
    )
  }
}

checkConceptSetList <- function(x) {
  errorMessage <- "conceptSetList must be a uniquely named list of integerish,
  no NA are allowed"
  if (!is.list(x)) {
    cli::cli_abort(errorMessage)
  }
  if (!all(sapply(x, is.numeric))) {
    cli::cli_abort(errorMessage)
  }
  x <- unlist(x)
  if (any(is.na(x))) {
    cli::cli_abort(errorMessage)
  }
  if (any(abs(x - round(x)) > sqrt(.Machine$double.eps))) {
    cli::cli_abort(errorMessage)
  }
  if (length(names(x)) != length(x)) {
    cli::cli_abort(errorMessage)
  }
  if (length(names(x)) != length(unique(names(x)))) {
    cli::cli_abort(errorMessage)
  }
}

checkName <- function(name) {
  checkmate::assertCharacter(name, len = 1, any.missing = FALSE)
  if (name %in% CDMConnector::tbl_group("all")) {
    cli::cli_abort(
      'name can not one of the stadard tables of the cdm. To see standard
      tables: CDMConnector::tbl_group("all")'
    )
  }
}

checkSummariseMode <- function(summariseMode) {
  if (!(summariseMode %in% c("AllEras", "FirstEra", "FixedTime"))) {
    cli::cli_abort(
      "`summariseMode` should be one of: AllEras, FirstEra, FixedTime"
    )
  }
}

checkFixedTime <- function(fixedTime) {
  checkmate::assertIntegerish(fixedTime, lower = 1, any.missing = F, len = 1)
}

checkDaysPriorHistory <- function(daysPriorHistory) {
  checkmate::assertIntegerish(
    daysPriorHistory, lower = 0, any.missing = F, len = 1, null.ok = T
  )
}

checkGapEra <- function(gapEra) {
  checkmate::assertIntegerish(gapEra, lower = 0, any.missing = F, len = 1)
}

checkPriorUseWashout <- function(priorUseWashout) {
  checkmate::assertIntegerish(
    priorUseWashout, lower = 0, any.missing = F, len = 1
  )
}

checkCohortDateRange <- function(cohortDateRange) {
  checkmate::assertDate(cohortDateRange, null.ok = T, len = 2)
  if (!is.na(cohortDateRange[1]) &
      !is.na(cohortDateRange[1]) &
      cohortDateRange[1] > cohortDateRange[2]) {
    cli::cli_abort(
      "cohortDateRange[1] should be equal or smaller than cohortDateRange[2]"
    )
  }
}

checkImputeDuration <- function(imputeDuration) {
  if (is.character(imputeDuration)) {
    checkmate::assertChoice(
      imputeDuration,
      c("eliminate", "median", "mean", "quantile25", "quantile75")
    )
  } else {
    checkmate::assertCount(
      imputeDuration, positive = TRUE
    )
  }
}

checkDurationRange <- function(durationRange) {
  errorMessage <- "durationRange has to be numeric of length 2 with no NA and
      durationRange[1] <= durationRange[2]"
  if (!is.numeric(durationRange) |
      length(durationRange) != 2 |
      any(is.na(durationRange))) {
    cli::cli_abort(errorMessage)
  }
  if (durationRange[1] > durationRange[2]) {
    cli::cli_abort(errorMessage)
  }
}

checkX <- function(x) {
  errorMessage <- "x should be a table with at least 'cohort_definition_id' and
  'subject_id' as columns."
  if (!("tbl" %in% class(x))) {
    cli::cli_abort(errorMessage)
  }
  if (!all(c("cohort_definition_id", "subject_id") %in% colnames(x))) {
    cli::cli_abort(errorMessage)
  }
}

checkAttrition <- function(attrition) {
  if (!is.null(attrition)) {
    errorMessage <- "attrition should be a table with at least:
  'cohort_definition_id', 'number_records', 'number_subjects', 'reason_id',
  'reason', 'excluded_records', 'excluded_subjects' as columns."
    if (!("tbl" %in% class(attrition))) {
      cli::cli_abort(errorMessage)
    }
    if (!all(c(
      'cohort_definition_id', 'number_records', 'number_subjects', 'reason_id',
      'reason', 'excluded_records', 'excluded_subjects'
    ) %in% colnames(attrition))) {
      cli::cli_abort(errorMessage)
    }
  }
}

checkReason <- function(reason) {
  checkmate::assertCharacter(reason, len = 1, min.chars = 1)
}

checkDrugUtilisationCohortSet <- function(cs) {
  expectedColnames <- c(
    "cohort_definition_id", "cohort_name", "summarise_mode", "fixed_time",
    "days_prior_history", "gap_era", "prior_use_washout",
    "cohort_dates_range_start", "cohort_dates_range_end", "impute_duration",
    "duration_range_min", "duration_range_max"
  )
  return(
    length(colnames(cs)) == length(expectedColnames) &
      all(expectedColnames %in% colnames(cs))
  )
}

checkConsistentCohortSet<- function(cs,
                                    conceptSetList,
                                    gapEra,
                                    imputeDuration,
                                    durationRange,
                                    missingGapEra,
                                    missingImputeDuration,
                                    missingDurationRange) {
  notPresent <- names(conceptSetList)[!(conceptSetList %in% cs$cohort_name)]
  if (length(notPresent) > 0) {
    cli::cli_alert_warning(paste0(
      "Different names in conceptSetList (",
      paste0(notPresent, collapse = ", "), ") than in the created cohortSet."
    ))
  }
  if (missingGapEra == TRUE) {
    if (length(unique(cs$gap_era)) > 1) {
      cli::cli_abort(
        "More than one gapEra found in cohortSet, please specify gapEra"
      )
    }
    gapEra <- unique(cs$gap_era)
  } else {
    if (!all(cs$gap_era == gapEra)) {
      cli::cli_alert_warning(glue::glue_collapse(
        "gapEra is different than at the cohort creation stage (input: {gapEra}, cohortSet: {cs$gap_era})."
      ))
    }
  }
  if (missingImputeDuration == TRUE) {
    if (length(unique(cs$impute_duration)) > 1) {
      cli::cli_abort(
        "More than one imputeDuration found in cohortSet, please specify imputeDuration"
      )
    }
    imputeDuration <- unique(cs$impute_duration)
  } else {
    if (imputeDuration != cs$impute_duration) {
      cli::cli_alert_warning(glue::glue(
        "imputeDuration is different than at the cohort creation stage (input: {imputeDuration}, cohortSet: {cs$impute_duration})."
      ))
    }
  }
  if (missingDurationRange == TRUE) {
    if (length(unique(cs$duration_range_min)) > 1 | length(unique(cs$duration_range_max)) > 1) {
      cli::cli_abort(
        "More than one durationRange found in cohortSet, please specify durationRange"
      )
    }
    durationRange <- c(
      unique(cs$duration_range_min), unique(cs$duration_range_max)
    )
  } else {
    if (!identical(durationRange, c(cs$duration_range_min, cs$duration_range_max))) {
      cli::cli_alert_warning(glue::glue_collapse(
        "durationRange is different than at the cohort creation stage (input: {durationRange}, cohortSet: {c(cs$duration_range_min, cs$duration_range_max)})"
      ))
    }
  }
  parameters <- list(
    gapEra = gapEra, imputeDuration = imputeDuration,
    durationRange = durationRange
  )
  return(parameters)
}

checkTargetCohortName <- function(targetCohortName) {
  errorMessage <- "targetCohortName must be a character string of length 1"
  check <- !is.character(targetCohortName) | length(targetCohortName) > 1 |
    any(is.na(targetCohortName)) | any(nchar(targetCohortName) == 0)
  if (check) {
    cli::cli_abort(errorMessage)
  }
}

checkPath <- function(path) {
  if(typeof(path) != "character" || length(path) != 1) {
    cli::cli_abort(paste0(
      "{path} is not a character of length 1"
    ))
  }

  if (!file.exists(path)) {
    stop(glue::glue("Invalid path {path}"))
  } else {
    if (dir.exists(path)) {
      conceptSets <- dplyr::tibble(concept_set_path = list.files(
        path = path,
        full.names = TRUE
      ))
    } else {
      conceptSets <- dplyr::tibble(concept_set_path = .env$path)
    }
    conceptSets <- conceptSets %>%
      dplyr::filter(tools::file_ext(.data$concept_set_path) == "json") %>%
      dplyr::mutate(
        concept_set_name =
          tools::file_path_sans_ext(basename(.data$concept_set_path))
      ) %>%
      dplyr::mutate(cohort_definition_id = dplyr::row_number())
    if (conceptSets %>% nrow() == 0) {
      stop(glue::glue("No 'json' file found in {path}"))
    }
  }
  return(conceptSets)
}

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

checkPatternTibble <- function(x) {
  if (!isTRUE(inherits(x, "tbl_dbi"))) {
    cli::cli_abort("x is not a valid table")
  }
  checkColnames <- all(c("amount", "amount_unit_concept_id", "numerator", "numerator_unit_concept_id", "denominator", "denominator_unit_concept_id") %in% colnames(x))
  if(!checkColnames) {
    cli::cli_abort(" 'amount', 'amount_unit_concept_id', 'numerator', 'numerator_unit_concept_id', 'denominator' and 'denominator_unit_concept_id' are not all columns of {x}")
  }
  invisible(NULL)
}

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


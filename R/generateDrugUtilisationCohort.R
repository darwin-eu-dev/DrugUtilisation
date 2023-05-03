generateDrugUtilisationCohort <- function(cdm,
                                          ingredientConceptId = NULL,
                                          conceptSetPath = NULL,
                                          studyStartDate = NULL,
                                          studyEndDate = NULL,
                                          summariseMode = "AllEras",
                                          fixedTime = 365,
                                          daysPriorHistory = 0,
                                          gapEra = 30,
                                          priorUseWashout = NULL,
                                          imputeDuration = "eliminate",
                                          durationRange = c(1, NA),
                                          tablePrefix = NULL) {
  errorMessage <- checkmate::makeAssertCollection()
  # first round of initial checks, assert Type
  checkmate::assertClass(
    cdm,
    classes = "cdm_reference",
    add = errorMessage
  )
  checkmate::assertCharacter(
    conceptSetPath,
    len = 1,
    null.ok = TRUE,
    add = errorMessage
  )
  checkmate::assertCount(
    ingredientConceptId,
    null.ok = TRUE,
    add = errorMessage
  )
  if (is.null(conceptSetPath) && is.null(ingredientConceptId)) {
    errorMessage$push(
      "'conceptSetPath' or 'ingredientConceptId' should be provided"
    )
  }
  checkmate::assertDate(
    studyStartDate,
    any.missing = FALSE,
    len = 1,
    null.ok = TRUE,
    add = errorMessage
  )
  checkmate::assertDate(
    studyEndDate,
    any.missing = FALSE,
    len = 1,
    null.ok = TRUE,
    add = errorMessage
  )
  checkmate::assertChoice(
    summariseMode,
    choices = c("AllEras", "FirstEra", "FixedTime"),
    add = errorMessage
  )
  if (summariseMode == "FixedTime") {
    checkmate::assertCount(
      fixedTime,
      positive = TRUE,
      add = errorMessage
    )
  }
  checkmate::assertCount(
    daysPriorHistory,
    null.ok = TRUE,
    add = errorMessage
  )
  checkmate::assertCount(
    gapEra,
    add = errorMessage
  )
  if (is.character(imputeDuration)) {
    checkmate::assertChoice(
      imputeDuration,
      choices = c("eliminate", "median", "mean", "quantile25", "quantile75"),
      add = errorMessage
    )
  } else {
    checkmate::assertCount(
      imputeDuration,
      positive = TRUE,
      add = errorMessage
    )
  }
  checkmate::assertNumeric(
    durationRange,
    len = 2,
    null.ok = TRUE,
    add = errorMessage
  )
  checkmate::reportAssertions(collection = errorMessage)

  if (is.null(durationRange)) {
    durationRange <- c(NA, NA)
  }

  # second round of initial checks
  checkmate::assertTRUE(
    all(
      c("drug_strength", "drug_exposure", "observation_period") %in% names(cdm)
    ),
    add = errorMessage
  )
  if (!is.null(conceptSetPath)) {
    if (!file.exists(conceptSetPath)) {
      stop(glue::glue("Invalid concept set path {conceptSetPath}"))
    } else {
      if (dir.exists(conceptSetPath)) {
        conceptSets <- dplyr::tibble(concept_set_path = list.files(
          path = conceptSetPath,
          full.names = TRUE
        ))
      } else {
        conceptSets <- dplyr::tibble(concept_set_path = .env$conceptSetPath)
      }
      conceptSets <- conceptSets %>%
        dplyr::filter(tools::file_ext(.data$concept_set_path) == "json") %>%
        dplyr::mutate(
          concept_set_name =
            tools::file_path_sans_ext(basename(.data$concept_set_path))
        ) %>%
        dplyr::mutate(cohort_definition_id = dplyr::row_number())
      if (conceptSets %>% nrow() == 0) {
        stop(glue::glue("No 'json' file found in {conceptSetPath}"))
      }
    }
  }
  if (sum(is.na(durationRange)) == 0) {
    checkmate::assertTRUE(
      durationRange[1] <= durationRange[2],
      add = errorMessage
    )
  }

  # checks for tableprefix
  checkmate::assertCharacter(
    tablePrefix, len = 1, null.ok = TRUE, add = errorMessage
  )

  checkmate::reportAssertions(collection = errorMessage)

  if (!is.null(conceptSetPath)) {
    tryCatch(
      expr = conceptList <- readConceptSets(conceptSets),
      error = function(e) {
        stop("The json file is not a properly formated OMOP concept set.")
      }
    )

    conceptList <- conceptList %>%
      dplyr::filter(.data$include_descendants == FALSE) %>%
      dplyr::union(
        cdm[["concept_ancestor"]] %>%
          dplyr::select(
            "concept_id" = "ancestor_concept_id",
            "descendant_concept_id"
          ) %>%
          dplyr::inner_join(
            conceptList %>%
              dplyr::filter(.data$include_descendants == TRUE),
            copy = TRUE,
            by = "concept_id"
          ) %>%
          dplyr::select(-"concept_id") %>%
          dplyr::rename("concept_id" = "descendant_concept_id") %>%
          dplyr::collect()
      ) %>%
      dplyr::select(-"include_descendants") %>%
      dplyr::rename("drug_concept_id" = "concept_id")
    # eliminate the ones that is_excluded = TRUE
    conceptList <- conceptList %>%
      dplyr::filter(.data$is_excluded == FALSE) %>%
      dplyr::select("cohort_definition_id", "drug_concept_id") %>%
      dplyr::anti_join(
        conceptList %>%
          dplyr::filter(.data$is_excluded == TRUE),
        by = "drug_concept_id"
      )
    if (!is.null(ingredientConceptId)) {
      conceptList <- cdm[["drug_strength"]] %>%
        dplyr::filter(.data$ingredient_concept_id == .env$ingredientConceptId) %>%
        dplyr::select("drug_concept_id") %>%
        dplyr::collect() %>%
        dplyr::inner_join(conceptList, by = "drug_concept_id")
    }
    conceptSets <- conceptSets %>%
      dplyr::select( "cohort_definition_id", "cohort_name" = "concept_set_name")
  } else {
    conceptSets <- dplyr::tibble(
      cohort_definition_id = 1,
      cohort_name = paste0("ingredient_concept_id_", ingredientConceptId)
    )
    conceptList <- cdm[["drug_strength"]] %>%
      dplyr::filter(.data$ingredient_concept_id == .env$ingredientConceptId) %>%
      dplyr::select("drug_concept_id") %>%
      dplyr::collect() %>%
      dplyr::mutate(cohort_definition_id = 1)
  }

  if (nrow(conceptList) == 0) {
    stop("No concepts were not found in the vocabulary using this settings")
  }

  # get sql dialect of the database
  dialect <- CDMConnector::dbms(attr(cdm, "dbcon"))
  # get the name of the info table

  # split conceptList in small bits smaller than 500k to avoid problems with
  # redshift
  numberMaxCodes <- 500000
  numberCodes <- nrow(conceptList)
  if (numberCodes <= numberMaxCodes) {
    idStart <- 1
    idEnd <- numberCodes
  } else {
    idStart <- seq(1, numberCodes, by = numberMaxCodes)
    idEnd <- idStart + numberMaxCodes - 1
    idEnd[idEnd > numberCodes] <- numberCodes
  }

  # subset drug_exposure and only get the drug concept ids that we are
  # interested in.
  for (k in 1:length(idStart)) {
    cohort.k <- cdm[["drug_exposure"]] %>%
      dplyr::select(
        "subject_id" = "person_id",
        "drug_concept_id",
        "drug_exposure_start_date",
        "drug_exposure_end_date"
      ) %>%
      dplyr::inner_join(
        conceptList[idStart[k]:idEnd[k],],
        by = "drug_concept_id",
        copy = TRUE
      ) %>%
      dplyr::compute()
    if (k == 1) {
      cohort <- cohort.k
    } else {
      cohort <- cohort %>%
        dplyr::union_all(cohort.k) %>%
        dplyr::compute()
    }
  }


  if (cohort %>% dplyr::tally() %>% dplyr::pull("n") == 0) {
    stop(
      "No record found with the current specifications in drug_exposure table"
    )
  }

  attrition <- addattritionLine(cohort, "Initial Exposures")

  # compute the number of days exposed according to:
  # days_exposed = end - start + 1
  cohort <- cohort %>%
    dplyr::mutate(days_exposed = dbplyr::sql(
      CDMConnector::datediff(
        start = "drug_exposure_start_date",
        end = "drug_exposure_end_date"
      )
    ) + 1)

  # impute or eliminate the exposures that duration does not fulfill the
  # conditions ( <=0; <durationRange[1]; >durationRange[2])
  cohort <- imputeVariable(
    x = cohort,
    variableName = "days_exposed",
    impute = imputeDuration,
    lowerBound = durationRange[1],
    upperBound = durationRange[2],
    imputeValueName = "imputeDuration"
  ) %>%
    dplyr::mutate(days_to_add = as.integer(.data$days_exposed - 1)) %>%
    dplyr::compute() %>%
    dplyr::mutate(drug_exposure_end_date = as.Date(dbplyr::sql(
      CDMConnector::dateadd(
        date = "drug_exposure_start_date",
        number = "days_to_add"
      )
    ))) %>%
    dplyr::compute()

  attrition <- attrition %>%
    dplyr::union_all(addattritionLine(cohort, "Imputation"))


  cohort <- cohort %>%
    dplyr::select(
      "cohort_definition_id",
      "subject_id",
      "date_event" = "drug_exposure_start_date"
    ) %>%
    dplyr::mutate(date_id = -1) %>%
    dplyr::union_all(
      cohort %>%
        dplyr::mutate(
          date_event = as.Date(!!CDMConnector::dateadd(
            date = "drug_exposure_end_date",
            number = gapEra
          )),
          date_id = 1
        ) %>%
        dplyr::select(
          "cohort_definition_id", "subject_id", "date_event", "date_id"
        )
    ) %>%
    dplyr::group_by(.data$cohort_definition_id, .data$subject_id) %>%
    dbplyr::window_order(.data$date_event, .data$date_id) %>%
    dplyr::mutate(cum_id = cumsum(.data$date_id)) %>%
    dplyr::filter(
      .data$cum_id == 0 || (.data$cum_id == -1 && .data$date_id == -1)
    ) %>%
    dplyr::mutate(
      name = dplyr::if_else(
        .data$date_id == -1,
        "cohort_start_date",
        "cohort_end_date"
      ),
      era_id = dplyr::if_else(
        .data$date_id == -1,
        1,
        0
      )
    ) %>%
    dplyr::mutate(era_id = cumsum(as.numeric(.data$era_id))) %>%
    dplyr::ungroup() %>%
    dbplyr::window_order() %>%
    dplyr::select(
      "cohort_definition_id", "subject_id", "era_id", "name", "date_event"
    ) %>%
    tidyr::pivot_wider(names_from = "name", values_from = "date_event") %>%
    dplyr::mutate(cohort_end_date = as.Date(!!CDMConnector::dateadd(
      date = "cohort_end_date",
      number = -gapEra
    ))) %>%
    dplyr::compute()

  attrition <- attrition %>%
    dplyr::union_all(addattritionLine(cohort, "Eras"))


  if (!is.null(priorUseWashout)) {
    cohort <- cohort %>%
      dplyr::left_join(
        cohort %>%
          dplyr::select(
            "cohort_definition_id", "subject_id", "era_id",
            "prior_era" = "cohort_end_date"
          ) %>%
          dplyr::mutate(era_id = .data$era_id + 1),
        by = c("cohort_definition_id", "subject_id", "era_id")
      ) %>%
      dplyr::mutate(prior_era = as.numeric(!!CDMConnector::datediff(
        "prior_era", "cohort_start_date"
      ))) %>%
      dplyr::filter(
        is.na(.data$prior_era) | .data$prior_era > .env$priorUseWashout
      ) %>%
      dplyr::select(-"prior_era", -"era_id") %>%
      dplyr::compute()
    attrition <- attrition %>%
      dplyr::union_all(addattritionLine(
        cohort,
        paste0("Prior washout of ", priorUseWashout, " days")
      ))

  } else {
    cohort <- cohort %>% dplyr::select(-"era_id")
  }

  if (!is.null(studyStartDate)) {
    cohort <- cohort %>%
      dplyr::filter(.data$cohort_start_date >= .env$studyStartDate)

    attrition <- attrition %>%
      dplyr::union_all(addattritionLine(
        cohort,
        paste0("Start after or at ", studyStartDate)
      ))

  }

  if (!is.null(studyEndDate)) {
    cohort <- cohort %>%
      dplyr::filter(.data$cohort_start_date <= .env$studyEndDate)

    attrition <- attrition %>%
      dplyr::union_all(addattritionLine(
        cohort,
        paste0("Start before or at ", studyEndDate)
      ))

  }

  if (!is.null(daysPriorHistory)) {
    cohort <- inObservation(cohort, cdm = cdm) %>%
      dplyr::filter(.data$in_observation == TRUE) %>%
      dplyr::compute()

    attrition <- attrition %>%
      dplyr::union_all(addattritionLine(
        cohort,
        "In observation on cohort_start_date"
      ))

    cohort <- addPriorHistory(cohort, cdm = cdm) %>%
      dplyr::filter(.data$prior_history >= .env$daysPriorHistory) %>%
      dplyr::select(-"prior_history")

    attrition <- attrition %>%
      dplyr::union_all(addattritionLine(
        cohort,
        paste0("At least ", daysPriorHistory, " days of prior history")
      ))

  }

  if (summariseMode == "FirstEra") {
    cohort <- cohort %>%
      dplyr::group_by(.data$cohort_definition_id, .data$subject_id) %>%
      dplyr::filter(
        .data$cohort_start_date == min(.data$cohort_start_date, na.rm = TRUE)
      ) %>%
      dplyr::ungroup()
    attrition <- attrition %>%
      dplyr::union_all(addattritionLine(cohort, "Only first era"))

  } else if (summariseMode == "FixedTime") {
    cohort <- cohort %>%
      dplyr::group_by(.data$cohort_definition_id, .data$subject_id) %>%
      dplyr::summarise(
        cohort_start_date = min(.data$cohort_start_date, na.rm = TRUE)
      ) %>%
      dplyr::mutate(cohort_end_date = !!CDMConnector::dateadd(
        "cohort_start_date",
        fixedTime - 1
      )) %>%
      dplyr::ungroup()
    attrition <- attrition %>%
      dplyr::union_all(addattritionLine(
        cohort,
        paste0("Only first era; fixedTime = ", fixedTime, " days")
      ))
  }

  cohort <- cohort %>%
    dplyr::select(
      "cohort_definition_id", "subject_id", "cohort_start_date",
      "cohort_end_date"
    )

  attrition <- attrition %>%
    dplyr::left_join(
      dplyr::tibble(
        order_id = c(1:10),
        reason = c(
          "Initial Exposures",
          "Imputation",
          "Eras",
          paste0("Prior washout of ", priorUseWashout, " days"),
          paste0("Start after or at ", studyStartDate),
          paste0("Start before or at ", studyEndDate),
          "In observation on cohort_start_date",
          paste0("At least ", daysPriorHistory, " days of prior history"),
          "Only first era",
          paste0("Only first era; fixedTime = ", fixedTime, " days")
        )
      ),
      by = "reason"
    ) %>%
    dplyr::arrange(.data$cohort_definition_id, .data$order_id) %>%
    dplyr::select(-"order_id")

  cohortCount <- cohort %>%
    dplyr::group_by(.data$cohort_definition_id) %>%
    dplyr::summarise(
      number_records = dplyr::n(),
      number_subjects = dplyr::n_distinct(.data$subject_id),
      .groups = "drop"
    ) %>%
    dplyr::compute()

  con <- attr(cdm, "dbcon")
  if(is.null(tablePrefix)){
    cohortRef <- cohort %>% CDMConnector::computeQuery()
    cohortSetTableName <- uniqueTableName()
    DBI::dbWriteTable(con, cohortSetTableName, conceptSets, temporary = TRUE)
    cohortSetRef <- dplyr::tbl(con, cohortSetTableName)
    cohortAttritionTableName <- uniqueTableName()
    DBI::dbWriteTable(con, cohortAttritionTableName, attrition, temporary = TRUE)
    cohortAttritionRef <- dplyr::tbl(con, cohortAttritionTableName)
    cohortCountRef <- cohortCount
  } else {
    writeSchema <- attr(cdm, "write_schema")
    cohortRef <- CDMConnector::computeQuery(
      x = cohort, name = tablePrefix, temporary = FALSE,
      schema = writeSchema, overwrite = TRUE
    )
    DBI::dbWriteTable(
      conn = con, name = inSchema(writeSchema, paste0(tablePrefix, "_set"), CDMConnector::dbms(con)),
      value = as.data.frame(conceptSets),
      overwrite = TRUE
    )
    cohortSetRef <- dplyr::tbl(con, inSchema(writeSchema, paste0(tablePrefix, "_set"), CDMConnector::dbms(con)))
    DBI::dbWriteTable(
      conn = con, name = inSchema(writeSchema, paste0(tablePrefix, "_attrition"), CDMConnector::dbms(con)),
      value = as.data.frame(attrition),
      overwrite = TRUE
    )
    cohortAttritionRef <- dplyr::tbl(con, inSchema(writeSchema, paste0(tablePrefix, "_attrition"), CDMConnector::dbms(con)))
    cohortCountRef <- CDMConnector::computeQuery(
      x = cohortCount, name = paste0(tablePrefix, "_count"), temporary = FALSE,
      schema = writeSchema, overwrite = TRUE
    )
  }

  cohort <- CDMConnector::newGeneratedCohortSet(
    cohortRef = cohortRef,
    cohortSetRef = cohortSetRef,
    cohortAttritionRef = cohortAttritionRef,
    cohortCountRef = cohortCountRef
  )

  return(cohort)
}

#' Impute or eliminate values under a certain conditions
#'
#' @param x x
#' @param variableName variableName
#' @param impute impute
#' @param lowerBound lowerBound
#' @param upperBound upperBound
#' @param imputeValueName imputeValueName
#'
#' @noRd
imputeVariable <- function(x,
                           variableName,
                           impute,
                           lowerBound,
                           upperBound,
                           imputeValueName) {
  # rename the variable of interest to variable
  x <- x %>%
    dplyr::rename("variable" = .env$variableName)
  # identify (as impute = 1)
  x <- x %>%
    dplyr::mutate(impute = dplyr::if_else(is.na(.data$variable),
                                          1,
                                          0
    ))

  # identify (as impute = 1) the values smaller than lower bound
  if (!is.na(lowerBound)) {
    x <- x %>%
      dplyr::mutate(impute = dplyr::if_else(
        is.na(.data$variable),
        1,
        dplyr::if_else(.data$variable < .env$lowerBound,
                       1,
                       .data$impute
        )
      ))
  }
  # identify (as impute = 1) the values greater than upper bound
  if (!is.na(upperBound)) {
    x <- x %>%
      dplyr::mutate(impute = dplyr::if_else(
        is.na(.data$variable),
        1,
        dplyr::if_else(.data$variable > .env$upperBound,
                       1,
                       .data$impute
        )
      ))
  }
  # if impute is false then all values with impute = 1 are not considered
  if (impute == "eliminate") {
    x <- x %>%
      dplyr::filter(.data$impute == 0)
  }

  if (impute == "median") {
    x <- x %>% dplyr::mutate(variable = dplyr::if_else(
      .data$impute == 1,
      stats::median(
        x %>% dplyr::filter(.data$impute == 0) %>%
          dplyr::pull("variable")
      ),
      .data$variable
    ))
  }


  if (impute == "mean") {
    x <- x %>% dplyr::mutate(variable = dplyr::if_else(
      .data$impute == 1,
      base::mean(
        x %>% dplyr::filter(.data$impute == 0) %>%
          dplyr::pull("variable")
      ),
      .data$variable
    ))
  }
  # %>%
  # dplyr::rename(!!imputeValueName := "imputeValue")

  if (impute == "quantile25") {
    x <- x %>% dplyr::mutate(variable = dplyr::if_else(
      .data$impute == 1,
      as.numeric(
        stats::quantile(
          x %>% dplyr::filter(.data$impute == 0) %>%
            dplyr::pull("variable"),
          0.25,
          na.rm = TRUE
        )
      ),
      .data$variable
    ))
  }


  if (impute == "quantile75") {
    x <- x %>% dplyr::mutate(variable = dplyr::if_else(
      .data$impute == 1,
      as.numeric(
        stats::quantile(
          x %>% dplyr::filter(.data$impute == 0) %>%
            dplyr::pull("variable"),
          0.75,
          na.rm = TRUE
        )
      ),
      .data$variable
    ))
  }


  if (is.numeric(impute)) {
    x <- x %>%
      dplyr::rename("imputeValue" = .env$imputeValueName) %>%
      dplyr::mutate(variable = dplyr::if_else(.data$impute == 1,
                                              .data$imputeValue,
                                              .data$variable
      ))
  }

  if (imputeValueName == "imputeDuration") {
    x <- x %>% dplyr::mutate(variable = floor(.data$variable))
  }

  x <- x %>%
    dplyr::select(-"impute") %>%
    dplyr::rename(!!variableName := "variable")

  return(x)
}

#' Add line to the attrition tibble
#'
#' @noRd
addattritionLine <- function(cohort, reason) {
  if ("cohort_definition_id" %in% colnames(cohort)) {
    cohort <- cohort %>% dplyr::group_by(.data$cohort_definition_id)
  }
  return(
    cohort %>%
      dplyr::summarise(
        number_subjects = dplyr::n_distinct(.data$subject_id),
        number_records = dplyr::n()
      ) %>%
      dplyr::collect() %>%
      dplyr::mutate(reason = .env$reason)
  )
}

#' Function to read the concept sets and export a tibble with
#' cohort_definition_id and drug_concept_id with the list of drug_concept_id
#' included in each concept set
#' @noRd
readConceptSets <- function(conceptSets) {
  for (k in 1:nrow(conceptSets)) {
    conceptSetName <- conceptSets$concept_set_name[k]
    conceptSet <- RJSONIO::fromJSON(conceptSets$concept_set_path[k])
    conceptSet <- lapply(conceptSet$items, function(x) {
      x <- append(x, x[["concept"]])
      x[["concept"]] <- NULL
      return(x)
    }) %>%
      dplyr::bind_rows() %>%
      dplyr::mutate(
        cohort_definition_id = .env$conceptSets$cohort_definition_id[k]
      )
    if (k == 1) {
      conceptList <- conceptSet
    } else {
      conceptList <- rbind(conceptList, conceptSet)
    }
  }
  conceptList <- conceptList %>%
    dplyr::select(
      "cohort_definition_id",
      "concept_id" = "CONCEPT_ID",
      "is_excluded" = "isExcluded",
      "include_descendants" = "includeDescendants"
    )
  return(conceptList)
}

#' @noRd
uniqueTableName <- function() {
  i <- getOption("dbplyr_table_name", 0) + 1
  options(dbplyr_table_name = i)
  sprintf("dbplyr_%03i", i)
}

#' @noRd
inSchema <- function(schema, table, dbms = NULL) {
  checkmate::assertCharacter(schema, min.len = 1, max.len = 2)
  checkmate::assertCharacter(table, len = 1)
  checkmate::assertCharacter(dbms, len = 1, null.ok = TRUE)

  if (!is.null(dbms) && (dbms %in% c("oracle"))) {
    # some dbms need in_schema, others DBI::Id
    switch(length(schema),
           dbplyr::in_schema(schema = schema, table = table),
           dbplyr::in_catalog(catalog = schema[1], schema = schema[2], table = table))
  } else {
    switch(length(schema),
           DBI::Id(schema = schema, table = table),
           DBI::Id(catalog = schema[1], schema = schema[2], table = table))
  }
}

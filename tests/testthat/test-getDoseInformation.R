test_that("test subfunctions: splitSubexposures", {
  x <- dplyr::tibble(
    subject_id = as.integer(c(
      1,
      1, 1,
      1, 1,
      1, 1, 1,
      1, 1, 1, 1,
      2, 2, 2, 2,
      2
    )),
    cohort_start_date = as.Date(c(
      "2000-01-01",
      "2000-02-01", "2000-02-01",
      "2000-03-05", "2000-03-05",
      "2000-04-01", "2000-04-01", "2000-04-01",
      "2000-05-01", "2000-05-01", "2000-05-01", "2000-05-01",
      "2000-01-03", "2000-01-03", "2000-01-03", "2000-01-03",
      "2000-02-01"
    )),
    cohort_end_date = as.Date(c(
      "2000-01-10",
      "2000-02-15", "2000-02-15",
      "2000-03-12", "2000-03-12",
      "2000-04-21", "2000-04-21", "2000-04-21",
      "2000-05-11", "2000-05-11", "2000-05-11", "2000-05-11",
      "2000-01-20", "2000-01-20", "2000-01-20", "2000-01-20",
      "2000-02-10"
    )),
    drug_exposure_start_date = as.Date(c(
      "2000-01-01",
      "2000-02-01", "2000-02-04",
      "2000-03-01", "2000-03-06",
      "2000-04-08", "2000-04-09", "2000-04-11",
      "2000-05-01", "2000-05-01", "2000-05-09", "2000-05-11",
      "2000-01-01", "2000-01-03", "2000-01-09", "2000-01-20",
      "2000-02-08"
    )),
    drug_exposure_end_date = as.Date(c(
      "2000-01-10",
      "2000-02-10", "2000-02-20",
      "2000-03-10", "2000-03-06",
      "2000-04-15", "2000-04-11", "2000-04-11",
      "2000-05-01", "2000-05-05", "2000-05-12", "2000-05-11",
      "2000-01-03", "2000-01-03", "2000-01-15", "2000-01-20",
      "2000-02-10"
    ))
  )
  cdm <- mockDrugUtilisation(cohort1 = x)
  y <- splitSubexposures(cdm[["cohort1"]]) %>% dplyr::collect()

  # get first cohort entry
  yy <- y %>%
    dplyr::filter(
      .data$cohort_start_date == as.Date("2000-01-01") &
        .data$subject_id == 1
    )
  numberExpectedSubexposures <- 1
  countsPerSubexposure <- 1
  dayStartSubexposure <- 1
  dayEndSubexposure <- 10
  expect_true(length(unique(yy$subexposure_id)) == numberExpectedSubexposures)
  for (k in 1:length(countsPerSubexposure)) {
    yyy <- yy %>% dplyr::filter(.data$subexposure_id == k)
    expect_true(
      lubridate::day(yyy$subexposure_start_date[1]) == dayStartSubexposure[k]
    )
    expect_true(
      lubridate::day(yyy$subexposure_end_date[1]) == dayEndSubexposure[k]
    )
    if (countsPerSubexposure[k] == 0) {
      expect_true(nrow(yyy) == 1)
      expect_true(sum(is.na(yyy)) == 2)
    } else {
      expect_true(nrow(yyy) == countsPerSubexposure[k])
      expect_true(sum(is.na(yyy)) == 0)
    }
  }

  # get second cohort entry
  yy <- y %>%
    dplyr::filter(
      .data$cohort_start_date == as.Date("2000-02-01") &
        .data$subject_id == 1
    )
  numberExpectedSubexposures <- 3
  countsPerSubexposure <- c(1, 2, 1)
  dayStartSubexposure <- c(1, 4, 11)
  dayEndSubexposure <- c(3, 10, 15)
  expect_true(length(unique(yy$subexposure_id)) == numberExpectedSubexposures)
  for (k in 1:length(countsPerSubexposure)) {
    yyy <- yy %>% dplyr::filter(.data$subexposure_id == k)
    expect_true(
      lubridate::day(yyy$subexposure_start_date[1]) == dayStartSubexposure[k]
    )
    expect_true(
      lubridate::day(yyy$subexposure_end_date[1]) == dayEndSubexposure[k]
    )
    if (countsPerSubexposure[k] == 0) {
      expect_true(nrow(yyy) == 1)
      expect_true(sum(is.na(yyy)) == 2)
    } else {
      expect_true(nrow(yyy) == countsPerSubexposure[k])
      expect_true(sum(is.na(yyy)) == 0)
    }
  }

  # get third cohort entry
  yy <- y %>%
    dplyr::filter(
      .data$cohort_start_date == as.Date("2000-03-05") &
        .data$subject_id == 1
    )
  numberExpectedSubexposures <- 4
  countsPerSubexposure <- c( 1,  2,  1,  0)
  dayStartSubexposure  <- c( 5,  6,  7, 11)
  dayEndSubexposure    <- c( 5,  6, 10, 12)
  expect_true(length(unique(yy$subexposure_id)) == numberExpectedSubexposures)
  for (k in 1:length(countsPerSubexposure)) {
    yyy <- yy %>% dplyr::filter(.data$subexposure_id == k)
    expect_true(
      lubridate::day(yyy$subexposure_start_date[1]) == dayStartSubexposure[k]
    )
    expect_true(
      lubridate::day(yyy$subexposure_end_date[1]) == dayEndSubexposure[k]
    )
    if (countsPerSubexposure[k] == 0) {
      expect_true(nrow(yyy) == 1)
      expect_true(sum(is.na(yyy)) == 2)
    } else {
      expect_true(nrow(yyy) == countsPerSubexposure[k])
      expect_true(sum(is.na(yyy)) == 0)
    }
  }

  # get fourth cohort entry
  yy <- y %>%
    dplyr::filter(
      .data$cohort_start_date == as.Date("2000-04-01") &
        .data$subject_id == 1
    )
  numberExpectedSubexposures <- 6
  countsPerSubexposure <- c( 0,  1,  2,  3,  1,  0)
  dayStartSubexposure  <- c( 1,  8,  9, 11, 12, 16)
  dayEndSubexposure    <- c( 7,  8, 10, 11, 15, 21)
  expect_true(length(unique(yy$subexposure_id)) == numberExpectedSubexposures)
  for (k in 1:length(countsPerSubexposure)) {
    yyy <- yy %>% dplyr::filter(.data$subexposure_id == k)
    expect_true(
      lubridate::day(yyy$subexposure_start_date[1]) == dayStartSubexposure[k]
    )
    expect_true(
      lubridate::day(yyy$subexposure_end_date[1]) == dayEndSubexposure[k]
    )
    if (countsPerSubexposure[k] == 0) {
      expect_true(nrow(yyy) == 1)
      expect_true(sum(is.na(yyy)) == 2)
    } else {
      expect_true(nrow(yyy) == countsPerSubexposure[k])
      expect_true(sum(is.na(yyy)) == 0)
    }
  }

  # get fifth cohort entry
  yy <- y %>%
    dplyr::filter(
      .data$cohort_start_date == as.Date("2000-05-01") &
        .data$subject_id == 1
    )
  numberExpectedSubexposures <- 5
  countsPerSubexposure <- c( 2,  1,  0,  1,  2)
  dayStartSubexposure  <- c( 1,  2,  6,  9, 11)
  dayEndSubexposure    <- c( 1,  5,  8, 10, 11)
  expect_true(length(unique(yy$subexposure_id)) == numberExpectedSubexposures)
  for (k in 1:length(countsPerSubexposure)) {
    yyy <- yy %>% dplyr::filter(.data$subexposure_id == k)
    expect_true(
      lubridate::day(yyy$subexposure_start_date[1]) == dayStartSubexposure[k]
    )
    expect_true(
      lubridate::day(yyy$subexposure_end_date[1]) == dayEndSubexposure[k]
    )
    if (countsPerSubexposure[k] == 0) {
      expect_true(nrow(yyy) == 1)
      expect_true(sum(is.na(yyy)) == 2)
    } else {
      expect_true(nrow(yyy) == countsPerSubexposure[k])
      expect_true(sum(is.na(yyy)) == 0)
    }
  }

  # get sixth cohort entry
  yy <- y %>%
    dplyr::filter(
      .data$cohort_start_date == as.Date("2000-01-03") &
        .data$subject_id == 2
    )
  numberExpectedSubexposures <- 5
  countsPerSubexposure <- c( 2,  0,  1,  0,  1)
  dayStartSubexposure  <- c( 3,  4,  9, 16, 20)
  dayEndSubexposure    <- c( 3,  8, 15, 19, 20)
  expect_true(length(unique(yy$subexposure_id)) == numberExpectedSubexposures)
  for (k in 1:length(countsPerSubexposure)) {
    yyy <- yy %>% dplyr::filter(.data$subexposure_id == k)
    expect_true(
      lubridate::day(yyy$subexposure_start_date[1]) == dayStartSubexposure[k]
    )
    expect_true(
      lubridate::day(yyy$subexposure_end_date[1]) == dayEndSubexposure[k]
    )
    if (countsPerSubexposure[k] == 0) {
      expect_true(nrow(yyy) == 1)
      expect_true(sum(is.na(yyy)) == 2)
    } else {
      expect_true(nrow(yyy) == countsPerSubexposure[k])
      expect_true(sum(is.na(yyy)) == 0)
    }
  }

  # get seventh cohort entry
  yy <- y %>%
    dplyr::filter(
      .data$cohort_start_date == as.Date("2000-02-01") &
        .data$subject_id == 2
    )
  numberExpectedSubexposures <- 2
  countsPerSubexposure <- c( 0,  1)
  dayStartSubexposure  <- c( 1,  8)
  dayEndSubexposure    <- c( 7, 10)
  expect_true(length(unique(yy$subexposure_id)) == numberExpectedSubexposures)
  for (k in 1:length(countsPerSubexposure)) {
    yyy <- yy %>% dplyr::filter(.data$subexposure_id == k)
    expect_true(
      lubridate::day(yyy$subexposure_start_date[1]) == dayStartSubexposure[k]
    )
    expect_true(
      lubridate::day(yyy$subexposure_end_date[1]) == dayEndSubexposure[k]
    )
    if (countsPerSubexposure[k] == 0) {
      expect_true(nrow(yyy) == 1)
      expect_true(sum(is.na(yyy)) == 2)
    } else {
      expect_true(nrow(yyy) == countsPerSubexposure[k])
      expect_true(sum(is.na(yyy)) == 0)
    }
  }

})

test_that("test input parameters errors", {

})

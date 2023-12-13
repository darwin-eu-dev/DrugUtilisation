test_that("test benchmarking function", {
  skip_on_cran()
  cdm <- mockDrugUtilisation(numberIndividuals = 100)


  # current mock only have concept set of length 4
  # add daily dose only work with 1 ingredient at the moment
  # benchmarking multiple cohorts
  timings <- benchmarkDUS(cdm,
    ingredientId = 1125315,
    numberOfCohort = 1:4
  )

  expect_true(tibble::is_tibble(timings))

  expect_true("DUS 1 cohorts" %in% timings$task)

  expect_true("DUS 4 cohorts" %in% timings$task)

  expect_true("add indication 2 cohorts" %in% timings$task)

  expect_true("add drug use for 3 cohorts" %in% timings$task)

  expect_true("summarise drug use for 4 cohorts" %in% timings$task)

  expect_true("add daily dose" %in% timings$task)

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

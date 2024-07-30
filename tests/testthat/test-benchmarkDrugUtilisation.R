test_that("test benchmarking function", {
  skip_on_cran()
  skip_if_not_installed("tictoc")

  cdm <- mockDrugUtilisation(
    con = connection(), writeSchema = schema(), numberIndividuals = 100
  )


  # current mock only have concept set of length 4
  # add daily dose only work with 1 ingredient at the moment
  # benchmarking multiple cohorts
  timings <- benchmarkDrugUtilisation(
    cdm = cdm, ingredientId = 1125315, numberOfCohort = 1:4
  )

  expect_true(tibble::is_tibble(timings))

  expect_true("DUS 1 cohorts" %in% timings$task)

  expect_true("DUS 4 cohorts" %in% timings$task)

  expect_true("add indication 2 cohorts" %in% timings$task)

  expect_true("add drug utilisation for 3 cohorts" %in% timings$task)

  expect_true("summarise drug utilisation for 4 cohorts" %in% timings$task)

  mockDisconnect(cdm = cdm)
})

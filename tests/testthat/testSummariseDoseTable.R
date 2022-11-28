# check inputs
# check output
# check variables
# check estimates
# check aggregation
# check obscure
test_that("expected errors on inputs", {
  # intantiate 2 cdm, cdm1 with different types of variables, cdm2 with only
  # numeric variables
  cdm1 <- mockDrugUtilisation(person = dplyr::tibble(
    cohort_definition_id = c(1, 1, 1, 2),
    subject_id = c(1, 1, 2, 1),
    cohort_start_date = as.Date(c(
      "2020-01-01", "2020-05-01", "2020-04-08", "2020-01-01"
    )),
    cohort_end_date = as.Date(c(
      "2020-01-10", "2020-06-01", "2020-07-18", "2020-01-11"
    )),
    number_x = c(1, 2, 3, 6),
    carcola = c(5, 6, 9, 7),
    piscina = c(TRUE, FALSE, TRUE, FALSE),
    cara = c("a", "b", "b", "a")
  ))
  cdm2 <- mockDrugUtilisation(person = dplyr::tibble(
    cohort_definition_id = c(1, 1, 1, 2),
    subject_id = c(1, 1, 2, 1),
    cohort_start_date = as.Date(c(
      "2020-01-01", "2020-05-01", "2020-04-08", "2020-01-01"
    )),
    cohort_end_date = as.Date(c(
      "2020-01-10", "2020-06-01", "2020-07-18", "2020-01-11"
    )),
    number_x = c(1, 2, 3, 6),
    carcola = c(5, 6, 9, 7)
  ))
  # no inputs
  expect_error(result <- summariseDoseTable())
  # only cdm
  expect_error(result <- summariseDoseTable(
    cdm = cdm1,
  ))
  # only cdm & doseCohortName with non numeric should fail
  expect_error(result <- summariseDoseTable(
    cdm = cdm1, doseCohortName = "person"
    ))
  # only cdm & doseCohortName works with only numeric
  result <- summariseDoseTable(
    cdm = cdm2, doseCohortName = "person"
  )
  expect_equal(summariseDoseTable(
    cdm = cdm2, doseCohortName = "person"
  ), summariseDoseTable(
    cdm = cdm1, doseCohortName = "person", variables = c("number_x", "carcola")
  ))
  # expect
})

test_that("check functionality", {
  cdm <- mockDrugUtilisation(person = dplyr::tibble(
    cohort_definition_id = c(1, 1, 1, 2),
    subject_id = c(1, 1, 2, 1),
    cohort_start_date = as.Date(c(
      "2020-01-01", "2020-05-01", "2020-04-08", "2020-01-01"
    )),
    cohort_end_date = as.Date(c(
      "2020-01-10", "2020-06-01", "2020-07-18", "2020-01-11"
    )),
    variable1 = c(1, 2, 3, 6),
    ed = c(1,2,3,4)
  ))


})

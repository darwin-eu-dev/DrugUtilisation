# check inputs
# check output
# check variables
# check estimates
# check aggregation
# check obscure
test_that("inputs and outputs", {

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
    variable1 = c(1, 2, 3, 6)
  ))
  result <- summariseDoseTable(
    cdm = cdm,
    doseCohortTable = "person",
    variables = c("variable1", "cohort_end_date"),
    estimates = "mean",
    minimumCellCounts = 0
  )

})

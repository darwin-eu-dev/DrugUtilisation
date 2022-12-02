test_that("test getAge", {
  cdm <- mockDrugUtilisation(
    person = dplyr::tibble(
      person_id = c(1, 2, 3, 4, 5),
      year_of_birth = c(2000, 2001, 2002, 2003, 2004),
      month_of_birth = c(12, 2, 3, NA, NA),
      day_of_birth = c(NA, 10, 15, 25, NA),
    )
  )
  expect_error(a)
})

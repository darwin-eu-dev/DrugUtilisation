test_that("expected errors on inputs", {

  #need person table
  #obsercation table
  cdm <- mockDrugUtilisation(
    cohort1 = dplyr::tibble(
      cohort_definition_id = c(1, 1, 1, 2, 2),
      subject_id = c(1, 1, 2, 1, 2),
      cohort_start_date = as.Date(c(
        "2010-01-01", "2010-02-01", "2010-01-01", "2010-03-01",
        "2010-02-01"
      )),
      cohort_end_date = as.Date(c(
        "2010-01-10", "2010-02-10", "2010-01-11", "2020-03-10",
        "2010-02-10"
      ))
    ),
    person = dplyr::tibble(
      person_id = c(1, 2),
      gender_concept_id = c(8507, 8532),
      year_of_birth = c(2000, 2001),
      month_of_birth = c(1, 1),
      day_of_birth = c(1, NA)
  ),
  visit_occurrence =  dplyr::tibble(
    person_id = c(1, 2),
    visit_concept_id = c(1, 2),
    visit_start_date = as.Date(c("2009-12-01","2009-12-01")),
    visit_end_date = as.Date(c("2010-12-01","2010-12-01")))
  )

  result <- getTableOne(cdm, targetCohortName = "cohort1", ageGroups = list(c(0,20), c(30,50)),
              windowVisitOcurrence = c(-365,0))

  expect_true(all(result[result$covariate =="ageGroup_0;20",]$value == 5))

  expect_true(result %>% dplyr::filter(covariate == "cohort_start_date",estimate == "max",
                                cohort_definition_id  == 1) %>% dplyr::pull(value) == as.Date("2010-02-01"))
  expect_true(result %>% dplyr::filter(covariate == "cohort_start_date",estimate == "max",
                                cohort_definition_id  == 2) %>% dplyr::pull(value) == as.Date("2010-03-01"))

})

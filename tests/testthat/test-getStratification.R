# test_that("test initial errors", {
#   cdm <- mockDrugUtilisation(
#     person = dplyr::tibble(
#       person_id = c(1, 2),
#       year_of_birth = as.integer(c(1995, 1993)),
#       month_of_birth = as.integer(c(10, 11)),
#       day_of_birth = as.integer(c(1, 12)),
#       gender_concept_id = c(8532, 8507)
#     ),
#     cohort1 = dplyr::tibble(
#       cohort_definition_id = c(1, 1, 1, 1, 2),
#       subject_id = c(1, 2, 3, 1, 1),
#       cohort_start_date = as.Date(c(
#         "2020-01-01", "2022-05-07", "2012-12-15", "2010-05-20", "2020-02-03"
#       )),
#       cohort_end_date = as.Date(c(
#         "2021-01-01", "2022-05-08", "2012-12-18", "2010-07-20", "2021-02-03"
#       ))
#     )
#   )
#   expect_error(getStratification())
#   expect_error(getStratification(cdm = cdm))
#   expect_error(getStratification(cdm = cdm, targetCohortName = 1))
#   expect_error(getStratification(cdm = cdm, targetCohortName = NA))
#   expect_error(getStratification(cdm = cdm, targetCohortName = "no"))
#   expect_error(getStratification(cdm = cdm, targetCohortName = "cohort1"))
#   expect_error(getStratification(
#     cdm = cdm, targetCohortName = "cohort1", targetCohortId = NA
#   ))
#   expect_error(getStratification(
#     cdm = cdm, targetCohortName = "cohort1", targetCohortId = "1"
#   ))
#   expect_error(getStratification(
#     cdm = cdm, targetCohortName = "cohort1", sex = "Both", targetCohortId = 3
#   ))
#   x <- getStratification(
#     cdm = cdm, targetCohortName = "cohort1", sex = "Both", targetCohortId = 1
#   )
#   expect_error(getStratification(
#     cdm = cdm, targetCohortName = "cohort1", sex = c("Both", "Both"),
#     targetCohortId = 1
#   ))
#   expect_error(getStratification(
#     cdm = cdm, targetCohortName = "cohort1", sex = c("Both", NA),
#     targetCohortId = 1
#   ))
#   expect_error(getStratification(
#     cdm = cdm, targetCohortName = "cohort1", sex = c("Both", "x"),
#     targetCohortId = 1
#   ))
#   x <- getStratification(
#     cdm = cdm, targetCohortName = "cohort1", sex = c("Both", "Male"),
#     targetCohortId = 1
#   )
#   expect_error(getStratification(
#     cdm = cdm, targetCohortName = "cohort1", ageGroup = NA,
#     targetCohortId = 1
#   ))
#   expect_error(getStratification(
#     cdm = cdm, targetCohortName = "cohort1", ageGroup = list(),
#     targetCohortId = 1
#   ))
#   expect_error(getStratification(
#     cdm = cdm, targetCohortName = "cohort1", ageGroup = list("A"),
#     targetCohortId = 1
#   ))
#   expect_error(getStratification(
#     cdm = cdm, targetCohortName = "cohort1", ageGroup = list(1),
#     targetCohortId = 1
#   ))
#   expect_error(getStratification(
#     cdm = cdm, targetCohortName = "cohort1", ageGroup = list(c(-1, 1)),
#     targetCohortId = 1
#   ))
#   expect_error(getStratification(
#     cdm = cdm, targetCohortName = "cohort1", ageGroup = list(c(4, 1)),
#     targetCohortId = 1
#   ))
#   expect_error(getStratification(
#     cdm = cdm, targetCohortName = "cohort1", ageGroup = list(c(0, 19), c(0)),
#     targetCohortId = 1
#   ))
#   x <- getStratification(
#     cdm = cdm, targetCohortName = "cohort1", ageGroup = list(c(0, 19), c(0, 0)),
#     targetCohortId = 1
#   )
#   x <- getStratification(
#     cdm = cdm, targetCohortName = "cohort1", ageGroup = list(c(0, 19), c(0, NA)),
#     targetCohortId = 1
#   )
#   x <- getStratification(
#     cdm = cdm, targetCohortName = "cohort1", ageGroup = list(c(0, 19), c(NA, 10)),
#     targetCohortId = 1
#   )
#   x <- getStratification(
#     cdm = cdm, targetCohortName = "cohort1", ageGroup = list(c(0, 19), c(NA, NA)),
#     targetCohortId = 1
#   )
#   expect_error(getStratification(
#     cdm = cdm, targetCohortName = "cohort1", indexYearGroup = list(),
#     targetCohortId = 1
#   ))
#   expect_error(getStratification(
#     cdm = cdm, targetCohortName = "cohort1", indexYearGroup = list("hola"),
#     targetCohortId = 1
#   ))
#   expect_error(getStratification(
#     cdm = cdm,
#     targetCohortName = "cohort1",
#     indexYearGroup = list(100, c(10:20), NA),
#     targetCohortId = 1
#   ))
#   expect_error(getStratification(
#     cdm = cdm,
#     targetCohortName = "cohort1",
#     indexYearGroup = list(100, c(10:20), as.numeric(NA)),
#     targetCohortId = 1
#   ))
#   expect_error(getStratification(
#     cdm = cdm,
#     targetCohortName = "cohort1",
#     indexYearGroup = list(100, c(10:20), c(20, 45)),
#     targetCohortId = 1
#   ))
#   x <- getStratification(
#     cdm = cdm,
#     targetCohortName = "cohort1",
#     indexYearGroup = list(100, c(10, 20), c(20, 45)),
#     targetCohortId = 1
#   )
# })
#
# test_that("test output format", {
#   cdm <- mockDrugUtilisation(
#     person = dplyr::tibble(
#       person_id = c(1, 2),
#       year_of_birth = as.integer(c(1995, 1993)),
#       month_of_birth = as.integer(c(10, 11)),
#       day_of_birth = as.integer(c(1, 12)),
#       gender_concept_id = c(8532, 8507)
#     ),
#     cohort1 = dplyr::tibble(
#       cohort_definition_id = c(1, 1, 1, 1, 2),
#       subject_id = c(1, 2, 3, 1, 1),
#       cohort_start_date = as.Date(c(
#         "2020-01-01", "2022-05-07", "2012-12-15", "2010-05-20", "2020-02-03"
#       )),
#       cohort_end_date = as.Date(c(
#         "2021-01-01", "2022-05-08", "2012-12-18", "2010-07-20", "2021-02-03"
#       ))
#     )
#   )
#   x <- getStratification(
#     cdm = cdm, targetCohortName = "cohort1", sex = "Both", targetCohortId = 1
#   )
#   expect_true("tbl" %in% class(x))
#   expect_true(length(colnames(x)) == 4)
#   expect_true(all(c(
#     "cohort_definition_id", "subject_id", "cohort_start_date", "cohort_end_date"
#   ) %in% colnames(x)))
# })
#
# test_that("test sex strata", {
#   cdm <- mockDrugUtilisation(
#     person = dplyr::tibble(
#       person_id = c(1, 2, 3, 4, 5, 6, 7, 8),
#       year_of_birth = as.integer(c(1991, 1990, 1989, 1997, 2002, 1879, 1978, 2000)),
#       month_of_birth = as.integer(NA),
#       day_of_birth = as.integer(NA),
#       gender_concept_id = c(8507, 8532, 8507, 8532, 8532, 8532, 8507, NA)
#     ),
#     cohort1 = dplyr::tibble(
#       cohort_definition_id = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1),
#       subject_id = c(1, 2, 3, 1, 1, 2, 3, 4, 5, 6, 7, 8),
#       cohort_start_date = as.Date(c(
#         "2010-01-01", "2010-05-07", "2010-12-15", "2011-05-20", "2012-02-03",
#         "2035-01-01", "2023-05-07", "2018-12-15", "2020-05-20", "2030-02-03",
#         "2000-01-01", "2015-05-07"
#       )),
#       cohort_end_date = as.Date(c(
#         "2010-01-01", "2010-05-07", "2010-12-15", "2011-05-20", "2012-02-03",
#         "2035-01-01", "2023-05-07", "2018-12-15", "2020-05-20", "2030-02-03",
#         "2000-01-01", "2015-05-07"
#       ))
#     ),
#     cohort2 = dplyr::tibble(
#       cohort_definition_id = c(1, 1, 2, 1, 2, 1, 3),
#       subject_id = c(1, 3, 1, 1, 2, 2, 4),
#       cohort_start_date = as.Date(c(
#         "2009-12-25", "2010-12-01", "2011-04-20", "2012-02-03", "2010-05-08",
#         "2034-12-18", "2017-05-08"
#       )),
#       cohort_end_date = as.Date(c(
#         "2009-12-25", "2010-12-01", "2011-04-20", "2012-02-03", "2010-05-08",
#         "2034-12-18", "2017-05-08"
#       ))
#     ),
#     condition_occurrence = dplyr::tibble(
#       condition_occurrence_id = 1,
#       person_id = 5,
#       condition_concept_id = 8,
#       condition_start_date = as.Date("2020-05-17"),
#       condition_end_date = as.Date("2020-05-17")
#     )
#   )
#
#   x <- getStratification(
#     cdm = cdm,
#     targetCohortName = "cohort1",
#     targetCohortId = 1,
#                          sex = c("Female", "Male")
#     )
#   strata <- attr(x, "cohortSet")
#   expect_true(strata %>% nrow() == 2)
#   expect_true(
#     x %>%
#       dplyr::filter(cohort_definition_id == 1) %>%
#       dplyr::tally() %>%
#       dplyr::pull("n") == 4
#     )
#   expect_true(
#     x %>%
#       dplyr::filter(cohort_definition_id == 2) %>%
#       dplyr::tally() %>%
#       dplyr::pull("n") == 5
#   )
#
#   x <- getStratification(
#     cdm = cdm,
#     targetCohortName = "cohort1",
#     targetCohortId = 1,
#     sex = c("Both", "Female", "Male")
#   )
#   strata <- attr(x, "cohortSet")
#   expect_true(strata %>% nrow() == 3)
#   expect_true(
#     x %>%
#       dplyr::filter(cohort_definition_id == 1) %>%
#       dplyr::tally() %>%
#       dplyr::pull("n") == 9
#   )
#   expect_true(
#     x %>%
#       dplyr::filter(cohort_definition_id == 2) %>%
#       dplyr::tally() %>%
#       dplyr::pull("n") == 4
#   )
#   expect_true(
#     x %>%
#       dplyr::filter(cohort_definition_id == 3) %>%
#       dplyr::tally() %>%
#       dplyr::pull("n") == 5
#   )
#
#   x <- getStratification(
#     cdm = cdm,
#     targetCohortName = "cohort1",
#     targetCohortId = 1,
#     sex = c("Male")
#   )
#   strata <- attr(x, "cohortSet")
#   expect_true(strata %>% nrow() == 1)
#   expect_true(
#     x %>%
#       dplyr::filter(cohort_definition_id == 1) %>%
#       dplyr::tally() %>%
#       dplyr::pull("n") == 5
#   )
#
# })
#
# test_that("test ageGroup strata", {
#   cdm <- mockDrugUtilisation(
#     person = dplyr::tibble(
#       person_id = c(1, 2, 3, 4, 5, 6, 7, 8),
#       year_of_birth = as.integer(c(1991, 1990, 1989, 1997, 2002, 1879, 1978, 2000)),
#       month_of_birth = as.integer(NA),
#       day_of_birth = as.integer(NA),
#       gender_concept_id = c(8507, 8532, 8507, 8532, 8532, 8532, 8507, NA)
#     ),
#     cohort1 = dplyr::tibble(
#       cohort_definition_id = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1),
#       subject_id = c(1, 2, 3, 1, 1, 2, 3, 4, 5, 6, 7, 8),
#       cohort_start_date = as.Date(c(
#         "2010-01-01", "2010-05-07", "2010-12-15", "2011-05-20", "2012-02-03",
#         "2035-01-01", "2023-05-07", "2018-12-15", "2020-05-20", "2030-02-03",
#         "2000-01-01", "2015-05-07"
#       )),
#       cohort_end_date = as.Date(c(
#         "2010-01-01", "2010-05-07", "2010-12-15", "2011-05-20", "2012-02-03",
#         "2035-01-01", "2023-05-07", "2018-12-15", "2020-05-20", "2030-02-03",
#         "2000-01-01", "2015-05-07"
#       ))
#     ),
#     cohort2 = dplyr::tibble(
#       cohort_definition_id = c(1, 1, 2, 1, 2, 1, 3),
#       subject_id = c(1, 3, 1, 1, 2, 2, 4),
#       cohort_start_date = as.Date(c(
#         "2009-12-25", "2010-12-01", "2011-04-20", "2012-02-03", "2010-05-08",
#         "2034-12-18", "2017-05-08"
#       )),
#       cohort_end_date = as.Date(c(
#         "2009-12-25", "2010-12-01", "2011-04-20", "2012-02-03", "2010-05-08",
#         "2034-12-18", "2017-05-08"
#       ))
#     ),
#     condition_occurrence = dplyr::tibble(
#       condition_occurrence_id = 1,
#       person_id = 5,
#       condition_concept_id = 8,
#       condition_start_date = as.Date("2020-05-17"),
#       condition_end_date = as.Date("2020-05-17")
#     )
#   )
#
#   x <- getStratification(
#     cdm = cdm,
#     targetCohortName = "cohort1",
#     targetCohortId = 1,
#     ageGroup = list(c(0,19), c(20, 39), c(40,59), c(0, 150), c(0,50))
#   )
#   strata <- attr(x, "cohortSet")
#   expect_true(strata %>% nrow() == 5)
#   expect_true(
#     x %>%
#       dplyr::filter(cohort_definition_id == 1) %>%
#       dplyr::tally() %>%
#       dplyr::pull("n") == 2
#   )
#   expect_true(
#     x %>%
#       dplyr::filter(cohort_definition_id == 2) %>%
#       dplyr::tally() %>%
#       dplyr::pull("n") == 6
#   )
#   expect_true(
#     x %>%
#       dplyr::filter(cohort_definition_id == 3) %>%
#       dplyr::tally() %>%
#       dplyr::pull("n") == 1
#   )
#   expect_true(
#     x %>%
#       dplyr::filter(cohort_definition_id == 4) %>%
#       dplyr::tally() %>%
#       dplyr::pull("n") == 9
#   )
#   expect_true(
#     x %>%
#       dplyr::filter(cohort_definition_id == 5) %>%
#       dplyr::tally() %>%
#       dplyr::pull("n") == 9
#   )
#
# })
#
# test_that("test indexYear strata", {
#   cdm <- mockDrugUtilisation(
#     person = dplyr::tibble(
#       person_id = c(1, 2, 3, 4, 5, 6, 7, 8),
#       year_of_birth = as.integer(c(1991, 1990, 1989, 1997, 2002, 1879, 1978, 2000)),
#       month_of_birth = as.integer(NA),
#       day_of_birth = as.integer(NA),
#       gender_concept_id = c(8507, 8532, 8507, 8532, 8532, 8532, 8507, NA)
#     ),
#     cohort1 = dplyr::tibble(
#       cohort_definition_id = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1),
#       subject_id = c(1, 2, 3, 1, 1, 2, 3, 4, 5, 6, 7, 8),
#       cohort_start_date = as.Date(c(
#         "2010-01-01", "2010-05-07", "2010-12-15", "2011-05-20", "2012-02-03",
#         "2035-01-01", "2023-05-07", "2018-12-15", "2020-05-20", "2030-02-03",
#         "2000-01-01", "2015-05-07"
#       )),
#       cohort_end_date = as.Date(c(
#         "2010-01-01", "2010-05-07", "2010-12-15", "2011-05-20", "2012-02-03",
#         "2035-01-01", "2023-05-07", "2018-12-15", "2020-05-20", "2030-02-03",
#         "2000-01-01", "2015-05-07"
#       ))
#     ),
#     cohort2 = dplyr::tibble(
#       cohort_definition_id = c(1, 1, 2, 1, 2, 1, 3),
#       subject_id = c(1, 3, 1, 1, 2, 2, 4),
#       cohort_start_date = as.Date(c(
#         "2009-12-25", "2010-12-01", "2011-04-20", "2012-02-03", "2010-05-08",
#         "2034-12-18", "2017-05-08"
#       )),
#       cohort_end_date = as.Date(c(
#         "2009-12-25", "2010-12-01", "2011-04-20", "2012-02-03", "2010-05-08",
#         "2034-12-18", "2017-05-08"
#       ))
#     ),
#     condition_occurrence = dplyr::tibble(
#       condition_occurrence_id = 1,
#       person_id = 5,
#       condition_concept_id = 8,
#       condition_start_date = as.Date("2020-05-17"),
#       condition_end_date = as.Date("2020-05-17")
#     )
#   )
#
#   x <- getStratification(
#     cdm = cdm,
#     targetCohortName = "cohort1",
#     targetCohortId = 1,
#     indexYearGroup = list(2010, c(2010,2020), c(2000,2035), 2034)
#   )
#   strata <- attr(x, "cohortSet")
#   expect_true(strata %>% nrow() == 4)
#   expect_true(
#     x %>%
#       dplyr::filter(cohort_definition_id == 1) %>%
#       dplyr::tally() %>%
#       dplyr::pull("n") == 3
#   )
#   expect_true(
#     x %>%
#       dplyr::filter(cohort_definition_id == 2) %>%
#       dplyr::tally() %>%
#       dplyr::pull("n") == 7
#   )
#   expect_true(
#     x %>%
#       dplyr::filter(cohort_definition_id == 3) %>%
#       dplyr::tally() %>%
#       dplyr::pull("n") == 9
#   )
#   expect_true(
#     x %>%
#       dplyr::filter(cohort_definition_id == 4) %>%
#       dplyr::tally() %>%
#       dplyr::pull("n") == 0
#   )
#
# })
#
# test_that("test indication strata", {
#   cdm <- mockDrugUtilisation(
#     person = dplyr::tibble(
#       person_id = c(1, 2, 3, 4, 5, 6, 7, 8),
#       year_of_birth = as.integer(c(1991, 1990, 1989, 1997, 2002, 1879, 1978, 2000)),
#       month_of_birth = as.integer(NA),
#       day_of_birth = as.integer(NA),
#       gender_concept_id = c(8507, 8532, 8507, 8532, 8532, 8532, 8507, NA)
#     ),
#     cohort1 = dplyr::tibble(
#       cohort_definition_id = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1),
#       subject_id = c(1, 2, 3, 1, 1, 2, 3, 4, 5, 6, 7, 8),
#       cohort_start_date = as.Date(c(
#         "2010-01-01", "2010-05-07", "2010-12-15", "2011-05-20", "2012-02-03",
#         "2035-01-01", "2023-05-07", "2018-12-15", "2020-05-20", "2030-02-03",
#         "2000-01-01", "2015-05-07"
#       )),
#       cohort_end_date = as.Date(c(
#         "2010-01-01", "2010-05-07", "2010-12-15", "2011-05-20", "2012-02-03",
#         "2035-01-01", "2023-05-07", "2018-12-15", "2020-05-20", "2030-02-03",
#         "2000-01-01", "2015-05-07"
#       ))
#     ),
#     cohort2 = dplyr::tibble(
#       cohort_definition_id = c(1, 1, 2, 1, 2, 1, 3),
#       subject_id = c(1, 3, 1, 1, 2, 2, 4),
#       cohort_start_date = as.Date(c(
#         "2009-12-25", "2010-12-01", "2011-04-20", "2012-02-03", "2010-05-08",
#         "2034-12-18", "2017-05-08"
#       )),
#       cohort_end_date = as.Date(c(
#         "2009-12-25", "2010-12-01", "2011-04-20", "2012-02-03", "2010-05-08",
#         "2034-12-18", "2017-05-08"
#       ))
#     ),
#     condition_occurrence = dplyr::tibble(
#       condition_occurrence_id = 1,
#       person_id = 5,
#       condition_concept_id = 8,
#       condition_start_date = as.Date("2020-05-17"),
#       condition_end_date = as.Date("2020-05-17")
#     )
#   )
#
#   indicationTable <- getIndication(
#     cdm = cdm,
#     targetCohortName = "cohort1",
#     targetCohortDefinitionIds = 1,
#     indicationCohortName = "cohort2",
#     indicationDefinitionSet = dplyr::tibble(
#       cohortId = c(1, 2),
#       cohortName = c("covid", "tuberculosis")
#     ),
#     indicationGap = c(7, 30, 10000),
#     unknownIndicationTables = "condition_occurrence"
#   )
#
#   x <- getStratification(
#     cdm = cdm,
#     targetCohortName = "cohort1",
#     targetCohortId = 1,
#     indicationTable = indicationTable
#   )
#   strata <- attr(x, "cohortSet")
#   expect_true(strata %>% nrow() == 13)
#   cdi_counts <- c(9, 2, 0, 1, 6, 4, 1, 1, 3, 6, 3, 1, 2)
#   for (k in 1:length(cdi_counts)){
#     expect_true(
#       x %>%
#         dplyr::filter(cohort_definition_id == k) %>%
#         dplyr::tally() %>%
#         dplyr::pull("n") == cdi_counts[k]
#     )
#   }
# })
#
# test_that("test multiple conditions", {
#   cdm <- mockDrugUtilisation(
#     person = dplyr::tibble(
#       person_id = c(1, 2, 3, 4, 5, 6, 7, 8),
#       year_of_birth = as.integer(c(1991, 1990, 1989, 1997, 2002, 1879, 1978, 2000)),
#       month_of_birth = as.integer(NA),
#       day_of_birth = as.integer(NA),
#       gender_concept_id = c(8507, 8532, 8507, 8532, 8532, 8532, 8507, NA)
#     ),
#     cohort1 = dplyr::tibble(
#       cohort_definition_id = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1),
#       subject_id = c(1, 2, 3, 1, 1, 2, 3, 4, 5, 6, 7, 8),
#       cohort_start_date = as.Date(c(
#         "2010-01-01", "2010-05-07", "2010-12-15", "2011-05-20", "2012-02-03",
#         "2035-01-01", "2023-05-07", "2018-12-15", "2020-05-20", "2030-02-03",
#         "2000-01-01", "2015-05-07"
#       )),
#       cohort_end_date = as.Date(c(
#         "2010-01-01", "2010-05-07", "2010-12-15", "2011-05-20", "2012-02-03",
#         "2035-01-01", "2023-05-07", "2018-12-15", "2020-05-20", "2030-02-03",
#         "2000-01-01", "2015-05-07"
#       ))
#     ),
#     cohort2 = dplyr::tibble(
#       cohort_definition_id = c(1, 1, 2, 1, 2, 1, 3),
#       subject_id = c(1, 3, 1, 1, 2, 2, 4),
#       cohort_start_date = as.Date(c(
#         "2009-12-25", "2010-12-01", "2011-04-20", "2012-02-03", "2010-05-08",
#         "2034-12-18", "2017-05-08"
#       )),
#       cohort_end_date = as.Date(c(
#         "2009-12-25", "2010-12-01", "2011-04-20", "2012-02-03", "2010-05-08",
#         "2034-12-18", "2017-05-08"
#       ))
#     ),
#     condition_occurrence = dplyr::tibble(
#       condition_occurrence_id = 1,
#       person_id = 5,
#       condition_concept_id = 8,
#       condition_start_date = as.Date("2020-05-17"),
#       condition_end_date = as.Date("2020-05-17")
#     )
#   )
#
#   indicationTable <- getIndication(
#     cdm = cdm,
#     targetCohortName = "cohort1",
#     targetCohortDefinitionIds = 1,
#     indicationCohortName = "cohort2",
#     indicationDefinitionSet = dplyr::tibble(
#       cohortId = c(1, 2),
#       cohortName = c("covid", "tuberculosis")
#     ),
#     indicationGap = 7,
#     unknownIndicationTables = "condition_occurrence"
#   )
#
#   x <- getStratification(
#     cdm = cdm,
#     targetCohortName = "cohort1",
#     targetCohortId = 1,
#     sex = c("Both", "Female"),
#     ageGroup = list(c(NA, NA), c(0, 19), c(20, 49)),
#     indexYearGroup = list(c(2010, 2019), c(2020, 2035)),
#     indicationTable = indicationTable
#   )
#   strata <- attr(x, "cohortSet")
#   expect_true(strata %>% nrow() == 60)
#   cdi_counts <- c(
#     6, 2, 0, 0, 4,
#     3, 0, 0, 1, 2,
#     1, 1, 0, 0, 0,
#     1, 0, 0, 1, 0,
#     5, 1, 0, 0, 4,
#     2, 0, 0, 0, 2,
#     2, 0, 0, 0, 2,
#     2, 0, 0, 1, 1,
#     0, 0, 0, 0, 0,
#     1, 0, 0, 1, 0,
#     2, 0, 0, 0, 2,
#     1, 0, 0, 0, 1
#   )
#   for (k in 1:length(cdi_counts)){
#     expect_true(
#       x %>%
#         dplyr::filter(cohort_definition_id == k) %>%
#         dplyr::tally() %>%
#         dplyr::pull("n") == cdi_counts[k]
#     )
#   }
# })
#
# test_that("test oneStrata option", {
#   cdm <- mockDrugUtilisation(
#     person = dplyr::tibble(
#       person_id = c(1, 2, 3, 4, 5, 6, 7, 8),
#       year_of_birth = as.integer(c(1991, 1990, 1989, 1997, 2002, 1879, 1978, 2000)),
#       month_of_birth = as.integer(NA),
#       day_of_birth = as.integer(NA),
#       gender_concept_id = c(8507, 8532, 8507, 8532, 8532, 8532, 8507, NA)
#     ),
#     cohort1 = dplyr::tibble(
#       cohort_definition_id = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1),
#       subject_id = c(1, 2, 3, 1, 1, 2, 3, 4, 5, 6, 7, 8),
#       cohort_start_date = as.Date(c(
#         "2010-01-01", "2010-05-07", "2010-12-15", "2011-05-20", "2012-02-03",
#         "2035-01-01", "2023-05-07", "2018-12-15", "2020-05-20", "2030-02-03",
#         "2000-01-01", "2015-05-07"
#       )),
#       cohort_end_date = as.Date(c(
#         "2010-01-01", "2010-05-07", "2010-12-15", "2011-05-20", "2012-02-03",
#         "2035-01-01", "2023-05-07", "2018-12-15", "2020-05-20", "2030-02-03",
#         "2000-01-01", "2015-05-07"
#       ))
#     ),
#     cohort2 = dplyr::tibble(
#       cohort_definition_id = c(1, 1, 2, 1, 2, 1, 3),
#       subject_id = c(1, 3, 1, 1, 2, 2, 4),
#       cohort_start_date = as.Date(c(
#         "2009-12-25", "2010-12-01", "2011-04-20", "2012-02-03", "2010-05-08",
#         "2034-12-18", "2017-05-08"
#       )),
#       cohort_end_date = as.Date(c(
#         "2009-12-25", "2010-12-01", "2011-04-20", "2012-02-03", "2010-05-08",
#         "2034-12-18", "2017-05-08"
#       ))
#     ),
#     condition_occurrence = dplyr::tibble(
#       condition_occurrence_id = 1,
#       person_id = 5,
#       condition_concept_id = 8,
#       condition_start_date = as.Date("2020-05-17"),
#       condition_end_date = as.Date("2020-05-17")
#     )
#   )
#
#   indicationTable <- getIndication(
#     cdm = cdm,
#     targetCohortName = "cohort1",
#     targetCohortDefinitionIds = 1,
#     indicationCohortName = "cohort2",
#     indicationDefinitionSet = dplyr::tibble(
#       cohortId = c(1, 2),
#       cohortName = c("covid", "tuberculosis")
#     ),
#     indicationGap = 7,
#     unknownIndicationTables = "condition_occurrence"
#   )
#
#   x <- getStratification(
#     cdm = cdm,
#     targetCohortName = "cohort1",
#     targetCohortId = 1,
#     sex = c("Both", "Female"),
#     ageGroup = list(c(NA, NA), c(0, 19), c(20, 49)),
#     indexYearGroup = list(c(2010, 2019), c(2020, 2035)),
#     indicationTable = indicationTable,
#     oneStrata = TRUE
#   )
#   strata <- attr(x, "cohortSet")
#   expect_true(strata %>% nrow() == 9)
#   cdi_counts <- c(6, 2, 0, 0, 4, 3, 1, 5, 2)
#   for (k in 1:length(cdi_counts)){
#     expect_true(
#       x %>%
#         dplyr::filter(cohort_definition_id == k) %>%
#         dplyr::tally() %>%
#         dplyr::pull("n") == cdi_counts[k]
#     )
#   }
# })

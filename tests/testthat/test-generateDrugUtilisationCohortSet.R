test_that("test inputs", {
  skip_on_cran()
  cdm <- mockDrugUtilisation(con = connection(), writeSchema = schema())
  expect_error(generateDrugUtilisationCohortSet())
  expect_error(generateDrugUtilisationCohortSet(cdm = cdm))
  expect_error(generateDrugUtilisationCohortSet(cdm, "dus", 1))
  expect_error(generateDrugUtilisationCohortSet(cdm, "dus", list(1)))
  expect_no_error(generateDrugUtilisationCohortSet(cdm, "dus", list(acetaminophen = 1)))
  cdmNew <- generateDrugUtilisationCohortSet(cdm, "dus", list(acetaminophen = 1125360))
  expect_true("cohort_table" %in% class(cdmNew$dus))
  expect_true(all(colnames(cdmNew$dus) == c(
    "cohort_definition_id", "subject_id", "cohort_start_date", "cohort_end_date"
  )))
  expect_error(generateDrugUtilisationCohortSet(
    cdm, "dus", list(acetaminophen = 1125360),
    gapEra = "7"
  ))

  mockDisconnect(cdm = cdm)
})

test_that("basic functionality drug_conceptId", {
  skip_on_cran()
  cdm <- mockDrugUtilisation(
    con = connection(),
    writeSchema = schema(),
    drug_exposure = dplyr::tibble(
      drug_exposure_id = 1:4,
      person_id = c(1, 1, 1, 1),
      drug_concept_id = sample(c(1125360, 2905077, 43135274), 4, replace = T),
      drug_exposure_start_date = as.Date(
        c("2020-04-01", "2020-06-01", "2021-02-12", "2021-03-01"), "%Y-%m-%d"
      ),
      drug_exposure_end_date = as.Date(
        c("2020-04-30", "2020-09-11", "2021-02-15", "2021-03-24"), "%Y-%m-%d"
      ),
      drug_type_concept_id = 38000177,
      quantity = 1
    )
  )
  acetaminophen <- list(acetaminophen = c(1125360, 2905077, 43135274))

  # check gap
  cdm1 <- generateDrugUtilisationCohortSet(
    cdm, "dus", acetaminophen,
    gapEra = 0
  )
  expect_true(cdm1$dus |> dplyr::tally() |> dplyr::pull() == 4)
  cdm1 <- generateDrugUtilisationCohortSet(
    cdm, "dus", acetaminophen,
    gapEra = 13
  )
  expect_true(cdm1$dus |> dplyr::tally() |> dplyr::pull() == 4)
  cdm1 <- generateDrugUtilisationCohortSet(
    cdm, "dus", acetaminophen,
    gapEra = 14
  )
  expect_true(cdm1$dus |> dplyr::tally() |> dplyr::pull() == 3)
  cdm1 <- generateDrugUtilisationCohortSet(
    cdm, "dus", acetaminophen,
    gapEra = 31
  )
  expect_true(cdm1$dus |> dplyr::tally() |> dplyr::pull() == 3)
  cdm1 <- generateDrugUtilisationCohortSet(
    cdm, "dus", acetaminophen,
    gapEra = 32
  )
  expect_true(cdm1$dus |> dplyr::tally() |> dplyr::pull() == 2)
  cdm1 <- generateDrugUtilisationCohortSet(
    cdm, "dus", acetaminophen,
    gapEra = 153
  )
  expect_true(cdm1$dus |> dplyr::tally() |> dplyr::pull() == 2)
  cdm1 <- generateDrugUtilisationCohortSet(
    cdm, "dus", acetaminophen,
    gapEra = 154
  )
  expect_true(cdm1$dus |> dplyr::tally() |> dplyr::pull() == 1)
  cdm1 <- generateDrugUtilisationCohortSet(
    cdm, "dus", acetaminophen,
    gapEra = 1500
  )
  expect_true(cdm1$dus |> dplyr::tally() |> dplyr::pull() == 1)

  # check cdm reference in attributes
  expect_true(!is.null(attr(cdm1$dus, "cdm_reference", exact = TRUE)))

  # check that missing end dates are dropped


  # check that end before start dates are dropped

  mockDisconnect(cdm = cdm)
})

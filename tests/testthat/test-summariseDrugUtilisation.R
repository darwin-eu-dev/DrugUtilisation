test_that("summariseDrugUtilisation works", {
  cdm <- mockDrugUtilisation(
    con = connection(),
    writeSchema = schema(),
    drug_exposure = dplyr::tibble(
      drug_exposure_id = 1:12,
      person_id = c(1, 1, 1, 2, 2, 3, 3, 1, 2, 4, 4, 1),
      drug_concept_id = c(
        1125360, 2905077, 1125360, 1125360, 1125315, 1125360, 1125360, 1503327,
        1503328, 1503297, 1503297, 1125360
      ),
      drug_exposure_start_date = as.Date(c(
        "2020-01-15", "2020-01-20", "2020-02-20", "2021-02-15", "2021-05-12",
        "2022-01-12", "2022-11-15", "2020-01-01", "2021-03-11", "2010-01-01",
        "2010-03-15", "2025-01-01"
      )),
      drug_exposure_end_date = as.Date(c(
        "2020-01-25", "2020-03-15", "2020-02-28", "2021-03-15", "2021-05-25",
        "2022-02-15", "2022-12-14", "2020-04-13", "2021-04-20", "2010-01-05",
        "2010-05-12", "2025-12-31"
      )),
      drug_type_concept_id = 0,
      quantity = c(10, 20, 30, 1, 10, 5, 15, 20, 30, 14, 10, 2)
    ),
    dus_cohort = dplyr::tibble(
      cohort_definition_id = c(1, 2, 1, 1, 1, 2),
      subject_id = c(1, 1, 2, 3, 4, 4),
      cohort_start_date = as.Date(c(
        "2020-01-15", "2020-01-24", "2021-01-15", "2022-02-01", "2010-01-05",
        "2010-01-05"
      )),
      cohort_end_date = as.Date(c(
        "2020-02-28", "2020-02-10", "2021-06-08", "2022-12-01", "2010-03-15",
        "2010-03-15"
      )),
      extra_column = "asd"
    ),
    observation_period = dplyr::tibble(
      observation_period_id = 1:4,
      person_id = 1:4,
      observation_period_start_date = as.Date("2000-01-01"),
      observation_period_end_date = as.Date("2030-01-01"),
      period_type_concept_id = 0
    )
  )

  x0 <- cdm$dus_cohort |>
    summariseDrugUtilisation(ingredientConceptId = 1125315)
  expect_true(inherits(x0, "summarised_result"))
  expect_true(all(unique(x0$variable_name) == c(
    "number records", "number subjects", "number exposures", "time to exposure",
    "cumulative quantity", "initial quantity", "number eras", "exposed time",
    "cumulative dose", "initial daily dose"
  )))
  expect_true(all(unique(x0$variable_level) %in% c(NA, "milligram")))
  expect_true(settings(x0)$result_type == "summarise_drug_utilisation")
  expect_true(all(visOmopResults::additionalColumns(x0) == c("concept_set", "ingredient")))
  expect_true(
    x0 |> dplyr:::filter(grepl("dose", variable_name)) |> dplyr::pull("variable_level") |> unique() == "milligram"
  )
  expect_true(
    x0 |> dplyr:::filter(grepl("dose", variable_name)) |> dplyr::pull("additional_level") |> unique() == "ingredient_1125315_descendants &&& acetaminophen"
  )
  # suppress works
  expect_true(all(
    unique(x0 |> omopgenerics::suppress(minCellCount = 3) |> dplyr::filter(group_level == "cohort_2") |> dplyr::pull("estimate_value")) %in%
      c(NA_character_, "0", "NaN")
  ))

  # strata and concept set
  x1 <- cdm$dus_cohort |>
    PatientProfiles::addSex(name = "dus_cohort") |>
    summariseDrugUtilisation(ingredientConceptId = c(1125315, 1539403, 1503297, 1516976), strata = list("sex"))
  expect_true(inherits(x1, "summarised_result"))
  expect_true(all(unique(x1$variable_name) == c(
    "number records", "number subjects", "number exposures", "time to exposure",
    "cumulative quantity", "initial quantity", "number eras", "exposed time",
    "cumulative dose", "initial daily dose"
  )))
  expect_true(all(unique(x1$variable_level) %in% c(NA, "milligram")))
  expect_true(all(
    x1 |> dplyr:::filter(grepl("dose", variable_name)) |> dplyr::pull("additional_level") |> unique() == c(
      "ingredient_1125315_descendants &&& acetaminophen", "ingredient_1503297_descendants &&& metformin"
    )
  ))
  expect_true(settings(x1)$result_type == "summarise_drug_utilisation")
  expect_true(all(x1 |> visOmopResults::strataColumns() == c("sex")))

  # concept
  codelist <- CodelistGenerator::getDrugIngredientCodes(cdm, name = "acetaminophen")
  x2 <- cdm$dus_cohort |>
    summariseDrugUtilisation(ingredientConceptId = c(1125315, 1539403, 1503297, 1516976), conceptSet = codelist)
  expect_true(inherits(x2, "summarised_result"))
  expect_true(all(unique(x2$variable_name) == c(
    "number records", "number subjects", "number exposures", "time to exposure",
    "cumulative quantity", "initial quantity", "number eras", "exposed time",
    "cumulative dose", "initial daily dose"
  )))
  expect_true(all(unique(x2$variable_level) %in% c(
    NA, "milligram"
  )))
  expect_true(all(
    x2 |> dplyr:::filter(grepl("dose", variable_name)) |> dplyr::pull("additional_level") |> unique() == c(
      "161_acetaminophen &&& acetaminophen"
    )
  ))
  # test censor and index  and no ingredient
  x3 <- cdm$dus_cohort |>
    dplyr::mutate(cohort_start = cohort_start_date, cohort_end = cohort_end_date) |>
    summariseDrugUtilisation(conceptSet = codelist, indexDate = "cohort_start", censorDate = "cohort_end", initialDailyDose = FALSE, cumulativeDose = FALSE)
  expect_true(inherits(x3, "summarised_result"))
  expect_true(all(unique(x3$variable_name) == c(
    "number records", "number subjects", "number exposures", "time to exposure",
    "cumulative quantity", "initial quantity", "number eras", "exposed time"
  )))
  expect_true(is.na(unique(x3$variable_level)))
  expect_true(all(
    x3 |> dplyr::pull("additional_level") |> unique() == c(
      "overall", "161_acetaminophen"
    )
  ))

  mockDisconnect(cdm = cdm)
})

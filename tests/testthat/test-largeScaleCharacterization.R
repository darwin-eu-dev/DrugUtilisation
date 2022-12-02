
test_that("checks example, summarise = FALSE", {
  cdm <- mockDrugUtilisation(
    person = dplyr::tibble(
      cohort_definition_id = c(1, 1, 2, 2),
      subject_id = c(1, 2, 1, 2),
      cohort_start_date = as.Date(c(
        "2020-01-01",
        "2020-03-01",
        "2020-04-01",
        "2020-05-01"
      )),
      cohort_end_date = as.Date(c(
        "2020-01-01",
        "2020-03-01",
        "2020-04-01",
        "2020-05-01"
      ))
    ),
    observation_period = dplyr::tibble(
      person_id = c(1, 2),
      observation_period_start_date = as.Date(c("2005-01-01", "2000-03-30")),
      observation_period_end_date = as.Date(c("2024-12-09", "2032-03-03"))
    ),
    condition_occurrence = dplyr::tibble(
      condition_occurrence_id = c(1, 2, 3, 4, 5, 6, 7),
      person_id = c(1, 2, 1, 2, 1, 5, 1),
      condition_concept_id = c(1, 1, 2, 1, 1, 1, 3),
      condition_start_date = as.Date(c(
        "2020-05-06",
        "2019-09-04",
        "2021-04-03",
        "2005-11-12",
        "2022-12-31",
        "2020-06-02",
        "2018-01-01"
      )),
      condition_end_date = as.Date(c(
        "2020-10-06",
        "2019-12-04",
        "2021-05-03",
        "2005-11-12",
        "2022-12-31",
        "2020-07-02",
        "2018-01-01"
      ))
    )
  )

  res <- largeScaleCharacterization(
    cdm = cdm,
    targetCohortName = "person",
    targetCohortId = 1,
    tablesToCharacterize = "condition_occurrence",
    overlap = FALSE,
    summarise = FALSE
  )

  resC <- res$characterization %>% dplyr::collect()

  expect_true(nrow(resC) == 8)
  expect_true(unique(resC$table_id) == 1)
  expect_true(all(resC$window_id %in% c(1, 2, 3, 9, 10, 11)))
  expect_true(all(resC$window_id[resC$person_id == 1] %in% c(1, 9, 10, 11)))
  expect_true(all(resC$window_id[resC$person_id == 2] %in% c(1, 2, 3)))
  expect_true(sum(resC$person_id == 2) == 3)
  expect_true(sum(resC$person_id == 1) == 5)
  expect_true(unique(resC$person_id[resC$cohort_start_date ==
    as.Date("2020-03-01")]) == 2)
  expect_true(unique(resC$person_id[resC$cohort_start_date ==
    as.Date("2020-01-01")]) == 1)
  expect_true(sum(resC$concept_id == 1) == 6)
  expect_true(sum(resC$concept_id == 2) == 1)
  expect_true(sum(resC$concept_id == 3) == 1)

  res <- largeScaleCharacterization(
    cdm = cdm,
    targetCohortName = "person",
    targetCohortId = 1,
    tablesToCharacterize = "condition_occurrence",
    overlap = TRUE,
    summarise = FALSE
  )

  resC <- res$characterization %>% dplyr::collect()
  expect_true(nrow(resC) == 9)
  expect_true(unique(resC$table_id) == 1)
  expect_true(all(resC$window_id %in% c(1, 2, 3, 4, 9, 10, 11)))
  expect_true(all(resC$window_id[resC$person_id == 1] %in% c(1, 9, 10, 11)))
  expect_true(all(resC$window_id[resC$person_id == 2] %in% c(1, 2, 3, 4)))
  expect_true(sum(resC$person_id == 2) == 4)
  expect_true(sum(resC$person_id == 1) == 5)
  expect_true(unique(resC$person_id[resC$cohort_start_date ==
    as.Date("2020-03-01")]) == 2)
  expect_true(unique(resC$person_id[resC$cohort_start_date ==
    as.Date("2020-01-01")]) == 1)
  expect_true(sum(resC$concept_id == 1) == 7)
  expect_true(sum(resC$concept_id == 2) == 1)
  expect_true(sum(resC$concept_id == 3) == 1)

  res <- largeScaleCharacterization(
    cdm = cdm,
    targetCohortName = "person",
    targetCohortId = 2,
    tablesToCharacterize = "condition_occurrence",
    overlap = FALSE,
    summarise = FALSE
  )
  resC <- res$characterization %>% dplyr::collect()

  res <- largeScaleCharacterization(
    cdm = cdm,
    targetCohortName = "person",
    targetCohortId = 2,
    tablesToCharacterize = "condition_occurrence",
    overlap = TRUE,
    summarise = FALSE
  )
  resC <- res$characterization %>% dplyr::collect()

  res <- largeScaleCharacterization(
    cdm = cdm,
    targetCohortName = "person",
    targetCohortId = 1:2,
    tablesToCharacterize = "condition_occurrence",
    overlap = FALSE,
    summarise = FALSE
  )
  resC <- res$characterization %>% dplyr::collect()

  res <- largeScaleCharacterization(
    cdm = cdm,
    targetCohortName = "person",
    targetCohortId = 1:2,
    tablesToCharacterize = "condition_occurrence",
    overlap = TRUE,
    summarise = FALSE
  )
  resC <- res$characterization %>% dplyr::collect()

  resNULL <- largeScaleCharacterization(
    cdm = cdm,
    targetCohortName = "person",
    targetCohortId = NULL,
    tablesToCharacterize = "condition_occurrence",
    overlap = TRUE,
    summarise = FALSE
  )
  expect_true(identical(resNULL$temporalWindows, res$temporalWindows))
  expect_true(identical(resNULL$tablesToCharacterize, res$tablesToCharacterize))
  expect_true(identical(resNULL$overlap, res$overlap))
  expect_true(identical(
    resNULL$characterization %>% dplyr::collect(),
    res$characterization %>% dplyr::collect()
  ))
})

test_that("checks example, summarise = TRUE", {
  cdm <- mockDrugUtilisation(
    observation_period = dplyr::tibble(
      observation_period_id = 1:10,
      person_id = 1:10,
      observation_period_start_date = as.Date("1900-01-01"),
      observation_period_end_date = as.Date("2100-01-01"),
    ),
    person = dplyr::tibble(
      cohort_definition_id = c(1, 1, 2, 2),
      subject_id = c(1, 2, 1, 2),
      cohort_start_date = as.Date(c(
        "2020-01-01",
        "2020-03-01",
        "2020-04-01",
        "2020-05-01"
      )),
      cohort_end_date = as.Date(c(
        "2020-01-01",
        "2020-03-01",
        "2020-04-01",
        "2020-05-01"
      ))
    ),
    condition_occurrence = dplyr::tibble(
      condition_occurrence_id = c(1, 2, 3, 4, 5, 6, 7),
      person_id = c(1, 2, 1, 2, 1, 5, 1),
      condition_concept_id = c(1, 1, 2, 1, 1, 1, 3),
      condition_start_date = as.Date(c(
        "2020-05-06",
        "2019-09-04",
        "2021-04-03",
        "2005-11-12",
        "2022-12-31",
        "2020-06-02",
        "2018-01-01"
      )),
      condition_end_date = as.Date(c(
        "2020-10-06",
        "2019-12-04",
        "2021-05-03",
        "2005-10-12",
        "2022-12-31",
        "2020-07-02",
        "2018-01-01"
      ))
    )
  )

  res <- largeScaleCharacterization(
    cdm = cdm,
    targetCohortName = "person",
    targetCohortId = 1,
    tablesToCharacterize = "condition_occurrence",
    overlap = FALSE,
    summarise = TRUE,
    minimumCellCount = 5
  )

  resC <- res$characterization

  expect_true(nrow(resC) == 8)
  expect_true(unique(resC$table_id) == 1)
  expect_true(all(resC$window_id %in% c(1, 2, 3, 9, 10, 11)))
  expect_true(sum(resC$concept_id == 1) == 6)
  expect_true(sum(resC$concept_id == 2) == 1)
  expect_true(sum(resC$concept_id == 3) == 1)
  expect_true(all(resC$obscured_counts == TRUE))
  expect_true(all(is.na(resC$counts)))

  res <- largeScaleCharacterization(
    cdm = cdm,
    targetCohortName = "person",
    targetCohortId = 1,
    tablesToCharacterize = "condition_occurrence",
    overlap = FALSE,
    summarise = TRUE,
    minimumCellCount = 0
  )

  resC <- res$characterization

  expect_true(nrow(resC) == 8)
  expect_true(unique(resC$table_id) == 1)
  expect_true(all(resC$window_id %in% c(1, 2, 3, 9, 10, 11)))
  expect_true(sum(resC$concept_id == 1) == 6)
  expect_true(sum(resC$concept_id == 2) == 1)
  expect_true(sum(resC$concept_id == 3) == 1)
  expect_true(all(resC$obscured_counts == FALSE))
  expect_true(all(resC$counts == 1))

  cdm <- mockDrugUtilisation(
    observation_period = dplyr::tibble(
      observation_period_id = 1:10,
      person_id = 1:10,
      observation_period_start_date = as.Date("1900-01-01"),
      observation_period_end_date = as.Date("2100-01-01"),
    ),
    person = dplyr::tibble(
      cohort_definition_id = c(1, 1, 2, 2),
      subject_id = c(1, 2, 1, 2),
      cohort_start_date = as.Date(c(
        "2020-01-01",
        "2020-03-01",
        "2020-04-01",
        "2020-05-01"
      )),
      cohort_end_date = as.Date(c(
        "2020-01-01",
        "2020-03-01",
        "2020-04-01",
        "2020-05-01"
      ))
    ),
    condition_occurrence = dplyr::tibble(
      condition_occurrence_id = c(1, 2, 3, 4, 5, 6, 7, 8),
      person_id = c(1, 2, 1, 2, 1, 5, 1, 1),
      condition_concept_id = c(1, 1, 2, 1, 1, 1, 3, 3),
      condition_start_date = as.Date(c(
        "2020-05-06",
        "2019-09-04",
        "2021-04-03",
        "2005-11-12",
        "2022-12-31",
        "2020-06-02",
        "2018-01-01",
        "2018-01-01"
      )),
      condition_end_date = as.Date(c(
        "2020-10-06",
        "2019-12-04",
        "2021-05-03",
        "2005-10-12",
        "2022-12-31",
        "2020-07-02",
        "2018-01-01",
        "2018-01-01"
      ))
    )
  )

  res <- largeScaleCharacterization(
    cdm = cdm,
    targetCohortName = "person",
    targetCohortId = 1,
    tablesToCharacterize = "condition_occurrence",
    overlap = FALSE,
    summarise = TRUE,
    minimumCellCount = 0
  )

  resC <- res$characterization
  expect_true(nrow(resC) == 8)
  expect_true(unique(resC$table_id) == 1)
  expect_true(all(resC$window_id %in% c(1, 2, 3, 9, 10, 11)))
  expect_true(sum(resC$concept_id == 1) == 6)
  expect_true(sum(resC$concept_id == 2) == 1)
  expect_true(sum(resC$concept_id == 3) == 1)
  expect_true(all(resC$obscured_counts == FALSE))
  expect_true(all(resC$counts == 1))

  cdm <- mockDrugUtilisation(
    observation_period = dplyr::tibble(
      observation_period_id = 1:10,
      person_id = 1:10,
      observation_period_start_date = as.Date("1900-01-01"),
      observation_period_end_date = as.Date("2100-01-01"),
    ),
    person = dplyr::tibble(
      cohort_definition_id = c(1, 1, 2, 2),
      subject_id = c(1, 2, 1, 2),
      cohort_start_date = as.Date(c(
        "2020-01-01",
        "2020-03-01",
        "2020-04-01",
        "2020-05-01"
      )),
      cohort_end_date = as.Date(c(
        "2020-01-01",
        "2020-03-01",
        "2020-04-01",
        "2020-05-01"
      ))
    ),
    condition_occurrence = dplyr::tibble(
      condition_occurrence_id = c(1, 2, 3, 4, 5, 6, 7, 8),
      person_id = c(1, 2, 1, 2, 1, 5, 1, 2),
      condition_concept_id = c(1, 1, 2, 1, 1, 1, 3, 3),
      condition_start_date = as.Date(c(
        "2020-05-06",
        "2019-09-04",
        "2021-04-03",
        "2005-11-12",
        "2022-12-31",
        "2020-06-02",
        "2018-01-01",
        "2018-01-01"
      )),
      condition_end_date = as.Date(c(
        "2020-10-06",
        "2019-12-04",
        "2021-05-03",
        "2005-10-12",
        "2022-12-31",
        "2020-07-02",
        "2018-01-01",
        "2018-01-01"
      ))
    )
  )

  res <- largeScaleCharacterization(
    cdm = cdm,
    targetCohortName = "person",
    targetCohortId = 1,
    tablesToCharacterize = "condition_occurrence",
    overlap = FALSE,
    summarise = TRUE,
    minimumCellCount = 0
  )

  resC <- res$characterization
  expect_true(nrow(resC) == 8)
  expect_true(unique(resC$table_id) == 1)
  expect_true(all(resC$window_id %in% c(1, 2, 3, 9, 10, 11)))
  expect_true(sum(resC$concept_id == 1) == 6)
  expect_true(sum(resC$concept_id == 2) == 1)
  expect_true(sum(resC$concept_id == 3) == 1)
  expect_true(all(resC$obscured_counts == FALSE))
  expect_false(all(resC$counts == 1))

  res <- largeScaleCharacterization(
    cdm = cdm,
    targetCohortName = "person",
    targetCohortId = 1,
    tablesToCharacterize = "condition_occurrence",
    overlap = FALSE,
    summarise = TRUE,
    minimumCellCount = 2
  )

  resC <- res$characterization
  expect_true(nrow(resC) == 8)
  expect_true(unique(resC$table_id) == 1)
  expect_true(all(resC$window_id %in% c(1, 2, 3, 9, 10, 11)))
  expect_true(sum(resC$concept_id == 1) == 6)
  expect_true(sum(resC$concept_id == 2) == 1)
  expect_true(sum(resC$concept_id == 3) == 1)
  expect_true(sum(resC$obscured_counts == FALSE) == 1)
  expect_true(sum(is.na(resC$counts)) == 7)

  res <- largeScaleCharacterization(
    cdm = cdm,
    targetCohortName = "person",
    tablesToCharacterize = "condition_occurrence",
    overlap = FALSE,
    summarise = TRUE,
    minimumCellCount = 0
  )

  resC <- res$characterization
  expect_true(all(c(1, 2) %in% resC$cohort_definition_id))
})

test_that("check 2 tables", {
  cdm <- mockDrugUtilisation(
    observation_period = dplyr::tibble(
      observation_period_id = 1,
      person_id = 1,
      observation_period_start_date = as.Date("2010-01-01"),
      observation_period_end_date = as.Date("2025-01-01")
    ),
    person = dplyr::tibble(
      cohort_definition_id = 1,
      subject_id = 1,
      cohort_start_date = as.Date("2020-01-01"),
      cohort_end_date = as.Date("2020-01-01")
    ),
    condition_occurrence = dplyr::tibble(
      condition_occurrence_id = 1,
      person_id = 1,
      condition_concept_id = 1,
      condition_start_date = as.Date("2020-05-06"),
      condition_end_date = as.Date("2020-10-06")
    ),
    drug_exposure = dplyr::tibble(
      drug_exposure_id = 1,
      person_id = 1,
      drug_concept_id = 101,
      drug_exposure_start_date = as.Date("2020-05-06"),
      drug_exposure_end_date = as.Date("2020-10-06")
    )
  )

  res <- largeScaleCharacterization(
    cdm = cdm,
    targetCohortName = "person",
    targetCohortId = 1,
    tablesToCharacterize = c("condition_occurrence", "drug_exposure"),
    overlap = FALSE,
    summarise = TRUE,
    minimumCellCount = 0
  )

  resC <- res$characterization

  expect_true(nrow(resC) == 4)
  expect_true(all(resC$table_id %in% c(1, 2)))
  expect_true(length(res$tablesToCharacterize) == 2)
})

test_that("event outside observation period are not considered", {
  # all events observed
  cdm <- mockDrugUtilisation(
    observation_period = dplyr::tibble(
      observation_period_id = 1,
      person_id = 1,
      observation_period_start_date = as.Date("2010-05-06"),
      observation_period_end_date = as.Date("2025-01-01")
    ),
    person = dplyr::tibble(
      cohort_definition_id = 1,
      subject_id = 1,
      cohort_start_date = as.Date("2019-01-01"),
      cohort_end_date = as.Date("2021-01-01")
    ),
    condition_occurrence = dplyr::tibble(
      condition_occurrence_id = 1,
      person_id = 1,
      condition_concept_id = 1,
      condition_start_date = as.Date("2020-05-06"),
      condition_end_date = as.Date("2020-10-06")
    ),
    drug_exposure = dplyr::tibble(
      drug_exposure_id = 1,
      person_id = 1,
      drug_concept_id = 101,
      drug_exposure_start_date = as.Date("2020-08-06"),
      drug_exposure_end_date = as.Date("2020-12-06")
    )
  )
  res <- largeScaleCharacterization(
    cdm = cdm,
    targetCohortName = "person",
    targetCohortId = 1,
    tablesToCharacterize = c("condition_occurrence", "drug_exposure"),
    overlap = FALSE,
    summarise = TRUE,
    minimumCellCount = 0
  )
  expect_true(nrow(res$characterization) == 2)
  res <- largeScaleCharacterization(
    cdm = cdm,
    targetCohortName = "person",
    targetCohortId = 1,
    tablesToCharacterize = c("condition_occurrence", "drug_exposure"),
    overlap = TRUE,
    summarise = TRUE,
    minimumCellCount = 0
  )
  expect_true(nrow(res$characterization) == 2)

  # drug exposure not observed
  cdm <- mockDrugUtilisation(
    observation_period = dplyr::tibble(
      observation_period_id = 1,
      person_id = 1,
      observation_period_start_date = as.Date("2010-05-06"),
      observation_period_end_date = as.Date("2020-07-01")
    ),
    person = dplyr::tibble(
      cohort_definition_id = 1,
      subject_id = 1,
      cohort_start_date = as.Date("2019-01-01"),
      cohort_end_date = as.Date("2021-01-01")
    ),
    condition_occurrence = dplyr::tibble(
      condition_occurrence_id = 1,
      person_id = 1,
      condition_concept_id = 1,
      condition_start_date = as.Date("2020-05-06"),
      condition_end_date = as.Date("2020-10-06")
    ),
    drug_exposure = dplyr::tibble(
      drug_exposure_id = 1,
      person_id = 1,
      drug_concept_id = 101,
      drug_exposure_start_date = as.Date("2020-08-06"),
      drug_exposure_end_date = as.Date("2020-12-06")
    )
  )
  res <- largeScaleCharacterization(
    cdm = cdm,
    targetCohortName = "person",
    targetCohortId = 1,
    tablesToCharacterize = c("condition_occurrence", "drug_exposure"),
    overlap = FALSE,
    summarise = TRUE,
    minimumCellCount = 0
  )
  expect_true(nrow(res$characterization) == 1)
  res <- largeScaleCharacterization(
    cdm = cdm,
    targetCohortName = "person",
    targetCohortId = 1,
    tablesToCharacterize = c("condition_occurrence", "drug_exposure"),
    overlap = TRUE,
    summarise = TRUE,
    minimumCellCount = 0
  )
  expect_true(nrow(res$characterization) == 1)

  # drug exposure not observed
  cdm <- mockDrugUtilisation(
    observation_period = dplyr::tibble(
      observation_period_id = 1,
      person_id = 1,
      observation_period_start_date = as.Date("2010-05-06"),
      observation_period_end_date = as.Date("2020-05-01")
    ),
    person = dplyr::tibble(
      cohort_definition_id = 1,
      subject_id = 1,
      cohort_start_date = as.Date("2019-01-01"),
      cohort_end_date = as.Date("2021-01-01")
    ),
    condition_occurrence = dplyr::tibble(
      condition_occurrence_id = 1,
      person_id = 1,
      condition_concept_id = 1,
      condition_start_date = as.Date("2020-05-06"),
      condition_end_date = as.Date("2020-10-06")
    ),
    drug_exposure = dplyr::tibble(
      drug_exposure_id = 1,
      person_id = 1,
      drug_concept_id = 101,
      drug_exposure_start_date = as.Date("2020-08-06"),
      drug_exposure_end_date = as.Date("2020-12-06")
    )
  )
  res <- largeScaleCharacterization(
    cdm = cdm,
    targetCohortName = "person",
    targetCohortId = 1,
    tablesToCharacterize = c("condition_occurrence", "drug_exposure"),
    overlap = FALSE,
    summarise = TRUE,
    minimumCellCount = 0
  )
  expect_true(nrow(res$characterization) == 0)

  # go backward it is not observed if overlap = FALSE, but it is observed if
  # overlap = TRUE
  cdm <- mockDrugUtilisation(
    observation_period = dplyr::tibble(
      observation_period_id = 1,
      person_id = 1,
      observation_period_start_date = as.Date("2010-10-06"),
      observation_period_end_date = as.Date("2022-05-01")
    ),
    person = dplyr::tibble(
      cohort_definition_id = 1,
      subject_id = 1,
      cohort_start_date = as.Date("2020-01-01"),
      cohort_end_date = as.Date("2021-01-01")
    ),
    condition_occurrence = dplyr::tibble(
      condition_occurrence_id = 1,
      person_id = 1,
      condition_concept_id = 101,
      condition_start_date = as.Date("2010-08-06"),
      condition_end_date = as.Date("2010-12-06")
    )
  )
  res <- largeScaleCharacterization(
    cdm = cdm,
    targetCohortName = "person",
    targetCohortId = 1,
    tablesToCharacterize = c("condition_occurrence"),
    overlap = FALSE,
    summarise = TRUE,
    minimumCellCount = 0
  )
  expect_true(nrow(res$characterization) == 0)
  res <- largeScaleCharacterization(
    cdm = cdm,
    targetCohortName = "person",
    targetCohortId = 1,
    tablesToCharacterize = c("condition_occurrence"),
    overlap = TRUE,
    summarise = TRUE,
    minimumCellCount = 0
  )
  expect_true(nrow(res$characterization) == 1)
})

# different windows of observation
test_that("check denominator values", {
  cdm <- mockDrugUtilisation(
    observation_period = dplyr::tibble(
      observation_period_id = 1:10,
      person_id = 1:10,
      observation_period_start_date = as.Date(c(
        "2010-01-01", "2010-01-01", "2010-01-01", "2010-01-01", "2010-01-01",
        "2025-01-01", "2026-10-03", "2027-12-02", "2029-01-01", "2010-01-01"
      )),
      observation_period_end_date = as.Date(c(
        "2031-01-01", "2023-01-01", "2023-04-01", "2024-01-31", "2025-01-01",
        "2026-01-01", "2027-01-01", "2028-01-01", "2029-01-01", "2030-01-01"
      ))
    ),
    person = dplyr::tibble(
      cohort_definition_id = 1,
      subject_id = c(1:9, 11),
      cohort_start_date = as.Date(c(
        "2021-01-01", "2022-01-01", "2023-01-01", "2024-01-01", "2025-01-01",
        "2026-01-01", "2027-01-01", "2028-01-01", "2029-01-01", "2030-01-01"
      )),
      cohort_end_date = as.Date(c(
        "2021-01-01", "2022-01-01", "2023-01-01", "2024-01-01", "2025-01-01",
        "2026-01-01", "2027-01-01", "2028-01-01", "2029-01-01", "2030-01-01"
      ))
    ),
    condition_occurrence = dplyr::tibble(
      condition_occurrence_id = 1,
      person_id = 1,
      condition_concept_id = 1,
      condition_start_date = as.Date("2020-05-06"),
      condition_end_date = as.Date("2020-10-06")
    )
  )
  res <- largeScaleCharacterization(
    cdm = cdm,
    targetCohortName = "person",
    targetCohortId = 1,
    tablesToCharacterize = c("condition_occurrence"),
    overlap = FALSE,
    summarise = FALSE,
    minimumCellCount = 0
  )
  den <- res$denominator %>% dplyr::collect()
  expect_true(nrow(den[den$person_id == 1, ]) == 11)
  expect_true(nrow(den[den$person_id == 2, ]) == 10)
  expect_true(nrow(den[den$person_id == 3, ]) == 9)
  expect_true(nrow(den[den$person_id == 4, ]) == 8)
  expect_true(nrow(den[den$person_id == 5, ]) == 6)
  expect_true(nrow(den[den$person_id == 6, ]) == 5)
  expect_true(nrow(den[den$person_id == 7, ]) == 4)
  expect_true(nrow(den[den$person_id == 8, ]) == 3)
  expect_true(nrow(den[den$person_id == 9, ]) == 1)
  expect_true(nrow(den[den$person_id == 10, ]) == 0)
  expect_true(nrow(den[den$person_id == 11, ]) == 0)
  expect_true(all(den$window_id[den$person_id == 1] %in% 1:11))
  expect_true(all(den$window_id[den$person_id == 2] %in% 1:10))
  expect_true(all(den$window_id[den$person_id == 3] %in% 1:9))
  expect_true(all(den$window_id[den$person_id == 4] %in% 1:8))
  expect_true(all(den$window_id[den$person_id == 5] %in% 1:6))
  expect_true(all(den$window_id[den$person_id == 6] %in% 2:6))
  expect_true(all(den$window_id[den$person_id == 7] %in% 3:6))
  expect_true(all(den$window_id[den$person_id == 8] %in% 4:6))
  expect_true(den$window_id[den$person_id == 9] == 6)
})

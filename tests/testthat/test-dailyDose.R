test_that("functionality of addDailyDose function",{
  skip_on_cran()
  drug_strength <- dplyr::tibble(
    drug_concept_id = c(
      2905077, 1516983, 2905075, 1503327, 1516978, 1503326, 1503328, 1516980,
      29050773, 1125360, 15033297, 15030327, 15033427, 15036327, 15394662,
      43135274, 11253605, 431352774, 431359274, 112530, 1539465, 29050772,
      431352074, 15394062, 43135277, 15033327, 11253603, 15516980, 5034327,
      1539462, 15033528, 15394636, 15176980, 1539463, 431395274, 15186980,
      15316978
    ),
    ingredient_concept_id = c(rep(1,37)),
    amount_value = c(100,200,300,400,500,600,700,rep(NA,30)),
    amount_unit_concept_id = c(
      8718, 9655, 8576, 44819154, 9551, 8587, 9573, rep(NA,30)
    ),
    numerator_value = c(
      rep(NA,7), 1, 300, 5, 10, 13, 20, 3, 5, 2, 1, 1, 4, 11, 270, 130, 32, 34,
      40, 42, 15, 100, 105, 25, 44, 7, 3, 8, 12, 1, 31
    ),
    denominator_unit_concept_id = c(
      rep(NA,7), 8576, 8587, 8505, 8505, 8587, 8587, 45744809, 8519, 8587, 8576,
      8576, 8587, 8576, 8587, 8576, 8587, 8587, 8505, 8587, 8576, 8587,
      45744809, 8505, 8519, 8576, 8587, 8576, 8587, 8576, 8587
    ),
    denominator_value = c(
      rep(NA,7), 241, 30, 23, 410, 143, 2, 43, 15, 21, 1, 11, 42, 151, 20,
      rep(NA,16)
    ),
    numerator_unit_concept_id = c(
      rep(NA,7), 8718, 8718, 9655, 8576, 44819154, 9551, 8576, 8576, 8576, 8576,
      8587, 8587, 9573, 9573, 8718, 8718, 9439, 9655, 44819154, 9551, 9551,
      8576, 8576, 8576, 8576, 8576, 8587, 8587, 9573, 9573
    )
  )
  conceptsToAdd <- dplyr::tibble(
    concept_id = 1, concept_name = "ingredient 1", domain_id = "Drug",
    vocabulary_id = "RxNorm", concept_class_id = "Ingredient",
    standard_concept = "S"
  ) %>%
    dplyr::bind_rows(
      dplyr::tibble(
        concept_id =  c(
          2905077, 1516983, 2905075, 1503327, 1516978, 1503326, 1503328, 1516980,
          29050773, 1125360, 15033297, 15030327, 15033427, 15036327, 15394662,
          43135274, 11253605, 431352774, 431359274, 112530, 1539465, 29050772,
          431352074, 15394062, 43135277, 15033327,  11253603, 15516980, 5034327,
          1539462, 15033528, 15394636, 15176980, 1539463, 431395274, 15186980,
          15316978
        ), concept_name = "NA", domain_id = "Drug", vocabulary_id = "RxNorm",
        concept_class_id = "Clinical Drug", standard_concept = "S"
      ) %>%
        dplyr::mutate(concept_name = paste0("drug", concept_id))
    )
  concept <- mockConcept %>%
    dplyr::anti_join(conceptsToAdd, by = "concept_id") %>%
    dplyr::bind_rows(conceptsToAdd)
  concept_ancestor <- mockConceptAncestor %>%
    dplyr::bind_rows(dplyr::tibble(
      ancestor_concept_id = 1,
      descendant_concept_id = conceptsToAdd$concept_id,
      min_levels_of_separation = 0,
      max_levels_of_separation = 0
    ))

  cdm <- mockDrugUtilisation(
    connectionDetails,
    seed = 11,
    drug_strength = drug_strength,
    concept = concept,
    numberIndividuals = 50,
    concept_ancestor = concept_ancestor
  )

  # should only add patterns 1 to 9, which are drugs 1:7, 10, 11, 25, 30
  daily_dose <- addDailyDose(cdm$drug_exposure, cdm, 1)

  expect_true(
    daily_dose %>% dplyr::tally() %>% dplyr::pull("n") ==
      cdm$drug_exposure %>% dplyr::tally() %>% dplyr::pull("n")
  )
  expect_true(
    length(colnames(cdm$drug_exposure)) + 2 == length(colnames(daily_dose))
  )
  expect_true(all(colnames(cdm$drug_exposure) %in% colnames(daily_dose)))
  expect_true(all(c("daily_dose", "unit") %in% colnames(daily_dose)))

  # expect_true(all(
  #   daily_dose %>%
  #     dplyr::filter(!is.na(daily_dose)) %>%
  #     dplyr::select(drug_concept_id) %>%
  #     dplyr::arrange(drug_concept_id) %>%
  #     dplyr::pull() ==
  #     c(1125360, 1516978, 1539462, 2905077)
  # ))

  patterns <- cdm$drug_exposure %>%
    addPattern(cdm, 1)
  expect_true(
    cdm$drug_exposure %>% dplyr::tally() %>% dplyr::pull() ==
      patterns %>% dplyr::tally() %>% dplyr::pull()
  )
  expect_true(length(colnames(cdm$drug_exposure)) + 2 == length(colnames(patterns)))
  expect_true(all(colnames(cdm$drug_exposure) %in% colnames(patterns)))
  expect_true(all(c("pattern_id", "unit") %in% colnames(patterns)))

  x <- daily_dose %>%
    dplyr::select("drug_exposure_id", "daily_dose", "unit") %>%
    dplyr::inner_join(
      patterns %>%
        dplyr::select("drug_exposure_id", "pattern_id", "unit"),
      by = "drug_exposure_id"
    ) %>%
    dplyr::collect()

  expect_true(all(colnames(x) %in% c(
    "drug_exposure_id", "daily_dose", "unit.x", "pattern_id", "unit.y"
  )))
  expect_true(all(
    x %>%
      dplyr::filter(is.na(.data$unit.x)) %>%
      dplyr::arrange(.data$drug_exposure_id) %>%
      dplyr::pull("drug_exposure_id") ==
      x %>%
      dplyr::filter(is.na(.data$unit.y)) %>%
      dplyr::arrange(.data$drug_exposure_id) %>%
      dplyr::pull("drug_exposure_id")
  ))
  expect_true(all(
    x %>%
      dplyr::filter(!is.na(.data$unit.x)) %>%
      dplyr::arrange(.data$drug_exposure_id) %>%
      dplyr::pull("unit.x") ==
      x %>%
      dplyr::filter(!is.na(.data$unit.y)) %>%
      dplyr::arrange(.data$drug_exposure_id) %>%
      dplyr::pull("unit.y")
  ))
  expect_true(all(
    x %>%
      dplyr::filter(is.na(.data$daily_dose)) %>%
      dplyr::arrange(.data$drug_exposure_id) %>%
      dplyr::pull("drug_exposure_id") ==
      x %>%
      dplyr::filter(is.na(.data$pattern_id)) %>%
      dplyr::arrange(.data$drug_exposure_id) %>%
      dplyr::pull("drug_exposure_id")
  ))

  coverage <- dailyDoseCoverage(cdm, 1)

  expect_true(all(colnames(coverage) %in% c(
    "concept_set", "drug_concept_id", "number_records", "number_dose",
    "coverage"
  )))

})


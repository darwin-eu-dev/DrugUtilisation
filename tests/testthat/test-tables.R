test_that("tableIndication works", {
  targetCohortName <- dplyr::tibble(
    cohort_definition_id = c(1, 1, 1, 2),
    subject_id = c(1, 1, 2, 3),
    cohort_start_date = as.Date(c(
      "2020-01-01", "2020-06-01", "2020-01-02", "2020-01-01"
    )),
    cohort_end_date = as.Date(c(
      "2020-04-01", "2020-08-01", "2020-02-02", "2020-03-01"
    ))
  )
  indicationCohortName <- dplyr::tibble(
    cohort_definition_id = c(1, 1, 2, 1),
    subject_id = c(1, 3, 1, 1),
    cohort_start_date = as.Date(c(
      "2019-12-30", "2020-01-01", "2020-05-25", "2020-05-25"
    )),
    cohort_end_date = as.Date(c(
      "2019-12-30", "2020-01-01", "2020-05-25", "2020-05-25"
    ))
  )
  attr(indicationCohortName, "cohort_set") <- dplyr::tibble(
    cohort_definition_id = c(1, 2),
    cohort_name = c("asthma", "covid")
  )
  condition_occurrence <- dplyr::tibble(
    person_id = 1,
    condition_start_date = as.Date("2020-05-31"),
    condition_end_date = as.Date("2020-05-31"),
    condition_occurrence_id = 1,
    condition_concept_id = 0,
    condition_type_concept_id = 0
  )
  observationPeriod <- dplyr::tibble(
    observation_period_id = c(1, 2, 3),
    person_id = c(1, 2, 3),
    observation_period_start_date = as.Date(c(
      "2015-01-01", "2016-05-15", "2012-12-30"
    )),
    observation_period_end_date = as.Date(c(
      "2025-01-01", "2026-05-15", "2030-12-30"
    )),
    period_type_concept_id = 44814724
  )
  cdm <-
    mockDrugUtilisation(
      connectionDetails,
      cohort1 = targetCohortName,
      cohort2 = indicationCohortName,
      condition_occurrence = condition_occurrence,
      observation_period = observationPeriod
    )

  res <- cdm[["cohort1"]] %>%
    addIndication(
      indicationCohortName = "cohort2", indicationWindow = list(c(0,0),c(-7,0),c(-30,0),c(-Inf,0)),
      unknownIndicationTable = "condition_occurrence"
    )

  # result <- summariseIndication(res)
  #
  # # default
  # default <- tableIndication(result)
  # expect_true("gt_tbl" %in% class(default))
  # expect_true(all(colnames(default$`_data`) == c(
  #   'Database name', 'Variable name', 'Indication', '[header]Cohort name\n[header_level]Cohort 1', '[header]Cohort name\n[header_level]Cohort 2'
  # )))
  # expect_true(all(default$`_data`$`Database name` == c(
  #   'DUS MOCK', '', '', '', 'DUS MOCK', '', '', '', 'DUS MOCK', '', '', '', 'DUS MOCK', '', '', ''
  # )))
  #
  # tib <- tableIndication(result, header = "variable", groupColumn = "cdm_name", type = "tibble")
  # expect_true(nrow(tib) == 2)
  # expect_true(all(c(
  #   'Database name', 'Cohort name', '[header_level]Indication on index date\n[header_level]Asthma',
  #   '[header_level]Indication on index date\n[header_level]Covid', '[header_level]Indication on index date\n[header_level]None',
  #   '[header_level]Indication on index date\n[header_level]Unknown', '[header_level]Indication during prior 7 days\n[header_level]Covid',
  #   '[header_level]Indication during prior 7 days\n[header_level]Asthma', '[header_level]Indication during prior 7 days\n[header_level]None',
  #   '[header_level]Indication during prior 7 days\n[header_level]Unknown', '[header_level]Indication during prior 30 days\n[header_level]Asthma',
  #   '[header_level]Indication during prior 30 days\n[header_level]Covid', '[header_level]Indication during prior 30 days\n[header_level]None',
  #   '[header_level]Indication during prior 30 days\n[header_level]Unknown', '[header_level]Indication any time prior\n[header_level]Asthma',
  #   '[header_level]Indication any time prior\n[header_level]Covid', '[header_level]Indication any time prior\n[header_level]None',
  #   '[header_level]Indication any time prior\n[header_level]Unknown'
  # ) %in% colnames(tib)))
  #
  # # strata
  # res <- res %>%
  #   PatientProfiles::addAge(
  #     ageGroup = list("<40" = c(0, 39), ">=40" = c(40, 150))
  #   ) %>%
  #   PatientProfiles::addSex()
  #
  # result <- summariseIndication(
  #   res, strata = list("age_group", "sex", c("age_group", "sex"))
  # )
  #
  # fx <- tableIndication(result, cdmName = FALSE, cohortName = FALSE, type = "flextable")
  # expect_true("flextable" %in% class(fx))
  # expect_true(all(colnames(fx$body$dataset) == c(
  #   'Variable name', 'Age group', 'Sex', 'Indication', 'Estimate value'
  # )))
  # expect_true(all(fx$body$dataset$`Variable name` |> levels() == c(
  #   "Indication any time prior", "Indication during prior 30 days", "Indication during prior 7 days", "Indication on index date"
  # )))
  #
  # # expected errors
  # expect_error(tableIndication(result, header = "variable"))
  # expect_error(tableIndication(result, groupColumn = "cdm_name", cdmName = FALSE))
})


test_that("tableDoseCoverage", {
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
    ),
    valid_start_date = as.Date("1900-01-01"),
    valid_end_date = as.Date("2100-01-01")
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

  concept_relationship <- dplyr::tibble(
    concept_id_1 = c(2905077, 1516983, 2905075, 1503327, 1516978, 1503326, 1503328, 1516980,
                     29050773, 1125360, 15033297, 15030327, 15033427, 15036327, 15394662,
                     43135274, 11253605, 431352774, 431359274, 112530, 1539465, 29050772,
                     431352074, 15394062, 43135277, 15033327, 11253603, 15516980, 5034327,
                     1539462, 15033528, 15394636, 15176980, 1539463, 431395274, 15186980,
                     15316978),
    concept_id_2 = c(19016586, 46275062, 35894935, 19135843, 19082107, 19011932, 19082108,
                     2008660,  2008661,  2008662, 19082109, 43126087, 19130307, 42629089,
                     19103220, 19082048, 19082049, 19082256, 19082050, 19082071, 19082072,
                     19135438, 19135446, 19135439, 19135440, 46234466, 19082653, 19057400,
                     19082227, 19082286, 19009068, 19082628, 19082224, 19095972, 19095973,
                     35604394, 702776 ),
    relationship_id = c(rep("RxNorm has dose form", 37)),
    valid_start_date = as.Date("1900-01-01"),
    valid_end_date = as.Date("2100-01-01")
  )

  cdm <- mockDrugUtilisation(
    connectionDetails,
    seed = 11,
    drug_strength = drug_strength,
    concept = concept,
    numberIndividuals = 50,
    concept_ancestor = concept_ancestor,
    concept_relationship = concept_relationship
  )

  coverage <- summariseDoseCoverage(cdm, 1)

  # default
  default <- tableDoseCoverage(coverage)
  expect_true("gt_tbl" %in% class(default))
  expect_true(all(colnames(default$`_data`) == c(
    'Database name', 'Ingredient name', 'Unit', 'Route', 'Pattern id',
    '[header_level]Number records\n[header_level]N', '[header_level]Missing dose\n[header_level]N (%)',
    '[header_level]Daily dose\n[header_level]Mean (SD)', '[header_level]Daily dose\n[header_level]Median (Q25 - Q75)'
  )))

  # other options working
  tib1 <- tableDoseCoverage(coverage, type = "tibble", ingridientName = FALSE, splitStrata = FALSE)
  expect_true(all(colnames(tib1) == c(
    'Database name', 'Strata name', 'Strata level', '[header_level]Number records\n[header_level]N',
    '[header_level]Missing dose\n[header_level]N (%)', '[header_level]Daily dose\n[header_level]Mean (SD)',
    '[header_level]Daily dose\n[header_level]Median (Q25 - Q75)'
  )))

  fx1 <- tableDoseCoverage(coverage, header = c("cdm_name", "group"), groupColumn = "variable_name", type = "flextable")
  expect_true("flextable" %in% class(fx1))
  expect_true(all(colnames(fx1$body$dataset) == c(
    'Variable name', 'Unit', 'Route', 'Pattern id', 'Estimate name', 'Database name\nDUS MOCK\nIngredient name\nIngredient 1'
  )))
  expect_true(all(fx1$body$dataset$`Variable name` |> levels() == c("Daily dose", "Missing dose", "Number records")))

  gt1 <- tableDoseCoverage(coverage, header = c("group"))
  expect_true(all(colnames(gt1$`_data`) == c(
    'Database name', 'Unit', 'Route', 'Pattern id', 'Variable', 'Estimate name', '[header]Ingredient name\n[header_level]Ingredient 1'
  )))

  # expected errors
  expect_error(tableDoseCoverage(coverage, header = "variable", groupColumn = "variable_name"))
  expect_error(tableDoseCoverage(coverage, groupColumn = "cdm_name", cdmName = FALSE))
  expect_error(tableDoseCoverage(coverage, header = "hi"))
})

test_that("check mock patterns", {
  cdm <- mockDrugUtilisation(connectionDetails)

  expect_no_error(patterns <- patternTable(cdm))
  expect_true(all(colnames(patterns) == c(
    "pattern_file_id", "pattern_id", "formula_id", "validity", "number_concepts",
    "number_ingredients", "number_records", "amount_numeric",
    "amount_unit_concept_id", "numerator_numeric", "numerator_unit_concept_id",
    "denominator_numeric", "denominator_unit_concept_id"
  )))
})

test_that("create patterns, correct output", {
 drug_strength <- dplyr::tibble(
   drug_concept_id = c(1, 1, 2),
   ingredient_concept_id = c(13, 14, 15),
   amount_value = c(3, NA, NA),
   amount_unit_concept_id = c(8718, NA, NA),
   numerator_value = c(NA, 20, 10),
   numerator_unit_concept_id = c(NA, 7, 8576),
   denominator_value = c(NA, 100, 100),
   denominator_unit_concept_id = c(NA, 10, 45744809)
 )
 concept <- dplyr::tibble(
   concept_name = c(
     "Drug1", "Drug2", "Ing1", "Ing2", "Ing3", "AmountUnit", "NumUnit1",
     "NumUnit2", "DenUnit", "DenUnit2", "international unit", "milligram",
     "Actuation"
   ),
   concept_class_id = c(
     rep("Branded drug", 2), rep("Ingredient", 3), rep("Unit", 8)
   ),
   domain_id = c(rep("Drug", 5), rep("Unit", 8)),
   concept_id = c(1, 2, 13, 14, 15, 4, 7, 8, 10, 11, 8718, 8576, 45744809)
 )

 cdm <- mockDrugUtilisation(
   connectionDetails, drug_strength = drug_strength, concept = concept
 )

 expect_no_error(patternTab <- patternTable(cdm))
 expect_true(all(colnames(patternTab) == c(
   "pattern_file_id", "pattern_id", "formula_id", "validity", "number_concepts",
   "number_ingredients", "number_records", "amount_numeric",
   "amount_unit_concept_id", "numerator_numeric", "numerator_unit_concept_id",
   "denominator_numeric", "denominator_unit_concept_id"
 )))

 expect_true(nrow(patternTab) == 3)
 expect_true(all(patternTab %>% dplyr::select(number_concepts) %>% dplyr::pull() == c(1,1,1)))
 patValid <- patternTab %>% dplyr::filter(.data$validity  == "pattern with formula")
 patNotValid <- patternTab %>% dplyr::anti_join(patValid, by = colnames(patValid))
 expect_true(nrow(patValid) == 2)
 expect_true(nrow(patNotValid) == 1)

})

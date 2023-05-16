test_that("create patterns, correct output", {
 drug_strength <- tibble::tibble(
   drug_concept_id = c(1,1,2),
   ingredient_concept_id = c(13,14,15),
   amount_value = c(3,NA,NA),
   amount_unit_concept_id = c(8718,NA,NA),
   numerator_value = c(NA,20,10),
   numerator_unit_concept_id = c(NA,7,8576),
   denominator_value = c(NA,100,100),
   denominator_unit_concept_id = c(NA,10,45744809)
 )

 concept <- tibble::tibble(
   concept_name = c("Drug1", "Drug2", "Ing1", "Ing2", "Ing3", "AmountUnit", "NumUnit1", "NumUnit2","DenUnit", "DenUnit2","international unit", "milligram", "Actuation"),
   concept_id = c(1,2,13,14,15,4,7,8,10,11,8718,8576,45744809)
 )

 cdm <- mockDrugUtilisation(drug_strength = drug_strength)
 cdm[["concept"]] <- concept

 newpattern <- createPatternsTable(cdm)

 expect_true(all(c("pattern_id", "amount", "amount_unit", "amount_unit_concept_id", "numerator", "numerator_unit",
                   "denominator", "denominator_unit", "numerator_unit_concept_id", "denominator_unit_concept_id",
                   "number_concepts", "number_ingredients", "valid") %in% colnames(newpattern)))
 expect_true(nrow(newpattern) == 3)
 expect_true(all(newpattern %>% dplyr::select(valid) %>% dplyr::pull() == c(FALSE, FALSE, TRUE)))
 expect_true(all(newpattern %>% dplyr::select(number_concepts) %>% dplyr::pull() == c(1,1,1)))
 DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)

})

test_that("create patterns, expected errors", {

  drug_strength <- tibble::tibble(
    drug_concept_id = c(1,1,2),
    ingredient_concept_id = c(13,14,15),
    amount_value = c(3,NA,NA),
    amount_unit_concept_id = c(8718,NA,NA),
    numerator_value = c(NA,20,10),
    numerator_unit_concept_id = c(NA,7,8576),
    denominator_value = c(NA,100,100),
    denominator_unit_concept_id = c(NA,10,45744809)
  )

  cdm <- mockDrugUtilisation(drug_strength = drug_strength)

  expect_error(createPatternsTable("cdm"))
  expect_error(createPatternsTable(cdm))
  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)

})

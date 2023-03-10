test_that("create patterns, correct output", {
 drug_strength <- tibble::tibble(
   drug_concept_id = c(1,1,2),
   ingredient_concept_id = c(13,14,15),
   amount_value = c(3,NA,NA),
   amount_unit_concept_id = c(4,NA,NA),
   numerator_value = c(NA,20,10),
   numerator_unit_concept_id = c(NA,7,8),
   denominator_value = c(NA,100,100),
   denominator_unit_concept_id = c(NA,10,11)
 )

 concept <- tibble::tibble(
   concept_name = c("Drug1", "Drug2", "Ing1", "Ing2", "Ing3", "AmountUnit", "NumUnit1", "NumUnit2","DenUnit", "DenUnit2"),
   concept_id = c(1,2,13,14,15,4,7,8,10,11)
 )



})

test_that("create patterns, expected errors", {

})

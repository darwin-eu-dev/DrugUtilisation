test_that("test functionality",{
  drug_strength <- dplyr::tibble(
    drug_concept_id = c(1:37),
    ingredient_concept_id = c(rep(1,37)),
    amount_value = c(-100,200,300,400,500,600,700,rep(NA,30)),
    amount = c(rep("numeric",7),rep(NA,30)),
    amount_unit_concept_id = c(8718, 9655, 8576, 44819154, 9551, 8587, 9573, rep(NA,30)),
    numerator_value = c(rep(NA,7),1,300,5,10,13,20,3,5,2,1,1,4,11,270,130,32,34,40,42,15,100,105,25,44,7,3,8,12,1,31),
    numerator = c(rep(NA,7),rep("numeric",30)),
    denominator_unit_concept_id = c(rep(NA,7),8576, 8587, 8505,8505,8587,8587,45744809,8519,8587,8576,8576,8587,8576,8587,8576,8587,8587,8505,8587,
                                    8576,8587,45744809,8505,8519,8576,8587,8576,8587,8576,8587),
    denominator_value = c(rep(NA,7),241,30,23,410,143,2,43,15,21,1,11,42,151,20,rep(NA,16)),
    denominator = c(rep(NA,7),rep("numeric",14),rep(NA,16)),
    numerator_unit_concept_id = c(rep(NA,7),8718,8718,9655,8576,44819154,9551,8576,8576,8576,8576,8587,8587,9573,9573,8718,8718,9439,9655,44819154,
                                  9551,9551,8576,8576,8576,8576,8576,8587,8587,9573,9573)
  )

  # pattern 1 also NA because of numerator < 0, pattern 3 NA because of quantity < 0, pattern 2 calculation changes
  drug_exposure <- dplyr::tibble(
    drug_exposure_id = c(1,2,3,4),
    person_id = c(1,1,2,3),
    drug_concept_id = c(1,2,15,3),
    drug_exposure_start_date = c(as.Date("2018-11-02"), as.Date("2010-04-04"), as.Date("2014-02-18"), as.Date("2014-01-07")),
    drug_exposure_end_date = c(as.Date("2018-11-09"), as.Date("2010-05-02"), as.Date("2014-02-25"), as.Date("2014-01-06")),
    quantity = c(8,21,8,8)
  )

  cdm <- mockDrugUtilisation(connectionDetails,
                             drug_strength = drug_strength,
                             drug_exposure = drug_exposure)

  concepts <- list(
    concept_1 = c(1,30,4),
    concept_2 = c(300, 15),
    concept_3 = c(2)
  )

  result <- dailyDoseCoverage(cdm, sample = 10, ingredient = 1, conceptList = concepts)

  expect_true(result - 33.33333 < 0.001)

  result <- dailyDoseCoverage(cdm, sample = 5, ingredient = 1, conceptList = concepts)

  expect_true(result == 0)

  concepts <- readConceptList(
    cdm = cdm, path =  system.file(package = "DrugUtilisation", "concepts")
  )

  result <- dailyDoseCoverage(cdm, sample = 10, ingredient = 1, conceptList = concepts)

  expect_true(result == 0)

  result <- dailyDoseCoverage(cdm, sample = 10, ingredient = 1)

  expect_true(result - 33.33333 < 0.001)

})

test_that("inputs for dailyDoseCoverage",{
  drug_strength <- dplyr::tibble(
    drug_concept_id = c(1:37),
    ingredient_concept_id = c(rep(1,37)),
    amount_value = c(100,200,300,400,500,600,700,rep(NA,30)),
    amount_unit_concept_id = c(8718, 9655, 8576, 44819154, 9551, 8587, 9573, rep(NA,30)),
    numerator_value = c(rep(NA,7),1,300,5,10,13,20,3,5,2,1,1,4,11,270,130,32,34,40,42,15,100,105,25,44,7,3,8,12,1,31),
    denominator_unit_concept_id = c(rep(NA,7),8576, 8587, 8505,8505,8587,8587,45744809,8519,8587,8576,8576,8587,8576,8587,8576,8587,8587,8505,8587,
                                    8576,8587,45744809,8505,8519,8576,8587,8576,8587,8576,8587),
    denominator_value = c(rep(NA,7),241,30,53,410,143,2,43,15,21,1,11,42,151,20,rep(NA,16)),
    numerator_unit_concept_id = c(rep(NA,7),8718,8718,9655,8576,44819154,9551,8576,8576,8576,8576,8587,8587,9573,9573,8718,8718,9439,9655,44819154,
                                  9551,9551,8576,8576,8576,8576,8576,8587,8587,9573,9573)
  )

  cdm <- mockDrugUtilisation(connectionDetails,
                             drug_strength = drug_strength,
                             numberIndividuals = 50)

  expect_error(dailyDoseCoverage())
  expect_error(dailyDoseCoverage(cdm = "cdm", ingredient = 2, sample = 10))
  expect_error(dailyDoseCoverage(cdm = cdm, ingredient = "alcohol", sample = 1))
  expect_error(dailyDoseCoverage(cdm = cdm, ingredient = 2, sample = "exposure"))
  expect_error(dailyDoseCoverage(cdm = cdm, ingredient = "alcohol", sample = c(4,5)))

})

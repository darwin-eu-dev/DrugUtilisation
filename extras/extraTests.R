# test_that("test for negative quantity or daily dose",{
#   drug_strength <- dplyr::tibble(
#     drug_concept_id = c(1:37),
#     ingredient_concept_id = c(rep(1,37)),
#     amount_value = c(-100,200,300,400,500,600,700,rep(NA,30)),
#     amount = c(rep("numeric",7),rep(NA,30)),
#     amount_unit_concept_id = c(8718, 9655, 8576, 44819154, 9551, 8587, 9573, rep(NA,30)),
#     numerator_value = c(rep(NA,7),1,300,5,10,13,20,3,5,2,1,1,4,11,270,130,32,34,40,42,15,100,105,25,44,7,3,8,12,1,31),
#     numerator = c(rep(NA,7),rep("numeric",30)),
#     denominator_unit_concept_id = c(rep(NA,7),8576, 8587, 8505,8505,8587,8587,45744809,8519,8587,8576,8576,8587,8576,8587,8576,8587,8587,8505,8587,
#                                     8576,8587,45744809,8505,8519,8576,8587,8576,8587,8576,8587),
#     denominator_value = c(rep(NA,7),241,30,23,410,143,2,43,15,21,1,11,42,151,20,rep(NA,16)),
#     denominator = c(rep(NA,7),rep("numeric",14),rep(NA,16)),
#     numerator_unit_concept_id = c(rep(NA,7),8718,8718,9655,8576,44819154,9551,8576,8576,8576,8576,8587,8587,9573,9573,8718,8718,9439,9655,44819154,
#                                   9551,9551,8576,8576,8576,8576,8576,8587,8587,9573,9573)
#   )
#
#   # pattern 1 also NA because of numerator < 0, pattern 3 NA because of quantity < 0, pattern 2 calculation changes
#   drug_exposure <- dplyr::tibble(
#     drug_exposure_id = c(1,2,3,4),
#     person_id = c(1,1,1,1),
#     drug_concept_id = c(1,2,15,3),
#     drug_exposure_start_date = c(as.Date("2018-11-02"), as.Date("2010-04-04"), as.Date("2014-02-18"), as.Date("2014-01-07")),
#     drug_exposure_end_date = c(as.Date("2018-11-09"), as.Date("2010-05-02"), as.Date("2014-02-25"), as.Date("2014-01-06")),
#     quantity = c(8,21,8,8)
#   )
#
#   cdm <- mockDrugUtilisation(connectionDetails,
#                              drug_strength = drug_strength,
#                              drug_exposure = drug_exposure)
#
#   result <- addDailyDose(cdm[["drug_exposure"]], 1) %>%
#     dplyr::collect() %>% dplyr::arrange(drug_concept_id)
#
#   expect_true(nrow(result) == cdm[["drug_exposure"]] %>% dplyr::tally() %>% dplyr::pull("n"))
#   expect_true(all(result %>% dplyr::filter(!is.na(daily_dose)) %>% dplyr::select(drug_concept_id) %>% unique() %in% c(2)))
#
#   compareNA <- function(v1,v2) {
#     same <- (v1 - v2 < 0.01) | (is.na(v1) & is.na(v2))
#     same[is.na(same)] <- FALSE
#     return(same)
#   }
#
#   # daily dose depends on pattern_id: standarisation and function
#   expect_true(all(compareNA(result %>% dplyr::select(daily_dose) %>%
#                               dplyr::arrange("drug_concept_id") %>% dplyr::pull(),
#                             c(NA, 0.1448, NA, NA))))
#
#   compareNAchar <- function(v1,v2) {
#     same <- (v1 == v2) | (is.na(v1) & is.na(v2))
#     same[is.na(same)] <- FALSE
#     return(same)
#   }
#   expect_true(all(compareNAchar(result %>% dplyr::select(unit) %>%
#                                   dplyr::arrange("drug_concept_id") %>% dplyr::pull(),
#                                 c("international unit", "milligram", "milligram", NA))))
#
# })
#
# test_that("test for different ingredient concept ids",{
#   drug_strength <- dplyr::tibble(
#     drug_concept_id = c(2905077,  1516983,   2905075,  1503327,  1516978,  1503326,   1503328,   1516980,  29050773, 1125360,   15033297,  15030327,
#                         15033427, 15036327,  15394662, 43135274, 11253605, 431352774, 431359274, 112530,   1539465,  29050772,  431352074, 15394062,
#                         43135277, 15033327,  11253603, 15516980, 5034327,  1539462,   15033528,  15394636, 15176980, 1539463,   431395274, 15186980,
#                         15316978),
#     ingredient_concept_id = c(2,1,rep(1,33),2,3),
#     amount_value = c(100,200,300,400,500,600,700,rep(NA,30)),
#     amount = c(rep("numeric",7),rep(NA,30)),
#     amount_unit_concept_id = c(8718, 9655, 8576, 44819154, 9551, 8587, 9573, rep(NA,30)),
#     numerator_value = c(rep(NA,7),1,300,5,10,13,20,3,5,2,1,1,4,11,270,130,32,34,40,42,15,100,105,25,44,7,3,8,12,1,31),
#     numerator = c(rep(NA,7),rep("numeric",30)),
#     denominator_unit_concept_id = c(rep(NA,7),8576, 8587, 8505,8505,8587,8587,45744809,8519,8587,8576,8576,8587,8576,8587,8576,8587,8587,8505,8587,
#                                     8576,8587,45744809,8505,8519,8576,8587,8576,8587,8576,8587),
#     denominator_value = c(rep(NA,7),241,30,23,410,143,2,43,15,21,1,11,42,151,20,rep(NA,16)),
#     denominator = c(rep(NA,7),rep("numeric",14),rep(NA,16)),
#     numerator_unit_concept_id = c(rep(NA,7),8718,8718,9655,8576,44819154,9551,8576,8576,8576,8576,8587,8587,9573,9573,8718,8718,9439,9655,44819154,
#                                   9551,9551,8576,8576,8576,8576,8576,8587,8587,9573,9573)
#   )
#
#   cdm <- mockDrugUtilisation(connectionDetails,
#                              seed = 11,
#                              drug_strength = drug_strength,
#                              numberIndividuals = 50)
#
#   cdm[["drug_exposure"]] <- cdm[["drug_exposure"]] %>%
#     dplyr::group_by(drug_concept_id) %>%
#     dplyr::filter(dplyr::row_number() == 1) %>%
#     dplyr::ungroup()
#
#   result <- addDailyDose(cdm[["drug_exposure"]], 2) %>%
#     dplyr::collect() %>% dplyr::arrange(drug_concept_id)
#
#   expect_true(nrow(result) == cdm[["drug_exposure"]] %>% dplyr::tally() %>% dplyr::pull("n"))
#   expect_true(all(result %>% dplyr::filter(!is.na(daily_dose)) %>% dplyr::select(drug_concept_id) %>% dplyr::pull() %in% c(2905077 )))
#
#   compareNA <- function(v1,v2) {
#     same <- (v1 - v2 < 0.01) | (is.na(v1) & is.na(v2))
#     same[is.na(same)] <- FALSE
#     return(same)
#   }
#
#   expect_true(all(compareNA(result %>% dplyr::select(daily_dose) %>% dplyr::pull(),
#                             c(NA, NA, NA, NA, NA, NA, NA, 5.555, NA))))
#
# })
#
# test_that("inputs for addDailyDose function",{
#   drug_strength <- dplyr::tibble(
#     drug_concept_id = c(1:37),
#     ingredient_concept_id = c(rep(1,37)),
#     amount_value = c(100,200,300,400,500,600,700,rep(NA,30)),
#     amount_unit_concept_id = c(8718, 9655, 8576, 44819154, 9551, 8587, 9573, rep(NA,30)),
#     numerator_value = c(rep(NA,7),1,300,5,10,13,20,3,5,2,1,1,4,11,270,130,32,34,40,42,15,100,105,25,44,7,3,8,12,1,31),
#     denominator_unit_concept_id = c(rep(NA,7),8576, 8587, 8505,8505,8587,8587,45744809,8519,8587,8576,8576,8587,8576,8587,8576,8587,8587,8505,8587,
#                                     8576,8587,45744809,8505,8519,8576,8587,8576,8587,8576,8587),
#     denominator_value = c(rep(NA,7),241,30,53,410,143,2,43,15,21,1,11,42,151,20,rep(NA,16)),
#     numerator_unit_concept_id = c(rep(NA,7),8718,8718,9655,8576,44819154,9551,8576,8576,8576,8576,8587,8587,9573,9573,8718,8718,9439,9655,44819154,
#                                   9551,9551,8576,8576,8576,8576,8576,8587,8587,9573,9573)
#   )
#
#   cdm <- mockDrugUtilisation(connectionDetails,
#                              drug_strength = drug_strength,
#                              numberIndividuals = 50)
#
#   expect_error(result <- addDailyDose())
#   expect_error(result <- addDailyDose())
#   expect_error(addDailyDose(cdm[["drug_exposure"]], "xx"))
#
# })

# test_that("test functionality",{
#   drug_strength <- dplyr::tibble(
#     drug_concept_id = c(1:37),
#     ingredient_concept_id = c(rep(1,37)),
#     amount_value = c(-100,200,300,400,500,600,700,rep(NA,30)),
#     amount = c(rep("numeric",7),rep(NA,30)),
#     amount_unit_concept_id = c(8718, 9655, 8576, 44819154, 9551, 8587, 9573, rep(NA,30)),
#     numerator_value = c(rep(NA,7),1,300,5,10,13,20,3,5,2,1,1,4,11,270,130,32,34,40,42,15,100,105,25,44,7,3,8,12,1,31),
#     numerator = c(rep(NA,7),rep("numeric",30)),
#     denominator_unit_concept_id = c(rep(NA,7),8576, 8587, 8505,8505,8587,8587,45744809,8519,8587,8576,8576,8587,8576,8587,8576,8587,8587,8505,8587,
#                                     8576,8587,45744809,8505,8519,8576,8587,8576,8587,8576,8587),
#     denominator_value = c(rep(NA,7),241,30,23,410,143,2,43,15,21,1,11,42,151,20,rep(NA,16)),
#     denominator = c(rep(NA,7),rep("numeric",14),rep(NA,16)),
#     numerator_unit_concept_id = c(rep(NA,7),8718,8718,9655,8576,44819154,9551,8576,8576,8576,8576,8587,8587,9573,9573,8718,8718,9439,9655,44819154,
#                                   9551,9551,8576,8576,8576,8576,8576,8587,8587,9573,9573)
#   )
#
#   # pattern 1 also NA because of numerator < 0, pattern 3 NA because of quantity < 0, pattern 2 calculation changes
#   drug_exposure <- dplyr::tibble(
#     drug_exposure_id = c(1,2,3,4),
#     person_id = c(1,1,2,3),
#     drug_concept_id = c(1,2,15,3),
#     drug_exposure_start_date = c(as.Date("2018-11-02"), as.Date("2010-04-04"), as.Date("2014-02-18"), as.Date("2014-01-07")),
#     drug_exposure_end_date = c(as.Date("2018-11-09"), as.Date("2010-05-02"), as.Date("2014-02-25"), as.Date("2014-01-06")),
#     quantity = c(8,21,8,8)
#   )
#
#   cdm <- mockDrugUtilisation(connectionDetails,
#                              drug_strength = drug_strength,
#                              drug_exposure = drug_exposure)
#
#   concepts <- list(
#     concept_1 = c(1,30,4),
#     concept_2 = c(300, 15),
#     concept_3 = c(2)
#   )
#
#   result <- dailyDoseCoverage(cdm, sample = 10, ingredient = 1, conceptList = concepts)
#
#   expect_true(result - 33.33333 < 0.001)
#
#   result <- dailyDoseCoverage(cdm, sample = 5, ingredient = 1, conceptList = concepts)
#
#   expect_true(result == 0)
#
#   concepts <- readConceptList(
#     cdm = cdm, path =  system.file(package = "DrugUtilisation", "concepts")
#   )
#
#   result <- dailyDoseCoverage(cdm, sample = 10, ingredient = 1, conceptList = concepts)
#
#   expect_true(is.na(result))
#
#   result <- dailyDoseCoverage(cdm, sample = 10, ingredient = 1)
#
#   expect_true(result - 33.33333 < 0.001)
#
# })
#
# test_that("inputs for dailyDoseCoverage",{
#   drug_strength <- dplyr::tibble(
#     drug_concept_id = c(1:37),
#     ingredient_concept_id = c(rep(1,37)),
#     amount_value = c(100,200,300,400,500,600,700,rep(NA,30)),
#     amount_unit_concept_id = c(8718, 9655, 8576, 44819154, 9551, 8587, 9573, rep(NA,30)),
#     numerator_value = c(rep(NA,7),1,300,5,10,13,20,3,5,2,1,1,4,11,270,130,32,34,40,42,15,100,105,25,44,7,3,8,12,1,31),
#     denominator_unit_concept_id = c(rep(NA,7),8576, 8587, 8505,8505,8587,8587,45744809,8519,8587,8576,8576,8587,8576,8587,8576,8587,8587,8505,8587,
#                                     8576,8587,45744809,8505,8519,8576,8587,8576,8587,8576,8587),
#     denominator_value = c(rep(NA,7),241,30,53,410,143,2,43,15,21,1,11,42,151,20,rep(NA,16)),
#     numerator_unit_concept_id = c(rep(NA,7),8718,8718,9655,8576,44819154,9551,8576,8576,8576,8576,8587,8587,9573,9573,8718,8718,9439,9655,44819154,
#                                   9551,9551,8576,8576,8576,8576,8576,8587,8587,9573,9573)
#   )
#
#   cdm <- mockDrugUtilisation(connectionDetails,
#                              drug_strength = drug_strength,
#                              numberIndividuals = 50)
#
#   expect_error(dailyDoseCoverage())
#   expect_error(dailyDoseCoverage(cdm = "cdm", ingredient = 2, sample = 10))
#   expect_error(dailyDoseCoverage(cdm = cdm, ingredient = "alcohol", sample = 1))
#   expect_error(dailyDoseCoverage(cdm = cdm, ingredient = 2, sample = "exposure"))
#   expect_error(dailyDoseCoverage(cdm = cdm, ingredient = "alcohol", sample = c(4,5)))
#
# })

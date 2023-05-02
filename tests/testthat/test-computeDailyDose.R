test_that("inputs for addDailyDose function",{
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

    cdm <- mockDrugUtilisation(drug_strength = drug_strength,
                               drug_exposure_size = 50)

    # no inputs
    expect_error(result <- addDailyDose())
    # only cdm
    expect_error(result <- addDailyDose(cdm = cdm))
    # invaliad ingredientConceptId input
    expect_error(addDailyDose(cdm$drug_exposure, cdm, "xx"))

    result <- addDailyDose(cdm$drug_exposure, cdm, 1)
    result <- result %>% dplyr::collect() %>% dplyr::arrange(drug_concept_id)

    expect_true(nrow(result) == cdm$drug_exposure %>% dplyr::tally() %>% dplyr::pull("n"))
    expect_true(all(result$drug_dose_type %in% c("tablets","quantified", "quantity", "puffs","compounded","timeBased" )))

    compareNA <- function(v1,v2) {
      same <- (v1 - v2 < 0.0001) | (is.na(v1) & is.na(v2))
      same[is.na(same)] <- FALSE
      return(same)
    }

    expect_true(all(compareNA(result %>% dplyr::select(daily_dose) %>% dplyr::pull(),
                          c(100, 100, 0.2, 600, 600, 600, 600, 600,
                            300, 0.000597, 0.937, 13, 20, 20, 3, 3, 5, NA, NA,
                            1, NA, 11, 270, 130, 130, 130, 32, 32, 32,
                            0.000034, 0.000034, 0.000034, 0.96, 0.96, 0.96,
                            42, 42, 100, 100, 105, 105, 105, 105, 600, 44,
                            7, 7, 3, 8, 1))))

})

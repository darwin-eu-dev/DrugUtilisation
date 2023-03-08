test_that("inputs for addDailyDose function",{
    drug_strength <- dplyr::tibble(
      drug_concept_id = c(1, 2, 3, 4, 5),
      ingredient_concept_id = c(1, 1, 1, 1, 1),
      amount_value = c(100, NA, NA, NA, NA),
      amount_unit_concept_id = c(8576, 8576, NA, NA, NA),
      numerator_value = c(NA, NA,1,300,5),
      numerator_unit_concept_id = c(NA, NA,8576,8576,8576),
      denominator_value = c(NA, NA, NA, 1, 5),
      denominator_unit_concept_id = c(NA, NA,8587,8587,8576)
    )

    cdm <- mockDrugUtilisation(drug_strength = drug_strength,
                               drug_exposure_size = 20)

    # no inputs
    expect_error(result <- addDailyDose())
    # only cdm
    expect_error(result <- addDailyDose(cdm = cdm))
    # invaliad ingredientConceptId input
    expect_error(addDailyDose(cdm$drug_exposure, cdm, "xx"))

    result <- addDailyDose(cdm$drug_exposure, cdm, 1)
    result <- result %>% dplyr::collect()

    expect_true(nrow(result) == cdm$drug_exposure %>% dplyr::tally() %>% dplyr::pull("n"))
    expect_true(all(result$drug_dose_type %in% c("tablets","quantified", "quantity", "puffs","compounded","timeBased" )))


    #test Tablets
    resultT <- result %>% dplyr::filter(drug_dose_type %in% c("tablets") & drug_concept_id ==1)
    expect_true(all.equal((resultT$daily_dose * resultT$quantity) ,100*(as.integer(difftime(resultT$drug_exposure_end_date, resultT$drug_exposure_start_date, units="days") + 1))))

    #test quantity
    resultT <- result %>% dplyr::filter(drug_dose_type %in% c("quantity") & drug_concept_id ==3)
    expect_true(all.equal((resultT$daily_dose * resultT$quantity) ,1*(as.integer(difftime(resultT$drug_exposure_end_date, resultT$drug_exposure_start_date, units="days") + 1))))

    #test quantified
    resultT <- result %>% dplyr::filter(drug_dose_type %in% c("quantified") & drug_concept_id ==5)
    expect_true(all.equal((resultT$daily_dose * resultT$quantity) ,5*(as.integer(difftime(resultT$drug_exposure_end_date, resultT$drug_exposure_start_date, units="days") + 1))))

    #no daily dose
    resultT <- result %>% dplyr::filter(drug_concept_id == 2)
    expect_true(sum(is.na(resultT$daily_dose)) == nrow(resultT))

})

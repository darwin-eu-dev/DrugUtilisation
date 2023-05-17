test_that("functionality of addDailyDose function",{
    drug_strength <- dplyr::tibble(
      drug_concept_id = c(1:37),
      ingredient_concept_id = c(rep(1,37)),
      amount_value = c(100,200,300,400,500,600,700,rep(NA,30)),
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

    cdm <- mockDrugUtilisation(connectionDetails,
                               drug_strength = drug_strength,
                               drug_exposure_size = 50)

    # should only add patterns 1 to 9, which are drugs 1:7, 10, 11, 25, 30
    result <- addDailyDose(cdm$drug_exposure, cdm, 1) %>%
      dplyr::collect() %>% dplyr::arrange(drug_concept_id)

    expect_true(nrow(result) == cdm$drug_exposure %>% dplyr::tally() %>% dplyr::pull("n"))
    expect_true(all(result %>% dplyr::filter(!is.na(daily_dose)) %>% dplyr::select(drug_concept_id) %>% dplyr::pull() %in% c(1:7,10,11,25,30)))

    compareNA <- function(v1,v2) {
      same <- (v1 - v2 < 0.01) | (is.na(v1) & is.na(v2))
      same[is.na(same)] <- FALSE
      return(same)
    }

    # daily dose depends on pattern_id: standarisation and function
    expect_true(all(compareNA(result %>% dplyr::select(daily_dose) %>%
                                dplyr::arrange("drug_concept_id") %>% dplyr::pull(),
                          c(100, 100, 0.2, 600, 600, 600, 600, 600,
                            NA, 0.005, 0.5853, NA, NA, NA, NA, NA, NA, NA, NA,
                            NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                            NA, 0.96, 0.96, 0.96, NA, NA, NA, NA, NA, NA,
                            NA, NA, 600, NA, NA, NA, NA, NA, NA))))

    DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("test for negative quantity or daily dose",{
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
    person_id = c(1,1,1,1),
    drug_concept_id = c(1,2,15,3),
    drug_exposure_start_date = c(as.Date("2018-11-02"), as.Date("2010-04-04"), as.Date("2014-02-18"), as.Date("2014-01-07")),
    drug_exposure_end_date = c(as.Date("2018-11-09"), as.Date("2010-05-02"), as.Date("2014-02-25"), as.Date("2014-01-06")),
    quantity = c(8,21,8,8)
  )

  cdm <- mockDrugUtilisation(connectionDetails,
                             drug_strength = drug_strength,
                             drug_exposure = drug_exposure)

  result <- addDailyDose(cdm$drug_exposure, cdm, 1) %>%
    dplyr::collect() %>% dplyr::arrange(drug_concept_id)

  expect_true(nrow(result) == cdm$drug_exposure %>% dplyr::tally() %>% dplyr::pull("n"))
  expect_true(all(result %>% dplyr::filter(!is.na(daily_dose)) %>% dplyr::select(drug_concept_id) %>% unique() %in% c(2)))

  compareNA <- function(v1,v2) {
    same <- (v1 - v2 < 0.01) | (is.na(v1) & is.na(v2))
    same[is.na(same)] <- FALSE
    return(same)
  }

  # daily dose depends on pattern_id: standarisation and function
  expect_true(all(compareNA(result %>% dplyr::select(daily_dose) %>%
                              dplyr::arrange("drug_concept_id") %>% dplyr::pull(),
                            c(NA, 0.1448, NA, NA))))

  compareNAchar <- function(v1,v2) {
    same <- (v1 == v2) | (is.na(v1) & is.na(v2))
    same[is.na(same)] <- FALSE
    return(same)
  }
  expect_true(all(compareNAchar(result %>% dplyr::select(unit) %>%
                              dplyr::arrange("drug_concept_id") %>% dplyr::pull(),
                            c("international unit", "milligram", "milligram", NA))))

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("test for different ingredient concept ids",{
  drug_strength <- dplyr::tibble(
    drug_concept_id = c(1:37),
    ingredient_concept_id = c(1,2,rep(1,33),2,3),
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

  cdm <- mockDrugUtilisation(connectionDetails,
                             drug_strength = drug_strength,
                             drug_exposure_size = 50)

  result <- addDailyDose(cdm$drug_exposure, cdm, 2) %>%
    dplyr::collect() %>% dplyr::arrange(drug_concept_id)

  expect_true(nrow(result) == cdm$drug_exposure %>% dplyr::tally() %>% dplyr::pull("n"))
  expect_true(all(result %>% dplyr::filter(!is.na(daily_dose)) %>% dplyr::select(drug_concept_id) %>% dplyr::pull() %in% c(2)))

  compareNA <- function(v1,v2) {
    same <- (v1 - v2 < 0.01) | (is.na(v1) & is.na(v2))
    same[is.na(same)] <- FALSE
    return(same)
  }

  expect_true(all(compareNA(result %>% dplyr::select(daily_dose) %>%
                              dplyr::arrange("drug_concept_id") %>% dplyr::pull(),
                            c(NA, NA, 0.2, NA, NA, NA, NA, NA,
                              NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                              NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                              NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                              NA, NA, NA, NA, NA, NA, NA, NA, NA))))

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

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

  cdm <- mockDrugUtilisation(connectionDetails,
                             drug_strength = drug_strength,
                             drug_exposure_size = 50)

  expect_error(result <- addDailyDose())
  expect_error(result <- addDailyDose(cdm = cdm))
  expect_error(addDailyDose(cdm$drug_exposure, cdm, "xx"))

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

# Pattern logic for patternfile (from the one created by createPatternsTable.R to the one with added "pattern_id" column)

patternfile <- patternfile %>%
  dplyr::mutate(
    pattern_id = ifelse(amount_unit_concept_id == 8718 & is.na(numerator_unit_concept_id) & is.na(denominator_unit_concept_id), 1,
                        ifelse(amount_unit_concept_id == 9655 & is.na(numerator_unit_concept_id) & is.na(denominator_unit_concept_id), 2,
                               ifelse(amount_unit_concept_id == 44819154 & is.na(numerator_unit_concept_id) & is.na(denominator_unit_concept_id), 3,
                                      ifelse(amount_unit_concept_id == 9551 & is.na(numerator_unit_concept_id) & is.na(denominator_unit_concept_id), 4,
                                             ifelse(amount_unit_concept_id == 8576 & is.na(numerator_unit_concept_id) & is.na(denominator_unit_concept_id), 2,
                                                    ifelse(amount_unit_concept_id == 8587 & is.na(numerator_unit_concept_id) & is.na(denominator_unit_concept_id), 5,
                                                           ifelse(amount_unit_concept_id == 9573 & is.na(numerator_unit_concept_id) & is.na(denominator_unit_concept_id), 6,
                                                                  ifelse(is.na(amount_unit_concept_id) & numerator_unit_concept_id == 8718 & !is.na(denominator) & denominator_unit_concept_id == 8576, 7,
                                                                         ifelse(is.na(amount_unit_concept_id) & numerator_unit_concept_id == 8718 & !is.na(denominator) & denominator_unit_concept_id == 8587, 8,
                                                                                ifelse(is.na(amount_unit_concept_id) & numerator_unit_concept_id == 9655 & !is.na(denominator) & denominator_unit_concept_id == 8505, 9,
                                                                                       ifelse(is.na(amount_unit_concept_id) & numerator_unit_concept_id == 44819154 & !is.na(denominator) & denominator_unit_concept_id == 8587, 10,
                                                                                              ifelse(is.na(amount_unit_concept_id) & numerator_unit_concept_id == 9551 & !is.na(denominator) & denominator_unit_concept_id == 8587, 11,
                                                                                                     ifelse(is.na(amount_unit_concept_id) & numerator_unit_concept_id == 8576 & !is.na(denominator) & denominator_unit_concept_id == 45744809, 12,
                                                                                                            ifelse(is.na(amount_unit_concept_id) & numerator_unit_concept_id == 8576 & !is.na(denominator) & denominator_unit_concept_id == 8505, 9,
                                                                                                                   ifelse(is.na(amount_unit_concept_id) & numerator_unit_concept_id == 8576 & !is.na(denominator) & denominator_unit_concept_id == 8519, 13,
                                                                                                                          ifelse(is.na(amount_unit_concept_id) & numerator_unit_concept_id == 8576 & !is.na(denominator) & denominator_unit_concept_id == 8576, 14,
                                                                                                                                 ifelse(is.na(amount_unit_concept_id) & numerator_unit_concept_id == 8576 & !is.na(denominator) & denominator_unit_concept_id == 8587, 13,
                                                                                                                                        ifelse(is.na(amount_unit_concept_id) & numerator_unit_concept_id == 8587 & !is.na(denominator) & denominator_unit_concept_id == 8576, 15,
                                                                                                                                               ifelse(is.na(amount_unit_concept_id) & numerator_unit_concept_id == 8587 & !is.na(denominator) & denominator_unit_concept_id == 8587, 16,
                                                                                                                                                      ifelse(is.na(amount_unit_concept_id) & numerator_unit_concept_id == 9573 & !is.na(denominator) & denominator_unit_concept_id == 8576, 17,
                                                                                                                                                             ifelse(is.na(amount_unit_concept_id) & numerator_unit_concept_id == 9573 & !is.na(denominator) & denominator_unit_concept_id == 8587, 18,
                                                                                                                                                                    ifelse(is.na(amount_unit_concept_id) & numerator_unit_concept_id == 8718 & is.na(denominator) & denominator_unit_concept_id == 8576, 19,
                                                                                                                                                                           ifelse(is.na(amount_unit_concept_id) & numerator_unit_concept_id == 8718 & is.na(denominator) & denominator_unit_concept_id == 8587, 20,
                                                                                                                                                                                  ifelse(is.na(amount_unit_concept_id) & numerator_unit_concept_id == 9439 & is.na(denominator) & denominator_unit_concept_id == 8587, 20,
                                                                                                                                                                                         ifelse(is.na(amount_unit_concept_id) & numerator_unit_concept_id == 9655 & is.na(denominator) & denominator_unit_concept_id == 8505, 21,
                                                                                                                                                                                                ifelse(is.na(amount_unit_concept_id) & numerator_unit_concept_id == 44819154 & is.na(denominator) & denominator_unit_concept_id == 8587, 22,
                                                                                                                                                                                                       ifelse(is.na(amount_unit_concept_id) & numerator_unit_concept_id == 9551 & is.na(denominator) & denominator_unit_concept_id == 8576, 23,
                                                                                                                                                                                                              ifelse(is.na(amount_unit_concept_id) & numerator_unit_concept_id == 9551 & is.na(denominator) & denominator_unit_concept_id == 8587, 24,
                                                                                                                                                                                                                     ifelse(is.na(amount_unit_concept_id) & numerator_unit_concept_id == 8576 & is.na(denominator) & denominator_unit_concept_id == 45744809, 25,
                                                                                                                                                                                                                            ifelse(is.na(amount_unit_concept_id) & numerator_unit_concept_id == 8576 & is.na(denominator) & denominator_unit_concept_id == 8505, 26,
                                                                                                                                                                                                                                   ifelse(is.na(amount_unit_concept_id) & numerator_unit_concept_id == 8576 & is.na(denominator) & denominator_unit_concept_id == 8519, 27,
                                                                                                                                                                                                                                          ifelse(is.na(amount_unit_concept_id) & numerator_unit_concept_id == 8576 & is.na(denominator) & denominator_unit_concept_id == 8576, 28,
                                                                                                                                                                                                                                                 ifelse(is.na(amount_unit_concept_id) & numerator_unit_concept_id == 8576 & is.na(denominator) & denominator_unit_concept_id == 8587, 29,
                                                                                                                                                                                                                                                        ifelse(is.na(amount_unit_concept_id) & numerator_unit_concept_id == 8587 & is.na(denominator) & denominator_unit_concept_id == 8576, 30,
                                                                                                                                                                                                                                                               ifelse(is.na(amount_unit_concept_id) & numerator_unit_concept_id == 8587 & is.na(denominator) & denominator_unit_concept_id == 8587, 31,
                                                                                                                                                                                                                                                                      ifelse(is.na(amount_unit_concept_id) & numerator_unit_concept_id == 9573 & is.na(denominator) & denominator_unit_concept_id == 8576, 32,
                                                                                                                                                                                                                                                                             ifelse(is.na(amount_unit_concept_id) & numerator_unit_concept_id == 9573 & is.na(denominator) & denominator_unit_concept_id == 8587, 33,
                                                                                                                                                                                                                                                                                    NA))))))))))))))))))))))))))))))))))))))

patternfile <- patternfile %>%
  mutate(pattern_id = ifelse(valid == FALSE, NA, pattern_id))
patternfile <- patternfile %>%
  dplyr::mutate(
    unit = ifelse(pattern_id %in% c(1, 7, 8, 19, 20), "international unit",
                  ifelse(pattern_id %in% c(2, 9, 12, 13, 14, 21, 25, 26, 27, 28, 29), "milligram",
                         ifelse(pattern_id %in% c(3, 10, 22), "millicurie",
                                ifelse(pattern_id %in% c(4, 11, 23, 24), "milliequivalent",
                                       ifelse(pattern_id %in% c(5, 15, 16, 30, 31), "milliliter",
                                              ifelse(pattern_id %in% c(6, 17, 18, 32, 33), "millimole",
                                                     NA)))))
    ))


write.csv(patternfile, file= here::here("inst", "pattern_drug_strength.csv"))

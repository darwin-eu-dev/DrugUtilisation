#build the pattern table using cratePatternsTable.R

# logic for valid versus non valid pattern: already implemented in the previous function
#patternfile <- patternfile %>%
#  dplyr::mutate(
#    valid =
#      dplyr::if_else ((!is.na(amount) & grepl("gram|international unit|liter|milliequivalent", amount_unit) & is.na(denominator_unit) & is.na(numerator_unit)) |
#                        (!is.na(numerator) & grepl("gram|international unit|liter|milliequivalent", numerator_unit) &
#                           grepl("hour", denominator_unit)),TRUE,FALSE))


# standardisation will be done a posteriori
# standards will be milliliter, milligram and international unit

# add pattern id
patternfile <- patternfile %>%
  dplyr::mutate(
    pattern_id = ifelse(!is.na(amount) & amount_unit_concept_id == 8718 & is.na(numerator_unit_concept_id) & is.na(denominator_unit_concept_id), 1,
                        ifelse(!is.na(amount) & amount_unit_concept_id == 9655 & is.na(numerator_unit_concept_id) & is.na(denominator_unit_concept_id), 2,
                               ifelse(!is.na(amount) & amount_unit_concept_id == 9551 & is.na(numerator_unit_concept_id) & is.na(denominator_unit_concept_id), 3,
                                      ifelse(!is.na(amount) & amount_unit_concept_id == 8576 & is.na(numerator_unit_concept_id) & is.na(denominator_unit_concept_id), 4,
                                             ifelse(!is.na(amount) & amount_unit_concept_id == 8587 & is.na(numerator_unit_concept_id) & is.na(denominator_unit_concept_id), 5,
                                                    ifelse(is.na(amount_unit_concept_id) & numerator_unit_concept_id == 9655 & !is.na(denominator) & denominator_unit_concept_id == 8505, 6,
                                                           ifelse(is.na(amount_unit_concept_id) & numerator_unit_concept_id == 8576 & !is.na(denominator) & denominator_unit_concept_id == 8505, 7,
                                                                  ifelse(is.na(amount_unit_concept_id) & numerator_unit_concept_id == 9655 & is.na(denominator) & denominator_unit_concept_id == 8505, 8,
                                                                         ifelse(is.na(amount_unit_concept_id) & numerator_unit_concept_id == 8576 & is.na(denominator) & denominator_unit_concept_id == 8505, 9,
                                                                                NA))))))))))

# add pattern_name
patternfile <- patternfile %>%
  dplyr::mutate(
    pattern_name = ifelse(!is.na(amount) & amount_unit_concept_id == 8718 & is.na(numerator_unit_concept_id) & is.na(denominator_unit_concept_id), "international_unit",
                          ifelse(!is.na(amount) & amount_unit_concept_id == 9655 & is.na(numerator_unit_concept_id) & is.na(denominator_unit_concept_id), "milligram",
                                 ifelse(!is.na(amount) & amount_unit_concept_id == 9551 & is.na(numerator_unit_concept_id) & is.na(denominator_unit_concept_id), "milliequivalent",
                                        ifelse(!is.na(amount) & amount_unit_concept_id == 8576 & is.na(numerator_unit_concept_id) & is.na(denominator_unit_concept_id), "milligram",
                                               ifelse(!is.na(amount) & amount_unit_concept_id == 8587 & is.na(numerator_unit_concept_id) & is.na(denominator_unit_concept_id), "milliliter",
                                                      ifelse(is.na(amount_unit_concept_id) & numerator_unit_concept_id == 9655 & !is.na(denominator) & denominator_unit_concept_id == 8505, "milligram_per_hour",
                                                             ifelse(is.na(amount_unit_concept_id) & numerator_unit_concept_id == 8576 & !is.na(denominator) & denominator_unit_concept_id == 8505, "milligram_per_hour",
                                                                    ifelse(is.na(amount_unit_concept_id) & numerator_unit_concept_id == 9655 & is.na(denominator) & denominator_unit_concept_id == 8505, "milligram_per_missing_hour",
                                                                           ifelse(is.na(amount_unit_concept_id) & numerator_unit_concept_id == 8576 & is.na(denominator) & denominator_unit_concept_id == 8505, "milligram_per_missing_hour",
                                                                                  NA))))))))))
# add daily dose unit
patternfile <- patternfile %>%
  mutate(pattern_id = ifelse(valid == FALSE, NA, pattern_id))
patternfile <- patternfile %>%
  dplyr::mutate(
    unit = ifelse(pattern_id %in% c(1), "international unit",
                  ifelse(pattern_id %in% c(2,4,6,7,8,9), "milligram",
                         ifelse(pattern_id %in% c(3), "milliequivalent",
                                ifelse(pattern_id %in% c(5), "milliliter",
                                       NA)))
    ))



# write csv
write.csv(patternfile, here::here("extras", paste0("pattern_drug_strength.csv")), row.names = FALSE)

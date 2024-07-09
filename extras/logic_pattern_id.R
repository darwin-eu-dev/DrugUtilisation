#build the pattern table using cratePatternsTable.R

# logic for valid versus non valid pattern: already implemented in the previous function
#patternfile <- patternfile |>
#  dplyr::mutate(
#    valid =
#      dplyr::if_else ((!is.na(amount) & grepl("gram|international unit|liter|milliequivalent", amount_unit) & is.na(denominator_unit) & is.na(numerator_unit)) |
#                        (!is.na(numerator) & grepl("gram|international unit|liter|milliequivalent", numerator_unit) &
#                           grepl("hour", denominator_unit)),TRUE,FALSE))


# standardisation will be done a posteriori
# standards will be milliliter, milligram and international unit

# add pattern id
patternfile <- patternfile |>
  dplyr::mutate(pattern_id = dplyr::case_when(
    !is.na(amount) & amount_unit_concept_id == 8718 & is.na(numerator_unit_concept_id) & is.na(denominator_unit_concept_id) ~ 1,
    !is.na(amount) & amount_unit_concept_id == 9655 & is.na(numerator_unit_concept_id) & is.na(denominator_unit_concept_id) ~ 2,
    !is.na(amount) & amount_unit_concept_id == 9551 & is.na(numerator_unit_concept_id) & is.na(denominator_unit_concept_id) ~ 3,
    !is.na(amount) & amount_unit_concept_id == 8576 & is.na(numerator_unit_concept_id) & is.na(denominator_unit_concept_id) ~ 4,
    !is.na(amount) & amount_unit_concept_id == 8587 & is.na(numerator_unit_concept_id) & is.na(denominator_unit_concept_id) ~ 5,
    is.na(amount_unit_concept_id) & numerator_unit_concept_id == 9655 & !is.na(denominator) & denominator_unit_concept_id == 8505 ~ 6,
    is.na(amount_unit_concept_id) & numerator_unit_concept_id == 8576 & !is.na(denominator) & denominator_unit_concept_id == 8505 ~ 7,
    is.na(amount_unit_concept_id) & numerator_unit_concept_id == 9655 & is.na(denominator) & denominator_unit_concept_id == 8505 ~ 8,
    is.na(amount_unit_concept_id) & numerator_unit_concept_id == 8576 & is.na(denominator) & denominator_unit_concept_id == 8505 ~ 9,
    TRUE ~ NA
  ))

# add pattern_name
patternfile <- patternfile |>
  dplyr::mutate(pattern_name = dplyr::case_when(
    !is.na(amount) & amount_unit_concept_id == 8718 & is.na(numerator_unit_concept_id) & is.na(denominator_unit_concept_id) ~ "international_unit",
    !is.na(amount) & amount_unit_concept_id == 9655 & is.na(numerator_unit_concept_id) & is.na(denominator_unit_concept_id) ~ "milligram",
    !is.na(amount) & amount_unit_concept_id == 9551 & is.na(numerator_unit_concept_id) & is.na(denominator_unit_concept_id) ~ "milliequivalent",
    !is.na(amount) & amount_unit_concept_id == 8576 & is.na(numerator_unit_concept_id) & is.na(denominator_unit_concept_id) ~ "milligram",
    !is.na(amount) & amount_unit_concept_id == 8587 & is.na(numerator_unit_concept_id) & is.na(denominator_unit_concept_id) ~ "milliliter",
    is.na(amount_unit_concept_id) & numerator_unit_concept_id == 9655 & !is.na(denominator) & denominator_unit_concept_id == 8505 ~ "milligram_per_hour",
    is.na(amount_unit_concept_id) & numerator_unit_concept_id == 8576 & !is.na(denominator) & denominator_unit_concept_id == 8505 ~ "milligram_per_hour",
    is.na(amount_unit_concept_id) & numerator_unit_concept_id == 9655 & is.na(denominator) & denominator_unit_concept_id == 8505 ~ "milligram_per_missing_hour",
    is.na(amount_unit_concept_id) & numerator_unit_concept_id == 8576 & is.na(denominator) & denominator_unit_concept_id == 8505 ~ "milligram_per_missing_hour",
    TRUE ~ NA
  ))

# add daily dose unit
patternfile <- patternfile |>
  mutate(pattern_id = ifelse(valid == FALSE, NA, pattern_id))
patternfile <- patternfile |>
  dplyr::mutate(unit = dplyr::case_when(
    pattern_id %in% c(1) ~ "international unit",
    pattern_id %in% c(2,4,6,7,8,9) ~ "milligram",
    pattern_id %in% c(3) ~ "milliequivalent",
    pattern_id %in% c(5) ~ "milliliter",
    TRUE ~ NA
  ))

# write csv
write.csv(patternfile, here::here("extras", paste0("pattern_drug_strength.csv")), row.names = FALSE)

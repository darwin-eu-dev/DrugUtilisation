# ## imputeDuration - NOT FINISHED
# When duration of the treatment can not be calculated due to some missing value, we can impute the duration by using *imputeDuration" input. For example, let's replace all the "drug_exposure_end_dates" of subject number 14:
#
# ```{r, message = FALSE, warning=FALSE}
# cdm$drug_exposure <- cdm$drug_exposure %>%
#   mutate(drug_exposure_end_date = dplyr::if_else(person_id == 14, NA, drug_exposure_end_date))
# ```
#
# Now, let's try to calculate the duration of its treatment:
# ```{r, message = FALSE, warning=FALSE}
# cdm <- generateDrugUtilisationCohortSet(
#   cdm  = cdm,
#   name = "acetaminophen_example3",
#   conceptSet = conceptList,
#   imputeDuration = "median"
# )
#
# cdm$acetaminophen_example3 %>%
#   addDrugUse(
#     ingredientConceptId = 1125315,
#     duration = TRUE,
#     quantity = FALSE,
#     dose     = FALSE,
#     imputeDailyDose =
#   ) %>%
#   dplyr::filter(subject_id == 14)
# ```
# ## imputeDailyDose - NOT FINISHED


library(DrugUtilisation)
library(PatientProfiles)
library(CodelistGenerator)

cdm <- mockDrugUtilisation()
cdm <- generateDrugUtilisationCohortSet(
  cdm, "dus_cohort", getDrugIngredientCodes(cdm, "acetaminophen")
)
cdm[["dus_cohort"]] <- cdm[["dus_cohort"]] %>%
  addDrugUse(cdm, 1125315)
result <- summariseDrugUse(cdm[["dus_cohort"]], cdm)
print(result)

cdm[["dus_cohort"]] <- cdm[["dus_cohort"]] %>%
  addSex(cdm) %>%
  addAge(cdm, ageGroup = list("<40" = c(0, 30), ">40" = c(40, 150)))

summariseDrugUse(
  cdm[["dus_cohort"]], cdm, strata = list(
    "age_group" = "age_group", "sex" = "sex"
  )
)


########################### summariseDrugUse
library(DrugUtilisation)
library(CodelistGenerator)
library(CDMConnector)
library(dplyr)
library(PatientProfiles)

cdm <- mockDrugUtilisation(numberIndividual  = 200)
conceptList <- CodelistGenerator::getDrugIngredientCodes(cdm, c("acetaminophen"))
cdm <- generateDrugUtilisationCohortSet(
  cdm  = cdm,
  name = "acetaminophen_users",
  conceptSet = conceptList
)

# Chunk 1
summariseDrugUse(cdm$acetaminophen_users)

# Chunk 2
cdm <- mockDrugUtilisation(numberIndividual  = 200)
conceptList <- CodelistGenerator::getDrugIngredientCodes(cdm, c("acetaminophen", "metformin"))
cdm <- generateDrugUtilisationCohortSet(
  cdm  = cdm,
  name = "acetaminophen_metformin_users",
  conceptSet = conceptList
)

summariseDrugUse(cdm$acetaminophen_metformin_users)

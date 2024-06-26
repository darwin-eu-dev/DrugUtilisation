% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/summariseDrugUse.R
\name{summariseDrugUse}
\alias{summariseDrugUse}
\title{This function is used to summarise the dose table over multiple cohorts.}
\usage{
summariseDrugUse(
  cohort,
  cdm = lifecycle::deprecated(),
  strata = list(),
  drugUseEstimates = c("min", "q05", "q25", "median", "q75", "q95", "max", "mean", "sd",
    "count_missing", "percentage_missing"),
  minCellCount = lifecycle::deprecated()
)
}
\arguments{
\item{cohort}{Cohort with drug use variables and strata.}

\item{cdm}{Deprecated.}

\item{strata}{Stratification list.}

\item{drugUseEstimates}{Estimates that we want for the columns.}

\item{minCellCount}{Deprecated.}
}
\value{
A summary of the drug use stratified by cohort_name and strata_name
}
\description{
This function is used to summarise the dose table over multiple cohorts.
}
\examples{
\donttest{
library(DrugUtilisation)
library(PatientProfiles)
library(CodelistGenerator)

cdm <- mockDrugUtilisation()
cdm <- generateDrugUtilisationCohortSet(
  cdm, "dus_cohort", getDrugIngredientCodes(cdm, "acetaminophen")
)
cdm[["dus_cohort"]] <- cdm[["dus_cohort"]] \%>\%
  addDrugUse(ingredientConceptId = 1125315)
result <- summariseDrugUse(cdm[["dus_cohort"]])
print(result)

cdm[["dus_cohort"]] <- cdm[["dus_cohort"]] \%>\%
  addSex() \%>\%
  addAge(ageGroup = list("<40" = c(0, 30), ">40" = c(40, 150)))

summariseDrugUse(
  cdm[["dus_cohort"]], strata = list(
   "age_group" = "age_group", "sex" = "sex",
   "age_group and sex" = c("age_group", "sex")
  )
)
}

}

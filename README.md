
<!-- README.md is generated from README.Rmd. Please edit that file -->

# DrugUtilisation <img src='man/figures/DrugUtilisation.png' align="right" height="139"/>

[![CRANstatus](https://www.r-pkg.org/badges/version/DrugUtilisation)](https://CRAN.R-project.org/package=DrugUtilisation)
[![codecov.io](https://codecov.io/github/darwin-eu/DrugUtilisation/coverage.svg?branch=main)](https://app.codecov.io/github/darwin-eu/DrugUtilisation?branch=main)
[![R-CMD-check](https://github.com/darwin-eu/DrugUtilisation/workflows/R-CMD-check/badge.svg)](https://github.com/darwin-eu/DrugUtilisation/actions)
[![Lifecycle:Experimental](https://img.shields.io/badge/Lifecycle-Experimental-339999)](https://lifecycle.r-lib.org/articles/stages.html)

## WARNING: This package is under development.

- **addDailyDose** function works for the following patterns in the
  drug_strength table:

| pattern_id |    amount_unit     | numerator_unit | denominator_unit |
|:----------:|:------------------:|:--------------:|:----------------:|
|     1      | international unit |       NA       |        NA        |
|     2      |     microgram      |       NA       |        NA        |
|     3      |  milliequivalent   |       NA       |        NA        |
|     4      |     milligram      |       NA       |        NA        |
|     5      |     millimiter     |       NA       |        NA        |
|     6      |         NA         |   microgram    |       hour       |
|     7      |         NA         |   milligram    |    actuation     |
|     8      |         NA         |   microgram    |       hour       |
|     9      |         NA         |   milligram    |       hour       |

## Package overview

DrugUtilisation contains functions to instantiate and characterize the
cohorts used in a Drug Utilisation Study in the OMOP common data model.
Main functionalities are:

- Create DrugUtilisation cohorts

- Add indications to this cohort

- Add the dosage of a certain ingredient (subseted for a list of drugs)

- Calculate the daily dose

- Create Concept based cohorts

- Read concepts from json files

- Summarise the drug use in a certain cohort

- Summarise the indications in a certain cohort

- Summarise the patients characteristics in a certain cohort

- Summarise the patients large scale characterics in a certain cohort

## Example

First, we need to create a cdm reference for the data we´ll be using.
Here we´ll generate an example with simulated data, but to see how you
would set this up for your database please consult the CDMConnector
package [connection
examples](https://darwin-eu.github.io/CDMConnector/articles/a04_DBI_connection_examples.html).

The package also provides a functionality to generate a
mockDrugUtilisation cdm reference:

``` r
library(DrugUtilisation)
cdm <- mockDrugUtilisation(numberIndividual = 100)
```

### Create a cohort of drug use

To create a cohort we will need a conceptList, this can be read from
json files:

``` r
conceptList <- readConceptList(here::here("Concepts"), cdm)
```

Or we can build our own list using other packages
(e.g. CodelistGenerator)

``` r
library(CodelistGenerator)
#> Warning: package 'CodelistGenerator' was built under R version 4.2.3
conceptList <- getDrugIngredientCodes(cdm, "acetaminophen")
conceptList
#> $acetaminophen
#> [1]  1125315 43135274  2905077  1125360
```

To generate the cohort of drug use we will use
`generateDrugUtilisationCohortSet`:

``` r
cdm <- generateDrugUtilisationCohortSet(
  cdm = cdm,
  name = "dus_cohort",
  conceptSet = conceptList,
  limit = "first",
  priorObservation = 365,
  gapEra = 30,
  priorUseWashout = 0,
  imputeDuration = "none", 
  durationRange = c(0, Inf)
)
```

#### Cohort attributes

The generated cohort will have the `GeneratedCohortSet` as seen in
CDMConnector

``` r
class(cdm[["dus_cohort"]])
#> [1] "cohort_table"          "GeneratedCohortSet"    "cdm_table"            
#> [4] "tbl_duckdb_connection" "tbl_dbi"               "tbl_sql"              
#> [7] "tbl_lazy"              "tbl"
```

Cohort set:

``` r
library(CDMConnector)
library(dplyr)
#> Warning: package 'dplyr' was built under R version 4.2.3
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
settings(cdm[["dus_cohort"]]) %>% glimpse()
#> Rows: 1
#> Columns: 11
#> $ cohort_definition_id    <int> 1
#> $ cohort_name             <chr> "acetaminophen"
#> $ duration_range_min      <chr> "0"
#> $ duration_range_max      <chr> "Inf"
#> $ impute_duration         <chr> "none"
#> $ gap_era                 <chr> "30"
#> $ prior_use_washout       <chr> "0"
#> $ prior_observation       <chr> "365"
#> $ cohort_date_range_start <chr> NA
#> $ cohort_date_range_end   <chr> NA
#> $ limit                   <chr> "first"
```

Cohort count:

``` r
cohortCount(cdm[["dus_cohort"]])
#> # A tibble: 1 × 3
#>   cohort_definition_id number_records number_subjects
#>                  <int>          <int>           <int>
#> 1                    1             35              35
```

Cohort attrition:

``` r
attrition(cdm[["dus_cohort"]]) %>% glimpse()
#> Rows: 4
#> Columns: 7
#> $ cohort_definition_id <int> 1, 1, 1, 1
#> $ number_records       <int> 71, 70, 41, 35
#> $ number_subjects      <int> 62, 62, 35, 35
#> $ reason_id            <int> 1, 2, 3, 4
#> $ reason               <chr> "Initial qualifying events", "join exposures sepa…
#> $ excluded_records     <int> 0, 1, 29, 6
#> $ excluded_subjects    <int> 0, 0, 27, 0
```

### Indication

Indications will always be cohorts. An option that the package has is to
create concept based cohorts using `generateConceptCohortSet`.

``` r
indications <- list(headache = 378253, influenza = 4266367)
cdm <- generateConceptCohortSet(cdm, indications, "indications_cohort")
cohortCount(cdm[["indications_cohort"]])
#> # A tibble: 2 × 3
#>   cohort_definition_id number_records number_subjects
#>                  <int>          <int>           <int>
#> 1                    1             52              52
#> 2                    2             46              46
```

Then we can add the indication using the function `addIndication`. That
will add a new column for each indication gap and indication.

``` r
x <- cdm[["dus_cohort"]] %>%
  addIndication(
    cdm = cdm, indicationCohortName = "indications_cohort", indicationGap = c(0, 30, 365), 
    unknownIndicationTable = c("condition_occurrence")
  )
#> Warning: The `cdm` argument of `addIndication()` is deprecated as of DrugUtilisation
#> 0.5.0.
#> This warning is displayed once every 8 hours.
#> Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
#> generated.
glimpse(x)
#> Rows: ??
#> Columns: 16
#> Database: DuckDB 0.7.1 [martics@Windows 10 x64:R 4.2.1/:memory:]
#> $ cohort_definition_id         <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,…
#> $ subject_id                   <int> 81, 47, 96, 75, 91, 93, 46, 88, 8, 54, 9,…
#> $ cohort_start_date            <date> 1995-09-13, 1963-12-22, 2011-05-25, 2010…
#> $ cohort_end_date              <date> 1997-11-17, 1965-09-16, 2013-01-23, 2011…
#> $ indication_gap_0_headache    <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
#> $ indication_gap_0_influenza   <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
#> $ indication_gap_0_none        <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,…
#> $ indication_gap_0_unknown     <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
#> $ indication_gap_30_headache   <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
#> $ indication_gap_30_influenza  <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
#> $ indication_gap_30_none       <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1,…
#> $ indication_gap_30_unknown    <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0,…
#> $ indication_gap_365_headache  <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
#> $ indication_gap_365_influenza <dbl> 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0,…
#> $ indication_gap_365_none      <dbl> 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 0, 1, 1,…
#> $ indication_gap_365_unknown   <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0,…
```

We can combine the indications in a single column using the
`indicationToStrata()` function. This column can be used as
stratification of the results if needed:

``` r
x <- x %>% indicationToStrata(keep = TRUE)
glimpse(x)
#> Rows: ??
#> Columns: 19
#> Database: DuckDB 0.7.1 [martics@Windows 10 x64:R 4.2.1/:memory:]
#> $ cohort_definition_id         <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,…
#> $ subject_id                   <int> 81, 47, 96, 75, 91, 93, 46, 88, 8, 54, 9,…
#> $ cohort_start_date            <date> 1995-09-13, 1963-12-22, 2011-05-25, 2010…
#> $ cohort_end_date              <date> 1997-11-17, 1965-09-16, 2013-01-23, 2011…
#> $ indication_gap_0_headache    <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
#> $ indication_gap_0_influenza   <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
#> $ indication_gap_0_none        <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,…
#> $ indication_gap_0_unknown     <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
#> $ indication_gap_30_headache   <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
#> $ indication_gap_30_influenza  <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
#> $ indication_gap_30_none       <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1,…
#> $ indication_gap_30_unknown    <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0,…
#> $ indication_gap_365_headache  <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
#> $ indication_gap_365_influenza <dbl> 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0,…
#> $ indication_gap_365_none      <dbl> 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 0, 1, 1,…
#> $ indication_gap_365_unknown   <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0,…
#> $ indication_gap_0             <chr> "None", "None", "None", "None", "None", "…
#> $ indication_gap_30            <chr> "None", "None", "None", "None", "None", "…
#> $ indication_gap_365           <chr> "None", "None", "None", "None", "None", "…
```

``` r
table(x %>% pull("indication_gap_365"))
#> 
#>  Headache Influenza      None   Unknown 
#>         2         3        27         3
```

### Summarise the indication

We can summarise the indication results using the `summariseIndication`
function:

``` r
summariseIndication(x, cdm)
#> # A tibble: 42 × 15
#>    cdm_name result_type      package_name package_version group_name group_level
#>    <chr>    <chr>            <chr>        <chr>           <chr>      <chr>      
#>  1 DUS MOCK summarised_indi… DrugUtilisa… 0.5.0           cohort_na… acetaminop…
#>  2 DUS MOCK summarised_indi… DrugUtilisa… 0.5.0           cohort_na… acetaminop…
#>  3 DUS MOCK summarised_indi… DrugUtilisa… 0.5.0           cohort_na… acetaminop…
#>  4 DUS MOCK summarised_indi… DrugUtilisa… 0.5.0           cohort_na… acetaminop…
#>  5 DUS MOCK summarised_indi… DrugUtilisa… 0.5.0           cohort_na… acetaminop…
#>  6 DUS MOCK summarised_indi… DrugUtilisa… 0.5.0           cohort_na… acetaminop…
#>  7 DUS MOCK summarised_indi… DrugUtilisa… 0.5.0           cohort_na… acetaminop…
#>  8 DUS MOCK summarised_indi… DrugUtilisa… 0.5.0           cohort_na… acetaminop…
#>  9 DUS MOCK summarised_indi… DrugUtilisa… 0.5.0           cohort_na… acetaminop…
#> 10 DUS MOCK summarised_indi… DrugUtilisa… 0.5.0           cohort_na… acetaminop…
#> # ℹ 32 more rows
#> # ℹ 9 more variables: strata_name <chr>, strata_level <chr>,
#> #   variable_name <chr>, variable_level <chr>, estimate_name <chr>,
#> #   estimate_type <chr>, estimate_value <chr>, additional_name <chr>,
#> #   additional_level <chr>
```

``` r
summariseIndication(x, cdm) %>% glimpse()
#> Rows: 42
#> Columns: 15
#> $ cdm_name         <chr> "DUS MOCK", "DUS MOCK", "DUS MOCK", "DUS MOCK", "DUS …
#> $ result_type      <chr> "summarised_indication", "summarised_indication", "su…
#> $ package_name     <chr> "DrugUtilisation", "DrugUtilisation", "DrugUtilisatio…
#> $ package_version  <chr> "0.5.0", "0.5.0", "0.5.0", "0.5.0", "0.5.0", "0.5.0",…
#> $ group_name       <chr> "cohort_name", "cohort_name", "cohort_name", "cohort_…
#> $ group_level      <chr> "acetaminophen", "acetaminophen", "acetaminophen", "a…
#> $ strata_name      <chr> "overall", "overall", "overall", "overall", "overall"…
#> $ strata_level     <chr> "overall", "overall", "overall", "overall", "overall"…
#> $ variable_name    <chr> "number subjects", "number records", "Indication on i…
#> $ variable_level   <chr> NA, NA, "Headache", "Headache", "Influenza", "Influen…
#> $ estimate_name    <chr> "count", "count", "count", "percentage", "count", "pe…
#> $ estimate_type    <chr> "numeric", "numeric", "numeric", "percentage", "numer…
#> $ estimate_value   <chr> "35", "35", "0", "0", "0", "0", "35", "100", "0", "0"…
#> $ additional_name  <chr> "overall", "overall", "overall", "overall", "overall"…
#> $ additional_level <chr> "overall", "overall", "overall", "overall", "overall"…
```

### Add strata

All summarise functions have the option to add strata. Strata will
always point to preexisting columns. Here we can see an example where we
create a `age_group` and `sex` columns using PatientProfiles and then we
use it as strata

``` r
library(PatientProfiles)
x <- x %>%
  addAge(cdm, ageGroup = list(c(0, 19), c(20, 39), c(40, 59), c(60, 79), c(80, 150))) %>%
  addSex(cdm)
#> Warning: The `cdm` argument of `addSex()` is deprecated as of PatientProfiles 0.6.0.
#> This warning is displayed once every 8 hours.
#> Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
#> generated.
#> Warning: The `cdm` argument of `addAge()` is deprecated as of PatientProfiles 0.6.0.
#> This warning is displayed once every 8 hours.
#> Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
#> generated.
summariseIndication(x, cdm, strata = list("age_group", "sex", c("age_group", "sex")))
#> # A tibble: 466 × 15
#>    cdm_name result_type      package_name package_version group_name group_level
#>    <chr>    <chr>            <chr>        <chr>           <chr>      <chr>      
#>  1 DUS MOCK summarised_indi… DrugUtilisa… 0.5.0           cohort_na… acetaminop…
#>  2 DUS MOCK summarised_indi… DrugUtilisa… 0.5.0           cohort_na… acetaminop…
#>  3 DUS MOCK summarised_indi… DrugUtilisa… 0.5.0           cohort_na… acetaminop…
#>  4 DUS MOCK summarised_indi… DrugUtilisa… 0.5.0           cohort_na… acetaminop…
#>  5 DUS MOCK summarised_indi… DrugUtilisa… 0.5.0           cohort_na… acetaminop…
#>  6 DUS MOCK summarised_indi… DrugUtilisa… 0.5.0           cohort_na… acetaminop…
#>  7 DUS MOCK summarised_indi… DrugUtilisa… 0.5.0           cohort_na… acetaminop…
#>  8 DUS MOCK summarised_indi… DrugUtilisa… 0.5.0           cohort_na… acetaminop…
#>  9 DUS MOCK summarised_indi… DrugUtilisa… 0.5.0           cohort_na… acetaminop…
#> 10 DUS MOCK summarised_indi… DrugUtilisa… 0.5.0           cohort_na… acetaminop…
#> # ℹ 456 more rows
#> # ℹ 9 more variables: strata_name <chr>, strata_level <chr>,
#> #   variable_name <chr>, variable_level <chr>, estimate_name <chr>,
#> #   estimate_type <chr>, estimate_value <chr>, additional_name <chr>,
#> #   additional_level <chr>
```

``` r
summariseIndication(x, cdm, strata = list("age_group", "sex", c("age_group", "sex"))) %>% glimpse()
#> Rows: 466
#> Columns: 15
#> $ cdm_name         <chr> "DUS MOCK", "DUS MOCK", "DUS MOCK", "DUS MOCK", "DUS …
#> $ result_type      <chr> "summarised_indication", "summarised_indication", "su…
#> $ package_name     <chr> "DrugUtilisation", "DrugUtilisation", "DrugUtilisatio…
#> $ package_version  <chr> "0.5.0", "0.5.0", "0.5.0", "0.5.0", "0.5.0", "0.5.0",…
#> $ group_name       <chr> "cohort_name", "cohort_name", "cohort_name", "cohort_…
#> $ group_level      <chr> "acetaminophen", "acetaminophen", "acetaminophen", "a…
#> $ strata_name      <chr> "overall", "overall", "overall", "overall", "overall"…
#> $ strata_level     <chr> "overall", "overall", "overall", "overall", "overall"…
#> $ variable_name    <chr> "number subjects", "number records", "Indication on i…
#> $ variable_level   <chr> NA, NA, "Headache", "Headache", "Influenza", "Influen…
#> $ estimate_name    <chr> "count", "count", "count", "percentage", "count", "pe…
#> $ estimate_type    <chr> "numeric", "numeric", "numeric", "percentage", "numer…
#> $ estimate_value   <chr> "35", "35", "0", "0", "0", "0", "35", "100", "0", "0"…
#> $ additional_name  <chr> "overall", "overall", "overall", "overall", "overall"…
#> $ additional_level <chr> "overall", "overall", "overall", "overall", "overall"…
```

### Daily dose

We can compute daily dose for a certain ingredient from a subset of
drug_exposure or the whole drug exposure (can be very computationally
expensive).

``` r
#cdm[["drug_exposure"]] %>%
#  addDailyDose(ingredientConceptId = 1125315) %>%
#  glimpse()
```

#### Coverage

Currently you can evaluate the coverage of daily dose for a conceptList
or overall using `dailyDoseCoverage`. You can restrict to a certain
ingredient, otherwise the result will be stratified by ingredient.

``` r
#dailyDoseCoverage(cdm = cdm, sample = NULL, ingredient = NULL, conceptList = NULL) 
```

### DrugUse

You can add columns related to the drug use using `addDrugUse`. You
always have to provide a reference ingredient.

``` r
#x <- x %>%
#  addDrugUse(
#    cdm = cdm,
#    ingredientConceptId = 1125315,
#    initialDailyDose = TRUE,
#    numberExposures = TRUE,
#    duration = TRUE,
#    cumulativeDose = TRUE,
#    numberEras = TRUE
#  )
```

### Summarise the drug use

You can summarise the drug use using `summariseDrugUse` function

``` r
#summariseDrugUse(x, cdm)
```

### Summarise patient characteristics

You can summarise the patient characteristics with
`summariseCharacteristics` function:

``` r
summariseCharacteristics(
  x, cdm, ageGroup = list(c(0, 24), c(25, 49), c(50, 74), c(75, 150)),
  tableIntersect = list(
    "Visits" = list(
      tableName = "visit_occurrence", value = "count", window = c(-365, 0)
    )
  ),
  cohortIntersect = list(
    "Indications" = list(
      targetCohortTable  = "indications_cohort", value = "flag", 
      window = c(-365, 0)
    )
  )
)
#> Warning: The `cdm` argument of `summariseCharacteristics()` is deprecated as of
#> PatientProfiles 0.6.0.
#> This warning is displayed once every 8 hours.
#> Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
#> generated.
#> ℹ adding demographics columns
#> ℹ adding table intersect columns for table: visit_occurrence
#> ℹ adding cohort intersect columns for table: indications_cohort
#> ℹ summarising data
#> ✔ summariseCharacteristics finished!
#> # A tibble: 66 × 15
#>    cdm_name result_type      package_name package_version group_name group_level
#>    <chr>    <chr>            <chr>        <chr>           <chr>      <chr>      
#>  1 DUS MOCK summarised_char… PatientProf… 0.6.0           cohort_na… acetaminop…
#>  2 DUS MOCK summarised_char… PatientProf… 0.6.0           cohort_na… acetaminop…
#>  3 DUS MOCK summarised_char… PatientProf… 0.6.0           cohort_na… acetaminop…
#>  4 DUS MOCK summarised_char… PatientProf… 0.6.0           cohort_na… acetaminop…
#>  5 DUS MOCK summarised_char… PatientProf… 0.6.0           cohort_na… acetaminop…
#>  6 DUS MOCK summarised_char… PatientProf… 0.6.0           cohort_na… acetaminop…
#>  7 DUS MOCK summarised_char… PatientProf… 0.6.0           cohort_na… acetaminop…
#>  8 DUS MOCK summarised_char… PatientProf… 0.6.0           cohort_na… acetaminop…
#>  9 DUS MOCK summarised_char… PatientProf… 0.6.0           cohort_na… acetaminop…
#> 10 DUS MOCK summarised_char… PatientProf… 0.6.0           cohort_na… acetaminop…
#> # ℹ 56 more rows
#> # ℹ 9 more variables: strata_name <chr>, strata_level <chr>,
#> #   variable_name <chr>, variable_level <chr>, estimate_name <chr>,
#> #   estimate_type <chr>, estimate_value <chr>, additional_name <chr>,
#> #   additional_level <chr>
```

### Summarise patients large scale characteristics

You can summarise the patient characteristics with
`summariseLargeScaleCharacteristics` function:

``` r
summariseLargeScaleCharacteristics(
  cohort = x, 
  window = list(c(-Inf, Inf)), 
  eventInWindow = "condition_occurrence", 
  episodeInWindow = "drug_exposure"
)
#> # A tibble: 26 × 15
#>    cdm_name result_type      package_name package_version group_name group_level
#>    <chr>    <chr>            <chr>        <chr>           <chr>      <chr>      
#>  1 DUS MOCK summarised_larg… PatientProf… 0.6.0           cohort_na… acetaminop…
#>  2 DUS MOCK summarised_larg… PatientProf… 0.6.0           cohort_na… acetaminop…
#>  3 DUS MOCK summarised_larg… PatientProf… 0.6.0           cohort_na… acetaminop…
#>  4 DUS MOCK summarised_larg… PatientProf… 0.6.0           cohort_na… acetaminop…
#>  5 DUS MOCK summarised_larg… PatientProf… 0.6.0           cohort_na… acetaminop…
#>  6 DUS MOCK summarised_larg… PatientProf… 0.6.0           cohort_na… acetaminop…
#>  7 DUS MOCK summarised_larg… PatientProf… 0.6.0           cohort_na… acetaminop…
#>  8 DUS MOCK summarised_larg… PatientProf… 0.6.0           cohort_na… acetaminop…
#>  9 DUS MOCK summarised_larg… PatientProf… 0.6.0           cohort_na… acetaminop…
#> 10 DUS MOCK summarised_larg… PatientProf… 0.6.0           cohort_na… acetaminop…
#> # ℹ 16 more rows
#> # ℹ 9 more variables: strata_name <chr>, strata_level <chr>,
#> #   variable_name <chr>, variable_level <chr>, estimate_name <chr>,
#> #   estimate_type <chr>, estimate_value <chr>, additional_name <chr>,
#> #   additional_level <chr>
```

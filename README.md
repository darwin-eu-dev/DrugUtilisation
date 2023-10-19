
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
conceptList <- getDrugIngredientCodes(cdm, "acetaminophen")
conceptList
#> $`Ingredient: acetaminophen (1125315)`
#> [1]  1125315  1125360  2905077 43135274
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
#> [1] "GeneratedCohortSet"    "tbl_duckdb_connection" "tbl_dbi"              
#> [4] "tbl_sql"               "tbl_lazy"              "tbl"
```

Cohort set:

``` r
library(CDMConnector)
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
cohortSet(cdm[["dus_cohort"]]) %>% glimpse()
#> Rows: 1
#> Columns: 11
#> $ cohort_definition_id    <int> 1
#> $ cohort_name             <chr> "Ingredient: acetaminophen (1125315)"
#> $ limit                   <chr> "first"
#> $ prior_observation       <chr> "365"
#> $ gap_era                 <chr> "30"
#> $ prior_use_washout       <chr> "0"
#> $ cohort_date_range_start <chr> NA
#> $ cohort_date_range_end   <chr> NA
#> $ impute_duration         <chr> "none"
#> $ duration_range_min      <chr> "0"
#> $ duration_range_max      <chr> "Inf"
```

Cohort count:

``` r
cohortCount(cdm[["dus_cohort"]])
#> # A tibble: 1 × 3
#>   cohort_definition_id number_records number_subjects
#>                  <int>          <dbl>           <dbl>
#> 1                    1             35              35
```

Cohort attrition:

``` r
cohortAttrition(cdm[["dus_cohort"]]) %>% glimpse()
#> Rows: 8
#> Columns: 7
#> $ cohort_definition_id <int> 1, 1, 1, 1, 1, 1, 1, 1
#> $ number_records       <dbl> 90, 90, 70, 70, 41, 41, 41, 35
#> $ number_subjects      <dbl> 62, 62, 62, 62, 35, 35, 35, 35
#> $ reason_id            <dbl> 1, 2, 3, 4, 5, 6, 7, 8
#> $ reason               <chr> "Qualifying initial records", "Duration imputatio…
#> $ excluded_records     <dbl> 0, 0, 20, 0, 29, 0, 0, 6
#> $ excluded_subjects    <dbl> 0, 0, 0, 0, 27, 0, 0, 0
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
#>                  <int>          <dbl>           <dbl>
#> 1                    2             46              46
#> 2                    1             52              52
```

Then we can add the indication using the function `addIndication`. That
will add a new column for each indication gap and indication.

``` r
x <- cdm[["dus_cohort"]] %>%
  addIndication(
    cdm = cdm, indicationCohortName = "indications_cohort", indicationGap = c(0, 30, 365), 
    unknownIndicationTable = c("condition_occurrence")
  )
glimpse(x)
#> Rows: ??
#> Columns: 16
#> Database: DuckDB 0.9.0 [martics@Windows 10 x64:R 4.2.3/:memory:]
#> $ cohort_definition_id         <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,…
#> $ subject_id                   <dbl> 7, 48, 57, 77, 3, 58, 73, 23, 40, 45, 51,…
#> $ cohort_start_date            <date> 1969-09-09, 1972-04-14, 2020-04-17, 1967…
#> $ cohort_end_date              <date> 1970-02-14, 1972-12-18, 2020-11-19, 1967…
#> $ indication_gap_0_influenza   <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
#> $ indication_gap_0_headache    <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
#> $ indication_gap_0_none        <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,…
#> $ indication_gap_30_headache   <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
#> $ indication_gap_30_influenza  <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
#> $ indication_gap_30_none       <dbl> 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,…
#> $ indication_gap_365_headache  <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0,…
#> $ indication_gap_365_influenza <dbl> 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
#> $ indication_gap_365_none      <dbl> 1, 1, 0, 0, 1, 1, 0, 1, 1, 1, 0, 1, 1, 1,…
#> $ indication_gap_0_unknown     <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
#> $ indication_gap_30_unknown    <dbl> 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
#> $ indication_gap_365_unknown   <dbl> 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0,…
```

We can combine the indications in a single column using the
`indicationToStrata()` function. This column can be used as
stratification of the results if needed:

``` r
x <- x %>% indicationToStrata(keep = TRUE)
glimpse(x)
#> Rows: ??
#> Columns: 19
#> Database: DuckDB 0.9.0 [martics@Windows 10 x64:R 4.2.3/:memory:]
#> $ cohort_definition_id         <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,…
#> $ subject_id                   <dbl> 7, 48, 57, 77, 3, 58, 73, 23, 40, 45, 51,…
#> $ cohort_start_date            <date> 1969-09-09, 1972-04-14, 2020-04-17, 1967…
#> $ cohort_end_date              <date> 1970-02-14, 1972-12-18, 2020-11-19, 1967…
#> $ indication_gap_0_influenza   <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
#> $ indication_gap_0_headache    <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
#> $ indication_gap_0_none        <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,…
#> $ indication_gap_30_headache   <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
#> $ indication_gap_30_influenza  <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
#> $ indication_gap_30_none       <dbl> 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,…
#> $ indication_gap_365_headache  <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0,…
#> $ indication_gap_365_influenza <dbl> 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
#> $ indication_gap_365_none      <dbl> 1, 1, 0, 0, 1, 1, 0, 1, 1, 1, 0, 1, 1, 1,…
#> $ indication_gap_0_unknown     <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
#> $ indication_gap_30_unknown    <dbl> 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
#> $ indication_gap_365_unknown   <dbl> 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0,…
#> $ indication_gap_0             <chr> "None", "None", "None", "None", "None", "…
#> $ indication_gap_30            <chr> "None", "None", "None", "Unknown", "None"…
#> $ indication_gap_365           <chr> "None", "None", "Influenza", "Unknown", "…
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
#> # A tibble: 40 × 11
#>    group_name  group_level      strata_name strata_level variable variable_level
#>    <chr>       <chr>            <chr>       <chr>        <chr>    <chr>         
#>  1 cohort_name Ingredient: ace… Overall     Overall      number … <NA>          
#>  2 cohort_name Ingredient: ace… Overall     Overall      number … <NA>          
#>  3 cohort_name Ingredient: ace… Overall     Overall      Indicat… Influenza     
#>  4 cohort_name Ingredient: ace… Overall     Overall      Indicat… Influenza     
#>  5 cohort_name Ingredient: ace… Overall     Overall      Indicat… Headache      
#>  6 cohort_name Ingredient: ace… Overall     Overall      Indicat… Headache      
#>  7 cohort_name Ingredient: ace… Overall     Overall      Indicat… None          
#>  8 cohort_name Ingredient: ace… Overall     Overall      Indicat… None          
#>  9 cohort_name Ingredient: ace… Overall     Overall      Indicat… Headache      
#> 10 cohort_name Ingredient: ace… Overall     Overall      Indicat… Headache      
#> # ℹ 30 more rows
#> # ℹ 5 more variables: variable_type <chr>, estimate_type <chr>, estimate <chr>,
#> #   cdm_name <chr>, result_type <chr>
```

``` r
summariseIndication(x, cdm) %>% glimpse()
#> Rows: 40
#> Columns: 11
#> $ group_name     <chr> "cohort_name", "cohort_name", "cohort_name", "cohort_na…
#> $ group_level    <chr> "Ingredient: acetaminophen (1125315)", "Ingredient: ace…
#> $ strata_name    <chr> "Overall", "Overall", "Overall", "Overall", "Overall", …
#> $ strata_level   <chr> "Overall", "Overall", "Overall", "Overall", "Overall", …
#> $ variable       <chr> "number subjects", "number records", "Indication on ind…
#> $ variable_level <chr> NA, NA, "Influenza", "Influenza", "Headache", "Headache…
#> $ variable_type  <chr> NA, NA, "binary", "binary", "binary", "binary", "binary…
#> $ estimate_type  <chr> "count", "count", "count", "percentage", "count", "perc…
#> $ estimate       <chr> "35", "35", "0", "0", "0", "0", "35", "100", "0", "0", …
#> $ cdm_name       <chr> "DUS MOCK", "DUS MOCK", "DUS MOCK", "DUS MOCK", "DUS MO…
#> $ result_type    <chr> "Summary indication", "Summary indication", "Summary in…
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
summariseIndication(x, cdm, strata = list("age_group", "sex", c("age_group", "sex")))
#> # A tibble: 440 × 11
#>    group_name  group_level      strata_name strata_level variable variable_level
#>    <chr>       <chr>            <chr>       <chr>        <chr>    <chr>         
#>  1 cohort_name Ingredient: ace… Overall     Overall      number … <NA>          
#>  2 cohort_name Ingredient: ace… Overall     Overall      number … <NA>          
#>  3 cohort_name Ingredient: ace… Overall     Overall      Indicat… Influenza     
#>  4 cohort_name Ingredient: ace… Overall     Overall      Indicat… Influenza     
#>  5 cohort_name Ingredient: ace… Overall     Overall      Indicat… Headache      
#>  6 cohort_name Ingredient: ace… Overall     Overall      Indicat… Headache      
#>  7 cohort_name Ingredient: ace… Overall     Overall      Indicat… None          
#>  8 cohort_name Ingredient: ace… Overall     Overall      Indicat… None          
#>  9 cohort_name Ingredient: ace… Overall     Overall      Indicat… Headache      
#> 10 cohort_name Ingredient: ace… Overall     Overall      Indicat… Headache      
#> # ℹ 430 more rows
#> # ℹ 5 more variables: variable_type <chr>, estimate_type <chr>, estimate <chr>,
#> #   cdm_name <chr>, result_type <chr>
```

``` r
summariseIndication(x, cdm, strata = list("age_group", "sex", c("age_group", "sex"))) %>% glimpse()
#> Rows: 440
#> Columns: 11
#> $ group_name     <chr> "cohort_name", "cohort_name", "cohort_name", "cohort_na…
#> $ group_level    <chr> "Ingredient: acetaminophen (1125315)", "Ingredient: ace…
#> $ strata_name    <chr> "Overall", "Overall", "Overall", "Overall", "Overall", …
#> $ strata_level   <chr> "Overall", "Overall", "Overall", "Overall", "Overall", …
#> $ variable       <chr> "number subjects", "number records", "Indication on ind…
#> $ variable_level <chr> NA, NA, "Influenza", "Influenza", "Headache", "Headache…
#> $ variable_type  <chr> NA, NA, "binary", "binary", "binary", "binary", "binary…
#> $ estimate_type  <chr> "count", "count", "count", "percentage", "count", "perc…
#> $ estimate       <chr> "35", "35", "0", "0", "0", "0", "35", "100", "0", "0", …
#> $ cdm_name       <chr> "DUS MOCK", "DUS MOCK", "DUS MOCK", "DUS MOCK", "DUS MO…
#> $ result_type    <chr> "Summary indication", "Summary indication", "Summary in…
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
#> # A tibble: 46 × 11
#>    cdm_name result_type group_name group_level strata_name strata_level variable
#>    <chr>    <chr>       <chr>      <chr>       <chr>       <chr>        <chr>   
#>  1 DUS MOCK Summary ch… cohort_na… Ingredient… Overall     Overall      Number …
#>  2 DUS MOCK Summary ch… cohort_na… Ingredient… Overall     Overall      Number …
#>  3 DUS MOCK Summary ch… cohort_na… Ingredient… Overall     Overall      Cohort …
#>  4 DUS MOCK Summary ch… cohort_na… Ingredient… Overall     Overall      Cohort …
#>  5 DUS MOCK Summary ch… cohort_na… Ingredient… Overall     Overall      Cohort …
#>  6 DUS MOCK Summary ch… cohort_na… Ingredient… Overall     Overall      Cohort …
#>  7 DUS MOCK Summary ch… cohort_na… Ingredient… Overall     Overall      Cohort …
#>  8 DUS MOCK Summary ch… cohort_na… Ingredient… Overall     Overall      Cohort …
#>  9 DUS MOCK Summary ch… cohort_na… Ingredient… Overall     Overall      Cohort …
#> 10 DUS MOCK Summary ch… cohort_na… Ingredient… Overall     Overall      Cohort …
#> # ℹ 36 more rows
#> # ℹ 4 more variables: variable_level <chr>, variable_type <chr>,
#> #   estimate_type <chr>, estimate <chr>
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
#> Warning in checkNewName(ageName, x): age already exists in x, it was renamed to
#> age_1
#> Warning in checkNewName(sexName, x): sex already exists in x, it was renamed to
#> sex_1
#> # A tibble: 26 × 14
#>    result_type          cdm_name group_name group_level strata_name strata_level
#>    <chr>                <chr>    <chr>      <chr>       <chr>       <chr>       
#>  1 Summarised Large Sc… DUS MOCK Cohort na… Ingredient… Overall     Overall     
#>  2 Summarised Large Sc… DUS MOCK Cohort na… Ingredient… Overall     Overall     
#>  3 Summarised Large Sc… DUS MOCK Cohort na… Ingredient… Overall     Overall     
#>  4 Summarised Large Sc… DUS MOCK Cohort na… Ingredient… Overall     Overall     
#>  5 Summarised Large Sc… DUS MOCK Cohort na… Ingredient… Overall     Overall     
#>  6 Summarised Large Sc… DUS MOCK Cohort na… Ingredient… Overall     Overall     
#>  7 Summarised Large Sc… DUS MOCK Cohort na… Ingredient… Overall     Overall     
#>  8 Summarised Large Sc… DUS MOCK Cohort na… Ingredient… Overall     Overall     
#>  9 Summarised Large Sc… DUS MOCK Cohort na… Ingredient… Overall     Overall     
#> 10 Summarised Large Sc… DUS MOCK Cohort na… Ingredient… Overall     Overall     
#> # ℹ 16 more rows
#> # ℹ 8 more variables: table_name <chr>, type <chr>, analysis <chr>,
#> #   concept <dbl>, variable <chr>, variable_level <chr>, estimate_type <chr>,
#> #   estimate <chr>
```

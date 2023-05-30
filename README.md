
<!-- README.md is generated from README.Rmd. Please edit that file -->

# DrugUtilisation <img src='man/figures/hex-HELP WANTED.png' align="right" height="139"/>

[![CRANstatus](https://www.r-pkg.org/badges/version/DrugUtilisation)](https://CRAN.R-project.org/package=DrugUtilisation)
[![codecov.io](https://codecov.io/github/darwin-eu-dev/DrugUtilisation/coverage.svg?branch=main)](https://app.codecov.io/github/darwin-eu/DrugUtilisation?branch=main)
[![R-CMD-check](https://github.com/darwin-eu-dev/DrugUtilisation/workflows/R-CMD-check/badge.svg)](https://github.com/darwin-eu-dev/DrugUtilisation/actions)
[![Lifecycle:Experimental](https://img.shields.io/badge/Lifecycle-Experimental-339999)](https://lifecycle.r-lib.org/articles/stages.html)

## WARNING: This package is under development.

- [addDailyDose](https://github.com/darwin-eu-dev/DrugUtilisation/R/addDailyDose.R)
  function works for the following patterns in the drug_strength table:

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

- Create DrugUsilisation cohorts

- Add indications to this cohort

- Add the dosage of a certain ingredient (subseted for a list of drugs)

- Calculate the daily dose

- Create Concept based cohorts

- Read concepts from json files

- Summarise the drug use in a certain cohort

- Summarise the indications in a certain cohort

- Summarise the patients characteristics in a certain cohort

- Summarise the patients large scale characterics in a certain cohort

## Package installation

You can install the this version of DrugUtilisation using the following
command:

``` r
install.packages("remotes")
remotes::install_github("darwin-eu-dev/DrugUtilisation")
```

When working with DrugUtilisation, you will use CDMConnector to manage
your connection to the database. If you don´t already have this
installed you can install it from
[CRAN](https://CRAN.R-project.org/package=CDMConnector).

``` r
install.packages("CDMConnector")
```

## Example

First, we need to create a cdm reference for the data we´ll be using.
Here we´ll generate an example with simulated data, but to see how you
would set this up for your database please consult the CDMConnector
package [connection
examples](https://darwin-eu.github.io/CDMConnector/articles/DBI_connection_examples.html).

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
#> $`Ingredient: acetaminophen (1125315)`
#> [1]  1125315 43135274  2905077  1125360
```

To generate the cohort of drug use we will use
`generateDrugUtilisationCohortSet`:

``` r
cdm <- generateDrugUtilisationCohortSet(
  cdm = cdm,
  name = "dus_cohort",
  conceptSetList = conceptList,
  summariseMode = "FirstEra", 
  daysPriorHistory = 365,
  gapEra = 30,
  priorUseWashout = 0,
  imputeDuration = "eliminate", 
  durationRange = c(0, Inf)
)
```

#### Cohort attributes

The generated cohort will have the `GeneratedCohortSet` as seen in
CDMConnector

``` r
class(cdm$dus_cohort)
#> [1] "GeneratedCohortSet"    "tbl_duckdb_connection" "tbl_dbi"              
#> [4] "tbl_sql"               "tbl_lazy"              "tbl"
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
cohortSet(cdm$dus_cohort) %>% glimpse()
#> Rows: 1
#> Columns: 12
#> $ cohort_definition_id    <int> 1
#> $ cohort_name             <chr> "Ingredient: acetaminophen (1125315)"
#> $ summarise_mode          <chr> "FirstEra"
#> $ fixed_time              <dbl> NA
#> $ days_prior_history      <dbl> 365
#> $ gap_era                 <dbl> 30
#> $ prior_use_washout       <dbl> 0
#> $ cohort_date_range_start <date> NA
#> $ cohort_date_range_end   <date> NA
#> $ impute_duration         <chr> "eliminate"
#> $ duration_range_min      <dbl> 0
#> $ duration_range_max      <dbl> Inf
```

Cohort count:

``` r
cohortCount(cdm$dus_cohort)
#> # A tibble: 1 × 3
#>   cohort_definition_id number_records number_subjects
#>                  <int>          <dbl>           <dbl>
#> 1                    1             46              46
```

Cohort attrition:

``` r
cohortAttrition(cdm$dus_cohort) %>% glimpse()
#> Rows: 8
#> Columns: 7
#> $ cohort_definition_id <int> 1, 1, 1, 1, 1, 1, 1, 1
#> $ number_records       <dbl> 109, 109, 83, 52, 52, 52, 52, 46
#> $ number_subjects      <dbl> 69, 69, 69, 46, 46, 46, 46, 46
#> $ reason_id            <dbl> 1, 2, 3, 4, 5, 6, 7, 8
#> $ reason               <chr> "Qualifying initial records", "Duration imputatio…
#> $ excluded_records     <dbl> 0, 0, 26, 31, 0, 0, 0, 6
#> $ excluded_subjects    <dbl> 0, 0, 0, 23, 0, 0, 0, 0
```

### Indication

Indications will always be cohorts. An option that the package has is to
create concept based cohorts using `generateConceptCohortSet`.

``` r
indications <- list(headache = 378253, influenza = 4266367)
cdm <- generateConceptCohortSet(cdm, "indications_cohort", indications)
cohortCount(cdm$indications_cohort)
#> # A tibble: 2 × 3
#>   cohort_definition_id number_records number_subjects
#>                  <int>          <dbl>           <dbl>
#> 1                    1             71              59
#> 2                    2             59              52
```

Then we can add the indication using the function `addIndication`. That
will add a new column for each indication gap. Indications are non
exclusive so a person can have multiple indications. Multiple
indications will be separated by the `&&` symbol:

``` r
x <- cdm$dus_cohort %>%
  addIndication(cdm, "indications_cohort", c(0, 30, 365), c("condition_occurrence"))
glimpse(x)
#> Rows: ??
#> Columns: 7
#> Database: DuckDB 0.7.1 [martics@Windows 10 x64:R 4.2.1/:memory:]
#> $ cohort_definition_id <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1…
#> $ subject_id           <dbl> 37, 61, 81, 84, 60, 47, 70, 90, 96, 75, 93, 46, 6…
#> $ cohort_start_date    <date> 2005-02-07, 2019-01-22, 1995-09-13, 2011-03-13, …
#> $ cohort_end_date      <date> 2005-04-15, 2019-03-27, 1997-11-17, 2013-04-26, …
#> $ indication_gap_0     <chr> "no indication", "no indication", "no indication"…
#> $ indication_gap_30    <chr> "no indication", "no indication", "no indication"…
#> $ indication_gap_365   <chr> "no indication", "no indication", "no indication"…
```

``` r
table(x %>% pull("indication_gap_0"))
#> 
#> no indication 
#>            46
```

``` r
table(x %>% pull("indication_gap_30"))
#> 
#> no indication 
#>            46
```

``` r
table(x %>% pull("indication_gap_365"))
#> 
#>            headache           influenza influenza&&headache       no indication 
#>                   1                   4                   1                  35 
#>  unknown indication 
#>                   5
```

### Summarise the indication

We can summarise the indication results using the `summariseIndication`
function:

``` r
summariseIndication(x, cdm)
#> # A tibble: 6 × 10
#>   cohort_name      strata_name strata_level indication_gap indication_name count
#>   <chr>            <chr>       <chr>                 <dbl> <chr>           <chr>
#> 1 Ingredient: ace… overall     <NA>                      0 no indication   "46" 
#> 2 Ingredient: ace… overall     <NA>                     30 no indication   "46" 
#> 3 Ingredient: ace… overall     <NA>                    365 headache        "<5" 
#> 4 Ingredient: ace… overall     <NA>                    365 influenza       " 5" 
#> 5 Ingredient: ace… overall     <NA>                    365 no indication   "35" 
#> 6 Ingredient: ace… overall     <NA>                    365 unknown indica… " 5" 
#> # ℹ 4 more variables: denominator <chr>, `%` <chr>, cdm_name <chr>,
#> #   generated_by <chr>
```

``` r
summariseIndication(x, cdm) %>% glimpse()
#> Rows: 6
#> Columns: 10
#> $ cohort_name     <chr> "Ingredient: acetaminophen (1125315)", "Ingredient: ac…
#> $ strata_name     <chr> "overall", "overall", "overall", "overall", "overall",…
#> $ strata_level    <chr> NA, NA, NA, NA, NA, NA
#> $ indication_gap  <dbl> 0, 30, 365, 365, 365, 365
#> $ indication_name <chr> "no indication", "no indication", "headache", "influen…
#> $ count           <chr> "46", "46", "<5", " 5", "35", " 5"
#> $ denominator     <chr> "46", "46", "46", "46", "46", "46"
#> $ `%`             <chr> "100%", "100%", " 4.35%", "10.87%", "76.09%", "10.87%"
#> $ cdm_name        <chr> NA, NA, NA, NA, NA, NA
#> $ generated_by    <chr> "DrugUtilisation_v0.2.0_summariseIndication", "DrugUti…
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
summariseIndication(x, cdm, strata = list("age" = "age_group", "sex" = "sex", "age & sex" = c("age_group", "sex")))
#> # A tibble: 84 × 10
#>    cohort_name     strata_name strata_level indication_gap indication_name count
#>    <chr>           <chr>       <chr>                 <dbl> <chr>           <chr>
#>  1 Ingredient: ac… age         0 to 19                   0 no indication   <NA> 
#>  2 Ingredient: ac… age         0 to 19                  30 no indication   <NA> 
#>  3 Ingredient: ac… age         0 to 19                 365 influenza       <NA> 
#>  4 Ingredient: ac… age         0 to 19                 365 no indication   <NA> 
#>  5 Ingredient: ac… age         0 to 19                 365 unknown indica… <NA> 
#>  6 Ingredient: ac… age         0 to 19                 365 headache        <NA> 
#>  7 Ingredient: ac… age         20 to 39                  0 no indication   <NA> 
#>  8 Ingredient: ac… age         20 to 39                 30 no indication   <NA> 
#>  9 Ingredient: ac… age         20 to 39                365 headache        <NA> 
#> 10 Ingredient: ac… age         20 to 39                365 influenza       <NA> 
#> # ℹ 74 more rows
#> # ℹ 4 more variables: denominator <chr>, `%` <chr>, cdm_name <chr>,
#> #   generated_by <chr>
```

``` r
summariseIndication(x, cdm, strata = list("age" = "age_group", "sex" = "sex", "age & sex" = c("age_group", "sex"))) %>% glimpse()
#> Rows: 84
#> Columns: 10
#> $ cohort_name     <chr> "Ingredient: acetaminophen (1125315)", "Ingredient: ac…
#> $ strata_name     <chr> "age", "age", "age", "age", "age", "age", "age", "age"…
#> $ strata_level    <chr> "0 to 19", "0 to 19", "0 to 19", "0 to 19", "0 to 19",…
#> $ indication_gap  <dbl> 0, 30, 365, 365, 365, 365, 0, 30, 365, 365, 365, 365, …
#> $ indication_name <chr> "no indication", "no indication", "influenza", "no ind…
#> $ count           <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, "<5", …
#> $ denominator     <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, "<5", …
#> $ `%`             <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, "100%"…
#> $ cdm_name        <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
#> $ generated_by    <chr> "DrugUtilisation_v0.2.0_summariseIndication", "DrugUti…
```

### Daily dose

We can compute daily dose for a certain ingredient from a subset of
drug_exposure or the whole drug exposure (can be very computationally
expensive).

``` r
#cdm$drug_exposure %>%
#  addDailyDose(cdm, ingredientConceptId = 1125315) %>%
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

You can summarise the patient characteristics with `summariseTableOne`
function:

``` r
summariseTableOne(
  x, cdm, ageGroup = list(c(0, 24), c(25, 49), c(50, 74), c(75, 150)),
  windowVisitOcurrence = c(-365, 0),
  covariates = list("indications_cohort" = c(-365, 0))
)
#> # A tibble: 34 × 9
#>    cohort_name strata_name strata_level variable variable_classificat…¹ estimate
#>    <chr>       <chr>       <chr>        <chr>    <chr>                  <chr>   
#>  1 Ingredient… overall     <NA>         number … <NA>                   count   
#>  2 Ingredient… overall     <NA>         number … <NA>                   count   
#>  3 Ingredient… overall     <NA>         age      numeric                median  
#>  4 Ingredient… overall     <NA>         age      numeric                q25     
#>  5 Ingredient… overall     <NA>         age      numeric                q75     
#>  6 Ingredient… overall     <NA>         future_… numeric                median  
#>  7 Ingredient… overall     <NA>         future_… numeric                q25     
#>  8 Ingredient… overall     <NA>         future_… numeric                q75     
#>  9 Ingredient… overall     <NA>         prior_h… numeric                median  
#> 10 Ingredient… overall     <NA>         prior_h… numeric                q25     
#> # ℹ 24 more rows
#> # ℹ abbreviated name: ¹​variable_classification
#> # ℹ 3 more variables: value <chr>, cdm_name <chr>, generated_by <chr>
```

### Summarise patients large scale characteristics

You can summarise the patient characteristics with `summariseTableOne`
function:

``` r
summariseLargeScaleCharacteristics(
  x, cdm, tablesToCharacterize = c("drug_exposure", "condition_occurrence")
)
#> # A tibble: 124 × 12
#>    cohort_name        strata_name strata_level table_name window_name concept_id
#>    <chr>              <chr>       <chr>        <chr>      <chr>            <dbl>
#>  1 Ingredient: aceta… overall     <NA>         condition… -inf to -3…     317009
#>  2 Ingredient: aceta… overall     <NA>         condition… -inf to -3…     378253
#>  3 Ingredient: aceta… overall     <NA>         condition… -inf to -3…    4266367
#>  4 Ingredient: aceta… overall     <NA>         condition… -365 to -91     317009
#>  5 Ingredient: aceta… overall     <NA>         condition… -365 to -91     378253
#>  6 Ingredient: aceta… overall     <NA>         condition… -365 to -91    4266367
#>  7 Ingredient: aceta… overall     <NA>         condition… -365 to -31     317009
#>  8 Ingredient: aceta… overall     <NA>         condition… -365 to -31     378253
#>  9 Ingredient: aceta… overall     <NA>         condition… -365 to -31    4266367
#> 10 Ingredient: aceta… overall     <NA>         condition… -90 to -1       317009
#> # ℹ 114 more rows
#> # ℹ 6 more variables: concept_name <chr>, count <chr>, denominator_count <chr>,
#> #   `%` <dbl>, cdm_name <chr>, generated_by <chr>
```

## ReportGenerator

This package is included in the `ReportGenerator` environment. See
exortable elements:

``` r
DrugUtilisation::ReportGenerator
```

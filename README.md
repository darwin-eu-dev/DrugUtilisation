
<!-- README.md is generated from README.Rmd. Please edit that file -->

# DrugUtilisation <img src="man/figures/logo.png" align="right" height="200"/>

[![CRANstatus](https://www.r-pkg.org/badges/version/DrugUtilisation)](https://CRAN.R-project.org/package=DrugUtilisation)
[![codecov.io](https://codecov.io/github/darwin-eu/DrugUtilisation/coverage.svg?branch=main)](https://app.codecov.io/github/darwin-eu/DrugUtilisation?branch=main)
[![R-CMD-check](https://github.com/darwin-eu/DrugUtilisation/workflows/R-CMD-check/badge.svg)](https://github.com/darwin-eu/DrugUtilisation/actions)
[![Lifecycle:Experimental](https://img.shields.io/badge/Lifecycle-Experimental-339999)](https://lifecycle.r-lib.org/articles/stages.html)

## Package overview

DrugUtilisation contains functions to instantiate and characterise drug
cohorts in data mapped to the OMOP Common Data Model. The package
supports:

- Creation of drug cohorts

- Identification of indications for those in a drug cohort

- Summarising drug utilisation among a cohort in terms of duration,
  quantity, and dose

- Description of treatment adherence based on proportion of patients
  covered

- Detailing treatment restart and switching after an initial treatment
  discontinuation

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

cdm <- mockDrugUtilisation(numberIndividual = 100)
```

### Create a cohort of acetaminophen users

To generate the cohort of acetaminophen users we will use
`generateIngredientCohortSet` and only include the first record per
person and require that they have at least 30 days observation in the
database prior to their drug start date.

``` r
cdm <- generateIngredientCohortSet(
  cdm = cdm,
  name = "dus_cohort",
  ingredient = "acetaminophen", 
  gapEra = 7
)
cdm$dus_cohort |> 
  requireIsFirstDrugEntry() |> 
  requireObservationBeforeDrug(priorObservation = 30)
#> # Source:   table<main.dus_cohort> [?? x 4]
#> # Database: DuckDB v1.0.0 [eburn@Windows 10 x64:R 4.2.1/:memory:]
#>    cohort_definition_id subject_id cohort_start_date cohort_end_date
#>                   <int>      <int> <date>            <date>         
#>  1                    1         27 2019-05-22        2020-02-05     
#>  2                    1         81 1995-09-13        1997-11-17     
#>  3                    1         88 2015-08-19        2016-05-09     
#>  4                    1        100 2022-12-01        2022-12-02     
#>  5                    1         45 2017-07-03        2018-07-06     
#>  6                    1         95 2001-06-13        2010-01-28     
#>  7                    1         19 2021-08-28        2022-08-26     
#>  8                    1         28 2018-08-05        2020-02-12     
#>  9                    1         40 2018-11-15        2020-03-07     
#> 10                    1         14 1990-04-27        1990-05-12     
#> # ℹ more rows
```

### Indications of acetaminophen users

Now we´ve created our cohort we could first summarise the indications of
the cohort. These indications will always be cohorts, so we first need
to create them. Here we create two indication cohorts, one for headache
and the other for influenza.

``` r
indications <- list(headache = 378253, influenza = 4266367)
cdm <- generateConceptCohortSet(cdm, 
                                conceptSet = indications, 
                                name = "indications_cohort")
```

We can summarise the indication results using the `summariseIndication`
function:

``` r
indication_summary <- cdm$dus_cohort |> 
  summariseIndication(indicationCohortName = "indications_cohort", 
                      unknownIndicationTable = "condition_occurrence",
                      indicationWindow = list(c(-30, 0)))
#> Warning: Your SQL query is over 10,000 characters which can cause issues on some database platforms!
#> Try calling computeQuery earlier in your pipeline.
#> Your SQL query is over 10,000 characters which can cause issues on some database platforms!
#> Try calling computeQuery earlier in your pipeline.
#> ℹ The following estimates will be computed:
#> • indication_m30_to_0: count, percentage
#> → Start summary of data, at 2024-07-23 21:27:56
#> 
#> ✔ Summary finished, at 2024-07-23 21:27:56
indication_summary |> glimpse()
#> Rows: 8
#> Columns: 13
#> $ result_id        <int> 1, 1, 1, 1, 1, 1, 1, 1
#> $ cdm_name         <chr> "DUS MOCK", "DUS MOCK", "DUS MOCK", "DUS MOCK", "DUS …
#> $ group_name       <chr> "cohort_name", "cohort_name", "cohort_name", "cohort_…
#> $ group_level      <chr> "161_acetaminophen", "161_acetaminophen", "161_acetam…
#> $ strata_name      <chr> "overall", "overall", "overall", "overall", "overall"…
#> $ strata_level     <chr> "overall", "overall", "overall", "overall", "overall"…
#> $ variable_name    <chr> "number records", "number subjects", "Indication time…
#> $ variable_level   <chr> NA, NA, "Influenza", "None", "Unknown", "Influenza", …
#> $ estimate_name    <chr> "count", "count", "count", "count", "count", "percent…
#> $ estimate_type    <chr> "integer", "integer", "integer", "integer", "integer"…
#> $ estimate_value   <chr> "58", "58", "2", "38", "18", "3.44827586206897", "65.…
#> $ additional_name  <chr> "overall", "overall", "overall", "overall", "overall"…
#> $ additional_level <chr> "overall", "overall", "overall", "overall", "overall"…
```

### Drug use

``` r
drug_utilisation_summary <- cdm$dus_cohort |> 
  summariseDrugUtilisation(ingredientConceptId = 1125315)
#> Warning: Your SQL query is over 10,000 characters which can cause issues on some database platforms!
#> Try calling computeQuery earlier in your pipeline.
drug_utilisation_summary |> glimpse()
#> Rows: 58
#> Columns: 13
#> $ result_id        <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,…
#> $ cdm_name         <chr> "DUS MOCK", "DUS MOCK", "DUS MOCK", "DUS MOCK", "DUS …
#> $ group_name       <chr> "cohort_name", "cohort_name", "cohort_name", "cohort_…
#> $ group_level      <chr> "161_acetaminophen", "161_acetaminophen", "161_acetam…
#> $ strata_name      <chr> "overall", "overall", "overall", "overall", "overall"…
#> $ strata_level     <chr> "overall", "overall", "overall", "overall", "overall"…
#> $ variable_name    <chr> "number records", "number subjects", "number exposure…
#> $ variable_level   <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
#> $ estimate_name    <chr> "count", "count", "q25", "median", "q75", "mean", "sd…
#> $ estimate_type    <chr> "integer", "integer", "integer", "integer", "integer"…
#> $ estimate_value   <chr> "58", "58", "1", "1", "1", "1.29310344827586", "0.649…
#> $ additional_name  <chr> "overall", "overall", "concept_set", "concept_set", "…
#> $ additional_level <chr> "overall", "overall", "ingredient_1125315_descendants…
```

## Further analyses

There are many more drug-related analyses that we could do with this
acetaminophen cohort or others. Please see the package vignettes for
more details.


<!-- README.md is generated from README.Rmd. Please edit that file -->

# DrugUtilisation

<!-- badges: start -->

[![R-CMD-check](https://github.com/darwin-eu/DrugUtilisation/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/darwin-eu/DrugUtilisation/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/darwin-eu/DrugUtilisation/branch/main/graph/badge.svg)](https://app.codecov.io/gh/darwin-eu/DrugUtilisation?branch=main)
<!-- badges: end -->

## Package overview

DrugUtilisation contains functions to obtain the instantiate and
characterize the cohorts used in a drug Utilisation study using the OMOP
common data model. Drug utilisation studies examine the marketing,
distribution, prescription and use of drugs in a society, with special
emphasis on the resulting medical, social and economic consequences.

## Package installation

You can install the development version of DrugUtilisation like so:

``` r
install.packages("remotes")
remotes::install_github("darwin-eu/DrugUtilisation")
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
package documentation.

``` r
library(CDMConnector)
library(DrugUtilisation)

# We first need to create a cdm_reference
cdm <- mockDrugUtilisationRef()
# and this is what this example data looks like
head(cdm$person)
head(cdm$observation_period)
head(cdm$drug_exposure)
head(cdm$drug_strength)
```

To create, instantiate and characterize the drug utilisation cohorts we
have to specify an ingredient (ingredientConceptId):

``` r
cdm <- instantiateDrugUtilisationCohorts(
  cdm = cdm,
  ingredientConceptId = 1
)
```

We can also specify a list of drugs and an interest ingredient:

``` r
library(dplyr)
scpecs <- tibble(drugConceptId = c(1,2))
cdm <- instantiateDrugUtilisationCohorts(
  cdm = cdm,
  ingredientConceptId = 1,
  specifications = specs
)
```

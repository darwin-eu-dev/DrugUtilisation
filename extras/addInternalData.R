library(here)
library(readr)

domainInformation <- read.csv(here("extras", "domain_information.csv"))
usethis::use_data(domainInformation, internal = TRUE, overwrite = TRUE)

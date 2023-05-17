test_that("test inputs", {
  cdm <- mockDrugUtilisation(connectionDetails)
  expect_error(readConceptList())
  expect_error(readConceptList(cdm = cdm))
  expect_error(readConceptList(cdm = cdm, path = 1))
  expect_error(readConceptList(cdm = cdm, path = "not/a/path"))
  x <- readConceptList(
    cdm = cdm, path =  system.file(package = "DrugUtilisation", "concepts2")
  )
  expect_true(typeof(x) == "list")
  expect_true(all(names(x) %in% c("concept1", "concept2", "concept3")))
  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("functionality with mock concepts", {
  conceptancestor <- dplyr::tibble(
    ancestor_concept_id = c(1,2,2,3),
    descendant_concept_id = c(1,2,3,3)
  )
  cdm <-   cdm <- mockDrugUtilisation(connectionDetails, seed = 1, concept_ancestor = conceptancestor)
  x <- readConceptList(
    cdm = cdm, path =  system.file(package = "DrugUtilisation", "concepts2")
  )
  expect_true(all(names(x) %in% c("concept1", "concept2", "concept3")))
  expect_true(all(x[["concept1"]] == c(1,3)))
  expect_true(all(x[["concept2"]] == c(2,3)))
  expect_true(all(x[["concept3"]] == c(1,2,3)))
  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})



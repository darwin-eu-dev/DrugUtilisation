test_that("test inputs", {
  skip_on_cran()
  cdm <- mockDrugUtilisation(con = connection(), writeSchema = schema())
  expect_warning(expect_error(readConceptList()))
  expect_warning(expect_error(readConceptList(cdm = cdm)))
  expect_warning(expect_error(readConceptList(cdm = cdm, path = 1)))
  expect_warning(expect_error(readConceptList(cdm = cdm, path = "not/a/path")))
  expect_warning(
    x <- readConceptList(
      cdm = cdm, path = system.file(package = "DrugUtilisation", "concepts")
    )
  )
  expect_true(typeof(x) == "list")
  expect_true(all(names(x) %in% c("influenza", "acetaminophen")))
  expect_true(x$influenza == 4266367)
  expect_true(all(x$acetaminophen %in% c(1125315, 43135274, 2905077, 1125360)))

  mockDisconnect(cdm = cdm)
})

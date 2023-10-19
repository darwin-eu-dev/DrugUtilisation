
test_that("basic functionality summariseCodelist", {
  cdm <- mockDrugUtilisation(connectionDetails)
  conceptSet <- list(
    "acetaminophen" = c(1125315, 1125360, 2905077, 43135274),
    "group A" = c(
      3665501, 378253, 317009, 761948, 1539403, 1503327, 1516980, 4141052,
      4313306
    )
  )
  expect_no_error(
    summariseCharacteristicsFromCodelist(cdm[["cohort1"]], cdm, conceptSet)
  )
  cdm[["cohort1"]] <- cdm[["cohort1"]] %>%
    dplyr::mutate(window1 = dplyr::if_else(cohort_start_date <= as.Date("2020-01-01"), "prior 2020", "after 2020")) %>%
    dplyr::mutate(window2 = dplyr::if_else(cohort_start_date <= as.Date("2021-01-01"), "prior 2021", "after 2021"))
  expect_no_error(
    summariseCharacteristicsFromCodelist(
      cdm[["cohort1"]], cdm, conceptSet, strata = list(
        "p2020" = c("window1"), "p2021" = c("window2"),
        "p2020 and p2021" = c("window1", "window2")
      )
    )
  )
})

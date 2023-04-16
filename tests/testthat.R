library(testthat)
library(DrugUtilisation)

equalTibble <- function(x, y) {
  colnamesX <- colnames(x)
  colnamesY <- colnames(y)
  if (!all(colnamesX %in% colnamesY)) {
    return(FALSE)
  }
  if (!all(colnamesY %in% colnamesX)) {
    return(FALSE)
  }
  y <- y %>%
    dplyr::select(dplyr::all_of(colnamesX))
  if (nrow(x) != nrow(y)) {
    return(FALSE)
  }
  x <- x %>%
    dplyr::arrange(!!!rlang::parse_exprs(colnamesX))
  y <- y %>%
    dplyr::arrange(!!!rlang::parse_exprs(colnamesX))
  for (colname in colnames(x)) {
    xx <- x[[colname]]
    yy <- y[[colname]]
    if (!all(is.na(xx) == is.na(yy))) {
      return(FALSE)
    }
    xx <- xx[!is.na(xx)]
    yy <- yy[!is.na(yy)]
    if (!all(xx == yy)) {
      return(FALSE)
    }
  }
  return(TRUE)
}

test_check("DrugUtilisation")

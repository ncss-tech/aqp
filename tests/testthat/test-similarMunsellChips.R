test_that("equivalentMunsellChips works", {
  # the 5YR 1/1 and 7.5YR 1/1 are perceptually equivalent at the 0.1% quantile threshold
  expect_equal(nrow(equivalentMunsellChips("7.5YR",1,1)[[1]]), 2)
})

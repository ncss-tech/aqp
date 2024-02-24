test_that("collapseHz works", {
  data("jacobs2000", package = "aqp")
  x <- collapseHz(jacobs2000, c(`A` = "^A",
                                `E` = "E", 
                                `Bt` = "[ABC]+t", 
                                `C` = "^C", 
                                `foo` = "bar"))
  expect_equal(length(jacobs2000), length(x))
  expect_equal(nrow(x), 29)
})

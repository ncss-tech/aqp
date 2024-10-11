test_that("collapseHz works", {
  data("jacobs2000", package = "aqp")
  .BOTTOM <- NULL
  
  # use existing generalized horizon labels
  new_labels <- c("A", "E", "Bt", "Bh", "C")
  patterns <- c("A", "E", "B.*t", "B.*h", "C")

  # calculate a new SPC with genhz column based on patterns
  jacobs2000_gen <- generalizeHz(jacobs2000, new = new_labels, pattern = patterns)

  # collapse that SPC based on genhz
  i <- collapseHz(jacobs2000_gen, hzdesgn = "genhz")
  expect_equal(length(jacobs2000), length(i))
  expect_equal(nrow(i), 26)
  expect_equal(i[7, , .BOTTOM], c(15, 41, 61, 132, 140, 152))

  x <- collapseHz(jacobs2000, c(`A` = "^A",
                                `E` = "E", 
                                `Bt` = "[ABC]+t", 
                                `C` = "^C", 
                                `foo` = "bar"))
  expect_equal(length(jacobs2000), length(x))
  expect_equal(nrow(x), 29)
})

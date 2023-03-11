test_that("subsetHz works", {
  data(sp3)

  depths(sp3) <- id ~ top + bottom

  # show just horizons with 10YR hues
  res1 <- subsetHz(sp3, hue == '10YR')
  expect_equal(nrow(res1), sum(sp3$hue == '10YR'))
  
  # test retaining empty profiles with drop=FALSE
  site(sp3)$foo <- "bar"
  res2 <- subsetHz(sp3, hue == '10YR', drop = FALSE)
  expect_equal(length(res2), length(sp3))
  expect_equal(sum(is.na(res2$foo)), 2)
})

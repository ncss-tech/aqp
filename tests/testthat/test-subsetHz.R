test_that("subsetHz works", {
  data(sp3)

  depths(sp3) <- id ~ top + bottom

  # show just horizons with 10YR hues
  expect_equal(nrow(subsetHz(sp3, hue == '10YR')), 
               sum(sp3$hue == '10YR'))
})

context("estimateSoilColor")

# note, not symmetric

test_that("works as expected", {
  
  # moist -> dry
  x <- estimateSoilColor(hue = '10YR', value = 3, chroma = 3, sourceMoistureState = 'moist')
  
  expect_equal(x$hue, '10YR')
  expect_equal(x$value, 4)
  expect_equal(x$chroma, 3)
  
  # dry -> moist
  x <- estimateSoilColor(hue = '10YR', value = 5, chroma = 3, sourceMoistureState = 'dry')
  
  expect_equal(x$hue, '10YR')
  expect_equal(x$value, 4)
  expect_equal(x$chroma, 3)
  
})

test_that("unusual colors", {
  
  # moist -> dry
  x <- estimateSoilColor(hue = '10YR', value = 3, chroma = 3, sourceMoistureState = 'moist')
  
  expect_equal(x$hue, '10YR')
  expect_equal(x$value, 4)
  expect_equal(x$chroma, 3)
  
  # dry -> moist
  x <- estimateSoilColor(hue = '10YR', value = 5, chroma = 3, sourceMoistureState = 'dry')
  
  expect_equal(x$hue, '10YR')
  expect_equal(x$value, 4)
  expect_equal(x$chroma, 3)
  
})


# test_that("more complex input", {
#   
#   # data("jacobs2000")
#   # m <- parseMunsell(jacobs2000$matrix_color_munsell, convertColors = FALSE)
#   
#   # m <- structure(list(
#   #   hue = c("10YR", "2.5Y", "10YR", "10YR", "10YR", 
#   #           NA, "2.5YR", "10YR", "10YR", "10YR", "10YR", "10YR", "2.5YR", 
#   #           "10YR", "10YR", "10YR", "2.5Y", "10YR", "10YR", "10YR", "10YR", 
#   #           "10YR", "10YR", "10YR", "10YR", "2.5Y", "10YR", "10YR", "2.5Y", 
#   #           "2.5Y", "10YR", "10YR", "10YR", "2.5Y", "2.5Y", "2.5Y", "10YR", 
#   #           "10YR", "10YR", "10YR", "7.5YR", "10YR", "10YR", "10YR", "10YR", 
#   #           "10YR"), 
#   #   value = c(4, 6, 5, 6, 6, NA, 4, 2, 6, 5, 6, 6, 4, 4, 
#   #             5, 7, 6, 6, 6, 7, 5, 6, 7, 7, 8, 8, 7, 5, 7, 7, 6, 4, 5, 6, 7, 
#   #             6, 7, 7, 4, 8, 3, 3, 6, 7, 2, 6), 
#   #   chroma = c(1, 6, 8, 8, 8, NA, 
#   #              8, 2, 4, 8, 8, 8, 8, 2, 2, 4, 6, 4, 4, 2, 1, 4, 4, 4, 1, 2, 2, 
#   #              2, 4, 3, 6, 6, 1, 3, 3, 3, 2, 1, 1, 1, 2, 2, 3, 2, 1, 1)
#   # ), row.names = c(NA, -46L), class = "data.frame")
#   # 
#   # x <- estimateSoilColor(m$hue, m$value, m$chroma, sourceMoistureState = 'moist')
#   # 
#   # # should be a data.frame of same length
#   # expect_true(inherits(x, 'data.frame'))
#   # expect_equal(nrow(m), nrow(x))
#   
#   # m <- structure(
#   #   list(
#   #     dry = c("10YR 4/1", "10YR 5/1", "10YR 6/2", "10Y 8/0", "10YR 6/0", "10YR 4/2", "10YR 5/2", "2.5Y 5/1", "10YR 4/0", "10YR 5/1"), 
#   #     moist = c("10YR 3/1", "10YR 3/1", "10YR 5/1", "10Y 8/0", "10YR 4/0", "10YR 2.5/1", "10YR 4/2", "2.5Y 4/1", "10YR 2.5/0", "10YR 3/1")
#   #   ), row.names = c(NA, -10L), class = "data.frame"
#   # )
#   # 
#   # m.dry <- parseMunsell(m$dry, convertColors = FALSE)
#   # m.moist <- parseMunsell(m$moist, convertColors = FALSE)
#   # 
#   # e.d <- estimateSoilColor(m.moist$hue, m.moist$value, m.moist$chroma, sourceMoistureState = 'moist')
#   # e.m <- estimateSoilColor(m.dry$hue, m.dry$value, m.dry$chroma, sourceMoistureState = 'dry')
#   # 
#   # od <- sprintf("%s %s/%s", m.dry$hue, m.dry$value, m.dry$chroma)
#   # ed <- sprintf("%s %s/%s", e.d$hue, e.d$value, e.d$chroma)
#   # 
#   # colorContrast(od, ed)
#   
# })

context("soil texture summary via marginal percentiles")

# sample data
data('sp4')

ssc <- sp4[grep('^Bt', sp4$name), c('sand', 'silt', 'clay')]
names(ssc) <- toupper(names(ssc))

test_that("textureTriangleSummary() works as expected", {
  
  # does it work?
  s <- textureTriangleSummary(ssc)
  
  # result is a matrix
  expect_true(inherits(s, 'matrix'))
  
  # column order preserved
  expect_true(
    all(dimnames(s)[[2]] == c('SAND', 'SILT', 'CLAY'))
  )
  
})

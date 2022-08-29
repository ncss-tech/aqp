context("soil texture simulation")

# sample data
data('sp6')

ssc <- sp6[grep('^Bt', sp6$name), c('sand', 'silt', 'clay')]
names(ssc) <- toupper(names(ssc))

test_that("bootstrapSoilTexture() works as expected", {
  
  skip_if_not_installed('compositions')
  
  # simulated under relatively stable conditions
  set.seed(1010101)
  s <- bootstrapSoilTexture(ssc, n = 50, method = 'dirichlet')
  
  # result is a list
  expect_true(inherits(s, 'list'))
  
  # there should be data.frame with 50 rows and 3 columns
  expect_true(inherits(s$samples, 'data.frame'))
  expect_true(nrow(s$samples) == 50)
  expect_true(ncol(s$samples) == 3)
  
  # mean sand, silt, clay values 
  # should be very close to these
  expect_true(
    all(round(s$mean) - c(31, 42, 27) < 3)
  )
  
  # column order preserved
  expect_true(
    all(names(s$samples) == c('SAND', 'SILT', 'CLAY'))
  )
  
})

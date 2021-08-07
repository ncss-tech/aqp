context("simulateColor")

# example data
data(sp1, package = 'aqp')
depths(sp1) <- id ~ top + bottom
site(sp1) <- ~ group

sp1$soil_color <- munsell2rgb(sp1$hue, sp1$value, sp1$chroma)

test_that("aggregate color (proportions method)", {
  
  horizons(sp1)$genhz <- 'A'
  a <- aggregateColor(sp1, groups='genhz', col='soil_color')
  
  
  # simulate
  set.seed(54654)
  cols <- simulateColor(method = 'proportions', n = 200, parameters = a)
  
  # expected output
  expect_true(inherits(cols, 'list'))
  
  # flatten
  cols <- unlist(cols)
  
  # check that requested number of samples are returned
  expect_true(length(cols) == 200)
  
  # tabulate, sort: most frequent color should be 7.5YR 3/2
  cols <- sort(table(cols), decreasing = TRUE)
  
  expect_equal(names(cols)[1], '7.5YR 3/2')
})


test_that("dE00 threshold method", {
  
  # params
  p <- list(
    'A' = list(m = '7.5YR 3/3', thresh = 20, hues = c('10YR', '7.5YR', '5YR'))
  )
  
  # simulate
  set.seed(54654)
  cols <- simulateColor(method = 'dE00', n = 200, parameters = p)
  
  # expected output
  expect_true(inherits(cols, 'list'))
  
  # flatten
  cols <- unlist(cols)
  
  # check that requested number of samples are returned
  expect_true(length(cols) == 200)
  
  # tabulate, sort: most frequent color should be 7.5YR 3/3
  cols <- sort(table(cols), decreasing = TRUE)
  
  # in the limit, the most frequent sample should be the same as the rep. color
  expect_equal(names(cols)[1], '7.5YR 3/3')
  
})

test_that("SPC modification", {
  
  # seed profile
  s <- sp1[2, ]
  
  # static hz variability
  horizons(s)$.hd <- 5
  
  n.sim <- 10
  
  # simulate
  ids <- sprintf("%s-%03d", 'sim', 1:n.sim)
  set.seed(10101)
  z <- perturb(s, id = ids, boundary.attr = '.hd', min.thickness = 5)

  p <- list(
    'Oi' = list(m = '7.5YR 2/2', thresh = 5, hues = c('7.5YR')),
    'A' = list(m = '7.5YR 4/3', thresh = 8, hues = c('7.5YR')),
    'C1' = list(m = '5YR 4/2', thresh = 8, hues = c('5YR', '7.5YR')),
    'C2' = list(m = '5YR 4/2', thresh = 20, hues = c('5YR', '7.5YR'))
  )
  
  # result is an SPC
  zz <- simulateColor(method = 'dE00', n = n.sim, parameters = p, SPC = z)
  
  expect_true(inherits(zz, 'SoilProfileCollection'))
  
})



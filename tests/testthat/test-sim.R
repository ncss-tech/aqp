context("soil profile simulation")

## sample data
data(sp3)
depths(sp3) <- id ~ top + bottom

# select a profile to use as the basis for simulation
s <- sp3[3, ]

# reset horizon names
s$name <- paste('H', seq_along(s$name), sep='')

## tests

test_that("sim() works as expected", {
  
  # simulate 25 new profiles
  sim.1 <- sim(s, n = 25)
  
  # NOTE: now following numeric id order for numeric id
  sim.2 <- sim(s, n = 25, hz.sd = c(1, 2, 5, 5, 5, 10))
  
  # result is an SPC
  expect_true(inherits(sim.1, 'SoilProfileCollection'))
  expect_true(inherits(sim.2, 'SoilProfileCollection'))
  
  # expected lengths
  expect_true(length(sim.1) == 25)
  expect_true(length(sim.2) == 25)
})


test_that("expected errors", {
  
  # only 1 seed can be used
  expect_error(sim(sp3[1:2, ], n = 25))
  
  # sd must recycle evenly over number of original horizons
  # NOTE: now following numeric id order for numeric id
  expect_error(sim(s, n = 25, hz.sd = 1:4))
  
})


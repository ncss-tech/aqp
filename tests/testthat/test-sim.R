context("soil profile simulation")

## sample data
data(sp3)

expect_silent({depths(sp3) <- id ~ top + bottom})

# select a profile to use as the basis for simulation
s <- sp3[3, ]

# reset horizon names
s$name <- paste('H', seq_along(s$name), sep = '')

## tests

test_that("sim() works as expected", {

  # simulate 25 new profiles
  expect_message({sim.1 <- sim(s, n = 25)},
                 "converting profile IDs from integer to character")

  expect_message({sim.2 <- sim(s, n = 25, hz.sd = c(1, 2, 5, 5, 5, 10, 3))},
   "converting profile IDs from integer to character")

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

test_that("permute_profile() works as expected", {
  # simulate 25 new profiles with a sd boundary thickness of 0.5 - 2.5cm
  s$bdy <- round(runif(nrow(s), 1, 5)) / 2
  diagnostic_hz(s) <- data.frame(id = profile_id(s),
                                 featkind = "foo",
                                 featdept = 0, featdepb = 10)
  restrictions(s) <- data.frame(id = profile_id(s),
                                 restrkind = "bar",
                                 restrdept = 0, restrdepb = 10)
  perp <- permute_profile(s, n = 25, boundary.attr = "bdy")

  # result is an SPC
  expect_true(inherits(perp, 'SoilProfileCollection'))

  # expected lengths
  expect_true(length(perp) == 25)

  perp2 <- permute_profile(s, id = 26:50, boundary.attr = "bdy", new.idname = "foo")

  # result is an SPC
  expect_true(inherits(perp2, 'SoilProfileCollection'))

  # expected lengths
  expect_true(length(perp2) == 25)

  # custom idname
  expect_equal(idname(perp2), "foo")

  # custom IDs
  expect_equal(profile_id(perp2), as.character(26:50))

  })


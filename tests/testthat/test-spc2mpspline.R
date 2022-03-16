context("spc2mpspline - 1cm spline interpolation w/ mpspline2")

test_that("spc2mpspline works as expected", {
  data(sp1)
  depths(sp1) <- id ~ top + bottom
  
  # # alternate/future: horizons with NA in property of interest are removed, not whole profiles
  # res0 <- spc2mpspline(sp1, "prop")
  # expect_equal(length(res0), length(sp1))
  
  # profiles with NA in property of interest are removed
  res1 <- spc2mpspline(sp1, "prop")
  expect_equal(length(res1), length(sp1) - 1)
  expect_equal(attr(res1, "removed"), "P001")

  # correspond to profiles P002 and P009
  expect_equal(max(res1), 240)
  expect_equal(min(res1), 59)

  # # actually fix the data
  sp1fix <- sp1
  
  # profile 1: set bedrock bottom depth to 200cm
  sp1fix@horizons[6,]$bottom <- 200
  
  # profile 1: set bedrock clay content to zero
  sp1fix@horizons[6,]$prop <- 0
  
  # pass d= argument for greater max depth
  res3 <- spc2mpspline(sp1fix, "prop", d = c(0, 5, 15, 30, 60, 100, 200, 300))
  expect_equal(length(res3), length(sp1fix)) # first profile was fixed

  expect_equal(max(res3), 240)
  expect_equal(min(res1), 59)

  # if you want to show original and spline together, create combined horizon var
  sp1$prop_combined <- sp1$prop
  res3$prop_combined <- res3$prop_spline

  # pbindlist input data works after making unique ID
  profile_id(res3) <- paste0(profile_id(res3), "_spline")
  expect_silent( {sp1union <- pbindlist(list(sp1, res3))} )
  expect_equal(length(sp1union), 2*length(sp1))

  # plot(sp1union, color = "prop_combined", divide.hz = FALSE)
})

test_that("spc2mpspline works as expected", {
  data(sp1)
  depths(sp1) <- id ~ top + bottom
  
  # horizons with NA in property of interest are removed (not whole profiles)
  res1 <- spc2mpspline(sp1, "prop", hzdesgn = 'name')
  expect_equal(length(res1), length(sp1))

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

test_that("alternate depth output methods", {
  data(sp1, package = "aqp")
  depths(sp1) <- id ~ top + bottom
  
  res1 <- spc2mpspline(sp1, 'prop', 
                       method = "est_icm", 
                       hzdesgn = 'name')
  # contains one horizon with equal top and bottom
  expect_equal(nrow(sp1) - 1, nrow(res1))
  
  res2 <- spc2mpspline(sp1, 'prop', 
                       method = "est_dcm", 
                       hzdesgn = 'name')
  # 6 layers per input profile in output
  expect_equal(6*length(sp1), nrow(res2))
  
})

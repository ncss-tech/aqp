context("spc2mpspline - 1cm spline interpolation w/ mpspline2")

test_that("spc2mpspline works as expected", {
  data(sp1)
  depths(sp1) <- id ~ top + bottom
  
  # profiles failing aqp::hzDepthTests are removed
  res1 <- spc2mpspline(sp1, "prop")
  expect_equal(length(res1), length(sp1) - 1) 
  expect_equal(attr(res1, "removed"), "P001")
  
  # P001 removed because of bad hz logic
  sp1filt <- filter(sp1, checkHzDepthLogic(sp1)$valid)
  res2 <- spc2mpspline(sp1filt, "prop")
  expect_equal(length(res2), length(sp1filt)) # due to 89-89cm R layer
  
  # plot(res2, color = "spline_prop")
  
  # max and min of SPC are equal for spline'd result due to truncation to available data interval 
  expect_equal(max(res1), 59)
  expect_equal(max(res1), min(res1))
  
  # actually fix the data
  sp1fix <- sp1
  sp1fix@horizons[6,]$bottom <- 200
  res3 <- spc2mpspline(sp1fix, "prop")
  expect_equal(length(res3), length(sp1fix)) # first profile was fixed
  
  # the available interval is still the same
  expect_equal(max(res1), 59)
  expect_equal(max(res1), min(res1))
  
  # if you want to show original and spline together, create combined horizon var
  sp1$prop_combined <- sp1$prop
  res3$prop_combined <- res3$prop_spline
  
  # unioning to input data works after making unique ID
  profile_id(res3) <- paste0(profile_id(res3), "_spline")
  expect_silent( {sp1union <- aqp::union(list(sp1, res3))} )
  expect_equal(length(sp1union), 2*length(sp1))

  # plot(sp1union, color = "prop_combined", divide.hz = FALSE)
})

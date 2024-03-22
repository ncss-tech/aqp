test_that("thicknessOf works", {
  data("jacobs2000")

  ## method="cumulative"
  
  # cumulative thickness of horizon designations matching "A|B"
  x1 <- thicknessOf(jacobs2000, "A|B", prefix = "AorB_")
  expect_equal(nrow(x1), length(jacobs2000))
  expect_equal(x1$AorB_thickness, c(131, 117, 136, 20, 54, 110, 43))

  ## method="minmax"
  
  # maximum bottom depth minus minimum top depth of horizon designations matching "A|B"
  x2 <- thicknessOf(jacobs2000, "A|B", method = "minmax", prefix = "AorB_")
  expect_equal(ncol(x2), 4)
  expect_equal(x2$AorB_top, rep(0, nrow(x2)))
  expect_equal(x2$AorB_thickness, c(156, 145, 175, 20, 135, 168, 140))
  expect_true(all(x2$AorB_thickness >= x1$AorB_thickness))
  
  ## custom logical function
  
  # calculate cumulative thickness of horizons containing >18% clay
  x3 <- thicknessOf(jacobs2000, FUN = function(x, ...) !is.na(x[["clay"]]) & x[["clay"]] > 18)
  expect_equal(x3$thickness, c(170, 167, 81, 0, 0, 49, 0))
  
  ## missing property and or depth data
  
  # function without NA handling, and na.rm=FALSE
  x4 <- thicknessOf(jacobs2000, FUN = function(x, ...) x[["clay"]] > 18)
  expect_equal(x4$thickness, c(170, 167, 81, 0, NA_integer_, 49, 0))
  
  # function without NA handling, and na.rm=TRUE
  x5 <- thicknessOf(jacobs2000, FUN = function(x, ...) x[["clay"]] > 18, na.rm = TRUE)
  expect_equal(x5$thickness, c(170, 167, 81, 0, 0, 49, 0))
  
  # missing horizon depths, and na.rm=FALSE
  jacobs2000@horizons$top[1] <- NA_integer_
  x6 <- thicknessOf(jacobs2000, "A|B")
  expect_equal(x6$thickness, c(NA_integer_, 117, 136, 20, 54, 110, 43))
  
  # missing horizond depths, and na.rm = TRUE
  x7 <- thicknessOf(jacobs2000, "A|B", na.rm = TRUE)
  expect_equal(x7$thickness, c(113, 117, 136, 20, 54, 110, 43))
})

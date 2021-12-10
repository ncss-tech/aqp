test_that("spc_horizonOffset works", {
  
  # sample data where two profiles have no bottom depths (but all top depths are present)
  h <- data.frame(
    id = c(1, 1, 1, 2, 2, 2, 2, 3, 3),
    top = c(0:2, 0:3, 0:1) * 10,
    bottom = c(rep(NA_integer_, 7), c(10, 99))
  )
  expect_warning(depths(h) <- id ~ top + bottom)
  
  # index of last horizon in each profile
  bottom.idx <- which(hzID(h) %in% getLastHorizonID(h)) 
  #> [1] 3 7 9
  
  # horizons 1 index above/below bottom
  expect_equal(spc_horizonOffset(h, bottom.idx, offset = -1), c(2, 6, 8))
  expect_equal(spc_horizonOffset(h, bottom.idx, offset = 1), numeric(0))
  
  # simplify = FALSE (list results)
  expect_equal(spc_horizonOffset(h, bottom.idx, offset = -1, simplify = FALSE), list(2, 6, 8))
  expect_equal(spc_horizonOffset(h, bottom.idx, offset = 1, simplify = FALSE), 
               list(numeric(0), numeric(0), numeric(0)))
  
  # horizons 2 indices above bottom
  expect_equal(spc_horizonOffset(h, bottom.idx, offset = -2), c(1, 5))
  expect_equal(spc_horizonOffset(h, bottom.idx, offset = 2), numeric(0))
  
  # horizons 1 + 2 indices above bottom
  expect_equal(spc_horizonOffset(h, bottom.idx, offset = -(1:2)), c(1, 2, 5, 6, 8))
  expect_equal(spc_horizonOffset(h, bottom.idx, offset = 1:2), numeric(0))
  
  # there are never horizons above the first horizon (in a profile)
  first <- h[, 1, .HZID]
  expect_equal(spc_horizonOffset(h, first, offset = -1), numeric(0))
  
  # the second horizon is always right after the first (if profile has more than 1 horizon)
  expect_equal(spc_horizonOffset(h, first, offset = 1), first + 1)
})

test_that("horizon_above/horizon_below works", {
  
  data(sp4)
  depths(sp4) <- id ~ top + bottom
  
  # horizons above any horizon with extractable Ca to Mg ratio less than 1:10
  x <- horizon_above(sp4, ex_Ca_to_Mg < 0.1)
  expect_equal(as.integer(hzID(x)), c(2, 3, 12, 25, 26))
  
  # horizons below any horizon with extractable Ca to Mg ratio less than 1:10
  x <- horizon_below(sp4, ex_Ca_to_Mg < 0.1)
  expect_equal(as.integer(hzID(x)), c(4, 27))
  
})

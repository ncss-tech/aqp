context("dice (update to slice)")

data(sp4, package = 'aqp')
depths(sp4) <- id ~ top + bottom

test_that("basic functionality", {
  
  # SPC
  s <- dice(sp4)
  
  # as a data.frame
  s.d <- dice(sp4, SPC = FALSE)
  
  # did it work?
  expect_true(inherits(s, 'SoilProfileCollection'))
  expect_true(inherits(s.d, 'data.frame'))
  
  # new metadata columns
  expect_true(all(c('sliceID', '.oldTop', '.oldBottom') %in% horizonNames(s)))
  expect_true(all(c('sliceID', '.oldTop', '.oldBottom') %in% names(s.d)))
  
  # there should be as many slices as original profiles are deep
  # names should be identical
  expect_equal(
    profileApply(sp4, max),
    profileApply(s, nrow)
  )
  
})

test_that("formula interface", {
  
  # reference
  s <- dice(sp4)
  
  # equivalent to `fm = NULL`
  s1 <- dice(sp4, fm = ~ .)
  expect_equivalent(s, s1)
  
  # LHS
  s2 <- dice(sp4, fm = 0:20 ~ .)
  
  # all should be <= 21 slices
  expect_true(all(profileApply(s2, nrow) <= 21))
  
  # RHS
  s3 <- dice(sp4, fm = ~ Ca)
  
  # all other hz attr should be missing
  hz.diff <- setdiff(horizonNames(sp4), horizonNames(s3))
  expect_false(
    all(
      horizonNames(s3) %in% c("name", "K", "Mg", "CEC_7", "ex_Ca_to_Mg", "sand", "silt", "clay", "CF")
    )
  )

  # LHS + RHS
  s4 <- dice(sp4, fm = 0:30 ~ Ca + K)
  
  # hz names
  expect_false(
    all(
      horizonNames(s4) %in% c("name", "Mg", "CEC_7", "ex_Ca_to_Mg", "sand", "silt", "clay", "CF")
    )
  )
  
  expect_true(all(profileApply(s4, nrow) <= 31))
  
})


test_that("percent missing calculation", {
  
  sp4$K[c(1, 5, 10, 30)] <- NA
  sp4$Ca[c(1, 5, 20, 30)] <- NA
  sp4$CEC_7[c(30)] <- NA
  
  s <- dice(sp4, fm = ~ Ca + K + CEC_7, pctMissing = TRUE)
  
  # visual check
  plotSPC(s, color = 'Ca')
  plotSPC(s, color = 'K')
  plotSPC(s, color = '.pctMissing')
  
  # new column
  expect_true('.pctMissing' %in% horizonNames(s))
  
  # some should be non-zero
  expect_true(any(s$.pctMissing > 0) & ! all(s$.pctMissing > 0))
  
  # check exact values
  # 1st horizon, 2/3 missing
  expect_true(all(s[1, ]$.pctMissing[1:3] == 2/3))
  # all horizons, 1/3 missing
  expect_true(all(s[7, ]$.pctMissing == 1/3))
  # last horizon, 100% missing
  expect_true(all(s[10, ]$.pctMissing[8:16] == 1))
})


test_that("padding with NA, backwards-compatible with slice", {
  
  s <- dice(sp4, fm = 0:80 ~ ., fill = TRUE)
  
  # all profiles should be the same "depth", including empty (NA) horizons
  expect_true(all(profileApply(s, max) == 80))
  
})


test_that("testing exact values", {
  
  # typical soil profile, no problem horizons
  x <- data.frame(
    id = 'A',
    top = c(0, 10, 15, 20, 40, 50, 100),
    bottom = c(10, 15, 20, 40, 50, 100, 165),
    p = 1:7
  )
  
  # init SPC
  depths(x) <- id ~ top + bottom
  
  # dice with defaults
  s <- dice(x)
  
  # horizon thickness
  thick <- x$bottom - x$top
  
  # check no. contiguous sliced values = horizon thickness
  expect_true(all(rle(s$p)$lengths == thick))
  
  # check values within chunks
  expect_true(all(rle(s$p)$values == x$p))
  
  # do it again with a formula
  s <- dice(x, fm = 0:106 ~ .)
  
  # the last horizon is truncated to 107cm
  expect_true(rle(s$p)$lengths[7] == 7)
  
})



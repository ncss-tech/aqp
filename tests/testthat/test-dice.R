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

test_that("data integrity, default arguments", {
  
  # SPC
  s <- dice(sp4, SPC = FALSE)
  
  # values match
  # reference horizons
  .ref <- horizons(sp4)
  
  # inner join on original hz ID
  x <- merge(.ref[, c('hzID', 'Mg')], s[, c('hzID', 'Mg')], by = 'hzID', sort = FALSE, all.x = FALSE)
  
  # sliced == original
  expect_equal(x$Mg.x, x$Mg.y)
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

.slices <<- 0:30
test_that("formula interface (with non-standard evaluation)", {
  d5 <- dice(sp4, .slices ~ Ca + K)
  expect_true(inherits(d5, 'SoilProfileCollection'))
})

test_that("discrete slices entirely within SPC", {
  
  # single slice
  s <- dice(sp4, fm = 5 ~ Mg, SPC = FALSE)
  
  # NA should be returned for slices within gaps / below profile bottoms
  expect_true(nrow(s) == length(sp4))
  
  # reference horizons
  .ref <- horizons(sp4)
  
  # inner join on original hz ID
  x <- merge(.ref[, c('hzID', 'Mg')], s[, c('hzID', 'Mg')], by = 'hzID', sort = FALSE, all.x = FALSE)
  
  # sliced == original
  expect_equal(x$Mg.x, x$Mg.y)

  # multiple slices, all within SPC depth interval
  .slices <- c(5, 10, 15)
  s <- dice(sp4, fm = c(5, 10, 15) ~ Mg, SPC = FALSE)
  
  # NA should be returned for slices within gaps / below profile bottoms
  expect_true(nrow(s) == length(sp4) * length(.slices))
  
  # reference horizons
  .ref <- horizons(sp4)
  
  # inner join on original hz ID
  x <- merge(.ref[, c('hzID', 'Mg')], s[, c('hzID', 'Mg')], by = 'hzID', sort = FALSE, all.x = FALSE)
  
  # sliced == original
  expect_equal(x$Mg.x, x$Mg.y)
  
})

test_that("slices below bottom of profiles or entire collection", {
  
  # single slice, deeper than some profiles
  s <- dice(sp4, fm = 25 ~ Mg, SPC = FALSE)
  
  # NA should be returned for slices within gaps / below profile bottoms
  # filled horizons will create hzID beyond original sequence
  expect_true(nrow(s) == length(sp4))
  
  # there should be 3 NA
  expect_true(length(which(is.na(s$Mg))) == 3)
  
  # reference horizons
  .ref <- horizons(sp4)
  
  # inner join on original hz ID
  x <- merge(s[, c('hzID', 'Mg')], .ref[, c('hzID', 'Mg')], by = 'hzID', sort = FALSE, all.x = TRUE)
  
  # after NA-removal
  x <- na.omit(x)
  # sliced == original
  expect_equal(x$Mg.x, x$Mg.y)
  
  # single slice, deeper than all profiles
  s <- dice(sp4, fm = 75 ~ Mg, SPC = FALSE)
  
  # NA should be returned for slices within gaps / below profile bottoms
  expect_true(nrow(s) == length(sp4))
  
  # there should be as many NA as profiles in sp4
  expect_true(length(which(is.na(s$Mg))) == length(sp4))
  
  # multiple slices, some beyond profile depths
  .slices <- c(5, 10, 15, 50, 100)
  s <- dice(sp4, fm = c(5, 10, 15, 50, 100) ~ Mg, SPC = FALSE)
  
  # NA should be returned for slices within gaps / below profile bottoms
  expect_true(nrow(s) == length(sp4) * length(.slices))
  
  # reference horizons
  .ref <- horizons(sp4)
  
  # inner join on original hz ID
  x <- merge(s[, c('hzID', 'Mg')], .ref[, c('hzID', 'Mg')], by = 'hzID', sort = FALSE, all.x = TRUE)
  
  # after NA-removal
  x <- na.omit(x)
  # sliced == original
  expect_equal(x$Mg.x, x$Mg.y)
  
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
  expect_true(any(s$.pctMissing > 0) & !all(s$.pctMissing > 0))
  
  # check exact values
  # 1st horizon, 2/3 missing
  expect_true(all(s[1, ]$.pctMissing[1:3] == 2/3))
  # all horizons, 1/3 missing
  expect_true(all(s[7, ]$.pctMissing == 1/3))
  # last horizon, 100% missing
  expect_true(all(s[10, ]$.pctMissing[8:16] == 1))
})


test_that("padding with NA, backwards-compatible with slice", {
  
  # fill = TRUE is implied with formula interface
  s <- dice(sp4, fm = 0:80 ~ .)
  
  # all profiles should be the same "depth", including empty (NA) horizons
  expect_true(all(profileApply(s, max) == 81))
  
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

test_that("overlapping horizons", {
  # overlapping horizons results in increased number of slices (according to overlapping thickness)
  x1 <- horizons(dice(sp4, ~ .))
  
  # create overlap
  sp4@horizons[2,]$bottom <- sp4@horizons[2,]$bottom + 12
  
  x2 <- horizons(dice(sp4, ~ .))
  
  # evaluate logic by profile, not horizon
  expect_message({ x3 <- horizons(dice(sp4, ~ ., byhz = FALSE)) })
  
  # default case--nothing removed, nothing added
  expect_equal(nrow(x1), 331)
  
  # 12cm of overlap
  expect_equal(nrow(x2), 331 + 12)
  
  # 1 profile removed due to overlap
  expect_equal(nrow(x3), 289)
})

test_that("dropped profile IDs", {
  
  # corrupt depth
  sp4$top[5] <- sp4$bottom[5]
  
  # offending horizons removed
  expect_message(s <- dice(sp4, byhz = TRUE))
  
  expect_equal(
    setdiff(profile_id(sp4), profile_id(s)),
    character(0)
  )
  
  # offending profiles removed
  expect_message(s <- dice(sp4, byhz = FALSE))
  
  # single dropped ID should be present in metadata
  expect_equal(
    setdiff(profile_id(sp4), profile_id(s)),
    metadata(s)$removed.profiles
  )
    
})

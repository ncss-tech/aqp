context("slab method for SoilProfileCollection objects")

test_that("basic slab functionality", {
  
  data(sp1, package = 'aqp')
  depths(sp1) <- id ~ top + bottom
 
  # aggregate entire collection
  # 1-unit slabs
  a.1 <- slab(sp1, fm = ~ prop, strict=TRUE)
  # 5-unit slabs
  a.2 <- slab(sp1, fm = ~ prop, strict=TRUE, slab.structure=5)
  # custom slabs
  a.3 <- slab(sp1, fm = ~ prop, strict=TRUE, slab.structure=c(0,5,10,25,50))
  
  # did it work?
  expect_true(inherits(a.1, 'data.frame'))
  expect_true(inherits(a.2, 'data.frame'))
  expect_true(inherits(a.3, 'data.frame'))
  
  # number of results, these are in long format
  expect_equal(nrow(a.1), 240)
  expect_equal(nrow(a.2), 48)
  expect_equal(nrow(a.3), 4)
  
  # required column names
  nm <- names(a.1)
  expect_true(any(grepl('variable', nm)))
  expect_true(any(grepl('all.profiles', nm)))
  expect_true(any(grepl('top', nm)))
  expect_true(any(grepl('bottom', nm)))
  expect_true(any(grepl('contributing_fraction', nm)))
  expect_true(any(grepl('p.q', nm)))
})

test_that("slab structure for a single slab", {
  data(sp4, package = 'aqp')
  depths(sp4) <- id ~ top + bottom
  
  sp4$group <- c(rep('A',5), rep('B',5))
  a.1 <- slab(
    sp4,
    fm = ~ sand + silt + clay,
    slab.structure = c(0, 10),
    slab.fun = mean,
    na.rm = TRUE
  )
  expect_equal(nrow(a.1), 3)
  
  # again, this time within groups defined by a site-level attribute:
  a.1 <- slab(
    sp4,
    fm = group ~ sand + silt + clay,
    slab.structure = c(0, 10),
    slab.fun = mean,
    na.rm = TRUE
  )
  expect_equal(nrow(a.1), 6)
})


test_that("extended slab functionality: weighted aggregation", {
  
  data(sp1, package = 'aqp')
  depths(sp1) <- id ~ top + bottom
  
  sp1$weights <- rep(1:2, length(sp1))[1:length(sp1)]
  sp1$wtgrp <- rep(1, length(sp1))
  
  # component weighted averages
  # mukey=461268; Doemill-Jokerst, 3 to 8 percent slopes (615)
  x <- data.frame(cokey = c(21469960L, 21469960L, 21469960L, 21469960L, 
                            21469960L, 21469961L, 21469961L, 21469961L), 
                  comppct_r = c(50L,  50L, 50L, 50L, 50L, 40L, 40L, 40L),
                  hzdept_r = c(0L, 3L, 13L, 23L, 36L, 0L, 3L, 10L), 
                  hzdepb_r = c(3L, 13L, 23L, 36L, 61L, 3L, 10L, 35L), 
                  ph1to1h2o_r = c(6.1, 6.8, 6.7, 6.7, NA, 6.4, 6.5, NA))
  depths(x) <- cokey ~ hzdept_r + hzdepb_r
  site(x) <- ~ comppct_r
  
  # custom function, with actual comppct_r
  a.0 <- slab(x, ~ ph1to1h2o_r,
      slab.structure = c(0, 10),
      weights = "comppct_r",
      slab.fun = weighted.mean,
      na.rm = TRUE
    )
  expect_equal(a.0$value, 6.54, tolerance = 0.005) 
  
  skip_if_not_installed("Hmisc")
  
  # we expect quantile estimates to vary (given weighted v.s. unweighted)
  a.0 <- slab(sp1, fm = ~ prop, weights = "weights")
  a.1 <- slab(sp1, fm = ~ prop, strict = TRUE, weights = "weights")
  a.2 <- slab(sp1, fm = wtgrp ~ prop, strict = TRUE, weights = "weights")
  a.3 <- slab(sp1, fm = wtgrp ~ prop, strict = TRUE)
  
  # expect consistent structure for weighted/unweighted: same column names except for group
  ungroupcols <- colnames(a.3)
  ungroupcols[2] <- "all.profiles"
  expect_equal(colnames(a.0), ungroupcols)
  expect_equal(colnames(a.1), ungroupcols)
  expect_equal(colnames(a.2), colnames(a.3))
  
  # contributing fractions should be identical
  expect_true(all(a.1$contributing_fraction == a.2$contributing_fraction))
  expect_true(all(a.2$contributing_fraction == a.3$contributing_fraction))
  
  # should match this within the tolerance:
  #   soilDB::get_SDA_property(property = 'ph1to1h2o_r', method = "weighted average",
  #                            mukeys = 461268, miscellaneous_areas = FALSE, include_minors = TRUE,
  #                            top_depth = 0, bottom_depth = 10)
                                  
  # comppct_r adjusted to get higher result more like doemill
  x$comppct_r <- c(90, 10)
  a.1 <- slab(x, ~ ph1to1h2o_r, slab.structure = c(0, 10), weights = "comppct_r")
  expect_equal(a.1$p.q50, 6.8)
  
  # comppct_r adjusted to get lower result more like jokerst
  x$comppct_r <- c(10, 90)
  a.2 <- slab(x, ~ ph1to1h2o_r, slab.structure = c(0, 10), weights = "comppct_r")
  expect_equal(a.2$p.q50, 6.5)
  
  # comppct_r adjusted to get lower result more like jokerst
  x$comppct_r <- c(NA, 90)
  na.1 <- slab(x, ~ ph1to1h2o_r, slab.structure = c(0, 10), weights = "comppct_r", na.rm = TRUE)
  expect_equal(na.1$p.q50, 6.5)
  
  na.2 <- slab(x, ~ ph1to1h2o_r, slab.structure = c(0, 10), weights = "comppct_r", slab.fun = weighted.mean, na.rm = TRUE)
  expect_equal(na.2$value, NA_real_)
  
})

test_that("slab calculations: mean, single profile", {
  
  data(sp1, package = 'aqp')
  depths(sp1) <- id ~ top + bottom
  
  # aggregate single profile
  # custom slabs
  a <- slab(
      sp1[1,],
      fm = ~ prop,
      strict = TRUE,
      slab.structure = c(0, 5, 10, 25, 100),
      slab.fun = mean,
      na.rm = TRUE
    )
  
  # using mean and similar functions will cause slab to store single result / slab into 'value' column
  expect_true(any(grepl('value', names(a))))
  
  # calculations done by hand (DEB)
  # weighted mean, single profile
  # 0-5: 9.400
  expect_equal(a$value[1], 9.4, tolerance = 0.0001)
  # 5-10: 7.000
  expect_equal(a$value[2], 7, tolerance = 0.0001)
  # 10-25: 8.466
  expect_equal(a$value[3], 8.4666, tolerance = 0.0001)
  # 25-100: 15.625
  expect_equal(a$value[4], 15.625, tolerance = 0.0001)
  
})


test_that("slab calculations: mean, several profiles", {
  
  data(sp1, package = 'aqp')
  depths(sp1) <- id ~ top + bottom
  
  # aggregate single profile
  # custom slabs
  a <- slab(sp1, fm = ~ prop, strict = TRUE, slab.structure = c(0, 5, 10, 25, 100), slab.fun = mean, na.rm = TRUE)
  
  # weighted mean calculations done by 1-unit slices
  # note slice formula notation works from horizon "tops"
  s.1 <- dice(sp1, 0:4 ~ prop, SPC = FALSE)$prop
  s.2 <- dice(sp1, 5:9 ~ prop, SPC = FALSE)$prop
  s.3 <- dice(sp1, 10:24 ~ prop, SPC = FALSE)$prop
  s.4 <- dice(sp1, 25:99 ~ prop, SPC = FALSE)$prop
  
  # 0-5
  expect_equal(a$value[1], mean(s.1, na.rm=TRUE), tolerance=0.0001)
  # 5-10
  expect_equal(a$value[2], mean(s.2, na.rm=TRUE), tolerance=0.0001)
  # 10-25
  expect_equal(a$value[3], mean(s.3, na.rm=TRUE), tolerance=0.0001)
  # 25-100
  expect_equal(a$value[4], mean(s.4, na.rm=TRUE), tolerance=0.0001)
  
})


test_that("edge case: slab.structure[2] > max(x)", {
  
  data(sp1, package = 'aqp')
  depths(sp1) <- id ~ top + bottom
  
  # single profile, to max(x)
  a.max <- slab(sp1[1, ], fm = ~ prop, strict=TRUE, slab.structure=c(0,max(sp1[1, ])), slab.fun = mean, na.rm=TRUE)
  
  # single profile
  # custom interval, exceeding max(sp1)
  a <- slab(sp1[1, ], fm = ~ prop, strict=TRUE, slab.structure=c(0,300), slab.fun = mean, na.rm=TRUE)
  
  # weighted mean should be the same
  expect_true(a.max$value == a$value)
  
  # contributing fractions will be different
  # smaller value when slab.structure[2] > max.d
  expect_true(a$contributing_fraction < a.max$contributing_fraction)
  
  # compare with hand-calculated value
  expect_equivalent(a$value, 13.58427, tolerance=0.001)
})

test_that("edge case: slab.structure[1] > 0 (w/ custom slab function)", {
  data(sp3, package = "aqp")
  depths(sp3) <- id ~ top + bottom
  
  # custom 'slab' function, returning mean +/- 1SD
  mean.and.sd <- function(values) {
    m <- mean(values, na.rm = TRUE)
    s <- sd(values, na.rm = TRUE)
    upper <- m + s
    lower <- m - s
    res <- c(mean = m,
             lower = lower,
             upper = upper)
    return(res)
  }
  
  ## this time, compute the weighted mean of selected properties, by profile ID
  a <- slab(sp3,
            fm = id ~ L + A + B,
            slab.structure = c(40, 60), 
            slab.fun = mean.and.sd
  )
  
  # convert long -> wide
  res <- data.table::dcast(
    data.table::as.data.table(a),
    formula = id + top + bottom ~ variable,
    value.var = 'mean'
  )
  
  expect_equal(nrow(res), length(sp3))
  expect_equal(ncol(res), 6L)
})

test_that("overlapping horizons", {
  data(sp4, package = 'aqp')
  depths(sp4) <- id ~ top + bottom
  
  # overlapping horizons results in increased number of slices (according to overlapping thickness)
  x1 <- slab(sp4, ~ K + Mg + Ca + CEC_7 + ex_Ca_to_Mg)
  
  # create overlap
  sp4@horizons[2,]$bottom <- sp4@horizons[2,]$bottom + 12
  
  # strict=TRUE byhz=FALSE removes overlap via whole profile
  expect_message({x2 <- slab(sp4, ~ K + Mg + Ca + CEC_7 + ex_Ca_to_Mg, strict = TRUE, byhz = FALSE)})
  expect_equal(nrow(x2), 245)
  
  # strict=TRUE / strict=FALSE (with default byhz) -- we get same number of rows with overlap as without
  expect_silent(x3 <- slab(sp4, ~ K + Mg + Ca + CEC_7 + ex_Ca_to_Mg, strict = FALSE))
  expect_equal(nrow(x3), 245)
  
  # try with a larger constant slab structure
  expect_silent(x3 <- slab(sp4, ~ K + Mg + Ca + CEC_7 + ex_Ca_to_Mg, slab.structure = 10, strict = FALSE))
  expect_equal(nrow(x3), 25)
})


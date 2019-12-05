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



test_that("slab calculations: mean, single profile", {
  
  data(sp1, package = 'aqp')
  depths(sp1) <- id ~ top + bottom
  
  # aggregate single profile
  # custom slabs
  a <- slab(sp1[1, ], fm = ~ prop, strict=TRUE, slab.structure=c(0,5,10,25,100), slab.fun = mean, na.rm=TRUE)
  
  # using mean and similar functions will cause slab to store single result / slab into 'value' column
  expect_true(any(grepl('value', names(a))))
  
  # calculations done by hand (DEB)
  # weighted mean, single profile
  # 0-5: 9.400
  expect_equal(a$value[1], 9.4, tolerance=0.0001)
  # 5-10: 7.000
  expect_equal(a$value[2], 7, tolerance=0.0001)
  # 10-25: 8.466
  expect_equal(a$value[3], 8.4666, tolerance=0.0001)
  # 25-100: 15.625
  expect_equal(a$value[4], 15.625, tolerance=0.0001)
  
})


test_that("slab calculations: mean, several profiles", {
  
  data(sp1, package = 'aqp')
  depths(sp1) <- id ~ top + bottom
  
  # aggregate single profile
  # custom slabs
  a <- slab(sp1, fm = ~ prop, strict=TRUE, slab.structure=c(0,5,10,25,100), slab.fun = mean, na.rm=TRUE)
  
  # weighted mean calculations done by 1-unit slices
  # note slice formula notiation works from horizon "tops"
  s.1 <- slice(sp1, 0:4 ~ prop, just.the.data = TRUE, strict = TRUE, top.down = TRUE)$prop
  s.2 <- slice(sp1, 5:9 ~ prop, just.the.data = TRUE, strict = TRUE, top.down = TRUE)$prop
  s.3 <- slice(sp1, 10:24 ~ prop, just.the.data = TRUE, strict = TRUE, top.down = TRUE)$prop
  s.4 <- slice(sp1, 25:99 ~ prop, just.the.data = TRUE, strict = TRUE, top.down = TRUE)$prop
  
  # 0-5
  expect_equal(a$value[1], mean(s.1, na.rm=TRUE), tolerance=0.0001)
  # 5-10
  expect_equal(a$value[2], mean(s.2, na.rm=TRUE), tolerance=0.0001)
  # 10-25
  expect_equal(a$value[3], mean(s.3, na.rm=TRUE), tolerance=0.0001)
  # 25-100
  expect_equal(a$value[4], mean(s.4, na.rm=TRUE), tolerance=0.0001)
  
})




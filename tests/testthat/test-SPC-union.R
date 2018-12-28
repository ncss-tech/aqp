context("SoilProfileCollection union method")

## make sample data
data(sp1, package = 'aqp')
depths(sp1) <- id ~ top + bottom
site(sp1) <- ~ group

sp1$x <- seq(-119, -120, length.out = length(sp1))
sp1$y <- seq(38, 39, length.out = length(sp1))

coordinates(sp1) <- ~ x + y


test_that("basic union tests", {
  
  # test data
  x <- sp1
  y <- sp1
  
  # alter horizon and site data in copy
  y$random <- runif(length(y))
  y$chroma <- NULL
  
  # add diagnostic hz
  diagnostic_hz(y) <- data.frame(id='P001', type='pizza')
  
  # this should not work, IDs aren't unqiue
  expect_error(union(list(x, y)))
  
  # fix IDs manually
  profile_id(y) <- sprintf("%s-copy", profile_id(y))
  
  # this should work
  z <- union(list(x,y))
  
  expect_match(class(z), 'SoilProfileCollection')
  expect_equal(length(z), length(x) + length(y))
  
  # full site/hz names
  expect_equal(siteNames(z), unique(c(siteNames(x), siteNames(y))))
  expect_equal(horizonNames(z), unique(c(horizonNames(x), horizonNames(y))))
  
  # diagnostic features
  expect_equal(diagnostic_hz(z)[[idname(z)]], 'P001-copy')
  
  # TODO: spatial data checks
})



context("SoilProfileCollection attribute guessing functions")

## sample data
data(sp3, package = 'aqp')
depths(sp3) <- id ~ top + bottom

## tests
test_that("basic functionality", {

  # historic horizon designation name (e.g. used by plotSPC)
  expect_equal(guessHzDesgnName(sp3), "name")

  # basic attribute name guessing
  expect_message(expect_equal(guessHzAttrName(sp3, "clay", ""), "clay"),
                "guessing horizon attribute 'clay' is stored in `clay`")

  # more complex attribute name guessing
  sp3$clay_r <- sp3$clay
  sp3$claytotal_r <- sp3$clay

  expect_message(expect_equal(guessHzAttrName(sp3, "clay", ""), "clay"),
                 "guessing horizon attribute 'clay' is stored in `clay`")

  expect_message(expect_equal(guessHzAttrName(sp3, "clay", c("_r", "total")), "claytotal_r"),
                 "guessing horizon attribute 'clay' is stored in `claytotal_r`")

  # basic attribute name guessing
  expect_equal(guessHzTexClName(sp3), "")

  # texcl
  horizons(sp3)$texcl <- "l"
  expect_equal(guessHzTexClName(sp3), "texcl")

  # texture
  horizons(sp3)$texture <-  horizons(sp3)$texcl
  horizons(sp3)$texcl <- NULL
  expect_equal(guessHzTexClName(sp3), "texture")

  # descriptive name
  sp3$hzdesgn <- sp3$name
  sp3$name <- NULL
  sp3$desgn <- 1:nrow(sp3)
  expect_equal(guessHzDesgnName(sp3), "hzdesgn")

  # unable to guess name
  sp3$foo <- sp3$hzdesgn
  sp3$hzdesgn <- NULL
  sp3$desgn <- NULL
  expect_message(expect_equal(guessHzDesgnName(sp3), NA),
                 "unable to guess column containing horizon designations")

  # custom name
  hzdesgnname(sp3) <- "foo"
  expect_equal(guessHzDesgnName(sp3), "foo")
})


context("generalize horizon names")

## sample data
x <- c('A', 'AC', 'Bt1', '^AC', 'C', 'BC', 'CB')

## tests

test_that("basic pattern matching", {

  # the third pattern will steal from the second
  n <- c('A', '^AC', 'C')
  p <- c('A', '\\^A', 'C')
  res <- generalize.hz(x, new = n, pat=p, non.matching.code = 'not-used')

  # matching only text, not factor levels
  expect_equal(as.character(res), c('A', 'C', 'not-used', 'C', 'C', 'C', 'C'))

  # check levels: these should match the ording of `n` + non matching code
  expect_equal(levels(res), c('A', '^AC', 'C', 'not-used'))
})


test_that("advanced pattern matching, requires perl", {

  # the third pattern may steal from the second
  n <- c('A', '^AC', 'C')
  # A -- ^A -- C without preceding A
  p <- c('A', '\\^A', '(?<!A)C')

  # the last pattern requires perl-compatible REGEX
  # error without perl=TRUE
  expect_error(suppressWarnings(generalize.hz(x, new = n, pat=p, non.matching.code = 'not-used')))

  # this should work
  res <- generalize.hz(x, new = n, pat=p, non.matching.code = 'not-used', perl=TRUE)

  # matching only text, not factor levels
  expect_equal(as.character(res), c('A', 'A', 'not-used', '^AC', 'C', 'C', 'C'))

  # check levels: these should match the ording of `n` + non matching code
  expect_equal(levels(res), c('A', '^AC', 'C', 'not-used'))
})

## TODO:
# more complex patterns: '/', lithologic discontinuities, look-ahead, anchoring, etc.

data(sp3)
depths(sp3) <- id ~ top + bottom

# crude generalized horizons
horizons(sp3) <- rbind(data.frame(hzID = sp3[,1]$hzID,     genhz = "A", stringsAsFactors = FALSE),
                       data.frame(hzID = sp3[,2:100]$hzID, genhz = "C", stringsAsFactors = FALSE))
# slightly realistic
sp3$genhz[sp3$clay > 16] <- "Bt"

test_that("guessGenHzLevels works as expected", {
  expect_equal(as.numeric(guessGenHzLevels(sp3)$median.depths), c(5,40,44))
})

test_that("hzTransitionProbabilities works as expected", {
  res <- hzTransitionProbabilities(sp3, "genhz")
  expect_equal(res, structure(c(0, 0, 0, 0.375, 1, 0.111111111111111,
                                0.625, 0, 0.888888888888889), .Dim = c(3L, 3L),
                              .Dimnames = list(c("A", "Bt", "C"),
                                               c("A", "Bt", "C")), ties = FALSE))
  res <- hzTransitionProbabilities(sp3, "genhz", loopTerminalStates = TRUE)
  expect_equal(res, structure(c(0, 0, 0, 0.375, 1, 0.111111111111111,
                                0.625, 0, 0.888888888888889),
                              .Dim = c(3L, 3L), .Dimnames = list(c("A", "Bt", "C"),
                                                                 c("A", "Bt", "C")), ties = FALSE))

  horizons(sp3)$genhz2 <- NA
  expect_message(hzTransitionProbabilities(sp3, "genhz2"))

  # ties in probability matrix
  dftest <- data.frame(id = c(1,1,1,2,2,2),
                       top = c(0,25,50,0,25,50),
                       bottom = c(25,50,100,25,50,100),
                       genhz = c("A","B","C","A","B","R"))
  depths(dftest) <- id ~ top + bottom
  expect_warning(hzTransitionProbabilities(dftest, "genhz"))
})

test_that("evalGenHZ works as expected", {
  res <- evalGenHZ(sp3, genhz = "genhz", vars = "clay")
  expect_equal(names(res), c("horizons","stats","dist"))
  expect_equal(as.character(res$stats$clay), c("11.26 (3.46)", "28.29 (11.49)", "8.3 (1.74)"))
})

test_that("generalize.hz works as expected", {
  res <- generalize.hz(sp3$name, new = "H", pat = ".*")
  expect_equal(levels(res), c("H","not-used"))
  expect_equal(as.character(res)[10], "H")

  res <- generalize.hz(sp3$genhz, new = c("A","Bt"), pat = c("A","[^A]"))
  expect_equal(levels(res), c("A","Bt","not-used"))
  expect_equal(as.character(res)[10], "Bt")
})

test_that("get.ml.hz works as expected", {
  
  data(sp3)
  depths(sp3) <- id ~ top + bottom
  
  # crude generalized horizons
  horizons(sp3) <- rbind(data.frame(hzID = sp3[,1]$hzID,     genhz = "A", stringsAsFactors = FALSE),
                         data.frame(hzID = sp3[,2:100]$hzID, genhz = "C", stringsAsFactors = FALSE))
  # slightly realistic
  sp3$genhz[sp3$clay > 16] <- "Bt"
  
  # make sure it runs
  res <- get.ml.hz(slab(sp3, fm = ~ genhz, cpm = 1, slab.structure = 0:max(sp3)))
  
  # expected output
  expect_equal(nrow(res), 3)
  expect_equal(res$top, c(0,10,60))
  expect_equal(res$confidence, c(50,49,67))
  
  # rough check on Brier scores / Shannon H
  expect_true(
    all(
      round(res$pseudo.brier, 2) == c(0.35, 0.48, 0.21)
      )
  )
  
  expect_true(
    all(
      round(res$mean.H, 2) == c(1.27, 1.01, 0.54)
    )
  )
  
})



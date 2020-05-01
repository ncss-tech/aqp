context("soil depth estimation")

## sample data
d <- 
  rbind(
  data.frame(
    id = c(1, 1, 1),
    top = c(0, 20, 35),
    bottom = c(20, 35, 110),
    name = c('A', 'Bt', 'C')
  ),
  data.frame(
    id = c(2, 2, 2),
    top = c(0, 20, 55),
    bottom = c(20, 55, 80),
    name = c('A', 'Bt', 'Cr')
  ),
  data.frame(
    id = c(3, 3, 3),
    top = c(0, 20, 48),
    bottom = c(20, 48, 130),
    name = c('A', 'Bt', 'Cd')
  ),
  data.frame(
    id = c(4, 4),
    top = c(0, 20),
    bottom = c(20, 180),
    name = c('A', 'R')
  ))


depths(d) <- id ~ top + bottom


## tests

test_that("error conditions", {
  
  # function will only accept a single profile
  expect_error(estimateSoilDepth(d, name='name', top='top', bottom='bottom'))
  
  # required column names: name, top, bottom not specified, defaults not appropriate
  # AGB: now capable of basic guessing of hzdesgn, and uses hzdepthcol slot internally
  # expect_error(estimateSoilDepth(d[1, ]))
})


test_that("basic soil depth evaluation, based on pattern matching of hz designation", {
  
  # setting hz desgn by argument works 
  res <- profileApply(d, estimateSoilDepth, name='name')
  expect_equivalent(res, c(110, 55, 48, 20))
  
  # setting hz desgn by argument works by guessing hzname
  res <- profileApply(d, estimateSoilDepth)
  expect_equivalent(res, c(110, 55, 48, 20))
  
  # setting nonexistent hzdesgn produces no error (by guessing hzname)
  res <- profileApply(d, estimateSoilDepth, name='goo')
  expect_equivalent(res, c(110, 55, 48, 20))
  
  # remove the guessable name, expect warning and bogus (-Inf) value
  d$xxx <- d$name
  d$name <- NULL
  expect_warning(expect_equivalent(estimateSoilDepth(d[1,], name='name'), -Inf))
  
  # backup use of S4 hzdesgncol slot in lieu of valid argument/guessable name column
  hzdesgnname(d) <- "xxx"
  res <- estimateSoilDepth(d[1,], name='name')  
  expect_equivalent(res, 110)
})


test_that("application of reasonable depth assumption of 150, given threshold of 100", {
  
  res <- profileApply(d, estimateSoilDepth, no.contact.depth=100, no.contact.assigned=150)
  
  expect_equivalent(res, c(150, 55, 48, 20))
})


test_that("depth to feature using REGEX on hzname: [Bt]", {
  
  # example from manual page, NA used when there is no 'Bt' found
  res <- profileApply(d, estimateSoilDepth, p='Bt', no.contact.depth=0, no.contact.assigned=NA)
  
  expect_equivalent(res, c(20, 20, 20, NA))
})


test_that("soil depth class assignment, using USDA-NRCS class breaks", {
  
  res <- getSoilDepthClass(d)
  
  # result should be a data.frame with as many rows as profiles in input
  expect_true(inherits(res, 'data.frame'))
  expect_equal(nrow(res), length(d))
  
  # depths, should be the same as prior tests using estimateSoilDepth
  expect_equivalent(res$depth, c(110, 55, 48, 20))
  
  # depth classes are returned as a factor sorted from shallow -> deep
  dc <- factor(c('deep', 'mod.deep', 'shallow', 'very.shallow'), levels=c('very.shallow', 'shallow', 'mod.deep', 'deep', 'very.deep'))
  expect_equivalent(res$depth.class, dc)
})



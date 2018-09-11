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
  expect_error(estimateSoilDepth(d[1, ]))
})


test_that("basic soil depth evaluation, based on pattern matching of hz designation", {
  
  res <- profileApply(d, estimateSoilDepth, name='name', top='top', bottom='bottom')
  
  expect_equivalent(res, c(110, 55, 48, 20))
})


test_that("application of reasonable depth assumption of 150, given threshold of 100", {
  
  res <- profileApply(d, estimateSoilDepth, name='name', top='top', bottom='bottom', 
                      no.contact.depth=100, no.contact.assigned=150)
  
  expect_equivalent(res, c(150, 55, 48, 20))
})


test_that("depth to feature using REGEX on hzname: [Bt]", {
  
  # example from manual page, NA used when there is no 'Bt' found
  res <- profileApply(d, estimateSoilDepth, name='name', top='top', bottom='bottom', 
                      p='Bt', no.contact.depth=0, no.contact.assigned=NA)
  
  expect_equivalent(res, c(20, 20, 20, NA))
})


test_that("soil depth class assignment, using USDA-NRCS class breaks", {
  
  res <- getSoilDepthClass(d, name='name', top='top', bottom='bottom')
  
  # result should be a data.frame with as many rows as profiles in input
  expect_equal(class(res), 'data.frame')
  expect_equal(nrow(res), length(d))
  
  # depths, should be the same as prior tests using estimateSoilDepth
  expect_equivalent(res$depth, c(110, 55, 48, 20))
  
  # depth classes are returned as a factor sorted from shallow -> deep
  dc <- factor(c('deep', 'mod.deep', 'shallow', 'very.shallow'), levels=c('very.shallow', 'shallow', 'mod.deep', 'deep', 'very.deep'))
  expect_equivalent(res$depth.class, dc)
})



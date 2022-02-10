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

  # not specified -> error 
  expect_error(profileApply(d, estimateSoilDepth))
  
  # not in horizonNames() -> error 
  expect_error(profileApply(d, estimateSoilDepth, name='goo'))
})


test_that("basic soil depth evaluation, based on pattern matching of hz designation", {

  # setting hz desgn by argument works
  res <- profileApply(d, estimateSoilDepth, name = 'name')
  expect_equivalent(res, c(110, 55, 48, 20))

  # use hzdesgnname(x)
  hzdesgnname(d) <- "name"
  res <- estimateSoilDepth(d[1,])
  expect_equivalent(res, 110)
})


test_that("application of reasonable depth assumption of 150, given threshold of 100", {
  
  res <- profileApply(d, estimateSoilDepth, name = 'name', no.contact.depth=100, no.contact.assigned=150)

  expect_equivalent(res, c(150, 55, 48, 20))
})


test_that("depth to feature using REGEX on hzname: [Bt]", {

  # example from manual page, NA used when there is no 'Bt' found
  res <- profileApply(d, estimateSoilDepth, name = 'name', p = 'Bt', no.contact.depth=0, no.contact.assigned=NA)

  expect_equivalent(res, c(20, 20, 20, NA))
})


test_that("depthOf - simple match", {
  expect_equal(depthOf(d[1,], "Cr|R|Cd"), NA_real_)
  expect_equal(depthOf(d[2,], "Cr|R|Cd"), 55)
  expect_equal(minDepthOf(d[2,], "Cr|R|Cd"), 55)
  expect_equal(maxDepthOf(d[2,], "Cr|R|Cd"), 55)
  expect_equal(maxDepthOf(d[2,], "Cr|R|Cd", top = FALSE), 80)
})

test_that("depthOf - multiple match", {
  expect_equal(depthOf(d[1,], "A|B|C"), c(0,20,35))
  expect_equal(depthOf(d[1,], "A|B|C", top = FALSE), c(20,35,110))
  expect_equal(minDepthOf(d[1,],"A|B|C"), 0)
  expect_equal(maxDepthOf(d[1,],"A|B|C"), 35)
  expect_equal(minDepthOf(d[1,], "A|B|C", top = FALSE), 20)
  expect_equal(maxDepthOf(d[1,], "A|B|C", top = FALSE), 110)
})

test_that("depthOf - no match", {
  expect_equal(depthOf(d[1,], "X"), NA_real_)
  expect_equal(depthOf(d[2,], "Cr|R|Cd", no.contact.depth = 50), NA_real_)

  expect_true(inherits(depthOf(d, "X"), 'data.frame'))
  d$name[1] <- "X"
  expect_true(inherits(maxDepthOf(d, "X"), 'data.frame'))
  
  d2 <- d
  d2$name <- NULL
  expect_error(depthOf(d2[1,], "A|B|C"))
})

test_that("soil depth class assignment, using USDA-NRCS class breaks", {

  res <- getSoilDepthClass(d, name = 'name')

  # result should be a data.frame with as many rows as profiles in input
  expect_true(inherits(res, 'data.frame'))
  expect_equal(nrow(res), length(d))

  # depths, should be the same as prior tests using estimateSoilDepth
  expect_equivalent(res$depth, c(110, 55, 48, 20))

  # depth classes are returned as a factor sorted from shallow -> deep
  dc <- factor(c('deep', 'mod.deep', 'shallow', 'very.shallow'), levels=c('very.shallow', 'shallow', 'mod.deep', 'deep', 'very.deep'))
  expect_equivalent(res$depth.class, dc)
})
  

test_that("data.table safety", {
  
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
  
  d <- data.table(d)
  depths(d) <- id ~ top + bottom
  
  res <- profileApply(d, estimateSoilDepth, name = 'name', no.contact.depth=100, no.contact.assigned=150)
  expect_equivalent(res, c(150, 55, 48, 20))
  
  sdc <- getSoilDepthClass(d, name = 'name', no.contact.depth=100, no.contact.assigned=150)
  expect_equivalent(sdc$depth, c(150, 55, 48, 20))
})



test_that("really deep", {
  
  d <-
    rbind(
      data.frame(
        id = c(1, 1, 1),
        top = c(0, 20, 35),
        bottom = c(20, 35, 2000),
        name = c('A', 'Bt', 'C')
      ))
  
  depths(d) <- id ~ top + bottom
  
  res <- profileApply(d, estimateSoilDepth, name = 'name')
  expect_equivalent(res, 2000)
  
  sdc <- getSoilDepthClass(d, name = 'name')
  expect_equivalent(sdc$depth, 2000)
  expect_equivalent(as.character(sdc$depth.class), 'very.deep')
})







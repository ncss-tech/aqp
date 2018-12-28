# pscs unit tests

context("particle size control section estimator")

data(sp1, package = 'aqp')
depths(sp1) <- id ~ top + bottom
site(sp1) <- ~ group

p <- sp1[1]
attr <- 'prop' # clay contents % 

q <- sp1[2]

test_that("estimatePSCS()", {
  e <- estimatePSCS(p, attr='prop', hzdesgn='name')
  expect_equivalent(e, c(49, 89))
  
  g <- estimatePSCS(q, attr='prop', hzdesgn='name')
  expect_equivalent(g, c(30, 59))
})




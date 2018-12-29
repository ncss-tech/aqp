# pscs unit tests

context("particle size control section estimator")

data(sp1, package = 'aqp')
depths(sp1) <- id ~ top + bottom
site(sp1) <- ~ group

p <- sp1[1]
attr <- 'prop' # clay contents % 

q <- sp1[2]

test_that("estimatePSCS()", {
  
  # this soil has a clay decrease then a clay increase and an argillic horizon
  # the argillic horizon ends at a bedrock contact
  e <- estimatePSCS(p, attr='prop', hzdesgn='name')
  expect_equivalent(e, c(49, 89))
  
  # this soil does not have an argillic, so it is 25-100 but has 5cm thick O horizon
  # and is moderately deep to bedrock contact
  g <- estimatePSCS(q, attr='prop', hzdesgn='name')
  expect_equivalent(g, c(30, 59))
  
})




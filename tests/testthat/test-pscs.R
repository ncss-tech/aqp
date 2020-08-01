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
  e <- estimatePSCS(p, clay.attr = 'prop', texcl.attr = "texture", hzdesgn = 'name')
  expect_equivalent(e, c(49, 89))

  # this soil does not have an argillic, so it is 25-100 but has 5cm thick O horizon
  # and is moderately deep to bedrock contact
  g <- estimatePSCS(q, clay.attr = 'prop', texcl.attr = "texture", hzdesgn = 'name')
  expect_equivalent(g, c(30, 59))

  ## special cases

  # thick (>50cm) Bt
  qbigbt <- sp1[3]
  qbigbt$name <- c("A","Bt1","Bt2","2Bt3", "2Bt4")
  g <- estimatePSCS(qbigbt, clay.attr = 'prop', texcl.attr = "texture", hzdesgn = 'name')
  expect_equivalent(g, c(2, 52))

  # thick Ap (>25cm)
  qbigap <- sp1[3]
  qbigap$name <- c("Ap1","Ap2","Ap3","C1", "C2")
  g <- estimatePSCS(qbigap, clay.attr = 'prop', texcl.attr = "texture", hzdesgn = 'name')
  expect_equivalent(g, c(35, 67))

  # soil less than 36cm deep
  qshallow <- trunc(q, 0, 27)
  g <- estimatePSCS(qshallow, clay.attr = 'prop', texcl.attr = "texture", hzdesgn = 'name')
  expect_equivalent(g, c(5, 27))

  # andisols
  qandisol <- q
  qandisol$tax_order <- "Andisols"
  g <- estimatePSCS(qandisol, clay.attr = 'prop', texcl.attr = "texture", hzdesgn = 'name')
  expect_equivalent(g, c(5, 59)) # note: starts at bottom of OSM

  # very shallow (bottom depth <25cm) argillic?
  qminiargi <- sp1[3]
  qminiargi$name <- c("A","Bt","C1","2C2", "2C3") #idk...
  g <- estimatePSCS(qminiargi, clay.attr = 'prop', texcl.attr = "texture", hzdesgn = 'name')
  expect_equivalent(g, c(2, 67)) # NOT 2, 13; would be 2,100 without limiting layer

  # error conditions
  q2 <- q
  expect_error(estimatePSCS(q2, clay.attr = 'foo', texcl.attr = "texture", hzdesgn = 'name'))
  q2$texture <- NULL
  expect_error(estimatePSCS(q2, clay.attr = 'prop', texcl.attr = "foo", hzdesgn = 'name'))
  q2 <- q
  q2$name <- NULL
  expect_error(estimatePSCS(q2, clay.attr = 'prop', texcl.attr = "texture", hzdesgn = 'foo'))
})




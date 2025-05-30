# pscs unit tests

context("particle size control section estimator")

data(sp1, package = 'aqp')
depths(sp1) <- id ~ top + bottom
site(sp1) <- ~ group

p <- sp1[1,]
attr <- 'prop' # clay contents %

q <- sp1[2,]

x <- data.frame(
  peiid = 706300,
  taxsubgrp = "Lithic Humicryods",
  top = c(0, 13, 16, 18, 24, 40),
  bottom = c(13, 16, 18, 24, 40, 65),
  name = c("Oi", "A", "E", "Bhs", "2C", "2R"),
  texture = c("SPM", "SIL", "SIL", "SIL", "SIL", "BR"),
  prop = c(0, 6, 6, 6, 6, 6)
)
depths(x) <- peiid ~ top + bottom
site(x) <- ~ taxsubgrp

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

test_that("estimatePSCS() thin soil profile with O horizon", {
  expect_equal(estimatePSCS(x, clay.attr = 'prop', texcl.attr = "texture", hzdesgn = 'name'), c(13, 40))
  expect_equal(estimatePSCS(c(q,x), clay.attr = 'prop', texcl.attr = "texture", hzdesgn = 'name'),
               data.frame(id = c("706300", "P002"), 
                          pscs_top = c(13, 30),
                          pscs_bottom = c(40, 59)))
})

test_that("estimatePSCS() multiple profiles",{
  e <- estimatePSCS(sp1, clay.attr = 'prop', texcl.attr = "texture", hzdesgn = 'name')
  expect_equal(e$pscs_top, c(49, 30, 2, 32, 5, 31, 25, 27, 28))
  expect_equal(e$pscs_bottom, c(89, 59, 52, 62, 55, 106, 100, 102, 103))
})

test_that("estimatePSCS() organic profiles",{
  sp1@horizons$name[1:60] <- "Oi"
  sp1@horizons$name[48:51] <- "W"
  sp1@horizons$name[57:60] <- "Oif"
  horizons(sp1)$lieutex <- NA
  oo <- estimatePSCS(sp1, hzdesgn = "name", clay.attr = "prop", texcl.attr = "texture", lieutex = "texture")
  expect_equal(oo$pscs_top, rep(0, 9))
  expect_equal(oo$pscs_bottom, c(89, 59, 67, 62, 68, 160, 160, 68, 133))
})

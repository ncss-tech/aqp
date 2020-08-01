context("estimate mollic epipedon bounds")
# construct a fake profile
spc <- data.frame(id=1, taxsubgrp = "Lithic Haploxeralfs",
                  hzname   = c("A","AB","Bt","BCt","R"),
                  hzdept   = c(0,  20, 32, 42,  49),
                  hzdepb   = c(20, 32, 42, 49, 200),
                  prop     = c(18, 22, 28, 24,  NA),
                  texcl    = c("l","l","cl", "l","br"),
                  d_value  = c(5,   5,  5,  6,  NA),
                  m_value  = c(2.5, 3,  3,  4,  NA),
                  m_chroma = c(2,   3,  4,  4,  NA))

# promote to SoilProfileCollection
depths(spc) <- id ~ hzdept + hzdepb
hzdesgnname(spc) <- 'hzname'
hztexclname(spc) <- 'texcl'

test_that("mollic.thickness.requirement", {
  expect_equal(mollic.thickness.requirement(spc, clay.attr = 'prop'), 18)
  expect_equal(mollic.thickness.requirement(spc,  clay.attr = 'prop', truncate = FALSE), 49 / 3)
  expect_equal(mollic.thickness.requirement(trunc(spc, 0, 9),  clay.attr = 'prop'), 10)
})

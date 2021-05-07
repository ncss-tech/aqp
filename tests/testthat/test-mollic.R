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

spc2 <- structure(list(id = c(1,1,1,1), hzname = c("A", "Bt1", "Bt2", "Bk"), 
                       hzdept = c(0L, 5L, 16L, 36L), 
                       hzdepb = c(5L, 16L, 36L, 66L), 
                       claytotest = c(11L, 30L, 32L, 11L), 
                       texcl = c("sl", "scl", "scl", "sl"), 
                       d_value = c(6L, 4L, 4L, 4L, 4L), m_chroma = c(2L, 2L,  3L, 3L)), 
                  class = "data.frame", row.names = c(NA, -4L))

# promote to SoilProfileCollection
depths(spc) <- id ~ hzdept + hzdepb
hzdesgnname(spc) <- 'hzname'
hztexclname(spc) <- 'texcl'

depths(spc2) <- id ~ hzdept + hzdepb
hzdesgnname(spc2) <- 'hzname'
hztexclname(spc2) <- 'texcl'


test_that("mollic.thickness.requirement", {
  expect_equal(mollic.thickness.requirement(spc, clay.attr = 'prop'), 18)
  expect_equal(mollic.thickness.requirement(spc,  clay.attr = 'prop', truncate = FALSE), 49 / 3)
  expect_equal(mollic.thickness.requirement(trunc(spc, 0, 9),  clay.attr = 'prop'), 10)
  
  # this is a controversial/undefined case:
  # 
  #  the Bk is identified as a cambic below an argillic 
  #  
  #  so most limiting crit is the bottom depth of cambic: 66/3 = 22 
  #  rather than 36/3 = 12 truncated to 18 which comes from argillic bottom depth/2nd carbonate upperbound
  expect_equal(mollic.thickness.requirement(spc2, clay.attr = 'claytotest'), 22)
})


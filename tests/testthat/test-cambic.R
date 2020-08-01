context("estimate cambic boundaries")

# construct a fake profile
# one dark colored horizon (AB) and two cambic subhorizons Bw + BC
spc <- data.frame(id = 1, taxsubgrp = "Lithic Haploxerepts",
                  hzname   = c("A","AB","Bw","BC","R"),
                  hzdept   = c(0,  20, 32, 42,  49),
                  hzdepb   = c(20, 32, 42, 49, 200),
                  clay     = c(19, 22, 22, 21,  NA),
                  texcl    = c("l","l","l","l","br"),
                  d_value  = c(5,   5,  5,  6,  NA),
                  m_value  = c(2.5, 3,  3,  4,  NA),
                  m_chroma = c(2,   3,  4,  4,  NA))

# promote to SoilProfileCollection
depths(spc) <- id ~ hzdept + hzdepb
hzdesgnname(spc) <- 'hzname'
hztexclname(spc) <- 'texcl'

test_that("getCambicBounds - basic functionality", {
  dfbound <- getCambicBounds(spc)
  expect_equal(nrow(dfbound), 1)
  expect_equal(as.numeric(dfbound[,c("cambic_top","cambic_bottom")]), c(32,49))

  # exclude by entry of non-cambic bounds
  expect_equal(nrow(getCambicBounds(spc, argi_bounds = c(32,49))), 0)

  # empty spc input
  expect_error(getCambicBounds(spc[0,]))
})

test_that("getCambicBounds - special cases", {
  spc2 <- spc
  spc2$texcl <- "S" # all sandy textures
  dfbound <- getCambicBounds(spc2)
  expect_equal(nrow(dfbound), 0)

  spc2 <- spc
  spc2$hzname <- c("A","Bt","BE","Bhs","Bt'")
  spc2$clay <- c(11,18,16,17,22)
  dfbound <- getCambicBounds(spc2)
  expect_equal(nrow(dfbound), 0)
})

test_that("getArgillicBounds - error conditions", {
  expect_error(getArgillicBounds(spc2, hzdesgn = "foo"))
  expect_error(getArgillicBounds(spc2, texcl.attr = "foo"))
  expect_error(getArgillicBounds(spc2, clay.attr = "foo"))
})

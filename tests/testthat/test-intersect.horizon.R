context("horizon intersection")

data(sp1, package = 'aqp')
depths(sp1) <- id ~ top + bottom
site(sp1) <- ~ group

p <- sp1[1]
attr <- 'prop' # clay contents % 

test_that("basic horizon intersection", {
  
})

intersect.horizon(p, 50, hzid='id') 

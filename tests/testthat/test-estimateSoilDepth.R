context("estimateSoilDepth")

## example data
d <- data.frame(
  id = '1',
  top = c(0, 10, 20, 30, 40, 50),
  bottom = c(10, 20, 30, 40, 50, 60),
  name = c('A', 'Bt1', 'Bt2', 'BC', 'Cr1', 'Cr2'),
  stringsAsFactors = FALSE
)

depths(d) <- id ~ top + bottom

# plotSPC(d, hz.depths = TRUE, name.style = 'center-center', cex.names = 1)

test_that("basic functionality", {
  
  # defaults
  e <- estimateSoilDepth(d, name = 'name')
  expect_equal(e, 40)
  
  # manually specify selection function
  # min == top of the top-most match
  e <- estimateSoilDepth(d, name = 'name', selection = min)
  expect_equal(e, 40)
  
  # max = top of the bottom-most match
  e <- estimateSoilDepth(d, name = 'name', selection = max)
  expect_equal(e, 50)
  
  # somewhere in-between
  e <- estimateSoilDepth(d, name = 'name', selection = mean)
  expect_equal(e, 45)
  
})

test_that("no match", {
  
  # no match, use depth of profile
  e <- estimateSoilDepth(d, name = 'name', p = 'xxx')
  expect_equal(e, 60)
  
  # no match, use `no contact` rules
  e <- estimateSoilDepth(d, name = 'name', p = 'xxx', no.contact.depth = 25, no.contact.assigned = 'nothing')
  expect_equal(e, 'nothing')
  
  # `no contact` depth rule not triggered
  e <- estimateSoilDepth(d, name = 'name', p = 'xxx', no.contact.depth = 75, no.contact.assigned = 'nothing')
  expect_equal(e, 60)
})



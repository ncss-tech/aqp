context("genSlabLabels")


test_that("reasonable results", {
  
  x <- genSlabLabels(slab.structure = 2, max.d = 50, n.profiles = 5)  
  
  # should be a factor
  expect_true(is.factor(x))
  
  # length of results (n.profiles * max.depth) 
  expect_true(length(x) == (5 * 50))
  
  # levels
  expect_equal(
    levels(x), 
    c("0-2", "2-4", "4-6", "6-8", "8-10", "10-12", "12-14", "14-16", 
      "16-18", "18-20", "20-22", "22-24", "24-26", "26-28", "28-30", 
      "30-32", "32-34", "34-36", "36-38", "38-40", "40-42", "42-44", 
      "44-46", "46-48", "48-50")
  )
  
  
})

test_that("1cm slices, single profile, variable max.d", {

  for(md in c(10, 25, 31, 55, 70, 150, 300)) {
    x <- genSlabLabels(slab.structure = 1, max.d = md, n.profiles = 1)  
    expect_true(length(x) == md)
  }
  
  
})


test_that("variable intervals", {

  x <- genSlabLabels(slab.structure = 25, max.d = 100, n.profiles = 1)  
  
  expect_equal(
    levels(x),
    c("0-25", "25-50", "50-75", "75-100")
  )
  
  x <- genSlabLabels(slab.structure = 50, max.d = 100, n.profiles = 1)  
  
  expect_equal(
    levels(x),
    c("0-50", "50-100")
  )

  
  x <- genSlabLabels(slab.structure = 33, max.d = 100, n.profiles = 1)  
  
  expect_equal(
    levels(x),
    c("0-33", "33-66", "66-99", "99-100")
  )  
})


test_that("user-defined top/bottom interval", {
  
  x <- genSlabLabels(slab.structure = c(0, 50), max.d = 100, n.profiles = 1)  
  expect_equal(levels(x), '0-50')
  
  x <- genSlabLabels(slab.structure = c(0, 25, 50), max.d = 50, n.profiles = 1)  
  expect_equal(levels(x), c('0-25', '25-50'))
  
  x <- genSlabLabels(slab.structure = c(0, 25, 50, 75, 80), max.d = 80, n.profiles = 1)  
  expect_equal(levels(x), c("0-25", "25-50", "50-75", "75-80"))
  
  
})



test_that("edge cases", {
  
  # bottom slab interval > max.d
  x <- genSlabLabels(slab.structure = c(0, 250), max.d = 100, n.profiles = 1)  
  
  expect_true(length(x) == 250)
  expect_equal(levels(x), '0-250')
  
  # bottom slab interval < max.d
  x <- genSlabLabels(slab.structure = c(0, 99), max.d = 100, n.profiles = 1)  
  
  expect_true(length(x) == 99)
  expect_equal(levels(x), '0-99')
  
})







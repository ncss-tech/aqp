context("SPC duplication")

# sample data
data(sp4)
depths(sp4) <- id ~ top + bottom
x <- duplicate(sp4, times = 2)


test_that("duplicate() functions as expected", {
  
  # new object should be 2x longer
  expect_true(length(x) == 2 * length(sp4))
  
  # all old IDs should be saved in .oldID
  id.setdiff <- setdiff(site(sp4)[['id']] , unique(x[['.oldID']]))
  expect_true(length(id.setdiff) == 0)
  
})

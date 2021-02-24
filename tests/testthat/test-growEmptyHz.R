context("growEmptyHz")

data(sp4)
depths(sp4) <- id ~ top + bottom

test_that("works as expected", {
  
  # grow to 100cm, all profiles modified
  g <- growEmptyHz(sp4, 100)
  expect_true(all(profileApply(g, max) == 100))
  
  # grow to 10cm, no profiles modified
  g <- growEmptyHz(sp4, 10)
  expect_true(all(profileApply(sp4, max) == profileApply(g, max)))
  
  # grow to 45cm, not all profiles modified
  g <- growEmptyHz(sp4, 45)
  expect_false(all(profileApply(g, max) == 45))
  
})


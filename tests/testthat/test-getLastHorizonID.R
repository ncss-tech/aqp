context("getLastHorizonID")



test_that("works as expected", {
  
  # IDs sort in non-intuitive alpha-order
  set.seed(10101)
  s <- lapply(1:20, random_profile, SPC = TRUE)
  s <- combine(s)
  res <- getLastHorizonID(s)
  
  # output
  expect_true(inherits(res, 'character'))
  expect_true(length(res) == length(s))
  
  # spot checks
  # plotSPC(s, name = 'hzID')
  expect_true(res[1] == "6")
  expect_true(res[2] == "11")
  expect_true(res[3] == "17")
  
  # ID ordering
  expect_true(all(profile_id(s) == names(res)))
  
  
  # IDs naturally sort in alpha-order
  s <- lapply(sample(letters, size = 26), random_profile, SPC = TRUE)
  s <- combine(s)
  res <- getLastHorizonID(s)
  
  # output with different sorting
  expect_true(inherits(res, 'character'))
  expect_true(length(res) == length(s))
  
  expect_true(all(profile_id(s) == names(res)))
  
})

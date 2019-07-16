context("findOverlap and fixOverlap")

## tests

test_that("findOverlap works as expected", {
  
  # data
  pos <- c(1, 1.1, 3, 4, 5, 5.2, 7, 8.6, 9, 10)
  
  # overlap with given thresholds
  res <- findOverlap(pos, thresh=0.6)
  expect_equal(res, c(1,2,5,6,8,9))
  
  res <- findOverlap(pos, thresh=0.3)
  expect_equal(res, c(1,2,5,6))
  
  res <- findOverlap(pos, thresh=0.1)
  expect_equal(res, vector(mode = 'integer', length = 0))
  
})


test_that("fixOverlaps works as expected", {
  
  # data
  pos <- c(1, 1.1, 3, 4, 5, 5.2, 7, 8.6, 9, 10)
  
  # test length of in/out
  res <- fixOverlap(pos, min.x = 0.8, max.x = length(pos) + 0.2)
  expect_equal(length(pos), length(res))
  
  # test rank order
  res <- fixOverlap(pos, min.x = 0.8, max.x = length(pos) + 0.2)
  expect_equal(rank(pos), rank(res))
  
  # test default boundary conditions
  expect_true(min(res) > 0.8)
  expect_true(max(res) < length(pos) + 0.2)

  # impossible task, default to integer sequence
  expect_message(res <- fixOverlap(pos, thresh=2, maxIter = 100), regexp = 'maximum number of iterations reached')
  expect_equal(res, 1:10)
  
})



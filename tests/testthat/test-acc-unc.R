context("classification accuracy and uncertainty")

# a good classifier
d.good <- data.frame(
  a = c(0.05, 0.05, 0.10),
  b = c(0.90, 0.85, 0.75),
  c = c(0.05, 0.10, 0.15),
  actual = c('b', 'b', 'b'),
  stringsAsFactors = FALSE
)

# a rather bad classifier
d.bad <- data.frame(
  a = c(0.05, 0.05, 0.10),
  b = c(0.90, 0.85, 0.75),
  c = c(0.05, 0.10, 0.15),
  actual = c('c', 'c', 'c'),
  stringsAsFactors = FALSE
)

# class labels are factors
d.factors <- data.frame(
  a = c(0.05, 0.05, 0.10),
  b = c(0.90, 0.85, 0.75),
  c = c(0.05, 0.10, 0.15),
  actual = c('b', 'b', 'b'),
  stringsAsFactors = TRUE
)

# hypothetical three class probabilitites: 0.5 0.3 0.2
probs <- c(5,3,2) / 10

test_that("shannonEntropy", {
  expect_equal(round(shannonEntropy(probs), 4), 1.4855)
  expect_equal(round(shannonEntropy(probs, 3), 4), 0.9372)
})

test_that("confusionIndex", {
  expect_equal(confusionIndex(probs), 0.8)
})

test_that("brierScore", {
  dff <- data.frame(diag(3))
  expect_equal(brierScore(dff, c("X1","X2","X3")), 1)

  set.seed(1)
  dff2 <- data.frame(matrix(runif(9), 3, 3))
  expect_equal(round(brierScore(dff2, c("X1","X2","X3")), 2), 1.31)
  
  # factor -> character conversion emits a message
  expect_message(brierScore(x = d.factors, classLabels = c('a', 'b', 'c'), actual = 'actual'))
  
  # results should be the same
  bs1 <- brierScore(x = d.good, classLabels = c('a', 'b', 'c'), actual = 'actual')
  bs2 <- brierScore(x = d.factors, classLabels = c('a', 'b', 'c'), actual = 'actual')
  
  expect_equal(bs1, bs2)
})





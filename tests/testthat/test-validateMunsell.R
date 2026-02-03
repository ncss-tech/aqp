context("Munsell notation validation")


test_that("validateMunsell() works as expected", {
  
  ex <- c(
    c('10YR 3/3', '5YR 2/2', '2.5G 8/8'), 
    c('2.6YR 3.3', '6YR 2.2/4.2', '2.5BG 3/3'),
    c('10Y 2.5/4', 'N 4/', 'N 4/0'), 
    c('2.5R /3', '10YR 4/'),
    c(NA, 'NA')
  )
  
  # data.frame(
  #   m = ex,
  #   v = validateMunsell(ex)
  # )
  
  expect_equal(
    validateMunsell(ex), 
    c(TRUE, TRUE, TRUE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE)
  )
  
})

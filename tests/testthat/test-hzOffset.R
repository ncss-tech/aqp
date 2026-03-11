test_that("hzOffset works", {
  
  # sample data where two profiles have no bottom depths (but all top depths are present)
  h <- data.frame(
    id = c(1, 1, 1, 2, 2, 2, 2, 3, 3),
    top = c(0:2, 0:3, 0:1) * 10,
    bottom = c(rep(NA_integer_, 7), c(10, 99))
  )
  expect_warning(depths(h) <- id ~ top + bottom)
  
  # index of last horizon in each profile
  bottom.idx <- which(hzID(h) %in% getLastHorizonID(h)) 
  #> [1] 3 7 9
  
  # horizons 1 index above/below bottom
  expect_equal(hzOffset(h, bottom.idx, offset = -1), c(2, 6, 8))
  expect_equal(hzOffset(h, bottom.idx, offset = 1), numeric(0))
  
  # simplify = FALSE (list results)
  expect_equal(hzOffset(h, bottom.idx, offset = -1, simplify = FALSE), list(2, 6, 8))
  expect_equal(hzOffset(h, bottom.idx, offset = 1, simplify = FALSE), 
               list(numeric(0), numeric(0), numeric(0)))
  
  # horizons 2 indices above bottom
  expect_equal(hzOffset(h, bottom.idx, offset = -2), c(1, 5))
  expect_equal(hzOffset(h, bottom.idx, offset = 2), numeric(0))
  
  # horizons 1 + 2 indices above bottom
  expect_equal(hzOffset(h, bottom.idx, offset = -(1:2)), c(1, 2, 5, 6, 8))
  expect_equal(hzOffset(h, bottom.idx, offset = 1:2), numeric(0))
  
  # there are never horizons above the first horizon (in a profile)
  first <- h[, 1, .HZID]
  expect_equal(hzOffset(h, first, offset = -1), numeric(0))
  
  # the second horizon is always right after the first (if profile has more than 1 horizon)
  expect_equal(hzOffset(h, first, offset = 1), first + 1)
  
  # single profile SPC has one value in result
  expect_equal(length(hzOffset(h[1,], first, offset = 1)), 1)
})

test_that("basic functionality of hzAbove() / hzBelow()", {
  
  data(sp4)
  depths(sp4) <- id ~ top + bottom
  
  # all horizons indices above horizons with ex-Ca:Mg ratio less than 1:10
  x <- hzAbove(sp4, ex_Ca_to_Mg < 0.1, SPC = FALSE, simplify = TRUE)
  expect_equal(x, c(1L, 2L, 3L, 10L, 11L, 12L, 23L, 24L, 25L, 26L))
  
  # all horizon indices below any horizon with ex-Ca:Mg ratio less than 1:10
  x <- hzBelow(sp4, ex_Ca_to_Mg < 0.1, SPC = FALSE, simplify = TRUE)
  expect_equal(x, c(4L, 27L))
  
})


test_that("interpret multiple reference horizons as a single span", {
  
  # example data
  x <- c(
    'P1:AAA|BwBwBwBw|CCCCCCC|CdCdCdCd',
    'P2:Ap|AA|E|BhsBhs|Bw1Bw1|CCCCC',
    'P3:A|Bt1Bt1Bt1|Bt2Bt2Bt2|Bt3|Cr|RRRRR',
    'P4:AA|EEE|BhsBhsBhsBhs|BwBw|CCCCC',
    'P5:AAAA|ACACACACAC|CCCCCCCCCCC|CdCdCd'
  )
  
  s <- quickSPC(x)

  # multiple matches
  .ex <- grepl('B', s$name)
  s$e <- .ex
  
  # interpret multiple reference hz as a single reference hz
  a <- hzAbove(s, .ex, SPC = FALSE, simplify = TRUE, single = TRUE)
  b <- hzBelow(s, .ex, SPC = FALSE, simplify = TRUE, single = TRUE)
  
  # hand-verified
  expect_equal(a, c(1L, 5L, 6L, 7L, 11L, 17L, 18L))
  expect_equal(b, c(3L, 4L, 10L, 15L, 16L, 21L))
    
})





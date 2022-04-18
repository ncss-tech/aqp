test_that("reduceSPC() works", {
  data(sp4)
  depths(sp4) <- id ~ top + bottom
  
  sp4$texcl <- ssc_to_texcl(sp4$sand, sp4$clay)
  
  hzdesgnname(sp4) <- "name"
  hztexclname(sp4) <- "texcl"
  
  site(sp4)$newsite <- LETTERS[1:length(sp4)]
  
  x <- reduceSPC(sp4, c("newsite", "CF"))
  
  expect_true(
    all(horizonNames(x) == c("id", "top", "bottom", "hzID", "name", "texcl", "CF")) &&
    all(siteNames(x) == c("id", "newsite"))
  )
})

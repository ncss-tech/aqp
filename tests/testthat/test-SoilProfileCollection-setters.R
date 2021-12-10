test_that(".prototypeSPC works", {
  # basic functionality: create a 0 length SPC with specified id ~ top + bottom
  x <- .prototypeSPC("id", c("top", "bottom"))
  expect_true(inherits(x, 'SoilProfileCollection'))
  expect_equal(length(x), 0)
  expect_equal(names(x), c(horizons1 = "id", horizons2 = "hzID", 
                           horizons3 = "top", horizons4 = "bottom"))
  
  # one additional horizon column works (degenerate case)
  x1 <- .prototypeSPC("id", c("top", "bottom"), 
                      hz = data.frame(id = 1, top = NA, bottom = NA, foo = "bar")[0,])
  x2 <- .prototypeSPC("id", c("top", "bottom"),  
                      hz = data.frame(id = 1:10, top = NA, bottom = NA, foo = "bar"))
  expect_equal(length(x1), 0) # zero length in, zero length out
  expect_true(length(x2) == 10 && nrow(x2) == 10) # all NA depths -> 1 NA row / profile
  expect_true("foo" %in% horizonNames(x1) && "foo" %in% horizonNames(x2))
  
  # two or more additional column (site and horizon) with depth filling works
  x3 <- .prototypeSPC("id", c("top", "bottom"), 
                      hz = data.frame(id = 1, top = NA, bottom = NA, foo = "bar", baz = "qux"),
                      st = data.frame(id = 1, sitevar = "eroded"),
                      fill_top = 0, fill_bottom = 200)
  expect_equal(length(x3), 1)
  expect_true(all(c("foo", "baz") %in% horizonNames(x3)))
  expect_true(all(c("id", "sitevar") %in% siteNames(x3)))
  expect_true(x3$top == 0 & x3$bottom == 200)
})

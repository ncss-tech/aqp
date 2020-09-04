context("profile-level harmonization of horizon properties")

set.seed(1)
spc <- aqp::union(lapply(1:10, random_profile, SPC = TRUE))

test_that("basic profile denormalization of \"range\" for one property", {
  expect_silent({
     h1 <- harmonize(spc, x.names = list(prop = c(q05 = "p1", q50 = "p2", q95 = "p3")), keep.cols = "p5")
    })
  
  # basic checks on [random] result
  expect_equal(length(h1), 30)
  expect_equal(nrow(h1), 141)
  expect_equal(round(mean(h1$prop),6), -0.390285)
  
  # original names are gone
  expect_true(!any(c("p1","p2","p3") %in% horizonNames(h1)))
  
  # harmonized name + keep columns are presnt
  expect_true(all(c("prop","p5") %in% horizonNames(h1)))
})

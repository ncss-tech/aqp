context("profile-level harmonization of horizon properties")

set.seed(1)
spc <- aqp::union(lapply(1:10, random_profile, SPC = TRUE))

# this method should be "immune" to column name ordering -- so shuffle them
spc.nhzcol <- length(horizonNames(spc))
replaceHorizons(spc) <- horizons(spc)[,sample(1:spc.nhzcol, spc.nhzcol)]

test_that("basic profile denormalization of \"range\" for one property", {
  expect_silent({
     h1 <- harmonize(spc, x.names = list(prop = c(q05 = "p1", q50 = "p2", q95 = "p3")), keep.cols = "p5")
    })
  
  # basic checks on [random] result
  expect_equal(length(h1), 30)
  expect_equal(nrow(h1), 141)
  expect_equal(round(mean(h1$prop),6), -0.390285)
  
  # original names are gone harmonized name + keep columns are present and in order
  expect_true(all(c("id","top","bottom","prop","p5","hzID") == horizonNames(h1)))
})

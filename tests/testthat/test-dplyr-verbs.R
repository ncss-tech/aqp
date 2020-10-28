context("dplyr-like verbs")

data(sp3)
depths(sp3) <- id ~ top + bottom
site(sp3)$group <- "A"
sp3$group[3:6] <- "B"

test_that("mutate & mutate_profile", {

  # mutate
  res <- mutate(sp3, thickness = bottom - top)
  expect_equal(mean(res$thickness), 18.5652174)
  # plot(res, color="thickness")

  # mutate_profile
  res <- mutate_profile(res, relthickness = (bottom - top) / sum(thickness))
  expect_equal(mean(res$relthickness), 0.2173913)
  
  # degenerate case where most profiles have only one horizon
  expect_warning(res2 <- mutate_profile(trunc(res, 0, 5), rt2 = (bottom - top) / sum(thickness)))
  expect_true(length(res2$rt2) == nrow(res2))
})

test_that("group_by & summarize", {

  sp3 <- group_by(sp3, group)
  expect_equal(metadata(sp3)$aqp_group_by, "group")

  # mean for A and B group horizon data
  summa <- summarize(sp3, round(mean(clay)))
  expect_equal(summa, structure(list(group = c("A", "B"),
                                     `round(mean(clay))` = c(11, 29)),
                                class = "data.frame", row.names = c(NA, -2L)))
})

#
# test_that("", {
#
# })

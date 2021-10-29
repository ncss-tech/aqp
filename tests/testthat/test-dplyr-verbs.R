context("dplyr-like verbs")

data(sp3)
depths(sp3) <- id ~ top + bottom
site(sp3)$group <- "A"
sp3$group[3:6] <- "B"

test_that("mutate & mutate_profile", {

  # mutate
  res <- transform(sp3, thickness = bottom - top)
  expect_equal(mean(res$thickness), 18.5652174)
  # plot(res, color="thickness")

  # mutate_profile
  res <- mutate_profile(res, relthickness = (bottom - top) / sum(thickness))
  expect_equal(mean(res$relthickness), 0.2173913)

  # degenerate case where most profiles have only one horizon
  (res2 <- mutate_profile(trunc(res, 0, 5), rt2 = (bottom - top) / sum(thickness)))
  expect_true(length(res2$rt2) == nrow(res2))
  
  # forcing horizon level result into site produces an error
  expect_error({res3 <- mutate_profile(trunc(res, 0, 5), rt2 = (bottom - top) / sum(thickness), horizon_level = FALSE)})
  
  # however forcing a site-level result into horizon works
  res4 <- mutate_profile(trunc(res, 0, 5), rt3 = sum((bottom - top) / sum(thickness)), horizon_level = TRUE)
  expect_equal(length(res4$rt3), nrow(res4))
})

test_that("group_by & summarize", {

  sp3 <- groupSPC(sp3, group)
  expect_equal(metadata(sp3)$aqp_group_by, "group")

  # mean for A and B group horizon data
  summa <- summarizeSPC(sp3, round(mean(clay)), round(sd(clay)))
  expect_equal(summa, structure(list(group = c("A", "B"),
                                     `round(mean(clay))` = c(11,29),
                                     `round(sd(clay))` = c(5, 12)),
                                class = "data.frame", row.names = c(NA, -2L)))
})

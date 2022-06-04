data(sp3)
depths(sp3) <- id ~ top + bottom
site(sp3)$group <- "A"
sp3$group[3:6] <- "B"

test_that("groupSPC & summarizeSPC", {
  
  sp3 <- groupSPC(sp3, group)
  expect_equal(metadata(sp3)$aqp_group_by, "group")
  
  # mean for A and B group horizon data
  summa <- summarizeSPC(sp3, round(mean(clay)), round(sd(clay)))
  expect_equal(summa, structure(list(group = c("A", "B"),
                                     `round(mean(clay))` = c(11,29),
                                     `round(sd(clay))` = c(5, 12)),
                                class = "data.frame", row.names = c(NA, -2L)))
})

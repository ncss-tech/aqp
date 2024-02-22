data(sp3)
depths(sp3) <- id ~ top + bottom

test_that("transform & mutate_profile", {
  
  # transform
  res <- transform(sp3, thickness = bottom - top)
  expect_equal(sum(res$thickness), 854)
  
  # transform (existing column)
  res <- transform(sp3, thickness = (bottom - top) / 100)
  expect_equal(sum(res$thickness), 8.54)
  
  # mutate_profile
  res <- mutate_profile(res, relthickness = (bottom - top) / (sum(thickness) * 100))
  expect_equal(sum(res$relthickness), 10)
  
  # mutate_profile (two existing columns)
  res <- mutate_profile(res, thickness = bottom - top,
                             relthickness = (thickness) / sum(thickness),
                             sumrelthickness1 = sum(relthickness))
  res <- mutate_profile(res, sumrelthickness2 = sum(relthickness))
  expect_equal(mean(res$relthickness), 0.2173913)
  expect_equal(length(res$sumrelthickness1), 10)
  expect_equal(length(res$sumrelthickness2), 10)

  # mutate existing column name (using same column as input)
  res <- mutate_profile(res, thickness = thickness / 10,
                             thickness = thickness * 10)
  
  # degenerate case where most profiles have only one horizon
  res2 <- mutate_profile(trunc(res, 0, 5), rt2 = (bottom - top) / sum(thickness))
  expect_true(length(res2$rt2) == nrow(res2))
  
  # forcing horizon level result into site produces an error
  expect_error({res3 <- mutate_profile(trunc(res, 0, 5), rt2 = (bottom - top) / sum(thickness), horizon_level = FALSE)})
  
  # however forcing a site-level result into horizon works
  res4 <- mutate_profile(trunc(res, 0, 5), rt3 = sum((bottom - top) / sum(thickness)), horizon_level = TRUE)
  expect_equal(length(res4$rt3), nrow(res4))
})

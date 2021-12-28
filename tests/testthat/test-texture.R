context("fragvol_to_texmod")


# fragvol_to_texmod ----
test_that("increasing volume of one size class", {
  expect_equal(c("by", "byv", "byx", NA), fragvol_to_texmod(data.frame(boulders = c(25, 48, 75, 95)))$texmod)
  expect_equal(c(NA, NA, NA, "by"), fragvol_to_texmod(data.frame(boulders = c(25, 48, 75, 95)))$lieutex)
})


test_that("2:1 rules for dominant/largest size class", {
  expect_equal("cbx", fragvol_to_texmod(data.frame(gravel = 20, cobbles = 40))$texmod)
  expect_equal("grx", fragvol_to_texmod(data.frame(gravel = 40, cobbles = 20))$texmod)
})


test_that("no fragments > 15%", {
  expect_equal(NA_character_, fragvol_to_texmod(data.frame(gravel = 1, cobbles = 2, flagstones = 10))$texmod)
})
  

test_that("sum of fragments plus parafragments >15%, but nopf<15 and pf<15", {
  expect_equal("pgr", fragvol_to_texmod(data.frame(gravel = 14, paragravel = 2))$texmod)
})


## texture_to_texmod ----

test_that("very cobbly loam (works)", {
  expect_equal(texture_to_texmod("CBV-L"), "cbv")
})

test_that("ashy cobbly sandy loam (works)", {
  expect_equal(texture_to_texmod("ASHY-CB-SL"), "cb")
})

test_that("ashy boulders (works)", {
  expect_equal(texture_to_texmod("ASHY-BY"), NA_character_)
})

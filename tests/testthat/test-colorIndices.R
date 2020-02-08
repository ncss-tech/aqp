context("Profile- and Horizon-level Color Indices")

data(sp1)
# convert colors from Munsell to hex-encoded RGB
sp1$soil_color <- with(sp1, munsell2rgb(hue, value, chroma))

# promote to SoilProfileCollection
depths(sp1) <- id ~ top + bottom
site(sp1) <- ~ group

## tests

test_that("horizonColorIndices works as expected", {
  # implicit left-join
  horizons(sp1) <- horizonColorIndices(sp1, hue="hue", value="value", chroma="chroma")
  expect_equivalent(length(sp1$hurst_redness), 60)
})

test_that("harden.rubification works as expected", {
  soil_hue <- "5YR"
  soil_chroma <- 4
  
  pm_hue <- "2.5Y"
  pm_chroma <- 2
  
  expect_equivalent(harden.rubification(soil_hue, soil_chroma, pm_hue, pm_chroma), 50) 
})

test_that("harden.melanization works as expected", {
  soil_value <- 2.5
  pm_value <- 5
  expect_equivalent(harden.melanization(soil_value, pm_value), 25)
})

test_that("thompson.bell.darkness works as expected", {
  expect_equivalent(thompson.bell.darkness(sp1[1], value="value", chroma="chroma"), 5.5)
})
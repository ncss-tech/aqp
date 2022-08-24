test_that("ph_to_rxnclass() works", {
  expect_equal(ph_to_rxnclass(6.2), "Slightly Acid")
  expect_equal(ph_to_rxnclass(c(6.2, NA, 6.3)), c("Slightly Acid", NA, "Slightly Acid"))
})

test_that("rxnclass_to_ph() works", {
  expect_equal(rxnclass_to_ph("Slightly Acid"), c(pH_low = 6.05, pH_high = 6.55))
  
  expect_equal(rxnclass_to_ph(c("Slightly Acid", NA, "Moderately Acid"), simplify = FALSE), 
               list(data.frame(pH_low = 5.55, pH_high = 6.55)))
})

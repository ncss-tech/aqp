test_that("ph_to_rxnclass() works", {
  expect_equal(ph_to_rxnclass(6.2), "Slightly Acid")
  expect_equal(ph_to_rxnclass(c(6.2, NA, 6.3)), c("Slightly Acid", NA, "Slightly Acid"))
  
  expect_equal(ph_to_rxnclass(c(6.2, NA, 6.8), as.is = FALSE),
               factor(c("Slightly Acid", NA, "Neutral"),
                      levels = ReactionClassLevels(as.is = TRUE),
                      ordered = TRUE))
  
  expect_equal(ph_to_rxnclass(c(6.2, NA, 6.8), as.is = FALSE, halfclass = TRUE),
               factor(c("Low Slightly Acid", NA, "Low Neutral"),
                      levels = ReactionClassLevels(halfclass = TRUE, as.is = TRUE),
                      ordered = TRUE))
  
  expect_equal(length(levels(ph_to_rxnclass(c(6.2, NA, 6.8), as.is = FALSE, droplevels = TRUE))), 2)
})

test_that("rxnclass_to_ph() works", {
  expect_equal(rxnclass_to_ph("Slightly Acid"), data.frame(pH_low = 6.05, pH_high = 6.55))
  
  expect_equal(rxnclass_to_ph(c("Slightly Acid", NA, "Moderately Acid"), simplify = FALSE), 
               list(data.frame(pH_low = 5.55, pH_high = 6.55)))
  
  expect_equal(rxnclass_to_ph(list(c("Slightly Acid", NA, "Moderately Acid"),
                                   c("Slightly Acid", NA, "Strongly Acid")), simplify = FALSE), 
               list(data.frame(pH_low = 5.55, pH_high = 6.55),
                    data.frame(pH_low = 5.05, pH_high = 6.55)))
})

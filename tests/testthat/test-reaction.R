test_that("ph_to_rxnclass() works", {
  expect_equal(as.character(ph_to_rxnclass(6.2)), "slightly acid")
  expect_equal(ph_to_rxnclass(c(6.2, NA, 6.3), as.is = TRUE), 
               c("slightly acid", NA, "slightly acid"))
  
  expect_equal(ph_to_rxnclass(c(6.2, NA, 6.8)),
               factor(c("slightly acid", NA, "neutral"),
                      levels = c("slightly acid", "neutral"),
                      ordered = TRUE))
  
  expect_equal(ph_to_rxnclass(c(6.2, NA, 6.8), halfclass = TRUE, droplevels = FALSE),
               factor(c("low slightly acid", NA, "low neutral"),
                      levels = ReactionClassLevels(halfclass = TRUE, as.is = TRUE),
                      ordered = TRUE))
  
  expect_equal(length(levels(ph_to_rxnclass(c(6.2, NA, 6.8)))), 2)
})

test_that("rxnclass_to_ph() works", {
  expect_equal(rxnclass_to_ph("slightly acid"), data.frame(pH_low = 6.05, pH_high = 6.55))
  
  expect_equal(rxnclass_to_ph(c("slightly acid", NA, "moderately acid"), simplify = FALSE), 
               list(data.frame(pH_low = 5.55, pH_high = 6.55)))
  
  expect_equal(rxnclass_to_ph(list(c("slightly acid", NA, "moderately acid"),
                                   c("slightly acid", NA, "strongly acid")), simplify = FALSE), 
               list(data.frame(pH_low = 5.55, pH_high = 6.55),
                    data.frame(pH_low = 5.05, pH_high = 6.55)))
})

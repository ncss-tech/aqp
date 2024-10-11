test_that("collapseHz works", {
  data("jacobs2000", package = "aqp")
  .BOTTOM <- NULL
  
  # use existing generalized horizon labels
  new_labels <- c("A", "E", "Bt", "Bh", "C")
  patterns <- c("A", "E", "B.*t", "B.*h", "C")

  # calculate a new SPC with genhz column based on patterns
  jacobs2000_gen <- generalizeHz(jacobs2000, new = new_labels, pattern = patterns)

  # create a missing value
  jacobs2000_gen$clay[19] <- NA
  
  # collapse that SPC based on genhz
  i <- collapseHz(jacobs2000_gen, hzdesgn = "genhz")
  expect_equal(length(jacobs2000), length(i))
  expect_equal(nrow(i), 26)
  expect_equal(i[7, , .BOTTOM], c(15, 41, 61, 132, 140, 152))

  # collapses adjacent horizons with same label
  i <- collapseHz(jacobs2000_gen, by = "genhz")
  ii <- collapseHz(jacobs2000_gen, by = "genhz", na.rm = TRUE)
  
  # no effect, horizon designations are unique within profiles
  j <- collapseHz(jacobs2000_gen, by = "name")
  
  expect_equal(nrow(j), 46)
  expect_equal(j[7, , .BOTTOM], jacobs2000[7, , .BOTTOM])
  
  # if using `by` argument, all values must not be NA
  expect_error(collapseHz(jacobs2000_gen, by = "matrix_color_munsell"),
               "Missing values are not allowed")
  
  # `by` column must also be a horizon-level variable
  expect_error(collapseHz(jacobs2000, by = "genhz"), "not a horizon-level variable")
  
  # matches input number of profiles
  expect_equal(length(jacobs2000), length(i))
  
  # horizons have been collapsed
  expect_equal(nrow(i), 26)
  
  # weighted mean (no NA values) works as expected (clay=47.15)
  expect_equal(i$clay[4],
               weighted.mean(jacobs2000_gen$clay[6:7], (jacobs2000_gen$bottom - jacobs2000_gen$top)[6:7]))
  
  # weighted mean (contains NA values, na.rm=FALSE) (clay is NA)
  expect_true(is.na(i$clay[11]))
  
  # weighted mean (contains NA values, na.rm=TRUE, clay=18.72414)
  expect_equal(ii$clay[11],
               weighted.mean(jacobs2000_gen$clay[17:20], (jacobs2000_gen$bottom - jacobs2000_gen$top)[17:20], na.rm = TRUE))
  
  # dominant condition (NA values retained)
  expect_true(is.na(i$depletion_munsell[13]))
  
  # dominant condition (NA values removed)
  expect_equal(ii$depletion_munsell[13], "10YR 8/2")
  
  plot(jacobs2000_gen, color = "concentration_pct")
  
  expect_equal(i[7, , .BOTTOM], c(15, 41, 61, 132, 140, 152))
  expect_true(is.numeric(i$clay))
  expect_true(is.numeric(j$clay))
  
  # "works" on empty SPC ()
  expect_equal(nrow(collapseHz(jacobs2000_gen[0,], by = "genhz")), 0)
                
  # works on SPC with filled profile (1 horizon with NA depths)
  all_na <- subsetHz(jacobs2000_gen[1,], TRUE)
  all_na$top <- NA
  all_na$bottom <- NA
  expect_warning(na_nonna <- c(all_na, jacobs2000_gen[2:5,]))
  expect_silent(f <- collapseHz(all_na, by = "genhz"))
  expect_silent(n <- collapseHz(na_nonna, by = "genhz"))
  expect_equal(nrow(n), 14)

  
  a_pattern <- c(`A` = "^A",
                 `E` = "E", 
                 `Bt` = "[ABC]+t", 
                 `C` = "^C", 
                 `foo` = "bar")
  x <- collapseHz(jacobs2000, a_pattern)
  expect_equal(length(jacobs2000), length(x))
  expect_equal(nrow(x), 29)
  expect_true(is.numeric(x$clay))
  
  m <- collapseHz(jacobs2000,
                  pattern = a_pattern,
                  AGGFUN = list(
                    matrix_color_munsell = function(x, top, bottom) {
                      thk <- bottom - top
                      if (length(x) > 1) {
                        xord <- order(thk, decreasing = TRUE)
                        data.frame(matrix_color_munsell = paste0(x, collapse = ";"),
                                   n_matrix_color = length(x))
                      } else {
                        data.frame(matrix_color_munsell = x,
                                   n_matrix_color = length(x))
                      }
                    }
                  )
                )
  profile_id(m) <- paste0(profile_id(m), "_collapse_custom")

  expect_true(all(c("matrix_color_munsell", "matrix_color_munsell.n_matrix_color") %in% names(m)))
  expect_equal(nrow(m), 29)
})

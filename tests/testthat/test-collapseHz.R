test_that("collapseHz works", {
  data("jacobs2000", package = "aqp")
  .BOTTOM <- NULL
  
  # use existing generalized horizon labels
  new_labels <- c("A", "E", "Bt", "Bh", "C")
  patterns <- c("A", "E", "B.*t", "B.*h", "C")

  # calculate a new SPC with genhz column based on patterns
  jacobs2000_gen <- generalizeHz(jacobs2000, new = new_labels, pattern = patterns)

  # collapse that SPC based on genhz
  i <- collapseHz(jacobs2000_gen, hzdesgn = "genhz")
  
  expect_equal(length(jacobs2000), length(i))
  expect_equal(nrow(i), 26)
  expect_equal(i[7, , .BOTTOM], c(15, 41, 61, 132, 140, 152))

  i <- collapseHz(jacobs2000_gen, by = "genhz")
  expect_equal(length(jacobs2000), length(i))
  expect_equal(nrow(i), 26)
  expect_equal(i[7, , .BOTTOM], c(15, 41, 61, 132, 140, 152))
  
  a_pattern <- c(`A` = "^A",
                 `E` = "E", 
                 `Bt` = "[ABC]+t", 
                 `C` = "^C", 
                 `foo` = "bar")
  x <- collapseHz(jacobs2000, a_pattern)
  expect_equal(length(jacobs2000), length(x))
  expect_equal(nrow(x), 29)
  
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

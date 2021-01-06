test_that("equivalentMunsellChips works", {

  # same "chip page position" different hue page; identify all perceptually equivalent chips

  expect_equal(as.list(equivalentMunsellChips("7.5YR",2,1)[[1]][,c("hue","value","chroma")]), list(hue = c("10YR", "2.5YR", "5YR", "7.5YR"),
                                                                                                   value = c(2,2,2,2),
                                                                                                   chroma = c(1,1,1,1)))

  expect_equal(as.list(equivalentMunsellChips("5YR",2,1)[[1]][,c("hue","value","chroma")]), list(hue = c("10R","2.5YR", "5YR", "7.5YR"),
                                                                                                 value = c(2,2,2,2),
                                                                                                 chroma = c(1,1,1,1)))

  expect_equal(as.list(equivalentMunsellChips("10YR",2,1)[[1]][,c("hue","value","chroma")]), list(hue = c("10YR","2.5Y", "7.5YR"),
                                                                                                  value = c(2,2,2),
                                                                                                  chroma = c(1,1,1)))
})

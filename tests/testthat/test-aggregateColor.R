context("aggregateColor")

## sample data
data(sp1, package = 'aqp')
depths(sp1) <- id ~ top + bottom
site(sp1) <- ~ group

sp1$soil_color <- munsell2rgb(sp1$hue, sp1$value, sp1$chroma)

## tests

test_that("basic functionality", {
  
  # group all horizons
  x <- sp1
  x$genhz <- rep('A', times=nrow(x))
  a <- aggregateColor(x, groups='genhz', col='soil_color')
  
  # object of correct structure
  expect_true(class(a) == 'list')
  expect_true(length(a) == 2)
  expect_true(all(names(a) == c('scaled.data', 'aggregate.data')))
  expect_true(names(a$scaled.data) == 'A')
  expect_true(a$aggregate.data$genhz == 'A')
  
  # number of colors in result should be the number of unique colors (after removing NA)
  expect_true(nrow(a$scaled.data$A) == length(na.omit(unique(x$soil_color))))
  # number of horizons in aggregate.data should be the same
  expect_true(a$aggregate.data$n == length(na.omit(unique(x$soil_color))))
  
  # colors / Munsell representations match
  test <- parseMunsell(a$scaled.data$A$munsell) == a$scaled.data$A$soil_color
  expect_true(all(test))
})

## TODO: test for expected error conditions


test_that("manual calculation, single profile", {
  
  x <- sp1[1, ]
  x$genhz <- rep('A', times=nrow(x))
  a <- aggregateColor(x, groups='genhz', col='soil_color')
  
  # known number of horizons / color
  # table(x$soil_color)
  expect_equal(a$scaled.data$A$n.hz, c(2,1,1,1))
  
  ## TODO: double check
  # weights
  expect_equal(round(a$scaled.data$A$weight, 3), c(0.342, 0.270, 0.258, 0.129))
  
  ## TODO: double check
  # weighted mean in CIE LAB space
  test <- with(a$aggregate.data, paste0(munsell.hue, ' ', munsell.value, '/', munsell.chroma))
  expect_equal(test, '10YR 3/2')
})

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
  expect_true(inherits(a,'list'))
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
test_that("expected error conditions", {
  expect_error(aggregateColor(x, groups='foo', col='soil_color'))
  expect_error(aggregateColor(x, groups='genhz', col='foo'))
  expect_error(aggregateColor(x, groups='genhz', col='soil_color', colorSpace = 'foo'))
  expect_error(aggregateColor(x, groups='genhz', col='soil_color', k=NA))
})


test_that("manual calculation using CIE2000 and LAB, single profile", {

  x <- sp1[1, ]
  x$genhz <- rep('A', times=nrow(x))
  a <- aggregateColor(x, groups='genhz', col='soil_color')
  a2 <- aggregateColor(x, groups='genhz', col='soil_color', colorSpace = 'LAB')
  a3 <- aggregateColor(x, groups='genhz', col='soil_color', colorSpace = 'LAB', k=1)

  # known number of horizons / color
  # table(x$soil_color)
  expect_equal(a$scaled.data$A$n.hz, c(2,1,1,1))
  expect_equal(a2$scaled.data$A$n.hz, c(2,1,1,1))
  expect_equal(a3$scaled.data$A$n.hz, 5)

  expect_equal(round(a$scaled.data$A$weight, 3), c(0.342, 0.270, 0.258, 0.129))

  test <- with(a$aggregate.data, paste0(munsell.hue, ' ', munsell.value, '/', munsell.chroma))
  test2 <- with(a2$aggregate.data, paste0(munsell.hue, ' ', munsell.value, '/', munsell.chroma))

  if(requireNamespace('farver', quietly = TRUE) & packageVersion("farver") >= '2.0.2') {
    expect_equal(test, '7.5YR 3/2')
    expect_equal(test2, '10YR 3/2')
  } else {
    expect_equal(test, '10YR 3/2')
    expect_equal(test2, '10YR 3/2')
  }
})

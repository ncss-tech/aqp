



x <- readRDS('e:/working_copies/soil-color/moist-dry-model/data/pedon-color-data.rds')

colorChart(x$moist)

colorChart(x$moist, x$genhz, size = FALSE, chip.cex = 1)


# groups > 1 have mal-formed N panels
# ... ! and missing chips
m <- c('10YR 4/4', 'N 2/', 'N 3/', '5Y 6/6', '5P 4/4')
g <- factor(c(1, 1, 2, 2, 2))
colorChart(m, g)


# works as expected
m <- c('10YR 4/4', '10YR 2/6', '5Y 3/8', '5Y 6/6', '5P 4/4')
g <- factor(c(1, 1, 2, 2, 2))
colorChart(m, g)

devtools::load_all()

library(aqp)
data("jacobs2000")

x <- jacobs2000[, 6:8, drop = FALSE]

# implement specialized output for profiles missing hz data
plotSPC(x, color = 'sand')

plotSPC(x, color = 'hzID')

horizons(x)$hzd <- 5
horizons(x)$hzto <- rep(1:4, length.out = nrow(x))

plotSPC(x, hz.distinctness.offset = 'hzd')
plotSPC(x, hz.topography.offset = 'hzto')



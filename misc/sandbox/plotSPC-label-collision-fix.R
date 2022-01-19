library(soilDB)

x <- fetchOSD(c('ames', 'lucy', 'zook', 'clarksville', 'fullerton', 'inks', 'vleck', 'pardee', 'cecil', 'pierre'))

# x <- fetchOSD('clarksville')

par(mar = c(0, 0, 0, 0), mfrow = c(2, 1))
par(mar = c(0, 0, 0, 0))

plotSPC(x, hz.depths = TRUE, name.style = 'center-center', plot.depth.axis = FALSE, cex.names = 0.8)
plotSPC(x, hz.depths = TRUE, name.style = 'center-center', plot.depth.axis = FALSE, cex.names = 0.8, fixLabelCollisions = TRUE)

plotSPC(x, hz.depths = TRUE, name.style = 'center-center', plot.depth.axis = FALSE, cex.names = 1)
plotSPC(x, hz.depths = TRUE, name.style = 'center-center', plot.depth.axis = FALSE, cex.names = 1, fixLabelCollisions = TRUE)

plotSPC(x, hz.depths = TRUE, name.style = 'center-center', plot.depth.axis = FALSE, cex.names = 0.66)
plotSPC(x, hz.depths = TRUE, name.style = 'center-center', plot.depth.axis = FALSE, cex.names = 0.66, fixLabelCollisions = TRUE)

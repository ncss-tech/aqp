library(aqp)
library(soilDB)
library(sharpshootR)

s <- c('inks' , 'pardee', 'clarksville', 'palau', 'hao', 'inks', 'eheuiki', 'puaulu')
x <- fetchOSD(s)


par(mar = c(0, 0, 0, 2))

.args <- list(width = 0.3, name.style = 'center-center', hz.depths = TRUE, cex.names = 1)
options(.aqp.plotSPC.args = .args)

plotSPC(x[1:2, ], fixLabelCollisions = FALSE)

plotSPC(x, hz.depths = FALSE)

plotSPC(x, fixLabelCollisions = FALSE)

plotSPC(x, fixLabelCollisions = FALSE, y.offset = 10)
plotSPC(x, fixLabelCollisions = FALSE, y.offset = 10, scaling.factor = 0.5)


plotSPC(x, fixOverlapArgs = list(method = 'S'))

plotSPC(x, fixOverlapArgs = list(method = 'E', q = 0.1))

plotSPC(x, fixOverlapArgs = list(method = 'E', q = 0.2))

plotSPC(x, fixOverlapArgs = list(method = 'E', q = 0.5))

plotSPC(x, fixOverlapArgs = list(method = 'E', q = 0.7))

plotSPC(x, fixOverlapArgs = list(method = 'E', q = 1))

plotSPC(x, fixOverlapArgs = list(method = 'E', q = 2))


plotSPC(x, fixOverlapArgs = list(method = 'E', q = 0.5))

plotSPC(x, fixOverlapArgs = list(method = 'E', q = 0.5), max.depth = 151)

plotSPC(x, fixOverlapArgs = list(method = 'E', q = 0.5), max.depth = 133)


plotSPC(x, fixOverlapArgs = list(method = 'E', q = 0.5), max.depth = 60)

plotSPC(x, fixOverlapArgs = list(method = 'E', q = 0.5), max.depth = 40)

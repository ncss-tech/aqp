library(aqp)
library(soilDB)
library(sharpshootR)

s <- c('inks' , 'pardee', 'clarksville', 'palau', 'hao', 'inks', 'eheuiki', 'puaulu')
x <- fetchOSD(s)


par(mar = c(0, 0, 0, 2))

.args <- list(width = 0.3, name.style = 'center-center', cex.names = 1)
options(.aqp.plotSPC.args = .args)

plotSPC(x)
plotSPC(x, max.depth = 100)
plotSPC(x, max.depth = 55)
plotSPC(x, max.depth = 33)


data("osd")

plotSPC(osd)

devtools::load_all()
library(soilDB)
library(sharpshootR)

s <- c('inks' , 'pardee', 'clarksville', 'palau', 'hao', 'inks', 'eheuiki', 'puaulu')
x <- fetchOSD(s)


par(mar = c(0, 0, 0, 2))

.args <- list(width = 0.3, name.style = 'center-center', cex.names = 1)
options(.aqp.plotSPC.args = .args)

plotSPC(x)

plotSPC(x, depth.axis = FALSE)

plotSPC(x, depth.axis = list())

plotSPC(x, depth.axis = list(cex = 0.66))

plotSPC(x, depth.axis = list(line = -1.5))

plotSPC(x, depth.axis = list(style = 'compact'))
plotSPC(x, depth.axis = list(style = 'traditional'))

plotSPC(x, depth.axis = list(style = 'x'))

plotSPC(x, max.depth = 100)
plotSPC(x, max.depth = 54)
plotSPC(x, max.depth = 33)
plotSPC(x, max.depth = 36)


plotSPC(x, hz.depths = TRUE)


data("osd")

plotSPC(osd)

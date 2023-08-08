devtools::load_all()
library(soilDB)
library(sharpshootR)

s <- c('inks' , 'pardee', 'clarksville', 'palau', 'hao', 'inks', 'eheuiki', 'puaulu')
x <- fetchOSD(s)

dev.off()

## aqp 1.x compatibility
par(mar = c(0, 0, 0, 2))
data("osd")

plotSPC(osd, plot.depth.axis = FALSE)
plotSPC(osd, plot.depth.axis = TRUE)
plotSPC(osd, cex.depth.axis = 2)
plotSPC(osd, axis.line.offset = 0)


dev.off()

par(mar = c(0, 0, 0, 2))

data("jacobs2000")
data("osd")

plotSPC(jacobs2000)
plotSPC(jacobs2000, max.depth = 250)

options(.aqp.plotSPC.args = NULL)
plotSPC(x)

par(mar = c(0, 0, 0, 1), mfcol = c(2, 1))
plotSPC(osd, depth.axis = list(style = 'compact'))
plotSPC(osd, depth.axis = list(style = 'traditional'))

par(mar = c(0, 0, 0, 3), mfcol = c(2, 1))
plotSPC(x, depth.axis = list(style = 'compact'))
plotSPC(x, depth.axis = list(style = 'traditional'))


dev.off()

par(bg = 'black', fg = 'white')
plotSPC(x)

par(bg = 'white', fg = 'black')
plotSPC(x)

dev.off()


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

plotSPC(x, depth.axis = list(interval = 15))
plotSPC(x, depth.axis = list(interval = 30))


plotSPC(x, max.depth = 210)
plotSPC(x, max.depth = 100)
plotSPC(x, max.depth = 54)
plotSPC(x, max.depth = 33)
plotSPC(x, max.depth = 36)


plotSPC(x, hz.depths = TRUE)


data("osd")

plotSPC(osd)

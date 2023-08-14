devtools::load_all()
library(soilDB)
library(sharpshootR)

s <- c('inks' , 'pardee', 'clarksville', 'palau', 'hao', 'inks', 'eheuiki', 'puaulu')
x <- fetchOSD(s)
z <- perturb(x[1, ], n = 25, boundary.attr = 'hzd')

dev.off()
options(.aqp.plotSPC.args = NULL)

## aqp 1.x compatibility
par(mar = c(0, 0, 0, 2))
data("osd")

# backwards compatibility
plotSPC(osd, plot.depth.axis = FALSE)
plotSPC(osd, plot.depth.axis = TRUE)
plotSPC(osd, cex.depth.axis = 2)
plotSPC(osd, axis.line.offset = 0)


dev.off()

# stricter interpretation of max.depth

data("jacobs2000")
data("osd")

plotSPC(jacobs2000)
plotSPC(jacobs2000, max.depth = 250)
plotSPC(jacobs2000, max.depth = 50)
plotSPC(jacobs2000, max.depth = 50, depth.axis = list(style = 'tape'))


# works
explainPlotSPC(osd, depth.axis = list(style = 'traditional', interval = 25), cex.names = 0.7)
explainPlotSPC(osd, depth.axis = list(style = 'compact', interval = 25), cex.names = 0.7)
explainPlotSPC(osd, depth.axis = list(style = 'tape', interval = 25), cex.names = 0.7)


# "tape" location is computed differently
explainPlotSPC(x, depth.axis = list(style = 'tape', interval = 25), cex.names = 0.7)

explainPlotSPC(z, print.id = FALSE, depth.axis = list(style = 'traditional', interval = 25))
explainPlotSPC(z, print.id = FALSE, depth.axis = list(style = 'compact', interval = 25))
explainPlotSPC(z, print.id = FALSE, depth.axis = list(style = 'tape', interval = 25))


# compare axis style
ragg::agg_png(filename = 'demo.png', width = 800, height = 1000, scaling = 1.5)

par(mar = c(1, 0, 0, 2), mfcol = c(3, 1))

options(.aqp.plotSPC.args = list(cex.names = 1, name = NA, name.style = 'center-center'))
plotSPC(osd, depth.axis = list(interval = 20, style = 'compact', line = -3))
mtext('compact', side = 2, line = -3)

plotSPC(osd, depth.axis = list(interval = 20, style = 'traditional', line = -3))
mtext('traditional', side = 2, line = -3)

plotSPC(osd, depth.axis = list(interval = 20, style = 'tape', line = 2))
mtext('tape', side = 2, line = -3)

dev.off()


par(mar = c(1, 0, 0, 3), mfcol = c(3, 1))
plotSPC(x, depth.axis = list(style = 'compact', line = -3))
plotSPC(x, depth.axis = list(style = 'traditional', line = -3))
plotSPC(x, depth.axis = list(style = 'tape', line = 2))


dev.off()

# test bg / fg colors
par(bg = 'black', fg = 'white')
plotSPC(x)

par(bg = 'white', fg = 'black')
plotSPC(x)

dev.off()



# test .aqp.plotSPC.args
# test new depth.axis interface
par(mar = c(0, 0, 0, 3))

.args <- list(width = 0.3, name.style = 'center-center', cex.names = 1)
options(.aqp.plotSPC.args = .args)

plotSPC(x)

plotSPC(x, depth.axis = FALSE)

plotSPC(x, depth.axis = list())

plotSPC(x, depth.axis = list(cex = 0.66))

plotSPC(x, depth.axis = list(line = -1.5))

plotSPC(x, depth.axis = list(style = 'compact'))
plotSPC(x, depth.axis = list(style = 'traditional'))
plotSPC(x, depth.axis = list(style = 'tape', line = 3))

plotSPC(x, depth.axis = list(style = 'x'))

plotSPC(x, depth.axis = list(interval = 15))
plotSPC(x, depth.axis = list(interval = 30))


plotSPC(x, max.depth = 210)
plotSPC(x, max.depth = 100)
plotSPC(x, max.depth = 54)
plotSPC(x, max.depth = 33)
plotSPC(x, max.depth = 36)

plotSPC(x, max.depth = 33, depth.axis = list(style = 'compact'))
plotSPC(x, max.depth = 33, depth.axis = list(style = 'tape'))


plotSPC(x, hz.depths = TRUE)


data("osd")

plotSPC(osd)

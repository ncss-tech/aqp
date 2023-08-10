devtools::load_all()
library(soilDB)
library(sharpshootR)

s <- c('inks' , 'pardee', 'clarksville', 'palau', 'hao', 'inks', 'eheuiki', 'puaulu')
x <- fetchOSD(s)

dev.off()
options(.aqp.plotSPC.args = NULL)

## aqp 1.x compatibility
par(mar = c(0, 0, 0, 2))
data("osd")

plotSPC(osd, plot.depth.axis = FALSE)
plotSPC(osd, plot.depth.axis = TRUE)
plotSPC(osd, cex.depth.axis = 2)
plotSPC(osd, axis.line.offset = 0)


dev.off()

# stricter interpretation of max.depth
par(mar = c(0, 0, 0, 2))

data("jacobs2000")
data("osd")

plotSPC(jacobs2000)
plotSPC(jacobs2000, max.depth = 250)
plotSPC(jacobs2000, max.depth = 50)

# works
par(mar = c(0, 0, 0, 2))
plotSPC(osd, depth.axis = list(style = 'traditional', interval = 25), cex.names = 0.7)
plotSPC(osd, depth.axis = list(style = 'compact', interval = 25), cex.names = 0.7)
plotSPC(osd, depth.axis = list(style = 'tape', interval = 25), cex.names = 0.7)


# "tape" is outside of the plot area
plotSPC(x, depth.axis = list(style = 'tape', interval = 25, line = 0), cex.names = 0.7)


# compare axis style
par(mar = c(0, 0, 0, 1), mfcol = c(3, 1))
plotSPC(osd, depth.axis = list(style = 'compact'))
plotSPC(osd, depth.axis = list(style = 'traditional'))
plotSPC(osd, depth.axis = list(style = 'tape'))

par(mar = c(0, 0, 0, 3), mfcol = c(3, 1))
plotSPC(x, depth.axis = list(style = 'compact'))
plotSPC(x, depth.axis = list(style = 'traditional'))
plotSPC(x, depth.axis = list(style = 'tape'))


dev.off()

# test bg / fg colors
par(bg = 'black', fg = 'white')
plotSPC(x)

par(bg = 'white', fg = 'black')
plotSPC(x)

dev.off()



# test .aqp.plotSPC.args
# test new depth.axis interface
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

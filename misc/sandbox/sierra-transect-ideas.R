library(aqp)
data("sierraTransect")

# horizon boundary viz
sierraTransect$hzd <- hzDistinctnessCodeToOffset(substr(sierraTransect$hz_boundary, 0, 1))

# split transects
g <- subset(sierraTransect, transect == 'Granite')

g.order <- order(g$elev)

par(mar = c(4, 0, 0, 2.5))

plotSPC(g, width = 0.3, name.style = 'center-center', cex.names = 0.9, plot.order = g.order, hz.distinctness.offset = 'hzd')
axis(1, at=1:length(g), labels=g$elev[g.order], line = 0)
mtext(text = 'Elevation (m)', side = 1, font = 2, line = 2.25)

library(aqp)
library(soilDB)
library(sharpshootR)

## TODO: get this list via line -> densified points -> SDA query -> component names
s <- c('exeter', 'san joaquin', 'tachi', 'tranquillity', 'zook', 'palau', 'pierre', 'chino', 'yolo')

x <- fetchOSD(s)

table(x$distinctness)
table(x$topography)

# x$hzdo <- runif(nrow(x), min=0.5, max=10)
x$hzdo <- hzDistinctnessCodeToOffset(x$distinctness, codes=c('very abrupt', 'abrupt', 'clear', 'gradual', 'diffuse'))
x$hzdo.const <- rep(10, times=nrow(x))

x$hzto <- hzTopographyCodeToOffset(x$topography, codes = c('smooth', 'wavy', 'irregular', 'broken'))


par(mar=c(0, 0, 0, 0), bg='black', fg='white')
plotSPC(x, width=0.33, cex.names=0.85, shrink=TRUE, name.style='left-center', plot.depth.axis=FALSE, hz.depths = TRUE)
plotSPC(x, width=0.33, cex.names=0.85, shrink=TRUE, name.style='left-center', plot.depth.axis=FALSE, hz.depths = TRUE, hz.distinctness.offset='hzdo')

# scaling factor applied to hz distinctness offsets
plotSPC(x, width=0.33, cex.names=0.85, shrink=TRUE, name.style='left-center', plot.depth.axis=FALSE, hz.depths = TRUE, hz.distinctness.offset='hzdo', scaling.factor = 0.5)

# hz topography
plotSPC(x, width=0.33, cex.names=0.85, shrink=TRUE, name.style='left-center', plot.depth.axis=FALSE, hz.depths = TRUE, hz.topography.offset = 'hzto')

# with line style
x$lty <- hzTopographyCodeToLineType(x$topography, codes = c('smooth', 'wavy', 'irregular', 'broken'))
plotSPC(x, width=0.33, cex.names=0.85, shrink=TRUE, name.style='left-center', plot.depth.axis=FALSE, hz.depths = TRUE, hz.topography.offset = 'hzto', hz.boundary.lty = 'lty')

# both!
plotSPC(x, width=0.33, cex.names=0.85, shrink=TRUE, name.style='left-center', plot.depth.axis=FALSE, hz.depths = TRUE, hz.distinctness.offset='hzdo', hz.topography.offset = 'hzto', hz.boundary.lty = 'lty')


par(mar=c(0, 0, 0, 3))
plotSPC(x, width=0.25, cex.names=0.85, shrink=TRUE)
plotSPC(x, width=0.33, cex.names=0.85, shrink=TRUE, name.style='left-top')
plotSPC(x, width=0.33, cex.names=0.85, shrink=TRUE, name.style='left-center')
plotSPC(x, width=0.33, cex.names=0.85, shrink=TRUE, name.style='left-center', hz.depths = TRUE)
plotSPC(x, width=0.33, cex.names=0.85, shrink=TRUE, name.style='center-center', hz.depths = TRUE)


par(mar=c(0,0,1,0), mfcol=c(4,1))
for(i in c('right-center', 'left-center', 'left-top', 'center-center')) {
  plotSPC(x, width=0.33, cex.names=0.85, shrink=TRUE, plot.depth.axis=FALSE, name.style=i)
  mtext(text = i, side=4, line=-4, cex=0.85)
}


dev.off()


plotSPC(x, width=0.33, cex.names=0.85, shrink=TRUE, name.style='right-center', plot.depth.axis=FALSE, hz.depths = TRUE, hz.distinctness.offset='hzdo.const')

plotSPC(x, width=0.33, cex.names=0.85, shrink=TRUE, name.style='left-center', plot.depth.axis=FALSE, hz.distinctness.offset='hzdo', divide.hz=FALSE)


par(mar=c(0, 0, 3, 3))
plotSPC(x, width=0.33, cex.names=0.85, shrink=TRUE, name.style='left-center', color='texture_class')
plotSPC(x, width=0.33, cex.names=0.85, shrink=TRUE, name.style='left-center', color='texture_class', col.palette.bias = 0.5)

plotSPC(x, width=0.33, cex.names=0.85, shrink=TRUE, name.style='left-center', color='bottom')
plotSPC(x, width=0.33, cex.names=0.85, shrink=TRUE, name.style='left-center', color='bottom', col.palette.bias = 2)


par(bg='black', fg='white')
SoilTaxonomyDendrogram(x, width=0.25, shrink=TRUE)
SoilTaxonomyDendrogram(x, width=0.33, cex.names=0.85, shrink=TRUE, name.style='left-top')
SoilTaxonomyDendrogram(x, width=0.33, cex.names=0.85, shrink=TRUE, name.style='left-center')

SoilTaxonomyDendrogram(x, width=0.33, cex.names=0.75, shrink=TRUE, name.style='center-center', plot.depth.axis=FALSE, hz.depths = TRUE, hz.distinctness.offset='hzdo')




x <- fetchOSD('scatlake')
y <- fetchKSSL('musick')

par(mar=c(0,0,0,0))

plotSPC(x, print.id = FALSE, plot.depth.axis = FALSE, cex.names = 1, scaling.factor = 1.2)
plotSPC(x, print.id = FALSE, plot.depth.axis = FALSE, cex.names = 1, scaling.factor = 1.2, name.style = 'left-top')
plotSPC(x, print.id = FALSE, plot.depth.axis = FALSE, cex.names = 1, scaling.factor = 1.2, name.style = 'left-center')
plotSPC(x, print.id = FALSE, plot.depth.axis = FALSE, cex.names = 1, scaling.factor = 1.2, name.style = 'left-center')

plotSPC(y, print.id = FALSE, plot.depth.axis = FALSE, width=0.3, shrink = FALSE)
plotSPC(y, print.id = FALSE, plot.depth.axis = FALSE, width=0.3, shrink = FALSE, name.style = 'left-top')
plotSPC(y, print.id = FALSE, plot.depth.axis = FALSE, width=0.3, shrink = FALSE, name.style = 'left-center')


par(mar=c(0,0,3,0))
plotSPC(y, print.id = FALSE, plot.depth.axis = FALSE, width=0.3, shrink = FALSE, color='clay')
plotSPC(y, print.id = FALSE, plot.depth.axis = FALSE, width=0.3, shrink = FALSE, color='clay', name.style = 'left-top')
plotSPC(y, print.id = FALSE, plot.depth.axis = FALSE, width=0.3, shrink = FALSE, color='clay', name.style = 'left-center')


data(sp1)
# convert colors from Munsell to hex-encoded RGB
sp1$soil_color <- with(sp1, munsell2rgb(hue, value, chroma))

# promote to SoilProfileCollection
depths(sp1) <- id ~ top + bottom

plotSPC(sp1, name.style='right-center')
plotSPC(sp1, name.style='left-top')
plotSPC(sp1, name.style='left-center')


# encode
sp1$hdo <- hzDistinctnessCodeToOffset(sp1$bound_distinct)
sp1$ht <- hzTopographyCodeToOffset(sp1$bound_topography)
horizons(sp1)$lty <- 1

# horizons(sp1)$hdo <- 10
# horizons(sp1)$ht <- 0

par(mar=c(0,0,0,1))
plotSPC(sp1, width=0.33, cex.names=0.7, shrink=TRUE, name.style='left-center', plot.depth.axis=FALSE, hz.distinctness.offset='hdo')
plotSPC(sp1, width=0.33, cex.names=0.7, shrink=TRUE, name.style='left-center', plot.depth.axis=FALSE, hz.distinctness.offset='hdo', lwd=2)



plotSPC(sp1, width=0.33, cex.names=0.7, shrink=TRUE)
plotSPC(sp1, width=0.33, cex.names=0.7, shrink=TRUE, hz.distinctness.offset='hdo')

horizons(sp1)$lty <- sample(1:3, size = nrow(sp1), replace = TRUE)
plotSPC(sp1, width=0.33, cex.names=0.7, shrink=TRUE, hz.distinctness.offset='hdo', hz.boundary.lty = 'lty', lwd = 1, name.style='left-center')

horizons(sp1)$lty <- 1
plotSPC(sp1, width=0.33, cex.names=0.7, shrink=TRUE, hz.distinctness.offset='hdo', hz.topography.offset = 'ht', lwd = 1, name.style='center-center')



## TODO: add horizon boundary data to fetchNASIS

data("loafercreek")






# https://cran.rstudio.com/web/packages/showtext/vignettes/introduction.html
# https://www.tsbvi.edu/resources/1087-download-braille-and-asl-specialty-fonts

library(aqp)
library(sharpshootR)
library(soilDB)
library(showtext)
library(svglite)

# soils of interest
s.list <- c('musick', 'cecil', 'drummer', 'amador', 'pentz', 'reiff', 
            'san joaquin','montpellier','grangeville','pollasky','ramona')

# fetch and convert data into an SPC
h <- fetchOSD(s.list)

# combine horizon designation + color in Munsell notation
h$munsell <- sprintf("%s %s %s", h$hue, h$value, h$chroma)
h$newname <- sprintf("%s\n%s", h$hzname, h$munsell)
hzdesgnname(h) <- 'newname'

SoilTaxonomyDendrogram(h, y.offset = 0.325, name = 'newname', width = 0.3, cex.taxon.labels = 0.55, name.style = 'center-center', hz.depths =  TRUE, plot.depth.axis = FALSE, cex.id = 0.5, cex.names = 0.55)



font_add('BRAILLE1', regular = 'E:/working-from-home-to-file/SPC-visually-impaired/BRAILLE1.ttf')

showtext_auto()

svglite(file = 'E:/working-from-home-to-file/SPC-visually-impaired/braille-example.svg', width = 16, height = 12, pointsize = 14, standalone = TRUE)

par(mar = c(0, 0, 0, 0), family = 'BRAILLE1')

# plot dendrogram + profiles
# SoilTaxonomyDendrogram(h, y.offset = 0.325, name = 'newname', width = 0.3, cex.taxon.labels = 0.55, name.style = 'center-center', hz.depths =  TRUE, plot.depth.axis = FALSE, cex.id = 0.66, cex.names = 0.55)

# plot dendrogram + profiles
plotSPC(h, width = 0.3, name.style = 'center-center', hz.depths =  TRUE, plot.depth.axis = FALSE, cex.id = 0.66, cex.names = 0.55, color = NA)

dev.off()

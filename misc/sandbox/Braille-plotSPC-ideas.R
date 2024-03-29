
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



font_add('BRAILLE1', regular = 'S:/NRCS/Archive_Dylan_Beaudette/AQP-related/SPC-visually-impaired/BRAILLE1.ttf')

showtext_auto()


svglite(file = 'E:/working_copies/ncss-tech.github.io/AQP/SVG-examples/braille-example.svg', width = 17, height = 12, pointsize = 14)

# pdf(file = 'braille-example.pdf', width = 16, height = 12, pointsize = 14)

par(mar = c(0, 0, 0, 0), family = 'BRAILLE1')

# plot dendrogram + profiles
# SoilTaxonomyDendrogram(h, y.offset = 0.325, name = 'newname', width = 0.3, cex.taxon.labels = 0.55, name.style = 'center-center', hz.depths =  TRUE, plot.depth.axis = FALSE, cex.id = 0.66, cex.names = 0.55)

# plot profiles
plotSPC(h, width = 0.33, name.style = 'center-center', hz.depths = TRUE, hz.depths.offset = 0.1, depth.axis = FALSE, cex.id = 0.66, cex.names = 0.66, color = NA, max.depth = 155, lwd = 1.5)

dev.off()


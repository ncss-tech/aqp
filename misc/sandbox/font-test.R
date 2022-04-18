
library(aqp)
library(sharpshootR)
library(soilDB)
library(showtext)

# soils of interest
s.list <- c('musick', 'cecil', 'drummer', 'amador', 'pentz', 'reiff', 
            'san joaquin','montpellier','grangeville','pollasky','ramona')

# fetch and convert data into an SPC
h <- fetchOSD(s.list)




png(file='font-test.png', width=850, height=450, type='cairo', res=120, antialias = 'subpixel')

showtext_auto()

par(family='Arial Narrow')

# plot dendrogram + profiles
SoilTaxonomyDendrogram(h, width=0.2, font.id=1)

dev.off()


# https://cran.rstudio.com/web/packages/showtext/vignettes/introduction.html
## Loading Google fonts (http://www.google.com/fonts)
font_add_google("Roboto Condensed")
showtext_auto()

png(file='font-test-RC.png', width=900, height=500, type='cairo', res=80, antialias = 'subpixel')

showtext_begin()
# plot dendrogram + profiles
SoilTaxonomyDendrogram(h, width=0.2, font.id=1)
showtext_end()

dev.off()

png(file='font-test-sans.png', width=900, height=500, type='cairo', res=80, antialias = 'subpixel')
par(family='sans')
# plot dendrogram + profiles
SoilTaxonomyDendrogram(h, width=0.2, font.id=1)
dev.off()

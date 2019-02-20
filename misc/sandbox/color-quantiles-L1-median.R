library(aqp)
library(soilDB)
library(Gmedian)
library(plyr)
library(latticeExtra)
library(cluster)
library(grDevices)
library(farver)



data(sp3)

depths(sp3) <- id ~ top + bottom
plot(sp3)
previewColors(sp3$soil_color)
x <- colorQuantiles(na.omit(sp3$soil_color))
plotColorQuantiles(x)

data("munsell")
rgb.color <- rgb(munsell[munsell$hue %in% c('2.5YR', '7.5YR') & munsell$value < 8 & munsell$chroma < 8, c('r', 'g', 'b')], maxColorValue = 1)
previewColors(rgb.color)
x <- colorQuantiles(rgb.color)
plotColorQuantiles(x)


data("loafercreek")
rgb.color <- na.omit(loafercreek$soil_color)
previewColors(rgb.color)
x <- colorQuantiles(rgb.color)
plotColorQuantiles(x)


soil <- 'drummer'
s <- fetchOSD(soil, extended = TRUE)
spc <- fetchOSD(c(soil, s$competing$competing))
# this will only work for established series, e.g. those that have been "mapped" somewhere
idx <- which(spc$series_status == 'established')
spc <- spc[idx, ]
# save family taxa and set of series names for later
fm.name <- unique(na.omit(spc$family))
s.names <- unique(site(spc)$id)
# get OSD + extended summaries for all competing series
s.competing.data <- fetchOSD(s.names, extended = TRUE)
spc <- s.competing.data$SPC

h <- horizons(spc)
h <- h[which(grepl('g', h$hzname)), ]
rgb.color <- na.omit(munsell2rgb(h$hue, h$value, h$chroma))

previewColors(rgb.color)
x <- colorQuantiles(rgb.color)
plotColorQuantiles(x)



s.list <- c('amador', 'redding', 'pentz', 'willows', 'pardee', 'yolo', 'hanford', 'cecil', 'sycamore', 'KLAMATH', 'MOGLIA', 'boomer', 'vleck', 'drummer', 'CANEYHEAD', 'musick', 'sierra', 'HAYNER', 'zook', 'argonaut', 'PALAU')
s <- fetchOSD(s.list)
h <- horizons(s)
rgb.color <- na.omit(munsell2rgb(h$hue, h$value, h$chroma))

previewColors(rgb.color)
x <- colorQuantiles(rgb.color)
plotColorQuantiles(x)



# get lab / morphologic data
x <- fetchKSSL(series='holland', returnMorphologicData = TRUE)

# extract pedons into SoilProfileCollection
s <- x$SPC

# extract horizon data from SPC
h <- horizons(s)

# simplify color data: 1 row / horizon, from morphologic data tables
x.colors <- simplifyColorData(x$morph$phcolor, id.var = 'labsampnum', wt='colorpct')

# merge color data into SPC
h <- join(h, x.colors, by='labsampnum', type='left', match='first')

# remove horizons that are missing moist colors
h <- subset(h, h$m_hue != '' & ! is.na(h$m_hue) & ! is.na(h$m_value) & ! is.na(h$m_chroma))

## for 
h$m_munsell <- paste0(h$m_hue, ' ', h$m_value, '/', h$m_chroma)
h$m_hue <- factor(h$m_hue, levels = c('10Y', '2.5Y', '10YR', '7.5YR', '5YR', '2.5YR'))


h.sub <- h[grep('^B', h$hzn_desgn), ]
table(h.sub$hzn_desgn)

previewColors(h.sub$moist_soil_color)
title('Holland - "B" Horizons')
x <- colorQuantiles(h.sub$moist_soil_color)
plotColorQuantiles(x)




xyplot(m_value ~ m_chroma | m_hue, as.table=TRUE, data=h.sub, subscripts = TRUE, xlim=c(0.5,8.5), ylim=c(0.5,8.5), scales=list(alternating=3, tick.number=8, y=list(rot=0)), xlab='Chroma', ylab='Value', layout=c(3,1), strip=strip.custom(bg=grey(0.85)), panel=function(x, y, subscripts=subscripts, ...) {
  
  p.data <- data.frame(x=x, y=y, col=h.sub$moist_soil_color[subscripts], m=h.sub$m_munsell[subscripts], stringsAsFactors = FALSE)
  tab <- prop.table(table(p.data$m, useNA = 'always'))
  tab <- as.data.frame(tab)
  names(tab) <- c('m', 'freq')
  p.data <- join(p.data, tab, by='m', type='left')
  p.data <- na.omit(p.data)
  p.data <- subset(p.data, subset=freq > 0.05)
  panel.grid(-1, -1)
  panel.xyplot(p.data$x, p.data$y, pch=15, col=p.data$col, cex=4 * sqrt(p.data$freq))
})


pp <- xyplot(m_value ~ m_chroma | m_hue + genhz, as.table=TRUE, data=h, subscripts = TRUE, xlim=c(0.5,8.5), ylim=c(0.5,8.5), scales=list(alternating=3, tick.number=8, y=list(rot=0)), xlab='Chroma', ylab='Value', subset=genhz != 'not-used', panel=function(x, y, subscripts=subscripts, ...) {
  
  p.data <- data.frame(x=x, y=y, col=h$soil_color[subscripts], m=h$m_munsell[subscripts], stringsAsFactors = FALSE)
  tab <- prop.table(table(p.data$m, useNA = 'always'))
  tab <- as.data.frame(tab)
  names(tab) <- c('m', 'freq')
  p.data <- join(p.data, tab, by='m', type='left')
  p.data <- na.omit(p.data)
  p.data <- subset(p.data, subset=freq > 0.05)
  panel.grid(-1, -1)
  panel.xyplot(p.data$x, p.data$y, pch=15, col=p.data$col, cex=4 * sqrt(p.data$freq))
})

useOuterStrips(pp, strip=strip.custom(bg=grey(0.85)), strip.left = strip.custom(bg=grey(0.85)))






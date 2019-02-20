library(soilDB)
library(sharpshootR)
library(igraph)
library(plyr)
library(reshape2)
library(markovchain)
library(RColorBrewer)
library(scales)
library(farver)
library(cluster)


## background
# https://en.wikipedia.org/wiki/Color_difference



# get lab / morphologic data
x <- fetchKSSL(series='pierre', returnMorphologicData = TRUE)

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

# re-assemble Munsell color notation for moist color
h$color <- paste0(h$m_hue, ' ', h$m_value, '/', h$m_chroma)

# pack horizon data back into SPC
horizons(s) <- h

# https://github.com/thomasp85/farver

cols <- parseMunsell(s[1, ]$color, return_triplets=TRUE)

# is this sRGB?
compare_colour(cols, from_space = 'rgb', white_from = "D65", method = 'cie2000')

# perceptual color change with depth, normalized to hz mid-point to mid-point distance in cm
colorDistanceByDepth <- function(i, cumulative=FALSE) {
  cols <- parseMunsell(i$color, return_triplets=TRUE)
  hd <- horizonDepths(i)
  i.h <- horizons(i)
  md <- (i.h[[hd[1]]] + i.h[[hd[2]]]) / 2
  
  # pair-wise color distances
  # is this sRGB?
  cols.dist <- compare_colour(cols, from_space = 'rgb', method = 'cie2000')
  
  ## TODO horizon boundary distinctness would make more sense
  # change with depth from midpoint to midpoint
  delta_depth <- diff(md)
  
  # change in distance
  # this comparing depth-wise, sequentially
  idx <- cbind(1:(ncol(cols.dist)-1), 2:ncol(cols.dist))
  delta_color_dist <- cols.dist[idx]
  
  ## TODO: ratio of D_color / boundary distinctness
  # for now, change with depth from midpoint to midpoint
  res <- delta_color_dist / delta_depth
    
  # there is no information for the first horizon, use 0 or distance from mean color?
  if(cumulative){
    res <- cumsum(c(0, delta_color_dist))
  } else {
    res <- c(0, delta_color_dist)
  }
  
  
  return(res)
}



s$color.dist <- round(profileApply(s, colorDistanceByDepth) * 10000)
s$cumulative.color.dist <- round(profileApply(s, colorDistanceByDepth, cumulative=TRUE) * 10000)

color.distances <- profileApply(s, colorDistanceByDepth, cumulative=TRUE, simplify = FALSE)

max.color.distance <- sapply(color.distances, max) * 10000
new.order <- order(max.color.distance)

par(mar=c(2,0,3,1), mfrow=c(2,1))
plot(s, color='moist_soil_color', name='cumulative.color.dist', plot.order=new.order, print.id=FALSE)
axis(side=1, at=1:length(s), labels = round(max.color.distance[new.order]), cex.axis=0.8)
title('Pierre Soil Sieres: CIE2000 Distance * 1000 / cm')

plot(s, color='cumulative.color.dist', print.id=FALSE, plot.order=new.order, col.legend.cex=0.85, col.label='Cumulative CIE2000 Distance * 10000 / cm')
axis(side=1, at=1:length(s), labels = round(max.color.distance[new.order]), cex.axis=0.8)





pig <- soilColorSignature(s, r = 'm_r', g = 'm_g', b='m_g', method = 'depthSlices')


# move row names over for distance matrix
row.names(pig) <- pig[, 1]
d <- daisy(pig[, -1])
dd <- diana(d)


par(mar=c(0,1,3,1))
plotProfileDendrogram(s, dd, dend.y.scale = max(d) * 2.5, scaling.factor = 0.4, y.offset = max(d) / 20, width=0.15, cex.names=0.45, color='moist_soil_color', print.id=FALSE, name='hzn_desgn')
title('Holland Soil Series: `depthSlices` based color signature')

plotProfileDendrogram(s, dd, dend.y.scale = max(d) * 2.5, scaling.factor = 0.4, y.offset = max(d) / 20, width=0.15, cex.names=0.45, color='color.dist', print.id=FALSE, name='hzn_desgn', col.legend.cex=0.85, col.label='CIE2000 Distance * 10000 / cm')

plotProfileDendrogram(s, dd, dend.y.scale = max(d) * 2.5, scaling.factor = 0.4, y.offset = max(d) / 20, width=0.15, cex.names=0.45, color='cumulative.color.dist', print.id=FALSE, name='hzn_desgn', col.legend.cex=0.85, col.label='Cumulative CIE2000 Distance * 10000 / cm')


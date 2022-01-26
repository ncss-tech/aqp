library(aqp)
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
x <- fetchKSSL(series='clarksville', returnMorphologicData = TRUE, simplifyColors = TRUE)

# extract pedons into SoilProfileCollection
s <- x$SPC

s <- subsetHz(s, ! is.na(m_hue) & ! is.na(m_value) & ! is.na(m_chroma))

s <- HzDepthLogicSubset(s)

s <- subApply(s, .fun = function(i) {nrow(i) > 1})

plotSPC(s[1:30, ])

s <- s[1:20, ]

s <- trunc(s, 0, 150)


# re-assemble Munsell color notation for moist color
horizons(s)$color <- sprintf("%s %s/%s", s$m_hue, s$m_value, s$m_chroma)

cols <- parseMunsell(s[1, ]$color, return_triplets=TRUE)

# sRGB
# must be scaled to [0, 255]
compare_colour(cols * 255, from_space = 'rgb', white_from = "D65", method = 'cie2000')


## TODO: this requires > 1 horizon
# perceptual color change with depth, normalized to hz mid-point to mid-point distance in cm
colorDistanceByDepth <- function(i, cumulative = FALSE) {
  cols <- parseMunsell(i$color, return_triplets = TRUE)
  hd <- horizonDepths(i)
  i.h <- horizons(i)
  md <- (i.h[[hd[1]]] + i.h[[hd[2]]]) / 2
  
  # pair-wise color distances
  # must be scaled to [0, 255]
  cols.dist <- compare_colour(cols * 255, from_space = 'rgb', method = 'cie2000')
  
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



s$color.dist <- round(profileApply(s, colorDistanceByDepth))
s$cumulative.color.dist <- round(profileApply(s, colorDistanceByDepth, cumulative=TRUE))

color.distances <- profileApply(s, colorDistanceByDepth, cumulative=TRUE, simplify = FALSE)

max.color.distance <- sapply(color.distances, max)
new.order <- order(max.color.distance)

par(mar=c(2.5, 0, 3, 1), mfrow=c(2,1))
plotSPC(s, color = 'moist_soil_color', name = 'color.dist', plot.order=new.order, print.id = FALSE, name.style = 'center-center', width = 0.35)
axis(side=1, at=1:length(s), labels = round(max.color.distance[new.order]), cex.axis=0.8)
title(bquote(Clarksville~Delta*E['00']/cm))

plot(s, name = 'cumulative.color.dist', color='cumulative.color.dist', print.id=FALSE, plot.order=new.order, col.legend.cex=0.85, col.label = bquote(Cumulative~Delta*E['00']/cm), name.style = 'center-center', width = 0.35)
axis(side=1, at=1:length(s), labels = round(max.color.distance[new.order]), cex.axis=0.8)





pig <- soilColorSignature(s, r = 'm_r', g = 'm_g', b='m_g', method = 'depthSlices')


# move row names over for distance matrix
row.names(pig) <- pig[, 1]
d <- daisy(pig[, -1])
dd <- diana(d)


dev.off()

par(mar=c(0,1,3,1))
plotProfileDendrogram(s, dd, dend.y.scale = max(d) * 2.5, scaling.factor = 0.4, y.offset = max(d) / 20, width=0.3, cex.names=0.45, color='moist_soil_color', print.id=FALSE, name='hzn_desgn', name.style = 'center-center')
title('Clarksville Soil Series: `depthSlices` based color signature')

plotProfileDendrogram(s, dd, dend.y.scale = max(d) * 2.5, scaling.factor = 0.4, y.offset = max(d) / 20, width=0.3, cex.names=0.45, color='color.dist', print.id=FALSE, name='hzn_desgn', name.style = 'center-center', col.legend.cex=0.85, col.label='CIE2000 Distance / cm')

plotProfileDendrogram(s, dd, dend.y.scale = max(d) * 2.5, scaling.factor = 0.4, y.offset = max(d) / 20, width=0.3, cex.names=0.45, color='cumulative.color.dist', print.id=FALSE, name='hzn_desgn', name.style = 'center-center', col.legend.cex=0.85, col.label='Cumulative CIE2000 Distance / cm')


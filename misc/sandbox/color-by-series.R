library(sharpshootR)
library(soilDB)
library(cluster)
library(plyr)


## TODO: this only includes lab pedons + morph data, there are a lot more without lab data...

x <- fetchKSSL('loafercreek', returnMorphologicData = TRUE)
pedons <- x$SPC

# simplify color data
# note that both horizon ID and color "percentage" must be specified
s.colors <- simplifyColorData(x$morph$phcolor, id.var = 'labsampnum', wt='colorpct')

# merge color data into SPC
h <- horizons(pedons)
h <- join(h, s.colors, by='labsampnum', type='left', match='first')
slot(pedons, 'horizons') <- h

pig <- soilColorSignature(pedons, r = 'm_r', g = 'm_g', b='m_b', method='depthSlices', RescaleLightnessBy = 5)

# account for missing data
idx <- which(complete.cases(pig[, -1]))
pig <- pig[idx, ]

# the first column is the ID
row.names(pig) <- pig[, 1]
d <- daisy(pig[, 2:6])
dd <- diana(d)

# index to those profiles present in `d`
idx <- which(profile_id(pedons) %in% pig$pedon_key)
pedons <- pedons[idx, ]


par(mar=c(0,0,1,1))

sdc <- getSoilDepthClass(pedons, name='hzn_desgn', top='hzn_top', bottom='hzn_bot')
max.d <- quantile(sdc$depth, probs = 0.75, na.rm=TRUE)

plot(pedons, color='moist_soil_color', plot.order=dd$order, name='', print.id=FALSE, width=0.5, max.depth=max.d, y.offset=-15, divide.hz=FALSE, plot.depth.axis=FALSE)


plotProfileDendrogram(pedons, dd, dend.y.scale = max(d) * 2, scaling.factor = max(d) / max.d, y.offset = max(d) / 10, width=0.5, cex.names=0.45, color='moist_soil_color', print.id=FALSE, divide.hz=FALSE, plot.depth.axis=FALSE)





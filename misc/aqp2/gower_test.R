library(aqp)

data(sp2)

depths(sp2) <- id ~ top + bottom
site(sp2) <-  ~ surface
hzdesgnname(sp2) <- "name"

# update soil colors that we have data for plot
sp2$soil_color <- munsell2rgb(sp2$hue, sp2$value, sp2$chroma)

# calculate some site attributes
sp2$depth <- profileApply(sp2, estimateSoilDepth, p = "^C|2C|3C")
sp2$darkness <- profileApply(sp2, thompson.bell.darkness, pattern = "^A|^2A",
                             value = "value", chroma = "chroma")

# calculate a horizon attribute
sp2$redness <- hurst.redness(sp2$hue, sp2$value, sp2$chroma)

# default order
plot(sp2[1:10,], label = "surface")

# calculate gower distance on sliced spc (for hz attributes)
gdist <- gower_distance(slice(sp2[1:10,], 0:150 ~ redness + prop + field_ph),
                        c('darkness','redness','prop','field_ph'))

# ordered by quasi-gower distance 
#  convincing separation of parent materials/age
#  use depth -- in this case this is pattern for ~solum thickness
#  also, use thompson-bell profile darkness index (site-level)
#  also, use hurst redness index (by horizon, traditionally for dry colors)
#  finally, clay content from horizon (prop)
#  
#  we order with respect to first profile, which was a holocene
plot(sp2[1:10,], 
     label = "surface",
     plot.order = order(gdist[1, ]),
     color = "prop")

# if you order wtih respect to different profile, the relative order is different
plot(sp2[1:10,], 
     label = "surface",
     plot.order = order(gdist[4, ]),
     color = "prop")

# if you order wtih respect to different profile, the relative order is different
plot(sp2[1:10,], 
     label = "surface",
     plot.order = order(gdist[2, ]),
     color = "prop")

library(soilDB)
data(loafercreek)
loafercreek <- rebuildSPC(loafercreek)
my.hz.vars <- c("clay", "phfield")
loaferclean <- filter(loafercreek, 
                      checkHzDepthLogic(loafercreek),
                      evalMissingData(loafercreek, my.hz.vars) > 0.9)
spc <- loaferclean

spc$darkness <- profileApply(loaferclean, thompson.bell.darkness)
spc$redness <- hurst.redness(loaferclean$d_hue, loaferclean$d_value, loaferclean$d_chroma)

all.vars <- c("slope_field", "darkness", "redness")

gdist <- gower_distance(spc, all.vars)
dorder <- order(gdist[1,])

plot(loaferclean, plot.order = dorder, color="clay")


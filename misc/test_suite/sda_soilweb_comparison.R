library(aqp)

source('mapunit_geom_by_ll_bbox.R')

# define bbox
b <- c(-120.54,38.61,-120.41,38.70)

# extract data from SDA, and time it
system.time(x.sda <- mapunit_geom_by_ll_bbox(b, source='sda'))

# extract KML from CASRL, and time it
# won't work right now... consider a WFS output from SoilWeb
# or a much simplified version of our KML output
# system.time(x.casrl <- mapunit_geom_by_ll_bbox(b, source='soilweb'))


# plot result
par(mar=c(0,0,0,0))
plot(x.sda)

# overlay original bbox
rect(bbox[1], bbox[2], bbox[3], bbox[4], border='red', lwd=2)


library(aqp)
library(soilDB)

x <- fetchOSD(c('leon'))

# tighter margins
# no clipping at figure boundary
par(mar = c(0, 1, 0, 1), xpd = NA)

# larger labels
# truncate at 175cm, with ragged bottom
# adjust width until "looks right"
# I like to label horizons at centers to save space in margins
# directly label horizon depths vs. depth axis
# suppress depth axis
plotSPC(
  x, 
  cex.names = 1, 
  max.depth = 175, 
  width = 0.2, 
  name.style = 'center-center', 
  hz.depths = TRUE, 
  depth.axis = FALSE
)


# 
# invertLabelColor()
# 
# SoilTextureLevels()
# 
# soilTextureColorPal()



plotSPC(
  x, 
  cex.names = 1.66,
  cex.id = 1,
  max.depth = 175, 
  width = 0.25, 
  name = NA, 
  hz.depths = TRUE,
  depth.axis = FALSE,
  fixOverlapArgs = list(method = 'E'),
  n = 11
)

for(i in 1:10) {
  plotSPC(
    x, 
    cex.names = 1.66,
    cex.id = 1,
    max.depth = 175, 
    width = 0.25, 
    name = NA, 
    hz.depths = TRUE,
    depth.axis = FALSE,
    fixOverlapArgs = list(method = 'S'),
    x.idx.offset = i,
    add = TRUE
  )
}





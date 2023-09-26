library(aqp)
library(soilDB)

# function example

# create an example profile
s <- quickSPC('p1:AA|Bt1Bt1Bt1|Bt2Bt2B|Bt3|Cr|RRRRR')

# warp each horizon
# values > 1: inflation
# values < 1: deflation (erosion / compaction)
s.w <- warpHorizons(s, fact = c(1.3, 0.7, 0.8, 1, 1, 1))

# combine original + warped
x <- combine(s, s.w)

# compute profile bottom depths
.bottoms <- x[, , .LAST, .BOTTOM]

# change in total depth after warping
# used to vertically offset the warped profile
.yoff <- c(0, .bottoms[1] - .bottoms[2])

# depths for line segments connecting horizon tops
.y1 <- x[1, , .TOP]
.y2 <- x[2, , .TOP] + .yoff[2]

# sketches
# can't automatically add a depth axis
par(mar = c(0.5, 0, 0, 2))
plotSPC(
  x, 
  name.style = 'center-center', 
  cex.names = 0.8, 
  width = 0.2, 
  max.depth = 150, 
  depth.axis = list(line = -3), 
  y.offset = .yoff
)

# illustrate warping with arrows
arrows(x0 = 1 + 0.25, y0 = .y1, x1 = 2 - 0.25, y1 = .y2, len = 0.1, col = 2)

# manually add depth axis
axis(side = 4, line = -3.5, las = 1, at = seq(from = 0, to = 150, by = 25))


## TODO: back-calculate 1D affine transform from A -> B, or from A -> collection
# (s.w[, , .BOTTOM] - s.w[, , .TOP]) / (s[, , .BOTTOM] - s[, , .TOP])
# 
# lm(s.w[, , .BOTTOM] - s.w[, , .TOP] ~ s[, , .BOTTOM] - s[, , .TOP])




## real data


o <- fetchOSD('zook')
oo <- warpHorizons(o, fact = c(1.8, 1.3, 0.6, 0.75, 0.8, 1, 1, 1))
x <- combine(o, oo)

.y1 <- x[1, , .TOP]
.y2 <- x[2, , .TOP]

par(mar = c(1, 0, 0 , 2))
plotSPC(x, name.style = 'center-center', cex.names = 0.8, width = 0.2, max.depth = 200, depth.axis = list(line = -3))
arrows(x0 = 1 + 0.25, y0 = .y1, x1 = 2 - 0.25, y1 = .y2, len = 0.1, col = 2)



o$fact <- c(1, 1, 1, 1, 1, 1, 1, 1)
oo$fact <- c(1.8, 1.3, 0.6, 0.75, 0.8, 1, 1, 1)
x <- combine(o, oo)

par(mar = c(1, 0, 3 , 1))
plotSPC(x, name.style = 'center-center', cex.names = 0.8, width = 0.2, max.depth = 200, depth.axis = FALSE, hz.depths = TRUE, color = 'fact')
arrows(x0 = 1 + 0.33, y0 = .y1, x1 = 2 - 0.22, y1 = .y2, len = 0.1, col = 'black')




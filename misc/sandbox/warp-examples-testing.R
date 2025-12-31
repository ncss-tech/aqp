library(aqp)
library(soilDB)


## Ideas:
# * re-name function to better describe the generic case of a 1D affine transformation: Ax + c
# * what are some other concrete uses?
# * how to specify scale + offset?
# * estimate scaling / transform from a collection of profiles 

## possible names
# * transformHorizons() <-- translation + scale
# * scaleHorizons() <-- scaling only
# * affineTransform() <-- not very useful, since we are only working with 1D data
# 


# create an example profile
s <- quickSPC('p1:AA|Bt1Bt1Bt1|Bt2Bt2B|Bt3|Cr|RRRRR')

s$warpFactor <- c(1.3, 0.7, 0.8, 1, 1, 1)

# warp each horizon
# values > 1: inflation
# values < 1: deflation (erosion / compaction)
s.w <- warpHorizons(s, fact = 'warpFactor', suffix = '-warped')

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
par(mar = c(0.25, 0, 3, 2))
plotSPC(
  x, 
  name.style = 'center-center', 
  color = 'warpFactor',
  col.palette = hcl.colors(n = 10, palette = 'zissou1'),
  cex.names = 0.8, 
  width = 0.2, 
  max.depth = 150, 
  depth.axis = list(line = -3), 
  y.offset = .yoff
)

# illustrate warping with arrows
arrows(x0 = 1 + 0.25, y0 = .y1, x1 = 2 - 0.25, y1 = .y2, len = 0.1, col = 'black')

# manually add depth axis
axis(side = 4, line = -3.5, las = 1, at = seq(from = 0, to = 150, by = 25))


## TODO: back-calculate 1D affine transform from A -> B, or from A -> collection
# (s.w[, , .BOTTOM] - s.w[, , .TOP]) / (s[, , .BOTTOM] - s[, , .TOP])
# 
# lm(s.w[, , .BOTTOM] - s.w[, , .TOP] ~ s[, , .BOTTOM] - s[, , .TOP])


## why not abuse an old favorite function for describing the mapping of one space to another?
## when working with profiles that share the same horizon scaffolding, apply by-horizon
library(vegan)

# 1D mapping between old -> new thickness
# this isn't quite right, because we loose track of possible offsets
# may have to use horizon top + bottom depths
p <- procrustes(
  X = cbind(s[, , .BOTTOM] - s[, , .TOP]),
  Y = cbind(s.w[, , .BOTTOM] - s.w[, , .TOP]),
  scale = TRUE
)

summary(p)


###

# scale soil depth to specific value

s <- quickSPC('p1:AA|Bt1Bt1Bt1|Bt2Bt2B|Bt3|Cr|RRRRR')

w <- warpHorizons(s, scaleTo = c(100))

# combine original + warped
x <- combine(s, w)

# depths for line segments connecting horizon tops
.y1 <- x[1, , .BOTTOM]
.y2 <- x[2, , .BOTTOM]

# sketches
# can't automatically add a depth axis
par(mar = c(0.5, 0, 0, 2))
plotSPC(
  x,
  name.style = 'center-center',
  cex.names = 0.8,
  width = 0.2
)

abline(h = c(0, 100), lty = 3)

# illustrate warping with arrows
arrows(x0 = 1 + 0.25, y0 = .y1, x1 = 2 - 0.25, y1 = .y2, len = 0.1, col = 2)




# rescale all profiles to 100cm soil depth
# (depth to contact)
.template <- c(
  'P1:AAA|BwBwBwBw|CCCCCCC|CdCdCdCd',
  'P2:ApAp|AA|E|BhsBhs|Bw1Bw1|CCCCC',
  'P3:A|Bt1Bt1Bt1|Bt2Bt2Bt2|Bt3|Cr|RRRRR',
  'P4:AAAAA|CCC|RRRRRR'
)

# each horizon label is '10' depth-units (default)
s <- quickSPC(.template)

# warp horizons by profile, result is a list of SPCs
w <- profileApply(s, FUN = function(i) {
  warpHorizons(i, scaleTo = 100, soilDepthFun = estimateSoilDepth)
})

# flatten list -> SoilProfileCollection
w <- combine(w)

# combine with original SPC
x <- combine(s, w)

# highlight "contact"
x$color <- rep(grey(0.9), times = nrow(x))
x$color[grep('R|Cr|Cd', x$name)] <- 'royalblue'

# sketches
par(mar = c(0.5, 0, 0, 2.5))
plotSPC(
  x,
  color = 'color',
  name.style = 'center-center',
  cex.names = 0.8,
  cex.id = 0.85,
  width = 0.3,
  max.depth = 165,
  depth.axis = list(line = -2)
)

abline(h = 100, lty = 3)


## real data


o <- fetchOSD('zook')
oo <- warpHorizons(o, fact = c(1.8, 1.3, 0.6, 0.75, 0.8, 1, 1, 1))
x <- combine(o, oo)

.y1 <- x[1, , .TOP]
.y2 <- x[2, , .TOP]

par(mar = c(1, 0, 0 , 2))
plotSPC(x, name.style = 'center-center', cex.names = 0.8, width = 0.2, max.depth = 200, depth.axis = list(line = -3))
arrows(x0 = 1 + 0.25, y0 = .y1, x1 = 2 - 0.25, y1 = .y2, len = 0.1, col = 2)


# symbolize warping factor with color
o$fact <- c(1, 1, 1, 1, 1, 1, 1, 1)
oo$fact <- c(1.8, 1.3, 0.6, 0.75, 0.8, 1, 1, 1)
x <- combine(o, oo)

par(mar = c(1, 0, 3 , 1))
plotSPC(x, name.style = 'center-center', cex.names = 0.8, width = 0.2, max.depth = 200, depth.axis = FALSE, hz.depths = TRUE, color = 'fact', col.label = 'Horizon Warp Factor')
arrows(x0 = 1 + 0.33, y0 = .y1, x1 = 2 - 0.22, y1 = .y2, len = 0.1, col = 'black')


## 
wf <- c(0.5, 0.8, 0.9, 0.95, 1, 1, 1, 3)

oo <- warpHorizons(o, fact = wf)
x <- combine(o, oo)

.y1 <- x[1, , .TOP]
.y2 <- x[2, , .TOP]

par(mar = c(1, 0, 0 , 2))
plotSPC(x, name.style = 'center-center', cex.names = 0.8, width = 0.2, max.depth = 200, depth.axis = list(line = -3))
arrows(x0 = 1 + 0.25, y0 = .y1, x1 = 2 - 0.25, y1 = .y2, len = 0.1, col = 2)


# symbolize warping factor with color
o$fact <- c(1, 1, 1, 1, 1, 1, 1, 1)
oo$fact <- wf
x <- combine(o, oo)

par(mar = c(1, 0, 3 , 1))
plotSPC(x, name.style = 'center-center', cex.names = 0.9, width = 0.22, max.depth = 200, depth.axis = FALSE, hz.depths = TRUE, color = 'fact', col.label = 'Horizon Warp Factor')
arrows(x0 = 1 + 0.38, y0 = .y1, x1 = 2 - 0.28, y1 = .y2, len = 0.1, col = 'black')






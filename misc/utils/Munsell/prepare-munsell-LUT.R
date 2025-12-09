## Prepare `munsell` LUT from Renotation Database
## D.E. Beaudette
## 2024-10-03
##
## Originally based on code from ~2006 as part of the Pedlogic project.
##

library(latticeExtra)
library(tactile)
library(grDevices)
library(scales)
library(purrr)
library(aqp)


# starting from aqp base directory
setwd('misc/utils/Munsell')
source('local-functions.R') 


##
## Notes / Ideas:
##
##   * univariate interpolation of odd-chroma and 0.5-value chips seems to work well
##   * consider retaining values 0.2-0.8 for improved interpolation
##   * is multivariate interpolation necessary? (I doubt it)
##



## munsell data comes with a lookup table in xyY colorspace
## url: http://www.cis.rit.edu/mcsl/online/munsell.php

## Munsell chroma, CIE x, y, and Y. 
## The chromaticity coordinates were calculated using 
## illuminant C and the CIE 1931 2 degree observer.
m <- read.table("munsell-all.dat.gz", header=TRUE)


## rescale Y
# note: the data from the Munsell group contains Y values 
# that are in the range of approx: 0-100

# these need to be rescaled to the range of 0-1, 
# but not using the existing min/max values
# instead, set the max Y value at 100
m$Y <- pmin(m$Y, 100)

# rescale Y to [0,1]
m$Y <- rescale(m$Y, to = c(0, 1))

## remove value < 1 --> 765 records
m <- subset(m, V >= 1)




##
## interpolate odd chroma chips
##

# also interpolate backwards to C == 1
z <- split(m, list(m$H, m$V))

# this combines original + interpolated values
m.new.chroma <- map(z, .f = interpolateChroma, .progress = TRUE)
m.new.chroma <- do.call('rbind', m.new.chroma)

# 8460 rows
nrow(m.new.chroma)


## graphical check
.cols <- hcl.colors(length(unique(m$V)))

p1 <- xyplot(
  x ~ C | factor(H), 
  groups = V, 
  data = m, 
  subset = H %in% c('2.5Y', '2.5YR', '2.5R'), 
  type = 'p', 
  par.settings = tactile.theme(superpose.symbol = list(col = .cols)), 
  as.table = TRUE, 
  scales = list(alternating = 1), 
  cex = 1.25, 
  xlim = c(-1, 25),
  panel = function(...) {
    panel.grid(-1, -1)
    panel.xyplot(...)
  }
) 

p2 <- xyplot(
  x ~ C | factor(H), 
  groups = V, 
  data = m.new.chroma, 
  subset = H %in% c('2.5Y', '2.5YR', '2.5R'), 
  type = 'p', 
  par.settings = tactile.theme(superpose.symbol = list(col = .cols)), 
  as.table = TRUE, 
  scales = list(alternating = 1),
  pch = 16,
  cex = 0.5, 
  xlim = c(-1, 25),
  panel = function(...) {
    panel.grid(-1, -1)
    panel.xyplot(...)
  }
) 

# ok
update(p1 + p2, auto.key = list(title = 'V'))


p1 <- xyplot(
  y ~ C | factor(H), 
  groups = V, 
  data = m, 
  subset = H %in% c('2.5Y', '2.5YR', '2.5R'), 
  type = 'p', 
  par.settings = tactile.theme(superpose.symbol = list(col = .cols)), 
  as.table = TRUE, 
  scales = list(alternating = 1), 
  cex = 1.25, 
  xlim = c(-1, 25),
  panel = function(...) {
    panel.grid(-1, -1)
    panel.xyplot(...)
  }
) 

p2 <- xyplot(
  y ~ C | factor(H), 
  groups = V, 
  data = m.new.chroma, 
  subset = H %in% c('2.5Y', '2.5YR', '2.5R'), 
  type = 'p', 
  par.settings = tactile.theme(superpose.symbol = list(col = .cols)), 
  as.table = TRUE, 
  scales = list(alternating = 1),
  pch = 16,
  cex = 0.5, 
  xlim = c(-1, 25),
  panel = function(...) {
    panel.grid(-1, -1)
    panel.xyplot(...)
  }
) 

# ok
update(p1 + p2, auto.key = list(title = 'V'))



# original
p1 <- xyplot(
  x ~ C | factor(V), 
  groups = factor(H, levels = c('2.5Y', '2.5YR', '2.5R')), 
  data = m, 
  type = 'p', 
  par.settings = tactile.theme(), 
  as.table = TRUE, 
  scales = list(alternating = 1), 
  cex = 1.25, 
  xlim = c(-1, 25)
) 

# interpolated
p2 <- xyplot(
  x ~ C | factor(V), 
  groups = factor(H, levels = c('2.5Y', '2.5YR', '2.5R')), 
  data = m.new.chroma, 
  type = 'p', 
  par.settings = tactile.theme(), 
  as.table = TRUE, 
  scales = list(alternating = 1), 
  cex = 0.5, 
  pch = 16
)

# good
update(p1 + p2, auto.key = list(title = 'H'))


# original
p1 <- xyplot(
  x ~ C | factor(V), 
  groups = factor(H, levels = c('2.5Y', '2.5YR', '2.5G')), 
  data = m, 
  type = 'p', 
  par.settings = tactile.theme(), 
  as.table = TRUE, 
  scales = list(alternating = 1), 
  cex = 1.25, 
  xlim = c(-1, 25)
) 

# interpolated
p2 <- xyplot(
  x ~ C | factor(V), 
  groups = factor(H, levels = c('2.5Y', '2.5YR', '2.5G')), 
  data = m.new.chroma, 
  type = 'p', 
  par.settings = tactile.theme(), 
  as.table = TRUE, 
  scales = list(alternating = 1), 
  cex = 0.5, 
  pch = 16
)

# good
update(p1 + p2, auto.key = list(title = 'H'))





## verify odd chroma frequencies
# good
table(m.new.chroma$C)



## TODO:
# * do we need multivariate interpolation?

.n <- length(unique(m.new.chroma$C[m.new.chroma$H %in% c('2.5Y', '2.5YR', '2.5R')]))
.cols <- hcl.colors(n = .n, palette = 'blues3')

xyplot(
  x ~ V | factor(H), 
  groups = C, 
  data = m.new.chroma, 
  subset = H %in% c('2.5Y', '2.5YR', '2.5R'),
  type = 'l', 
  par.settings = tactile.theme(superpose.line = list(col = .cols, lwd = 2)), 
  as.table = TRUE, 
  scales = list(alternating = 1),
  panel = function(...) {
    panel.grid(-1, -1)
    panel.xyplot(...)
  }
) 

xyplot(
  y ~ V | factor(H), 
  groups = C, 
  data = m.new.chroma, 
  subset = H %in% c('2.5Y', '2.5YR', '2.5R'), 
  type = 'l', 
  par.settings = tactile.theme(superpose.line = list(col = .cols, lwd = 2)), 
  as.table = TRUE, 
  scales = list(alternating = 1),
  panel = function(...) {
    panel.grid(-1, -1)
    panel.xyplot(...)
  }
) 

xyplot(
  Y ~ V | factor(H), 
  groups = C, 
  data = m.new.chroma, 
  subset = H %in% c('2.5Y', '2.5YR', '2.5R'), 
  type = 'l', 
  par.settings = tactile.theme(superpose.line = list(col = .cols)), 
  as.table = TRUE, 
  scales = list(alternating = 1),
  panel = function(...) {
    panel.grid(-1, -1)
    panel.xyplot(...)
  }
) 





##
## interpolate all half-value chips
##

z <- split(m.new.chroma, list(m.new.chroma$H, m.new.chroma$C))
zz <- map(z, .f = interpolateValue2, .progress = TRUE)

# remove NULLs
# these are H/C combinations where interpolation is not possible
idx <- which(!sapply(zz, is.null))
zz <- zz[idx]

zz <- do.call('rbind', zz)
nrow(zz)

# stack interpolated values
m.new.chroma <- rbind(m.new.chroma, zz)

# sort
m.new.chroma <- m.new.chroma[order(m.new.chroma$H, m.new.chroma$V, m.new.chroma$C), ]

# 15700
nrow(m.new.chroma)


## for backwards compatibility, retain specific Munsell values
table(m.new.chroma$V)

m.new.chroma <- subset(
  m.new.chroma,
  subset = V %in% c(1, 2, 2.5, 3, 4, 5, 6, 7, 8, 8.5, 9, 9.5, 10)
)

# check: ok
table(m.new.chroma$V)


## TODO: flag within single data.frame, these two are out of sync

## graphical check
g <- make.groups(
  m.new.chroma,
  zz
)

# ok
xyplot(
  x ~ V | factor(C), 
  groups = which, 
  data = g, 
  subset = H %in% c('2.5YR'), 
  type = 'p', 
  par.settings = tactile.theme(), 
  as.table = TRUE, 
  scales = list(alternating = 1), 
  cex = 0.5, 
  pch = 16, 
  xlim = c(0, 10)
)

xyplot(
  y ~ V | factor(C), 
  groups = which, 
  data = g, 
  subset = H %in% c('2.5YR'), 
  type = 'p', 
  par.settings = tactile.theme(), 
  as.table = TRUE, 
  scales = list(alternating = 1), 
  cex = 0.5, 
  pch = 16, 
  xlim = c(0, 10)
)

xyplot(
  x ~ V | factor(C), 
  groups = which, 
  data = g, 
  subset = H %in% c('5G'), 
  type = 'p', 
  par.settings = tactile.theme(), 
  as.table = TRUE, 
  scales = list(alternating = 1), 
  cex = 0.5, 
  pch = 16, 
  xlim = c(0, 10)
)


.n <- length(unique(m.new.chroma$C[m.new.chroma$H %in% c('2.5Y', '2.5YR', '2.5R')]))
.cols <- hcl.colors(n = .n, palette = 'zissou1')

xyplot(
  x ~ V | factor(H), 
  groups = C, 
  data = m.new.chroma, 
  subset = H %in% c('2.5Y', '2.5YR', '2.5R'),
  type = 'b', 
  par.settings = tactile.theme(
    background = list(col = 'black'),
    axis.text = list(col = 'white'),
    par.xlab.text = list(col = 'white'),
    par.ylab.text = list(col = 'white'),
    superpose.symbol = list(col = .cols, pch = 16),
    superpose.line = list(col = .cols, lwd = 1)
  ), 
  as.table = TRUE, 
  scales = list(alternating = 1, x = list(at = seq(1, 10))),
  panel = function(...) {
    panel.grid(-1, -1)
    panel.xyplot(...)
  }
) 

xyplot(
  x ~ V | factor(H), 
  groups = C, 
  data = m.new.chroma, 
  subset = H %in% c('2.5Y', '2.5YR', '2.5R'),
  type = 'b', 
  par.settings = tactile.theme(
    superpose.symbol = list(col = .cols, pch = 16),
    superpose.line = list(col = .cols, lwd = 1)
  ), 
  as.table = TRUE, 
  scales = list(alternating = 1, x = list(at = seq(1, 10))),
  panel = function(...) {
    panel.grid(-1, -1)
    panel.xyplot(...)
  }
) 



##
## convert xyY [C] ---> XYZ [D65]
##
m.XYZ <- xyY2XYZ(m.new.chroma)

summary(m.XYZ)



##
## convert XYZ ---> sRGB
##
m.sRGB <- XYZ2rgb(m.XYZ)

## check:
# should give the same results
# note explicit reference illuminant conversion XYZ coordinates are D65
# z <- convertColor(m.XYZ, from = 'XYZ', to = 'sRGB', from.ref.white = 'D65', to.ref.white = 'D65')
#
# relatively small differences
# colMeans(m.sRGB - z)



m.final <- data.frame(m.new.chroma, m.sRGB)

plot_cols <- rgb(m.final$R, m.final$G, m.final$B)


p1 <- xyplot(
  V ~ C | factor(H, levels = c('2.5Y', '10YR', '7.5YR', '5YR', '2.5YR', '10R')),
  main = "Common Soil Colors", 
  data = m.final, 
  subset = H %in% c('2.5Y', '10YR', '7.5YR', '5YR', '2.5YR', '10R') & V <= 8 & C <= 8, 
  as.table = TRUE, 
  subscripts = TRUE, 
  scales = list(alternating = 1, y = list(at = 1:8)),
  xlab = 'Chroma', 
  ylab = 'Value',
  par.settings = tactile.theme(),
  panel = function(x, y, subscripts, ...) {
    panel.xyplot(x, y, pch = 15, cex = 2, col = plot_cols[subscripts])
  }
)

p1


##
## subset / rename columns
##
m.final <- m.final[, c('H', 'V', 'C', 'R', 'G', 'B')]


##
## add neutral chips
##

## NOTES:
## * 2025-12-05: TODO scan missing half-chips 8.5 and 9.5
## * 2025-12-08: interpolate in CIELAB results in colors that are too light (why?)

## * TODO N 8.5/ and N 9/ appear too dark ---> rescan


# manually edited file, exported from Nix Pro app
n <- read.csv(file = 'neutrals_colordata.csv')

n <- n[, c('id', 'Lin.sRGB.R', 'Lin.sRGB.G', 'Lin.sRGB.B', 'L', 'A', 'B')]
names(n) <- c('id', 'sR', 'sG', 'sB', 'L', 'A', 'B')

n$V <- as.numeric(sapply(strsplit(n$id, '-', fixed = TRUE), '[', 1))

# good
previewColors(rgb(n$sR, n$sG, n$sB, maxColorValue = 1))

# take mean over replicates
n.agg <- aggregate(cbind(sR, sG, sB) ~ V, data = n, FUN = mean)

n.agg$H <- 'N'
n.agg$C <- 0

n.agg <- n.agg[, c('H', 'V', 'C', 'sR', 'sG', 'sB')]

# linear interpolation in sRGB space
# 2.5 and 8.5 chips
n.agg.2.5 <- interpolateValue(n.agg[1:2, ], vars = c('sR', 'sG', 'sB'), new.V = 2.5)
n.agg.8.5 <- interpolateValue(n.agg[7:8, ], vars = c('sR', 'sG', 'sB'), new.V = 8.5)

# combine and re-order based on value
n.agg.final <- rbind(n.agg, n.agg.2.5, n.agg.8.5)
n.agg.final <- n.agg.final[order(n.agg.final$V), ]

names(n.agg.final)[4:6] <- c('R', 'G', 'B')

## 
# # convert LAB -> sRGB
# .rgb <- convertColor(n.agg.final[, c('L', 'A', 'B')], from = 'Lab', to = 'sRGB')
# .rgb <- data.frame(.rgb)
# names(.rgb) <- c('R', 'G', 'B')
# 
# # swap LAB for sRGB
# n.agg.final <- cbind(n.agg.final[, 1:3], .rgb)

# check:
.cols <- rgb(n.agg.final$R, n.agg.final$G, n.agg.final$B, maxColorValue = 1)
.labs <- sprintf("%s %s/", n.agg.final$H, n.agg.final$V)
soilPalette(.cols, lab = .labs)

# append neutral chips with rest of the data
m.final <- rbind(m.final, n.agg.final)

# 2022:       9,227 (2.5 value chips)
# 2024a:      15,709 (all half-value chips)
# 2024b:      10,447 (select half-value chips)
# 2025-12-08: 10,448 (additional N chips)
nrow(m.final)


##
## add CIELAB coordinates
##
lab <- convertColor(m.final[, c('R', 'G', 'B')], from = 'sRGB', to = 'Lab')
m.final.lab <- data.frame(m.final, lab)

##
## cleanup names / row.names
##
names(m.final.lab) <- c('hue', 'value', 'chroma', 'r', 'g', 'b', 'L', 'A', 'B')
row.names(m.final.lab) <- NULL

str(m.final.lab)


##
## check differences from previous versions of the LUT
##

## 2022:
## the new N chips are the top differences
## everything else has dE00 < 4
## mostly value == 1


## 2024:
## dE00 > 0.4 (but all < 1.5) are 2.5 value chips
## 
## likely related to interpolation over full range of V vs. single, 
## linear interpolation 2->2.5<-3




## make backup copy of old LUT
# data(munsell)
# saveRDS(munsell, file = 'munsell-LUT-2024-09-25.rds')

z.old <- readRDS('munsell-LUT-2024-09-25.rds')

z <- merge(z.old, m.final.lab, by = c('hue', 'value', 'chroma'), all.x = TRUE, sort = FALSE)

str(z)

# looks pretty good, note changes in N chips
xyplot(L.y ~ L.x, data = z)
xyplot(A.y ~ A.x, data = z)
xyplot(B.y ~ B.x, data = z)

# my original estimates were too light
z[z$hue == 'N', ]

## DE00 old vs new
library(farver)

d <- vector(mode = 'numeric', length = nrow(z))

for(i in 1:nrow(z)) {
  d[i] <- compare_colour(
    from = z[i, c('L.x', 'A.x', 'B.x')], 
    to = z[i, c('L.y', 'A.y', 'B.y')], 
    from_space = 'lab', 
    to_space = 'lab', 
    white_from = 'D65', 
    white_to = 'D65', 
    method = 'cie2000'
  )
}

hist(d)


# changes with dE00 > 2
idx <- which(d > 2)
zz <- z[idx, ]
zz$dE00 <- d[idx]
zz <- zz[order(zz$dE00, decreasing = TRUE), ]

head(zz, 50)

table(zz$hue)
table(zz$value)
table(zz$chroma)


# changes with dE00 > 0.4
idx <- which(d > 0.4)
zz <- z[idx, ]
zz$dE00 <- d[idx]
zz <- zz[order(zz$dE00, decreasing = TRUE), ]

nrow(zz)
head(zz, 20)

table(zz$hue)
table(zz$value)
table(zz$chroma)




##
## save to munsell.rda
##
munsell <- m.final.lab
save(munsell, file = '../../../data/munsell.rda', compress = 'xz')




## install / or reload from source
# 
# munsell2rgb('10YR', 9, 2, returnLAB = TRUE)
# munsell2rgb('10YR', 9.5, 2, returnLAB = TRUE)
# 
# # dE00 ~ 3
# # colorContrastPlot('10YR 9/2', '10YR 9.5/2')
# 
# 
# munsell2rgb('10YR', 3.5, 2, returnLAB = TRUE)
# munsell2rgb('10YR', 4, 2, returnLAB = TRUE)
# 
# munsell2rgb('10YR', 2.5, 2, returnLAB = TRUE)
# munsell2rgb('10YR', 2, 2, returnLAB = TRUE)
# 
# munsell2rgb('10YR', 2, 1, returnLAB = TRUE)
# munsell2rgb('10YR', 5, 1, returnLAB = TRUE)
# 
# 
# # check neutral
# m <- sprintf('N %s/', c(2, 2.5,  3:8))
# cols <- parseMunsell(m)
# soilPalette(cols, lab = m)
# 
# 
# 

library(latticeExtra)
library(tactile)
library(grDevices)
library(scales)
library(purrr)
library(aqp)

source('local-functions.R') 


##
## Notes / Ideas:
##
##   * univariate interpolation of odd-chroma and 0.5-value chips seems to work well
##   * 
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

## remove vale < 1 --> 765 records
m <- subset(m, V >= 1)




##
## interpolate odd chroma chips
##

# also interpolate backwards to C == 1
m.split <- split(m, list(m$H, m$V))

# this combines original + interpolated values
m.new.chroma <- map(m.split, .f = interpolateChroma, .progress = TRUE)
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
p1 <- xyplot(x ~ C | factor(V), groups = H, data = m, subset = H %in% c('2.5YR', '2.5Y', '5G'), type = 'p', par.settings = tactile.theme(), as.table = TRUE, scales = list(alternating = 1), cex = 1.25, xlim = c(-1, 25)) 
# interpolated
p2 <- xyplot(x ~ C | factor(V), groups = H, data = m.new.chroma, subset = H %in% c('2.5YR', '2.5Y', '5G'), type = 'p', par.settings = tactile.theme(), as.table = TRUE, scales = list(alternating = 1), cex = 0.5, pch = 16)

# good
p1 + p2


# original
p1 <- xyplot(y ~ C | factor(V), groups = H, data = m, subset = H %in% c('2.5YR', '2.5Y', '5G'), type = 'p', par.settings = tactile.theme(), as.table = TRUE, scales = list(alternating = 1), cex = 1.25, xlim = c(-1, 25)) 
# interpolated
p2 <- xyplot(y ~ C | factor(V), groups = H, data = m.new.chroma, subset = H %in% c('2.5YR', '2.5Y', '5G'), type = 'p', par.settings = tactile.theme(), as.table = TRUE, scales = list(alternating = 1), cex = 0.5, pch = 16)

# good
p.1 + p.2

# verify odd chroma frequencies
table(m.new.chroma$C)


## TODO:
# * vectorize interpolateValue() or re-factor
# * do we need multivariate interpolation?
# 

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





## interpolate all half-value chips
# .s <- seq(from = 1.5,  to = 9.5, by = 1)
# map(m.new.chroma, .f = interpolateValue, .progress = TRUE)



## interpolate 2.5 values
# only need 2 value-slices
m.sub <- subset(m.new.chroma, V %in% c(2, 3))
m.sub <- split(m.sub, list(m.sub$H, m.sub$C))

# note: some combinations are missing values 2 AND 3
table(sapply(m.sub, nrow))
# 0    1    2 
# 1102  140  718 

# only process those with 2 records
idx <- which(sapply(m.sub, nrow) == 2)
m.sub <- m.sub[idx]

m.2.5.values <- map(m.sub, .f = interpolateValue, .progress = TRUE)
m.2.5.values <- do.call('rbind', m.2.5.values)

# 758 rows
nrow(m.2.5.values)
head(m.2.5.values)

## stack interpolated 2.5 values
m.new.chroma <- rbind(m.new.chroma, m.2.5.values)

# sort
m.new.chroma <- m.new.chroma[order(m.new.chroma$H, m.new.chroma$V, m.new.chroma$C), ]

str(m.new.chroma)

## graphical check
g <- make.groups(
  m.new.chroma,
  m.2.5.values
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


p1 <- xyplot(V ~ C | factor(H, levels=c('2.5Y', '10YR', '7.5YR', '5YR', '2.5YR', '10R')),
             main="Common Soil Colors", 
             data=m.final, subset=H %in% c('2.5Y', '10YR', '7.5YR', '5YR', '2.5YR', '10R') & V > 1 & V <= 8 & C <= 8, 
             as.table=TRUE, subscripts=TRUE, xlab='Chroma', ylab='Value',
             par.settings = tactile.theme(),
             panel=function(x, y, subscripts, ...) 
             {
               panel.xyplot(x, y, pch=15, cex=2, col=plot_cols[subscripts])
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

# manually edited file, exported from Nix Pro app
n <- read.csv(file = 'neutrals_colordata.csv')

n <- n[, c('id', 'Lin.sRGB.R', 'Lin.sRGB.G', 'Lin.sRGB.B')]
names(n) <- c('id', 'R', 'G', 'B')

n$V <- as.numeric(sapply(strsplit(n$id, '-', fixed = TRUE), '[', 1))

previewColors(rgb(n$R, n$G, n$B))

# TODO: eval over replicates

# take mean over replicates
n.agg <- aggregate(cbind(R, G, B) ~ V, data = n, FUN = mean)

n.agg$H <- 'N'
n.agg$C <- 0

n.agg <- n.agg[, c('H', 'V', 'C', 'R', 'G', 'B')]

# interpolate 2.5 value
n.agg.2.5 <- interpolateValue(n.agg[1:2, ], vars = c('R', 'G', 'B'))

n.agg.final <- rbind(n.agg, n.agg.2.5)
n.agg.final <- n.agg.final[order(n.agg.final$V), ]


# combine
m.final <- rbind(m.final, n.agg.final)

# 9227
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
## check
##

## make backup copy of old LUT
# data(munsell)
# saveRDS(munsell, file = 'munsell-LUT-2022-03-29.rds')

z.old <- readRDS('munsell-LUT-2022-03-29.rds')

z <- merge(z.old, m.final.lab, by = c('hue', 'value', 'chroma'), all.x = TRUE)

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


# changes with dE00 > 2
idx <- which(d > 2)
zz <- z[idx, ]
zz$dE00 <- d[idx]
zz <- zz[order(zz$dE00, decreasing = TRUE), ]

head(zz, 50)

table(zz$hue)
table(zz$value)
table(zz$chroma)

## N chips are the top differences
## everything else is dE00 < 4
## mostly value == 1




##
## save to munsell.rda
##
munsell <- m.final.lab
save(munsell, file = '../../../data/munsell.rda', compress = 'xz')


## install / or reload from source


munsell2rgb('10YR', 3.5, 2, returnLAB = TRUE)
munsell2rgb('10YR', 4, 2, returnLAB = TRUE)

munsell2rgb('10YR', 2.5, 2, returnLAB = TRUE)
munsell2rgb('10YR', 2, 2, returnLAB = TRUE)

munsell2rgb('10YR', 2, 1, returnLAB = TRUE)
munsell2rgb('10YR', 5, 1, returnLAB = TRUE)


# check neutral
m <- sprintf('N %s/', 2:9)
cols <- parseMunsell(m)
soilPalette(cols, lab = m)




devtools::load_all()

##
## nutty idea: label collision fixes via simulation of electrostatic charged particles
##

## thanks Keith !!!


## input should be pre-sorted ASC

x <- c(1, 1.2, 2, 3, 4.75, 5, 8)
x <- sort(x)

(tr <- electroStatics_1D(x, thresh = 0.3, trace = TRUE, q = 0.1))
tr$converged





# x <- sort(c(1, 2, 3, 3.4, 3.5, 5, 6, 6.1, 10))

# impossible for both
# x <- sort(c(1, 2, 3.4, 3.4, 3.4, 3.4, 6, 8, 10, 12, 13, 13, 15, 15.5))

# x <- c(1, rep(5, times = 10), 12)

# x <- sort(1:15 + abs(rnorm(15, mean = 0, sd = 2)))

# x <- sort(c(1, 2, 3, rep(4:5, each = 2), 7, 9))

# x <- sort(c(1, 12, 5, 5, 4, 4, 6, 6, 6, 6, 6))

# x <- c(1, 2, 3, 3.4, 3.5, 5, 6, 10)
 
# .thresh <- 0.6
# .q <- 10


## simulated  horizon depths
x <- c(0, 2, 5, 12, 18, 20, 35, 40, 55, 90, 120, 150)
.thresh <- 6
.q <- 1


# ##
# x <- c(0, 5, 12, 18, 20, 35, 40, 55, 90, 120, 150)
# .thresh <- 9
# .q <- 0.2
# 
# 
# 
# ## edge conditions
# x <- c(0, 3, 20, 35, 40, 55, 90, 120, 145, 150)
# .thresh <- 6
# .q <- 0.1


## note: in general, as the complexity increases (larger thresholds, more overlap), q need to be larger


cols <- hcl.colors(n = 9, palette = 'Zissou 1', rev = TRUE)
cols <- colorRampPalette(cols)(length(x))


## TODO: animate this

system.time(z <- fixOverlap(x, thresh = .thresh, method = 'E', maxIter = 100, trace = TRUE, q = .q))
.n <- nrow(z$xnew)

par(mar = c(0, 2, 1, 0.5), bg = 'black', fg = 'white')
layout(matrix(c(1, 2, 3, 4), ncol = 2, nrow = 2), heights = c(0.33, 0.66))

plot(seq_along(z$cost), z$cost, las = 1, type = 'b', axes = FALSE, cex = 0.66, xlim = c(1, .n))
mtext(text = sprintf("Converged (%s): %s", .n, z$converged), at = 0, side = 3, line = 0, cex = 0.75, font = 3, adj = 0)
matplot(rbind(x, z$xnew), type = 'l', lty = 1, las = 1, axes = FALSE, col = cols, lwd = 1)

points(x = rep(1, times = length(x)), y = x, cex = 0.66, pch = 16, col = cols)
points(x = rep(.n + 1, times = length(x)), y = z$xnew[.n, ], cex = 0.66, pch = 16, col = cols)

text(x = 1, y = x, col = cols, labels = seq_along(x), cex = 0.66, font = 2, pos = 2)
text(x = .n + 1, y = z$xnew[.n, ], col = cols, labels = seq_along(x), cex = 0.66, font = 2, pos = 4)

axis(side = 2, at = unique(x), labels = round(unique(x), 1), col.axis = par('fg'), las = 1, cex.axis = 0.6)

overlapMetrics(z$xnew[nrow(z$xnew), ], thresh = .thresh)


## SANN_1D doesn't always preserve rank ordering
##  ->> not designed to use unsorted input
##  ->> maybe impossible with ties in x?

system.time(z <- fixOverlap(x, thresh = .thresh, method = 'S', trace = TRUE, maxIter = 1000))
.n <- nrow(z$states)

plot(seq_along(z$stats), z$stats, las = 1, type = 'b', axes = FALSE, cex = 0.66, xlim = c(1, .n))
mtext(text = sprintf("Converged (%s): %s", .n, z$converged), at = 0, side = 3, line = 0, cex = 0.75, font = 3, adj = 0)

matplot(z$states, type = 'l', lty = 1, las = 1, axes = FALSE, col = cols)

points(x = rep(1, times = length(x)), y = z$states[1, ], cex = 0.66, pch = 16, col = cols)
points(x = rep(.n, times = length(x)), y = z$x, cex = 0.66, pch = 16, col = cols)

text(x = 1, y = z$states[1, ], col = cols, labels = seq_along(x), cex = 0.66, font = 2, pos = 2)
text(x = .n, y = z$x, col = cols, labels = seq_along(x), cex = 0.66, font = 2, pos = 4)

axis(side = 2, at = unique(x), labels = round(unique(x), 1), col.axis = par('fg'), las = 1, cex.axis = 0.6)



dev.off()

## 
x <- c(0, 5, 12, 18, 20, 35, 40, 55, 90, 120, 150)
.thresh <- 9


z.s <- fixOverlap(x, thresh = .thresh, method = 'S')
z.e <- fixOverlap(x, thresh = .thresh, method = 'E', q = 2)

s <- rep(1, times = length(x))
r <- rank(x)

par(mar = c(3, 6, 0, 1))
plot(x, s, type = 'n', axes = FALSE, xlab = '', ylab = '', ylim = c(0.8, 1.2))
segments(x0 = x, x1 = z.s, y0 = s, y1 = s + 0.1, col = 'grey')
segments(x0 = x, x1 = z.e, y0 = s, y1 = s - 0.1, col = 'grey')
text(z.s, s + 0.1, labels = r, pch = 'E', font = 2, cex = 1, pos = 3)
text(z.e, s - 0.1, labels = r, pch = 'E', font = 2, cex = 1, pos = 1)
points(x, s, pch = 22, cex = 1.5, bg = 'royalblue')
axis(side = 1, at = pretty(x, n = 10))
mtext('Electrostatics', side = 2, at = 0.9, las = 1, cex = 1)
mtext('Simulated\nAnnealing', side = 2, at = 1.1, las = 1, cex = 1)



library(soilDB)

# some interesting soil series
s <- c('leon', 'musick', 'clarksville', 'pardee', 'lucy', 'pierre', 'drummer', 'zook', 'san joaquin')

# get basic morphology and extended data from SoilWeb cache
osds.full <- fetchOSD(s, extended = TRUE)

# save copy of SoilProfileCollection for later
osds <- osds.full$SPC

par(mar = c(0, 0, 0, 0))
plotSPC(osds, cex.names = 0.75, name.style = 'center-center', width = 0.3, plot.depth.axis = FALSE, hz.depths = TRUE, hz.depths.offset = 0.05, fixLabelCollisions = TRUE)

plotSPC(osds, cex.names = 1, print.id = FALSE, name.style = 'center-center', width = 0.3, plot.depth.axis = FALSE, hz.depths = TRUE, hz.depths.offset = 0.05, fixLabelCollisions = TRUE)

plotSPC(osds, cex.names = 1, print.id = FALSE, name.style = 'center-center', width = 0.3, plot.depth.axis = FALSE, hz.depths = TRUE, hz.depths.offset = 0.05, fixLabelCollisions = TRUE, max.depth = 135)


plotSPC(osds, cex.names = 0.5, print.id = FALSE, name.style = 'center-center', width = 0.3, plot.depth.axis = FALSE, hz.depths = TRUE, hz.depths.offset = 0.05, fixLabelCollisions = TRUE, scaling.factor = 0.5)

plotSPC(osds, cex.names = 0.5, print.id = FALSE, name.style = 'center-center', width = 0.3, plot.depth.axis = FALSE, hz.depths = TRUE, hz.depths.offset = 0.05, fixLabelCollisions = TRUE, scaling.factor = 0.25)


## very thin horizons

library(soilDB)
data("loafercreek")
hzdesgnname(loafercreek) <- 'hzname'


# profiles 20-40
x <- loafercreek[20:40, ]

# new labels
n <- c('O', 'A', 'B', 'Bt', 'BC', 'Cr', 'R')
# GHL patterns
p <- c('O', 'A', 'B', 'Bt', 'BC', 'Cr', 'R')

# note additional argument
horizons(x)$genhz <- generalize.hz(x$hzname, new = n, pat = p)

# the most important step, genhz must be encoded as an ordered factor
x$genhz <- ordered(x$genhz)


x$flag <- profileApply(x, function(i) {
  any(i$genhz == 'not-used')
})

# SPC-aware version of subset()
y <- subset(x, !flag)

# check: ok
table(y$genhz)

# add short ID for checking our work
site(y)$shortID <- 1:length(y)

par(mar = c(0, 0, 3, 0))

plotSPC(y, color = 'genhz', width = 0.3, name.style = 'center-center', plot.depth.axis = FALSE, hz.depths = TRUE, cex.names = 0.8, fixLabelCollisions = TRUE, col.label = 'Generalized Horizon Label', label = 'shortID', max.depth = 50, scaling.factor = 0.1)


plotSPC(y[15:19, ], color = 'genhz', width = 0.3, name.style = 'center-center', plot.depth.axis = FALSE, hz.depths = TRUE, cex.names = 0.8, fixLabelCollisions = TRUE, col.label = 'Generalized Horizon Label', label = 'shortID')

plotSPC(y[15:19, ], color = 'genhz', width = 0.3, name.style = 'center-center', plot.depth.axis = FALSE, hz.depths = TRUE, cex.names = 0.8, fixLabelCollisions = TRUE, col.label = 'Generalized Horizon Label', label = 'shortID', max.depth = 107)

plotSPC(y[15:19, ], color = 'genhz', width = 0.3, name.style = 'center-center', plot.depth.axis = FALSE, hz.depths = TRUE, cex.names = 0.8, fixLabelCollisions = TRUE, col.label = 'Generalized Horizon Label', label = 'shortID', max.depth = 5)




## TODO: errors when scale.factor is very small
library(sharpshootR)
library(soilDB)

# some interesting soil series
s <- c('leon', 'musick', 'clarksville', 'pardee', 'lucy', 'pierre', 'drummer', 'zook', 'san joaquin')
osds <- fetchOSD(s)

SoilTaxonomyDendrogram(osds, cex.names = 0.8, print.id = FALSE, name.style = 'center-center', width = 0.3, plot.depth.axis = FALSE, hz.depths = TRUE, hz.depths.offset = 0.05, fixLabelCollisions = TRUE)


SoilTaxonomyDendrogram(osds, cex.names = 0.8, print.id = FALSE, name.style = 'center-center', width = 0.3, plot.depth.axis = FALSE, hz.depths = TRUE, hz.depths.offset = 0.05, fixLabelCollisions = TRUE, max.depth = 125, scaling.factor = 0.015)


SoilTaxonomyDendrogram(trunc(osds, 0, 150), cex.names = 0.8, print.id = FALSE, name.style = 'center-center', width = 0.3, plot.depth.axis = FALSE, hz.depths = TRUE, hz.depths.offset = 0.05, fixLabelCollisions = TRUE)


o <- osds[6, ]
x <- unique(c(o[, , .TOP], o[, , .BOTTOM]))

z.s <- fixOverlap(x, thresh = 9.1, method = 'S')
z.e <- fixOverlap(x, thresh = 9.1, method = 'E', q = 0.1)

s <- rep(1, times = length(x))
r <- rank(x)

par(mar = c(3, 6, 0, 1))
plot(x, s, type = 'n', axes = FALSE, xlab = '', ylab = '', ylim = c(0.8, 1.2))
segments(x0 = x, x1 = z.s, y0 = s, y1 = s + 0.1, col = 'grey')
segments(x0 = x, x1 = z.e, y0 = s, y1 = s - 0.1, col = 'grey')
text(z.s, s + 0.1, labels = r, pch = 'E', font = 2, cex = 1, pos = 3)
text(z.e, s - 0.1, labels = r, pch = 'E', font = 2, cex = 1, pos = 1)
points(x, s, pch = 22, cex = 1.5, bg = 'royalblue')
axis(side = 1, at = pretty(x))
mtext('Electrostatics', side = 2, at = 0.9, las = 1, cex = 1)
mtext('Simulated\nAnnealing', side = 2, at = 1.1, las = 1, cex = 1)






devtools::load_all()

##
## nutty idea: label collision fixes via simulation of electrostatic charged particles
##






## 
x <- c(0, 5, 12, 18, 20, 35, 40, 55, 90, 120, 150)
.thresh <- 9


z.s <- fixOverlap(x, thresh = .thresh, method = 'S')
z.e <- fixOverlap(x, thresh = .thresh, method = 'E', q = 1)

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





# some interesting soil series
s <- c('leon', 'musick', 'clarksville', 'pardee', 'lucy', 'pierre', 'drummer', 'zook', 'san joaquin')
osds <- soilDB::fetchOSD(s)

par(mar = c(0, 0, 0, 0))
plotSPC(osds, cex.names = 0.75, name.style = 'center-center', width = 0.3, plot.depth.axis = FALSE, hz.depths = TRUE, hz.depths.offset = 0.05, fixLabelCollisions = TRUE)

plotSPC(trunc(osds, 0, 25), cex.names = 0.75, name.style = 'center-center', width = 0.3, plot.depth.axis = FALSE, hz.depths = TRUE, hz.depths.offset = 0.05, fixLabelCollisions = TRUE)


plotSPC(osds, cex.names = 1, print.id = FALSE, name.style = 'center-center', width = 0.3, plot.depth.axis = FALSE, hz.depths = TRUE, hz.depths.offset = 0.05, fixLabelCollisions = TRUE)

plotSPC(osds, cex.names = 1, print.id = FALSE, name.style = 'center-center', width = 0.3, plot.depth.axis = FALSE, hz.depths = TRUE, hz.depths.offset = 0.05, fixLabelCollisions = TRUE, max.depth = 135)

plotSPC(osds, cex.names = 1, print.id = FALSE, name.style = 'center-center', width = 0.3, plot.depth.axis = FALSE, hz.depths = TRUE, hz.depths.offset = 0.05, fixLabelCollisions = TRUE, max.depth = 50)

plotSPC(osds, cex.names = 1, print.id = FALSE, name.style = 'center-center', width = 0.3, plot.depth.axis = FALSE, hz.depths = TRUE, hz.depths.offset = 0.05, fixLabelCollisions = TRUE, y.offset = 30, max.depth = 135)

plotSPC(osds, cex.names = 1, print.id = FALSE, name.style = 'center-center', width = 0.3, plot.depth.axis = FALSE, hz.depths = TRUE, hz.depths.offset = 0.05, fixLabelCollisions = TRUE, y.offset = 30, scaling.factor = 0.5, max.depth = 135)

plotSPC(osds, cex.names = 1, print.id = FALSE, name.style = 'center-center', width = 0.3, plot.depth.axis = FALSE, hz.depths = TRUE, hz.depths.offset = 0.05, fixLabelCollisions = TRUE, y.offset = 30, scaling.factor = 0.5)


plotSPC(osds, cex.names = 0.6, print.id = FALSE, name.style = 'center-center', width = 0.3, plot.depth.axis = FALSE, hz.depths = TRUE, hz.depths.offset = 0.05, fixLabelCollisions = TRUE, y.offset = 300, max.depth = 135)



plotSPC(osds, cex.names = 0.5, print.id = FALSE, name.style = 'center-center', width = 0.3, plot.depth.axis = FALSE, hz.depths = TRUE, hz.depths.offset = 0.05, fixLabelCollisions = TRUE, scaling.factor = 0.5)

plotSPC(osds, cex.names = 0.5, print.id = FALSE, name.style = 'center-center', width = 0.3, plot.depth.axis = FALSE, hz.depths = TRUE, hz.depths.offset = 0.05, fixLabelCollisions = TRUE, scaling.factor = 0.25)


## simulate environment setup by SoilTaxonomyDendrogram()
s <- c('leon', 'musick', 'pardee')
osds <- soilDB::fetchOSD(s)

par(mar = c(0,0,0,0))

# no errors
plot(1, 1, cex = 0.8, ylim = c(4,0), xlim = c(0.5, length(osds) + 1), type = 'n')
plotSPC(osds, cex.names = 0.8, print.id = FALSE, name.style = 'center-center', width = 0.3, plot.depth.axis = FALSE, hz.depths = TRUE, hz.depths.offset = 0.05, fixLabelCollisions = TRUE, y.offset = 0.5, scaling.factor = 0.015, add = TRUE)

plot(1, 1, cex = 0.8, ylim = c(4,0), xlim = c(0.5, length(osds) + 1), type = 'n')
plotSPC(osds, cex.names = 0.8, print.id = FALSE, name.style = 'center-center', width = 0.3, plot.depth.axis = FALSE, hz.depths = TRUE, hz.depths.offset = 0.05, fixLabelCollisions = TRUE, y.offset = 0.5, scaling.factor = 0.015, add = TRUE, max.depth = 100)

plot(1, 1, cex = 0.8, ylim = c(4,0), xlim = c(0.5, length(osds) + 1), type = 'n')
plotSPC(osds, plot.order = c(3, 2, 1), cex.names = 0.8, print.id = FALSE, name.style = 'center-center', width = 0.3, plot.depth.axis = FALSE, hz.depths = TRUE, hz.depths.offset = 0.05, fixLabelCollisions = TRUE, y.offset = 1, scaling.factor = 0.015, add = TRUE)


plot(1, 1, cex = 0.8, ylim = c(4,0), xlim = c(0.5, length(osds) + 1), type = 'n')
plotSPC(osds, cex.names = 0.8, print.id = FALSE, name.style = 'center-center', width = 0.3, plot.depth.axis = FALSE, hz.depths = TRUE, hz.depths.offset = 0.05, fixLabelCollisions = TRUE, y.offset = 2, scaling.factor = 0.015, add = TRUE)




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


plotSPC(y[15:19, ], color = 'genhz', width = 0.3, name.style = 'center-center', plot.depth.axis = FALSE, hz.depths = TRUE, cex.names = 0.8, fixLabelCollisions = TRUE, col.label = 'Generalized Horizon Label', label = 'shortID', fixOverlapArgs = list(medhod = 'E', q = 2))


plotSPC(y[15:19, ], color = 'genhz', width = 0.3, name.style = 'center-center', plot.depth.axis = FALSE, hz.depths = TRUE, cex.names = 0.8, fixLabelCollisions = TRUE, col.label = 'Generalized Horizon Label', label = 'shortID', fixOverlapArgs = list(medhod = 'S', adj = 2))

plotSPC(y[15:19, ], color = 'genhz', width = 0.3, name.style = 'center-center', plot.depth.axis = FALSE, hz.depths = TRUE, cex.names = 0.8, fixLabelCollisions = TRUE, col.label = 'Generalized Horizon Label', label = 'shortID', max.depth = 107)

plotSPC(y[17:18, ], color = 'genhz', width = 0.2, name.style = 'center-center', plot.depth.axis = FALSE, hz.depths = TRUE, cex.names = 0.8, fixLabelCollisions = TRUE, col.label = 'Generalized Horizon Label', label = 'shortID', max.depth = 109)


plotSPC(y[15:19, ], color = 'genhz', width = 0.3, name.style = 'center-center', plot.depth.axis = FALSE, hz.depths = TRUE, cex.names = 0.8, fixLabelCollisions = TRUE, col.label = 'Generalized Horizon Label', label = 'shortID', max.depth = 5)



devtools::load_all(path = '../sharpshootR/', reset = TRUE)
devtools::load_all(reset = TRUE)

# some interesting soil series
s <- c('leon', 'musick', 'clarksville', 'pardee', 'lucy', 'pierre', 'drummer', 'zook', 'san joaquin')
osds <- soilDB::fetchOSD(s)

SoilTaxonomyDendrogram(osds, cex.names = 0.8, print.id = FALSE, name.style = 'center-center', width = 0.3, plot.depth.axis = FALSE, hz.depths = TRUE, hz.depths.offset = 0.05, fixLabelCollisions = TRUE)


SoilTaxonomyDendrogram(osds, cex.names = 0.8, print.id = FALSE, name.style = 'center-center', width = 0.3, plot.depth.axis = FALSE, hz.depths = TRUE, hz.depths.offset = 0.05, fixLabelCollisions = TRUE, max.depth = 125)


SoilTaxonomyDendrogram(trunc(osds, 0, 90), cex.names = 0.8, print.id = FALSE, name.style = 'center-center', width = 0.3, plot.depth.axis = FALSE, hz.depths = TRUE, hz.depths.offset = 0.05, fixLabelCollisions = TRUE, scaling.factor = 0.03)






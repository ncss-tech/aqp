
## TODO: convert this into a vignette
# https://github.com/ncss-tech/aqp/issues/342


# install mpsline2 from GH

library(aqp)
library(mpspline2)
library(reshape2)
library(lattice)
library(tactile)
library(vegan)

## thoughts:
# * variable depths option from EAS 
# * slab() should truncate to the actual bottom of a profile
# * add support for wt. mean intervals from mpspline() -> mixed-support SPC
# * slab2SPC() or option to slab() to re-shape into an SPC


# make some example data
x <- list(
  id = 'A',
  depths = c(25, 33, 100, 150,  175),
  name = c('A', 'Bw', 'Bt1', 'Bt2', 'BC'),
  p1 = c(12, 15, 35, 20, 15),
  p2 = c(7.2, 7, 6.8, 6.5, 6.5),
  soil_color = c('10YR 3/3', '10YR 4/4', '10YR 4/6', '5G 6/2', '5BG 4/4')
)

x <- quickSPC(x)

# fake site data
site(x)$fake_site_attr <- 1:length(x)

# check source data
par(mar = c(0,0,3,1))
plotSPC(x, color = 'p1')
plotSPC(x, color = 'p2')

s <- seq(0, 1, by = 0.25)
m <- lapply(s, function(i) {
  .spc <- spc2mpspline(x, var_name = c('p1', 'p2'), lam = i, method = 'est_1cm')
  profile_id(.spc) <- sprintf("%s-%s", profile_id(.spc), format(i, digits = 2))
  return(.spc)
})

z <- combine(m)
z$p1 <- z$p1_spline
z$p2 <- z$p2_spline

xx <- combine(z, x)

par(mar = c(0, 0, 3, 2))
plotSPC(xx, color = 'p1', col.palette = hcl.colors(10, 'viridis'), divide.hz = FALSE, width = 0.35, cex.names = 0.8, name = NA)
mtext(side = 3, at = 1, line = -1.5, text = 'value of lambda')

# o <- order(xx$p1_rmse)
# plotSPC(xx, color = 'p1', col.palette = hcl.colors(10, 'viridis'), divide.hz = FALSE, width = 0.35, plot.order = o)

d <- dice(xx)
d$p1diff <- rep(NA, times = nrow(d))

d$p1diff <- profileApply(d, function(i) {
  d[1, ]$p1 - i$p1
})

plotSPC(d, color = 'p1diff', col.palette = hcl.colors(10, 'viridis'), divide.hz = FALSE, width = 0.35, cex.names = 0.8, name = NA)
mtext(side = 3, at = 2, line = -1.5, text = 'value of lambda')



tps <- tactile.theme(superpose.line = list(lwd = 2))

# compare depth-functions by method, no aggregation
xyplot(cbind(top, bottom) ~ p1, id=xx$id, groups = factor(id),
       data=as(xx, 'data.frame'),
       par.settings=tps,
       auto.key=list(columns=3, lines=TRUE, points=FALSE),
       strip=strip.custom(bg=grey(0.85)),
       ylim=c(185, -5), as.table=TRUE, panel=panel.depth_function,
       scales = list(alternating = 1),
       asp = 1
)

xyplot(cbind(top, bottom) ~ p1 | id, id=xx$id,
       data=as(xx, 'data.frame'),
       par.settings=tps,
       auto.key=list(columns=4, lines=TRUE, points=FALSE),
       strip=strip.custom(bg=grey(0.85)),
       ylim=c(185, -5), as.table=TRUE, panel=panel.depth_function,
       layout = c(6, 1),
       scales = list(alternating = 1)
)



## test effect of lambda
slab(xx, id ~ p1, slab.structure = c(0, 50), slab.fun = mean, na.rm = TRUE)
slab(xx, id ~ p1, slab.structure = c(0, 50), slab.fun = sum, na.rm = TRUE)




####################### more complex examples / eval ########################

# make some example data
ids <- LETTERS[1:6]

set.seed(10101)
x <- lapply(ids, random_profile, n = c(6, 7, 8), n_prop = 2, method = 'LPP', SPC = TRUE, lpp.u = 40)
x <- combine(x)

# fake site data
site(x)$fake_site_attr <- 1:length(x)

# check source data
par(mar = c(0,0,3,1))
plotSPC(x, color = 'p1')
plotSPC(x, color = 'p2')

# 1-cm estimates
# TODO: support mixture of 1cm + new depth intervals in the same resulting SPC via `d` argument
m <- spc2mpspline(x, var_name = c('p1', 'p2'), lam = 0.1, method = 'est_1cm')
m2 <- spc2mpspline(x, var_name = c('p1', 'p2'), lam = 0.1, method = 'est_dcm', d = c(0, 5, 15, 30, 60, 100, 200))

# hzID(m)
# hzidname(m)

# copy EAS values into original column for later combine()
m$p1 <- m$p1_spline
m$p2 <- m$p2_spline
m2$p1 <- m2$p1_spline
m2$p2 <- m2$p2_spline

# check: OK
plotSPC(m, color = 'p1', name = NA, divide.hz = FALSE)
plotSPC(m, color = 'p2', name = NA, divide.hz = FALSE)

plotSPC(m2, color = 'p1', name = NA, divide.hz = FALSE)
plotSPC(m2, color = 'p2', name = NA, divide.hz = FALSE)



par(mar = c(0, 0, 3, 1.5))
plotSPC(x, color = 'p1', x.idx.offset = -0.4,  width = 0.2)
plotSPC(m, color = 'p1', name = NA, divide.hz = FALSE, depth.axis = FALSE, show.legend = FALSE, add = TRUE, width = 0.15, x.idx.offset = 0.1)

plotMultipleSPC(
  list(x, m, m2),
  merged.colors = hcl.colors(10, 'viridis'), 
  merged.legend = 'p1',
  bracket.base.depth = 150, 
  group.labels = c('original', 'mpspline 1cm', 'mpspline'), label.offset = 5,
  args = list(
    list(),
    list(divide.hz = FALSE),
    list()
  )
)


# don't forget na.rm = TRUE
a <- slab(x, id ~ p1 + p2, slab.structure = c(0, 5, 15, 30, 60, 100, 200), slab.fun = mean, na.rm = TRUE)


a <- dcast(a, id + top + bottom ~ variable)
depths(a) <- id ~ top + bottom

# copy original ID
site(x)$original_id <- profile_id(x)
site(m)$original_id <- profile_id(m)
site(m2)$original_id <- profile_id(m2)
site(a)$original_id <- profile_id(a)

# make IDs unique
profile_id(x) <- sprintf("%s-original", profile_id(x))
profile_id(m) <- sprintf("%s-EAS-1cm", profile_id(m))
profile_id(m2) <- sprintf("%s-EAS-interval", profile_id(m2))
profile_id(a) <- sprintf("%s-slab", profile_id(a))

# add site-level grouping var
site(x)$provenance <- 'original'
site(m)$provenance <- 'EAS-1cm'
site(m2)$provenance <- 'EAS-interval'
site(a)$provenance <- 'slab'

# combine
z <- combine(list(x, m, m2, a))
z$provenance <- factor(z$provenance, levels = c('original', 'EAS-1cm', 'slab', 'EAS-interval'))
z$original_id <- factor(z$original_id)

plotSPC(z, color = 'p1', divide.hz = FALSE, width = 0.25, cex.names = 0.66, max.depth = 150)
plotSPC(z, color = 'p2', divide.hz = FALSE, width = 0.25, cex.names = 0.66, max.depth = 150)



# plot by group
par(mar=c(0, 0, 3, 1))
plotSPC(z, color='p1', max.depth = max(x), name=NA, divide.hz=FALSE, width = 0.3, col.palette = hcl.colors(10), n.legend = 8)

groupedProfilePlot(z, groups = 'original_id', color='p1', max.depth = max(x), group.name.offset = -10, name=NA, divide.hz=FALSE, width=0.3, col.palette = hcl.colors(10), n.legend = 8)

groupedProfilePlot(z, groups = 'provenance', color='p1', max.depth = max(x), group.name.offset = -10, name=NA, divide.hz=FALSE, width = 0.3, col.palette = hcl.colors(10), n.legend = 8)

# compare depth-functions by method, no aggregation
xyplot(cbind(top, bottom) ~ p1 | original_id, id=z$id, groups=provenance, 
       data=as(z, 'data.frame'),
       par.settings=tps,
       auto.key=list(columns=4, lines=TRUE, points=FALSE),
       strip=strip.custom(bg=grey(0.85)),
       ylim=c(125, -5), as.table=TRUE, panel=panel.depth_function,
       layout = c(6, 1),
       scales = list(x = list(relation = 'free')),
       ylab = 'Depth (cm)',
       xlab = 'simulated property'
)



# aggregate by provenance
z.agg <- slab(z, fm = provenance ~ p1)

xyplot(top ~ p.q50, groups=provenance, data=z.agg, ylab='Depth', asp=1.5,
       lower=z.agg$p.q25, upper=z.agg$p.q75, ylim=c(125,-5),
       xlab='median bounded by 25th and 75th percentiles',
       sync.colors=TRUE, alpha=0.25,
       par.settings=tps,
       auto.key=list(columns=4, lines=TRUE, points=FALSE),
       panel=panel.depth_function, 
       prepanel=prepanel.depth_function,
       cf=z.agg$contributing_fraction,
       scales=list(x=list(alternating=1))
)

xyplot(top ~ p.q50, groups=provenance, data=z.agg, ylab='Depth', asp=1.5,
       ylim=c(125,-5),
       xlab='median bounded by 25th and 75th percentiles',
       par.settings=tps,
       auto.key=list(columns=4, lines=TRUE, points=FALSE),
       panel=panel.depth_function, 
       prepanel=prepanel.depth_function,
       scales=list(x=list(alternating=1))
)

xyplot(top ~ p.q50 | provenance, data=z.agg, ylab='Depth', asp=1.5,
       lower=z.agg$p.q25, upper=z.agg$p.q75, ylim=c(125, -5),
       xlab='median bounded by 25th and 75th percentiles',
       sync.colors=TRUE, alpha=0.5,
       strip=strip.custom(bg=grey(0.85)),
       par.settings = tps,
       panel=panel.depth_function, 
       prepanel=prepanel.depth_function,
       cf=z.agg$contributing_fraction,
       scales=list(x=list(alternating=1))
)


# * truncate comparisons to 110cm
d <- NCSP(z, vars = c('p1', 'p2'), maxDepth = 100)

# interesting
par(mar = c(0, 0, 3, 3))
plotProfileDendrogram(z, cluster::diana(d), dend.y.scale = 175, scaling.factor = 0.95, y.offset = 2, color = 'p1', divide.hz = FALSE, width = 0.3, max.depth = 125, cex.names = 0.8)

plotProfileDendrogram(z, cluster::diana(d), dend.y.scale = 175, scaling.factor = 0.95, y.offset = 2, color = 'p2', divide.hz = FALSE, width = 0.3, max.depth = 125, cex.names = 0.8)


## TODO: viz via nMDS and betadispersion
b <- betadisper(d, group = z$provenance, type = 'median', sqrt.dist = TRUE)

plot(b)
boxplot(b)

par(mar = c(4.5, 7, 3, 1))
plot(TukeyHSD(b), las = 1,  cex.axis = 0.66)


## what about simple summaries within a fixed depth interval?
# nearly identical

slab(z, provenance ~ p1 + p2, slab.structure = c(0, 50), slab.fun = mean, na.rm = TRUE)

slab(z, provenance ~ p1, slab.structure = c(0, 50), slab.fun = sum, na.rm = TRUE)







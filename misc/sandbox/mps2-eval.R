# install mpsline2 from CRAN

library(aqp)
library(sharpshootR)
library(mpspline2)
library(reshape2)
library(lattice)

## thoughts:
# * variable depths option from EAS 
# * slab() should truncate to the actual bottom of a profile
# * add support for wt. mean intervals from mpspline() -> mixed-support SPC
# * slab2SPC() or option to slab() to re-shape into an SPC



## don't need these anymore
# helper functions
# source('https://raw.githubusercontent.com/ncss-tech/aqp/master/misc/sandbox/mps-functions.R')

# fewer moving parts
options(stringsAsFactors = FALSE)


# make some example data
ids <- LETTERS[1:6]

set.seed(10101)
x <- lapply(ids, random_profile, n=c(6, 7, 8), n_prop=2, method='LPP', SPC=TRUE)
x <- combine(x)

# fake site data
site(x)$fake_site_attr <- 1:length(x)

# check source data
par(mar=c(0,0,3,1))
plotSPC(x, color='p1', col.palette = hcl.colors(10, 'viridis'))
plotSPC(x, color='p2', col.palette = hcl.colors(10, 'viridis'))

# 1-cm estimates
# TODO: support mixture of 1cm + new depth intervals in the same resulting SPC via `d` argument
m <- spc2mpspline(x, var_name = 'p1', lam = 0.1, method = 'est_1cm')
m2 <- spc2mpspline(x, var_name = 'p1', lam = 0.1, method = 'est_dcm', d = c(0, 5, 15, 30, 60, 100, 200))

hzID(m)
hzidname(m)

# copy EAS values into original column for later combine()
m$p1 <- m$p1_spline
m2$p1 <- m2$p1_spline

# check: OK
plotSPC(m, color = 'p1', name = NA, divide.hz = FALSE, col.palette = hcl.colors(10, 'viridis'))
plotSPC(m2, color = 'p1', name = NA, divide.hz = FALSE, col.palette = hcl.colors(10, 'viridis'))



par(mar = c(0, 0, 3, 1.5))
plotSPC(x, color = 'p1', x.idx.offset = -0.4,  width = 0.2)
plotSPC(m, color = 'p1', name = NA, divide.hz = FALSE, depth.axis = FALSE, show.legend = FALSE, add = TRUE, width = 0.15, x.idx.offset = 0.1)

plotMultipleSPC(
  list(x, m, m2),
  merged.colors = hcl.colors(10, 'viridis'), 
  merged.legend = 'p1',
  bracket.base.depth = 150, 
  group.labels = c('original', 'mpspline'), 
  args = list(
    list(),
    list(divide.hz = FALSE),
    list()
  )
)


# don't forget na.rm = TRUE
a <- slab(x, id ~ p1, slab.structure = c(0, 5, 15, 30, 60, 100, 200), slab.fun = mean, na.rm = TRUE)


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

plotSPC(z, color='p1', divide.hz = FALSE, width = 0.25, cex.names = 0.66, max.depth = 150)



# # latest version, integrates most of the code from my previous 
# # m <- mpspline(x, var_name = 'p1', d=c(0, 5, 15, 30, 60, 100, 200), out_style = 'spc')
# 
# ## TODO: this can only perform EAS for single horizon-level attribute
# ## TODO: this needs an additional wrapper as of mpspline2_0.1.3 
# 
# # SPC -> MPS -> SPC
# # m <- mpsplineSPC(x, var='p1', d=c(0, 5, 15, 30, 60, 100, 200))
# 
# # check: OK
# str(m)
# 
# # note sorting
# profile_id(m)
# 
# m$id_group <- factor(m$id_group)
# 

# plot by group
par(mar=c(0, 0, 3, 1))
plotSPC(z, color='p1', max.depth=175, name=NA, divide.hz=FALSE, width = 0.3, col.palette = hcl.colors(10), n.legend = 8)

groupedProfilePlot(z, groups = 'original_id', color='p1', max.depth = max(x), group.name.offset = -10, name=NA, divide.hz=FALSE, width=0.3, col.palette = hcl.colors(10), n.legend = 8)

groupedProfilePlot(z, groups = 'provenance', color='p1', max.depth = max(x), group.name.offset = -10, name=NA, divide.hz=FALSE, width = 0.3, col.palette = hcl.colors(10), n.legend = 8)

tps <- list(superpose.line=list(lwd=2, col=c('royalblue', 'darkgreen', 'firebrick', 'orange')))

# compare depth-functions by method, no aggregation
xyplot(cbind(top, bottom) ~ p1 | original_id, id=z$id, groups=provenance, 
       data=as(z, 'data.frame'),
       par.settings=tps,
       auto.key=list(columns=4, lines=TRUE, points=FALSE),
       strip=strip.custom(bg=grey(0.85)),
       ylim=c(175, -5), as.table=TRUE, panel=panel.depth_function,
       layout = c(6, 1),
       scales = list(x = list(relation = 'free'))
)



# aggregate by provenance
z.agg <- slab(z, fm = provenance ~ p1)

xyplot(top ~ p.q50, groups=provenance, data=z.agg, ylab='Depth', asp=1.5,
       lower=z.agg$p.q25, upper=z.agg$p.q75, ylim=c(200,-5),
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
       ylim=c(200,-5),
       xlab='median bounded by 25th and 75th percentiles',
       par.settings=tps,
       auto.key=list(columns=4, lines=TRUE, points=FALSE),
       panel=panel.depth_function, 
       prepanel=prepanel.depth_function,
       scales=list(x=list(alternating=1))
)

xyplot(top ~ p.q50 | provenance, data=z.agg, ylab='Depth', asp=1.5,
       lower=z.agg$p.q25, upper=z.agg$p.q75, ylim=c(175,-5),
       xlab='median bounded by 25th and 75th percentiles',
       sync.colors=TRUE, alpha=0.5,
       strip=strip.custom(bg=grey(0.85)),
       par.settings=list(superpose.line=list(lwd=2, col=c('RoyalBlue', 'firebrick'))),
       panel=panel.depth_function, 
       prepanel=prepanel.depth_function,
       cf=z.agg$contributing_fraction,
       scales=list(x=list(alternating=1))
)


# * only using a single property.. need to update EAS code to do more than a single property
# * truncate comparisons to 110cm
d <- NCSP(z, vars = 'p1', maxDepth = 110)

# interesting
par(mar = c(0, 0, 0, 3))
plotProfileDendrogram(z, cluster::diana(d), dend.y.scale = 175, scaling.factor = 0.85, y.offset = 2, color = 'p1', divide.hz = FALSE, width = 0.3, max.depth = 150, cex.names = 0.8)


## TODO: viz via nMDS and betadispersion

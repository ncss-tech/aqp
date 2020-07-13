# install mpsline2
# remotes::install_github('obrl-soil/mpspline2')

library(aqp)
library(sharpshootR)
library(mpspline2)
library(lattice)

# helper functions
source('https://raw.githubusercontent.com/ncss-tech/aqp/master/misc/sandbox/mps-functions.R')

# fewer moving parts
options(stringsAsFactors=FALSE)


# make some example data
ids <- LETTERS[1:6]

set.seed(10101)
x <- lapply(ids, random_profile, n=c(6, 7, 8), n_prop=2, method='LPP', SPC=TRUE)
x <- union(x)

# fake site data
site(x)$fake_site_attr <- 1:length(x)

# check source data
par(mar=c(0,0,3,1))
plot(x, color='p1')
plot(x, color='p2')

# latest version, integrates most of the code from my previous 
# m <- mpspline(x, var_name = 'p1', d=c(0, 5, 15, 30, 60, 100, 200), out_style = 'spc')

## TODO: this can only perform EAS for single horizon-level attribute
## TODO: this needs an additional wrapper as of mpspline2_0.1.3 

# SPC -> MPS -> SPC
m <- mpsplineSPC(x, var='p1', d=c(0, 5, 15, 30, 60, 100, 200))

# check: OK
str(m)

# note sorting
profile_id(m)

m$id_group <- factor(m$id_group)


# plot by group
par(mar=c(0, 0, 3, 1))
plot(m, color='p1', max.depth=175, name='', divide.hz=FALSE, width = 0.3, name.style = 'left-center')

groupedProfilePlot(m, groups = 'id_group', color='p1', max.depth=175, group.name.offset = -10, name='', divide.hz=FALSE, width=0.3, name.style = 'left-center')

groupedProfilePlot(m, groups = 'method_group', color='p1', max.depth=175, group.name.offset = -10, name='', divide.hz=FALSE, width = 0.3, name.style = 'left-center')


# compare depth-functions by method, no aggregation
xyplot(cbind(top, bottom) ~ p1 | factor(id_group), id=m$id, groups=factor(method_group), 
       data=as(m, 'data.frame'),
       par.settings=list(superpose.line=list(lwd=2, col=c('RoyalBlue', 'darkgreen', 'firebrick'))),
       auto.key=list(columns=3, lines=TRUE, points=FALSE),
       strip=strip.custom(bg=grey(0.85)),
       ylim=c(175, -5), as.table=TRUE, panel=panel.depth_function
       )



# aggregate by method_group
a <- slab(m, fm = method_group ~ p1)

xyplot(top ~ p.q50, groups=factor(method_group), data=a, ylab='Depth', asp=1.5,
       lower=a$p.q25, upper=a$p.q75, ylim=c(200,-5),
       xlab='median bounded by 25th and 75th percentiles',
       sync.colors=TRUE, alpha=0.25,
       par.settings=list(superpose.line=list(lwd=2, col=c('RoyalBlue', 'darkgreen', 'firebrick'))),
       auto.key=list(columns=3, lines=TRUE, points=FALSE),
       panel=panel.depth_function, 
       prepanel=prepanel.depth_function,
       cf=a$contributing_fraction,
       scales=list(x=list(alternating=1))
)

xyplot(top ~ p.q50, groups=factor(method_group), data=a, ylab='Depth', asp=1.5,
       ylim=c(200,-5),
       xlab='median bounded by 25th and 75th percentiles',
       par.settings=list(superpose.line=list(lwd=2, col=c('RoyalBlue', 'darkgreen', 'firebrick'))),
       auto.key=list(columns=3, lines=TRUE, points=FALSE),
       panel=panel.depth_function, 
       prepanel=prepanel.depth_function,
       scales=list(x=list(alternating=1))
)

xyplot(top ~ p.q50 | factor(method_group), data=a, ylab='Depth', asp=1.5,
       lower=a$p.q25, upper=a$p.q75, ylim=c(200,-5),
       xlab='median bounded by 25th and 75th percentiles',
       sync.colors=TRUE, alpha=0.5,
       strip=strip.custom(bg=grey(0.85)),
       par.settings=list(superpose.line=list(lwd=2, col=c('RoyalBlue', 'firebrick'))),
       panel=panel.depth_function, 
       prepanel=prepanel.depth_function,
       cf=a$contributing_fraction,
       scales=list(x=list(alternating=1))
)


## TODO: finish this
# * only using a single property.. need to update EAS code to do more than a single property
d <- profile_compare(m, vars=c('p1', 'p1'), max_d=150, k=0)

# interesting
plotProfileDendrogram(m, cluster::diana(d), scaling.factor = 0.85, y.offset = 10, color = 'p1', divide.hz=FALSE, width=0.3, name.style = 'left-center')


## TODO: viz via nMDS and betadispersion

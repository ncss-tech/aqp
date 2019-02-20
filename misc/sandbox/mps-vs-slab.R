library(aqp)
library(soilDB)
library(GSIF)
library(plyr)
library(reshape2)
library(stringr)
library(lattice)

# x <- fetchKSSL(series='auburn')
# 
# plot(x, color='clay', name='hzn_desgn', print.id=FALSE)
# 
# s <- slice(x, 0:24 ~ clay)
# a.slab.profiles <- slab(x, pedon_key ~ clay, slab.structure = c(0, 20), slab.fun = mean, na.rm=TRUE)
# a.slab.all <- slab(x, ~ clay, slab.structure = c(0, 20), slab.fun = mean, na.rm=TRUE)
# 
# 
# a.mps <- mpspline(x, 'clay', lam=0.1, d=c(0,20))
# 
# apply(a.mps$var.1cm, 2, mean, na.rm=TRUE)
# 




###

# pretty IDs to avoid integer -> character -> sorting madness
ids <- sprintf("%02d", 1:10)
x <- ldply(ids, random_profile, n=c(6, 7, 8), n_prop=1, method='LPP', 
           lpp.a=5, lpp.b=15, lpp.d=5, lpp.e=5, lpp.u=25)

# promote to SPC and plot
depths(x ) <- id ~ top + bottom
plot(x, color='p1')

# slice for later
s <- slice(x, 0:200 ~ p1)
plot(s, color='p1', divide.hz=FALSE)

# fit MP spline by profile
x.mps <- mpspline(x, 'p1')

# extract IDs from MPS, should be the same as original vector of IDs
ids <- x.mps$idcol

# extract data
values <- x.mps$var.std
# add ids
values$id <- ids
# drop soil depth column
values$`soil depth` <- NULL

# melt wide-format data
m <- melt(values, id.vars = 'id', measure.vars = names(values)[1:6])

# fix depths
m$variable <- as.character(m$variable)
# remove ' cm'
m$variable <- gsub(pattern = ' cm', replacement = '', x = m$variable, fixed = TRUE)
# split
m.depths <- str_split(m$variable, pattern = '-', simplify = TRUE)

# add depths
m$top <- as.integer(m.depths[, 1])
m$bottom <- as.integer(m.depths[, 2])

# drop / re-name as needed to conform to original SPC
m$p1 <- m$value
m$name <- m$variable
m$variable <- NULL
m$value <- NULL

# re-order horizon data columns just in case
m <- m[, c('id', 'top', 'bottom', 'name', 'p1')]

# sort on ID / top depth
m <- m[order(m$id, m$top), ]

# edit ID to make unique--required for stacking
m$id <- sprintf("%s-mps", m$id)


# convert back to SPC
depths(m) <- id ~ top + bottom

# check: OK
plot(m, color='p1')


## do the same with MPS 1-cm data
dd <- as.data.frame(x.mps$var.1cm)
names(dd) <- ids
dd$top <- 0:199
dd$bottom <- 1:200

# condition into a proper data.frame that we can turn into an SPC
m.dd <- melt(dd, id.vars=c('top', 'bottom'))
names(m.dd) <- c('top', 'bottom', 'id', 'p1')
m.dd$name <- sprintf("%d-%d", m.dd$top, m.dd$bottom)
m.dd <- m.dd[, c('id', 'top', 'bottom', 'name', 'p1')]
# edit ID to make unique--required for stacking
m.dd$id <- sprintf("%s-mps-1cm", m.dd$id)

# upgrade to SPC
depths(m.dd) <- id ~ top + bottom


## combine and check
# add groups for plotting / aggregation
x$group <- rep('original', times=length(x))
m$group <- rep('MPS', times=length(m))
m.dd$group <- rep('MPS-1cm', times=length(m.dd))

# combine SPCs 
z <- union(list(x, m, m.dd))

# add new group for profiles
z$profile.group <- factor(str_sub(profile_id(z), 0, 2))

par(mar=c(0,0,3,1))
groupedProfilePlot(z, groups='profile.group', color='p1', id.style='side', divide.hz=FALSE, name='')


## TODO: finish this
# this is redundant
a <- slab(z, fm = group ~ p1)

xyplot(top ~ p.q50, groups=factor(group), data=a, ylab='Depth', asp=1.5,
       lower=a$p.q25, upper=a$p.q75, ylim=c(200,-5),
       xlab='median bounded by 25th and 75th percentiles',
       sync.colors=TRUE, alpha=0.5,
       par.settings=list(superpose.line=list(lwd=2, col=c('RoyalBlue', 'darkgreen', 'firebrick'))),
       auto.key=list(columns=3, lines=TRUE, points=FALSE),
       panel=panel.depth_function, 
       prepanel=prepanel.depth_function,
       cf=a$contributing_fraction,
       scales=list(x=list(alternating=1))
)

xyplot(top ~ p.q50, groups=factor(group), data=a, ylab='Depth', asp=1.5,
      ylim=c(200,-5),
       xlab='median bounded by 25th and 75th percentiles',
       par.settings=list(superpose.line=list(lwd=2, col=c('RoyalBlue', 'darkgreen', 'firebrick'))),
       auto.key=list(columns=3, lines=TRUE, points=FALSE),
       panel=panel.depth_function, 
       prepanel=prepanel.depth_function,
       scales=list(x=list(alternating=1))
)

xyplot(top ~ p.q50 | factor(group), data=a, ylab='Depth', asp=1.5,
       lower=a$p.q25, upper=a$p.q75, ylim=c(200,-5),
       xlab='median bounded by 25th and 75th percentiles',
       sync.colors=TRUE, alpha=0.5,
       par.settings=list(superpose.line=list(lwd=2, col=c('RoyalBlue', 'firebrick'))),
       panel=panel.depth_function, 
       prepanel=prepanel.depth_function,
       cf=a$contributing_fraction,
       scales=list(x=list(alternating=1))
)



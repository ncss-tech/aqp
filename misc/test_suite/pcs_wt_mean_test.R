library(aqp)

set.seed(101010)
d <- random_profile(1, n=6, exact=TRUE)

# setup property
d$prop <- d$p1

# setup slab boundaries
s.top <- 25
s.bottom <- 50

# use slab boundaries as segmenting vector
(a <- soil.slot(d, seg_vect=c(s.top,s.bottom))$p.mean)

# manually calculate wt-mean
dp <- c(28-25,35-28,44-35,50-44)
p <- c(6.473898,8.459331,22.038054,14.651045)
sum(dp*p) / sum(dp)

14.59543

# vs. output from soil.slot

14.59543

# the same!

# how about with soil.slot.multiple?
d$groups <- 1
(b <- soil.slot.multiple(d, g='groups', vars='prop', seg_vect=c(s.top,s.bottom))$p.mean)

# the same!

14.59543



##
## alternate approaches ?
## 

# locate horizons within a slab
hz.first <- which(diff(sign(s.top - d$top)) != 0)
hz.last <- which(diff(sign(s.bottom - d$top)) != 0)

d[hz.first:hz.last, ]


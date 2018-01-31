library(aqp)
library(profr)
library(plyr)
library(lattice)

# simulations
n <- 1000

# generate some fake soils data
d <- ldply(1:n, random_profile)
depths(d) <- id ~ top + bottom

# create several groups of different sizes
d$g0 <- rep(1:n, each=1) # 1 / group
d$g1 <- rep(1:(n/5), each=5) # 5 / group
d$g2 <- rep(1:(n/10), each=10) # 10 / group
d$g3 <- rep(1:(n/20), each=20) # 20 / group


# aggregate across varying group sizes
s <- system.time(slab(d, ~ p1 + p2 + p3))[3]
s0 <- system.time(slab(d, g0 ~ p1 + p2 + p3))[3]
s1 <- system.time(slab(d, g1 ~ p1 + p2 + p3))[3]
s2 <- system.time(slab(d, g2 ~ p1 + p2 + p3))[3]
s3 <- system.time(slab(d, g3 ~ p1 + p2 + p3))[3]

# combine results into data.frame
tt <- data.frame(profiles.per.group=factor(c(1, 5, 10, 20, 100)), time=c(s0, s1, s2, s3, s))

# viz results
xyplot(time ~ profiles.per.group, data=tt, type='b', pch=15, cex=2, col='black', main='1000 Profiles')



# profile slice method: pretty quick, scales linearly
system.time(s <- slice(d, 1:100 ~ p1 + p2 + p3))
p <- profr(s <- slice(d, 1:100 ~ p1 + p2 + p3))
plot(p, cex=0.5, minlabel=0.01)

# no groups: 3.2 seconds !
system.time(s <- slab(d, ~ p1 + p2 + p3))
p <- profr(s <- slab(d, ~ p1 + p2 + p3))
plot(p, cex=0.5, minlabel=0.01)

# minimal grouping: 10x slower ~ 45 seconds using idata.frame, 42 second without it
# ddply/ldply are the slowest components
# slice is only a small fraction of the total time (1.3 seconds out of 45+ seconds for slab)
# 3.5 seconds with aggregate
system.time(s <- slab(d, g3 ~ p1 + p2 + p3))
p <- profr(s <- slab(d, g3 ~ p1 + p2 + p3))
plot(p, cex=0.5, minlabel=0.01)


p.i <- ddply(p, .(f), .fun=summarise, total_time=sum(time))
p.i <- p.i[order(p.i$total_time, decreasing=TRUE), ]
p.i$prop <- p.i$total_time / p.i$total_time[1]
p.h <- head(p.i[-1, ], 10)
	
dotplot(f ~ total_time, data=p.h)

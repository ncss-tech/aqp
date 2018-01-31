# simple profiling of some functions
library(aqp)
library(plyr)
library(cluster)
library(profr)
library(lattice)
library(RColorBrewer)

# 1. profile_compare: simple case
d <- ldply(1:100, random_profile)
p <- profr(dd <- profile_compare(d, vars=c('p1','p2','p3','p4','p5'), max_d=50, k=0))
plot(p, cex=0.5, minlabel=0.01)


# ## slice() method
# d <- ldply(1:10, random_profile)
# depths(d) <- id ~ top + bottom
# p <- profr(s <- slice(d, 5 ~ p1 + p2 + p3, just.the.data=TRUE))
# plot(p, cex=0.5, minlabel=0.01)


p.i <- ddply(p, .(f), .fun=summarise, total_time=sum(time))
p.i <- p.i[order(p.i$total_time, decreasing=TRUE), ]
p.i$prop <- p.i$total_time / p.i$total_time[1]
p.h <- head(p.i[-1, ], 10)
	
dotplot(f ~ total_time, data=p.h)
dotplot(f ~ prop, data=p.h)







# 2. compare number of profiles with fixed parameters
s <- c(50, 75, 100, 250, 500, 750, 1000)
p <- list()
d <- list()

for(i in seq_along(s))
	d[[i]] <- ldply(1:s[i], random_profile)

for(i in seq_along(s))
	p[[as.character(s[i])]] <- profr(dd <- profile_compare(d[[i]], vars=c('p1','p2','p3','p4','p5'), max_d=50, k=0))

l <- ldply(p, .fun=function(i) {	
	p.i <- ddply(i, .(f), .fun=summarise, total_time=sum(time))
	p.i <- p.i[order(p.i$total_time, decreasing=TRUE), ]
	p.i$prop <- p.i$total_time / p.i$total_time[1]
	head(p.i[-1, ], 10)
	})

l$n.profiles <- as.numeric(l$.id)
l <- l[order(l$n.profiles), ]

xyplot(total_time ~ n.profiles | f, data=l, type='b', as.table=TRUE, scales=list(alternating=1))
xyplot(f ~ total_time | factor(n.profiles), data=l, type=c('p','g'), as.table=TRUE, scales=list(alternating=1))


xyplot(prop ~ n.profiles | f, data=l, type='b', as.table=TRUE, scales=list(alternating=1))

# ... t.default() becomes a hog @ n >= 500 profiles


# 3. compare number of depth slices: 100 profiles
s <- c(10, 25, 50, 100, 150)
p <- list()

for(i in seq_along(s))
	{
	print(paste(s[i], 'depth slices'))	
	p[[as.character(s[i])]] <- profr(dd <- profile_compare(d[[3]], vars=c('p1','p2','p3','p4','p5'), max_d=s[[i]], k=0))
	}
	
l <- ldply(p, .fun=function(i) {	
	p.i <- ddply(i, .(f), .fun=summarise, total_time=sum(time))
	p.i <- p.i[order(p.i$total_time, decreasing=TRUE), ]
	p.i$prop <- p.i$total_time / p.i$total_time[1]
	head(p.i[-1, ], 10)
	})

l$n.slices <- as.numeric(l$.id)
l <- l[order(l$n.slices), ]

xyplot(total_time ~ n.slices | f, data=l, type='b', as.table=TRUE, scales=list(alternating=1))
plot.2 <- xyplot(prop ~ n.slices | f, data=l, type='b', as.table=TRUE, scales=list(alternating=1))


# 4. compare number of variables: 100 profiles, 100 slices
s <- c(2,3,4,5)
p <- list()

for(i in seq_along(s))
	{
	print(paste(s[i], 'variables'))	
	p[[as.character(s[i])]] <- profr(dd <- profile_compare(d[[3]], vars=c('p1','p2','p3','p4','p5')[seq(1, s[i])], max_d=100, k=0))
	}
	
l <- ldply(p, .fun=function(i) {	
	p.i <- ddply(i, .(f), .fun=summarise, total_time=sum(time))
	p.i <- p.i[order(p.i$total_time, decreasing=TRUE), ]
	p.i$prop <- p.i$total_time / p.i$total_time[1]
	head(p.i[-1, ], 10)
	})

l$n.vars <- as.numeric(l$.id)
l <- l[order(l$n.vars), ]

xyplot(total_time ~ n.vars | f, data=l, type='b', as.table=TRUE, scales=list(alternating=1))
plot.3 <- xyplot(prop ~ n.vars | f, data=l, type='b', as.table=TRUE, scales=list(alternating=1))




# compare number of variables + number of profiles
s <- expand.grid(n=c(50, 100, 250, 500, 750), v=c(2,4,6,8,10))
d <- list()

for(i in seq_along(s$n))
	d[[i]] <- ldply(1:s$n[i], function(i) random_profile(i, n_prop=10))

p <- ddply(s, .(n, v), .fun=function(j) {
	idx <- match(j$n, s$n)
	profr(dd <- profile_compare(d[[idx]], vars=c('p1','p2','p3','p4','p5','p6','p7','p8','p9','p10')[seq(1, j$v)], max_d=100, k=0))
	}, .progress='text')

p.1 <- ddply(p, .(n, v, f), .fun=summarise, total_time=sum(time))
p.2 <- ddply(p.1, .(n, v), .fun=function(i) {
	i <- i[order(i$total_time, decreasing=TRUE), ]
	
	# find idx for overhead stuff related to ddply() 
	f.idx <- match('profile_compare', i$f)
	prof.idx <- match('profr', i$f)
	
	# keep the run time for our function
	i.f <- i[f.idx, ]
	# remove overhead times
	i <- i[-(1:prof.idx), ]
	
	# compute proportions, and keep top 10
	i$prop <- i$total_time / i.f$total_time
	head(i, 10)
	})

p.2 <- subset(p.2, subset=n > 10)
p.2$f <- factor(p.2$f)


# vis 2D profiling
cols <- rev(brewer.pal(n=11, name='Spectral'))
cols.palette <- colorRampPalette(cols)
ncuts <- 40

plot.4 <- levelplot(prop ~ factor(n)*factor(v) | f, data=p.2, as.table=TRUE, 
scales=list(x=list(cex=0.66, rot=45)), col.regions=cols.palette(ncuts), cuts=ncuts-1, 
strip=strip.custom(bg=grey(0.8)), xlab='No. Profiles', ylab='No. Variables Used', 
par.strip.text=list(cex=0.66))

plot.5 <- levelplot(total_time ~ factor(n)*factor(v) | f, data=p.2, as.table=TRUE, 
scales=list(x=list(cex=0.66, rot=45)), col.regions=cols.palette(ncuts), cuts=ncuts-1, 
strip=strip.custom(bg=grey(0.8)), xlab='No. Profiles', ylab='No. Variables Used', 
par.strip.text=list(cex=0.66))



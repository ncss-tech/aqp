# ideas: 
# spatial.median (ICSNP)
# multivariate quantiles (mvtnorm)
# http://stats.stackexchange.com/questions/23054/how-to-find-quantiles-for-multivariate-data-using-r
# it should make sense to do the quasi-multivatiate quantile estmation in additive-log-ratio space, or in the cartesian space used by soil.texture
#
# "approximation" of the 2D quantile function only uses clay+silt
# this is likely wrong, and maybe we can use ideas from aplpack::bagplot() for a more robust version


## TODO:
## 1. allow iteration over groups for plotting multiple horizons
## 2. consider embedding into lattice panels
## 3. consider integration with soiltexture package

# compute and plot "low"--"representative value"--"high" soil textures based on:
# ssc: data.frame/matrix of [sand, silt, clay]
# p: requested percentiles
# method: which method, approximate or based on real observations
# n.closest: n-closest (observed) textures to use
# delta: step-size used to form low-high region via 'approx' method
texture.triangle.low.rv.high <- function(ssc, p=c(0.05, 0.5, 0.95), method=c('approx', 'closest'), n.closest=3, delta=1, pop.rv.col='red', range.col='RoyalBlue', range.alpha=75) {
	
	# sanity checks
	if(delta < 1)
		stop('delta values smaller than 1 may result in memory overflow', call.=FALSE)
	
	# colors
	range.col <- rgb(t(col2rgb(range.col)), maxColorValue=255, alpha=range.alpha)
	
	# compute _univariate_ low - rv - high by variable
	ssc.stats <- apply(ssc, 2, quantile, probs=p)
	
  ## TODO: does this make more sense?
#   library(compositions)
# 	a <- acomp(ssc)
# 	a.trans <- alr(a)
# 	a.q <- apply(a.trans, 2, quantile, p=c(0.05, 0.5, 0.95))
# 	ssc.stats <- matrix(alrInv(a.q, orig=a.trans), ncol=3) * 100
  
  sand.stats <- sort(ssc.stats[, 1])
	silt.stats <- sort(ssc.stats[, 2])
	clay.stats <- sort(ssc.stats[, 3])
  
	# basic plot of all the data
	soil.texture(ssc, show.names=FALSE, cex=0.5, col.symbols='black', axis.labels=c('Sand', 'Silt', 'Clay'), show.grid=TRUE)
	
	if(method[1] == 'approx') {
		# make a grid of sand, silt, clay values within the low--high range of the data
		sand.seq <- round(seq(from=sand.stats[1], to=sand.stats[3], by=delta))
		silt.seq <- round(seq(from=silt.stats[1], to=silt.stats[3], by=delta))
		clay.seq <- round(seq(from=clay.stats[1], to=clay.stats[3], by=delta))
		g <- expand.grid(sand=sand.seq, silt=silt.seq, clay=clay.seq)
		
		# subset to only include those sand, silt, clay values that sum to 100%
		real.textures <- which(apply(g, 1, sum) - 100 == 0)
		g <- g[real.textures, ]
		
		# plot low and high values with no symbol, so that we can access the {x,y} screen coordinates
		tp.low.high <- triax.points(g, col.symbols='black', pch=NA)
	}
	
	# note sure how useful this approach is
	# get N-closest observations
	else {
		# determine the absolute distance between {low, rv, high} and observed textures
		sand.test <- abs(outer(ssc.stats[, 1], ssc$sand, FUN='-'))
		silt.test <- abs(outer(ssc.stats[, 2], ssc$silt, FUN='-'))
		clay.test <- abs(outer(ssc.stats[, 3], ssc$clay, FUN='-'))
		
		# locate n closest (observed) textures to {low, rv, high} by index
		closest.idx <- 1:n.closest
		sand.bounds <- apply(sand.test, 1, function(i) order(i)[closest.idx])
		silt.bounds <- apply(silt.test, 1, function(i) order(i)[closest.idx])
		clay.bounds <- apply(clay.test, 1, function(i) order(i)[closest.idx])
		
		# extract closest observed textures to {low, rv, high}
		if(n.closest > 1) {
			sand.bounds.textures <- apply(sand.bounds, 2, function(i) ssc[i, ])
			silt.bounds.textures <- apply(silt.bounds, 2, function(i) ssc[i, ])
			clay.bounds.textures <- apply(clay.bounds, 2, function(i) ssc[i, ])
		}
		else {
			stop('not yet implemented!', call.=FALSE)
		}
		
		# combine low and high textures:
		ssc.low.high <- do.call('rbind', c(sand.bounds.textures[c(1, 3)], silt.bounds.textures[c(1, 3)], clay.bounds.textures[c(1, 3)]))
		
		# plot low and high values with no symbol, so that we can access the {x,y} screen coordinates
		tp.low.high <- triax.points(ssc.low.high, col.symbols='black', pch=NA)
	}
	
	
	# add low--high polygon
	poly.order <- chull(tp.low.high$x, tp.low.high$y)
	polygon(tp.low.high$x[poly.order], tp.low.high$y[poly.order], col=range.col)
	
	# plot population RV
	triax.points(matrix(ssc.stats[2, ], nrow=1), bg.symbols=pop.rv.col, pch=22)
	
	# add legend
	low.high.range.text <- paste('Low-High Range (', paste(p[c(1,3)], collapse='-'), ')', sep='')
	legend('topleft', legend=c('Population RV', low.high.range.text), pch=c(22, 22), pt.bg=c(pop.rv.col, range.col), col=c('black', 'black'), bty='n', cex=0.75)
}

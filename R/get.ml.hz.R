# generate a data.frame of ML horizonation
# using the output from slab() and a vector of horizon names
get.ml.hz <- function(x, hz.names) {
	# get index to max probability, 
	# but only when there is at least one value > 0
	f.ML.hz <- function(i) {
		if(any(i > 0))
			which.max(i)
		else
			NA
	}
	
	# get most probable horizon, by slice
	x$name <- hz.names[apply(x[, hz.names], 1, f.ML.hz)]
	
	# extract ML hz sequences
	x.rle <- rle(as.vector(na.omit(x$name)))
	x.hz.bounds <- cumsum(x.rle$lengths) - 1
	
	# composite into a data.frame
	# note: we always start from 0
	x.ml <- data.frame(hz=x.rle$value, top=c(0, x.hz.bounds[-length(x.hz.bounds)]), bottom=x.hz.bounds, stringsAsFactors=FALSE)
	
	# in cases where probability depth-functions cross more than once,
	# it is necessary to account for overlaps
	x.ml <- ddply(x.ml, 'hz', summarise, top=min(top), bottom=max(bottom))
	
	# re-order just in case
	x.ml <- x.ml[order(x.ml$top), ]
	
	# integrate probability density function over ML bounds
	x.ml$confidence <- NA
	for(i in seq_along(x.ml$hz)) {
		slice.seq <- seq(from=x.ml$top[i], to=x.ml$bottom[i])
		x.i <- x[slice.seq, x.ml$hz[i]]
		hz.int.prob.pct <- round( (sum(x.i) / length(slice.seq)) * 100)
		x.ml$confidence[i] <- hz.int.prob.pct
	}
	
	return(x.ml)
}

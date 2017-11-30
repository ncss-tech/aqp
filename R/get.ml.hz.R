
## TODO: check for rowSums != 1
##       this will cause shannon H to fail

## TODO: conversion from original <-> safe names is clunky

# generate a data.frame of ML horizonation
# using the output from slab() and a vector of horizon names
get.ml.hz <- function(x, o.names=attr(x, which='original.levels')) {
  
  # trick R CMD check
  H = top = bottom = NULL
  
  # sanity check
  if(missing(o.names) & is.null(attr(x, which='original.levels')))
    stop('x not derived from slab() or o.names is missing')
  
  ## this should accomodate DF-safe names returned by slab()
  # make DF-safe names for hz that violate DF constraints
  safe.names <- make.names(o.names)
  
  # LUT for names
  names.LUT <- data.frame(original=o.names, safe=safe.names, stringsAsFactors = FALSE)
  
	# get index to max probability, 
	# but only when there is at least one value > 0 and all are not NA
	.f.ML.hz <- function(i) {
		if(any(i > 0) & !all(is.na(i)))
			which.max(i)
		else
			NA
	}
	
	
	# get most probable, original,  horizon designation by slice
	x$name <- safe.names[apply(x[, safe.names], 1, .f.ML.hz)]
	
	# extract ML hz sequences
	x.rle <- rle(as.vector(na.omit(x$name)))
	x.hz.bounds <- cumsum(x.rle$lengths)
	
	# composite into a data.frame
	# note: we always start from 0
	x.ml <- data.frame(hz=x.rle$value, top=c(0, x.hz.bounds[-length(x.hz.bounds)]), bottom=x.hz.bounds, stringsAsFactors=FALSE)
	
	# in cases where probability depth-functions cross more than once,
	# it is necessary to account for overlaps
	x.ml <- ddply(x.ml, 'hz', summarise, top=min(top), bottom=max(bottom))
	
	# re-order using vector of original horizon names-- this will result in NAs if a named horizon was not the most likely
	x.ml <- x.ml[match(safe.names, x.ml$hz), ]
	x.ml <- na.omit(x.ml)
	
	# integrate probability density function over ML bounds
	x.ml$confidence <- NA
	for(i in seq_along(x.ml$hz)) {
		slice.seq <- seq(from=x.ml$top[i], to=x.ml$bottom[i])
		x.i <- x[slice.seq, x.ml$hz[i]]
		hz.int.prob.pct <- round( (sum(x.i) / length(slice.seq)) * 100)
		x.ml$confidence[i] <- hz.int.prob.pct
	}
	
  # compute a pseudo-brier score using ML hz as the "true" outcome
  # brier's multi-class score : http://en.wikipedia.org/wiki/Brier_score#Original_definition_by_Brier
	# filter NA: why would this happen?
	idx <- which(!is.na(x$name))
  x.bs <- ddply(x[idx, ], 'name', brierScore, classLabels=safe.names, actual='name')
  
  # shannon entropy, (log base 2)bits)
  x$H <- apply(x[, safe.names], 1, shannonEntropy, b=2)
  x.H <- ddply(x[idx, ], 'name', plyr::summarize, H=mean(H))
    
  # fix names for joining
  names(x.bs) <- c('hz', 'pseudo.brier')
  names(x.H) <- c('hz', 'mean.H')
  
  # join brier scores to ML hz table
  x.ml <- join(x.ml, x.bs, by='hz')
  
  # join shannon H to ML hz table
  x.ml <- join(x.ml, x.H, by='hz')
  
  # convert safe names -> original names
  x.ml$hz <- names.LUT$original[match(x.ml$hz, names.LUT$safe)]
  
	return(x.ml)
}

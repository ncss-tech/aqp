# convert a set of horizon depths and property into a continuous sequence
# returning a vector of standardized length
# suitable for slotting
## segment_size argument does nothing
unroll <- function(top, bottom, prop, max_depth, segment_size=NA, bottom_padding_value=NA)
	{
	
	# inverse RLE, to generate repeating sequence of property, n times
	p <- inverse.rle(list(lengths=bottom-top, values=prop))
	
	# total depth, in unit length
	p.len <- length(p)
	
	# number of NAs to prepend, in case our profile does not start at 0
	num.NA.prepend <- abs(0 - min(top))
	
	# number of NAs we need to append to match the deepest profile
	num.NA.append <- max_depth - (p.len + num.NA.prepend)
	
	# debug
	# print(paste(max_depth, num.NA.prepend, p.len, num.NA.append))
	
	# padd the result with NA: from the top down
	p.pad <- c(rep(NA, times=num.NA.prepend), p)
	
	# but only if the number of NA to append is positive
	if(sign(num.NA.append) == 1)
		p.pad <- c(p.pad, rep(bottom_padding_value, times=num.NA.append))
	
	# return vector, padded to max_depth
	return(as.vector(p.pad))
	}
# TODO : this function doesn't help when the upper/lower CI limits aren't defined
# revert to standard pre-panel function in that case
# used to pre-compute wider xlim range based on upper/lower values
prepanel.depth_function <- function(x, y, upper, lower, subscripts, groups=NULL, ...) {

# working with grouped data and paneled data
if(!missing(groups) & !missing(subscripts))
	{
	d <- data.frame(yhat=x, top=y, upper=upper[subscripts], lower=lower[subscripts], groups=groups[subscripts])
	
	# levels in the groups, for color matching
	ll <- levels(d$groups)
	}

# no grouping, add a fake group for compatiblity
if(missing(groups))
	{
	d <- data.frame(yhat=x, top=y, upper=upper[subscripts], lower=lower[subscripts], groups=factor(1))
	# levels in the groups, for color matching
	ll <- levels(d$groups)
	}

# compute better xlim based on range of confidence band 
the_range <- c(min(c(d$lower,d$yhat), na.rm=TRUE), max(c(d$upper, d$yhat), na.rm=TRUE))
return(list(xlim=the_range))
}

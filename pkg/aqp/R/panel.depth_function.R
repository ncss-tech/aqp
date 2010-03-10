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
the_range <- c(min(d$lower, na.rm=TRUE), max(d$upper, na.rm=TRUE))
return(list(xlim=the_range))
}


# note: confidence bands not defined when NA is present
panel.depth_function <- function(x, y, upper, lower, subscripts=NULL, groups=NULL, ...) {

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

# add grid
panel.grid(h=-1, v=-1, lty=3, col=1)

# add conf. intervals / aggregation uncertainty
by(d, d$groups, function(d_i) {
# cannot have NA in values that define polygon boundaries
d_i <- subset(d_i, subset=is.na(d_i$upper) == FALSE & is.na(d_i$lower) == FALSE)

# make conf.int polygon
panel.polygon(x=c(d_i$lower, rev(d_i$upper)), y=c(d_i$top, rev(d_i$top)), col=grey(0.7), border=NA, ...)
})

# add main lines
by(d, d$groups, function(d_i) {
# lookup color
m <- match(unique(d_i$group), ll)
# add line
panel.lines(d_i$yhat, d_i$top, lwd=trellis.par.get('superpose.line')$lwd, col=trellis.par.get('superpose.line')$col[m], lty=trellis.par.get('superpose.line')$lty[m])
})


}

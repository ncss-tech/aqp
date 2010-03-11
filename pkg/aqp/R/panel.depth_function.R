

# add a manual page for this
# note: confidence bands not defined when NA is present
panel.depth_function <- function(x, y, upper=NA, lower=NA, subscripts=NULL, groups=NULL, ...) {

# add grid
panel.grid(h=-1, v=-1, lty=3, col=1)


# TODO: add uncertainty viz.
# when the length of 'y' is > 'x', we are plotting a step function
if(length(y) > length(x))
	{
	# re-make a nice dataframe
	d <- data.frame(prop=x, bnd=y, upper=upper[subscripts], lower=lower[subscripts], groups=groups[subscripts])
	
	# add line segments that form step-function
	by(d, d$groups, make.segments, ...)	
	}

# normal plot -- not a step function
else
	{
		# if we have an upper and lower bound defined, plot them
	if(!missing(upper) & !missing(lower))
		{
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
		
		
		# add conf. intervals / aggregation uncertainty
		by(d, d$groups, function(d_i) 
			{
			# cannot have NA in values that define polygon boundaries
			d_i <- subset(d_i, subset=is.na(d_i$upper) == FALSE & is.na(d_i$lower) == FALSE)
			
			# make conf.int polygon
			panel.polygon(x=c(d_i$lower, rev(d_i$upper)), y=c(d_i$top, rev(d_i$top)), col=grey(0.7), border=NA, ...)
			})
		}
	# no upper, lower bounds
	else
		{
		d <- data.frame(yhat=x, top=y, groups=groups[subscripts])
		# levels in the groups, for color matching
		ll <- levels(d$groups)	
		}
	
	
	# add main lines
	by(d, d$groups, function(d_i) 
		{
		# lookup color
		m <- match(unique(d_i$group), ll)
		# add line
		panel.lines(d_i$yhat, d_i$top, lwd=trellis.par.get('superpose.line')$lwd, col=trellis.par.get('superpose.line')$col[m], lty=trellis.par.get('superpose.line')$lty[m])
		})
	
	}


}

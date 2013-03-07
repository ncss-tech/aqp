## TODO: large gaps in data result in some strange-looking polygons

## TODO: iterate over profile IDs instead of groups
# note: confidence bands not defined when NA is present
panel.depth_function <- function(x, y, id, upper=NA, lower=NA, subscripts=NULL, groups=NULL, sync.colors=FALSE, cf=NULL, ...) {
  
# add grid
panel.grid(h=-1, v=-1, lty=3, col=1)

# load current line style
superpose.line <- trellis.par.get("superpose.line")

# TODO: add uncertainty viz.
# when the length of 'y' is > 'x', we are plotting a step function
if(length(y) > length(x)) {
	if(missing(id))
		stop('must provide a profile id')
		
	message('plotting segments...')
	
	# re-make a nice dataframe, assuming that we have 'groups' defined
	if(!missing(groups))
		d <- data.frame(prop=x, bnd=y, upper=upper[subscripts], lower=lower[subscripts], groups=groups[subscripts], id=id[subscripts])
	
	# if 'groups' is missing, add a fake 'groups' column
	else
		d <- data.frame(prop=x, bnd=y, upper=upper[subscripts], lower=lower[subscripts], groups=factor(1), id=id[subscripts])
	
	# add line segments that form step-function
	## TODO: iterate over profile IDs instead of groups
	by(d, d$id, make.segments, ...)	
	}

# normal plot -- not a step function
else {
	
	# if we have an upper and lower bound defined, plot them
	if(!missing(upper) & !missing(lower)) {
		# working with grouped data and paneled data
		if(!missing(groups) & !missing(subscripts)) {
			d <- data.frame(yhat=x, top=y, upper=upper[subscripts], lower=lower[subscripts], groups=groups[subscripts])
			# levels in the groups, for color matching
			ll <- levels(d$groups)
			n_groups <- length(ll)
			}
		
		# no grouping, add a fake group for compatiblity
		if(missing(groups)) {
			d <- data.frame(yhat=x, top=y, upper=upper[subscripts], lower=lower[subscripts], groups=factor(1))
			# levels in the groups, for color matching
			ll <- levels(d$groups)
			n_groups <- length(ll)
			}
		
    # optionally sync region + main line colors
    if(sync.colors)
      region.col <- rep(superpose.line$col, length.out=n_groups)
    else
      region.col <- rep(grey(0.7), length.out=n_groups)
      
		# add conf. intervals / aggregation uncertainty
		by(d, d$groups, function(d_i) {
      # lookup color
  	  m <- match(unique(d_i$group), ll)
      
			# cannot have NA in values that define polygon boundaries
			d_i <- subset(d_i, subset=is.na(d_i$upper) == FALSE & is.na(d_i$lower) == FALSE)
			
			# make conf.int polygon
			panel.polygon(x=c(d_i$lower, rev(d_i$upper)), y=c(d_i$top, rev(d_i$top)), col=region.col[m], border=NA, ...)
			})
		}
	# no upper, lower bounds
	else {
		d <- data.frame(yhat=x, top=y, groups=groups[subscripts])
		# levels in the groups, for color matching
		ll <- levels(d$groups)	
		n_groups <- length(ll)
		}
	
	
	
  # setup style parameters for main lines 
  # repeat enough times for the current number of groups
  line.col <- rep(superpose.line$col, length.out=n_groups)
	line.lty <- rep(superpose.line$lty, length.out=n_groups)
	line.lwd <- rep(superpose.line$lwd, length.out=n_groups)
	
	# add main lines
	by(d, d$groups, function(d_i){
		# lookup color
		m <- match(unique(d_i$group), ll)
		
		
		# add line
		panel.lines(d_i$yhat, d_i$top, lwd=line.lwd[m], col=line.col[m], lty=line.lty[m])
		})
	}

	
	# TODO: might be interesting to plot group-wise CFs using same colors as lines / polygons
  # annotate with contributing fraction
  if(! is.null(cf)) {
    
  	# if plotting with grouped data, inform the user we are computing the mean CF / slice
  	if(!missing(groups))
  		warning('depth-wise mean contributing fraction values are printed', call.=FALSE)

  	# aggregate group-wise to get a single CF / depth-slice
  	cf.i.agg <- tapply(cf[subscripts], y, FUN=mean, na.rm=TRUE)
  	
    # make a function for linear interpolation of CF values based on depth
  	# note that we are using unique(y) as grouped data will repeat y n-group times
    cf.approx.fun <- approxfun(unique(y), cf.i.agg, method='linear')
    
    # generate annotated depths: 5 cm to 95th percentile of max depth
    y.q95 <- quantile(y, probs=c(0.95), na.rm=TRUE)
    a.seq <- seq(from=5, to=y.q95, by=20)
    
    # interpolate CF at annotated depths
    a.CF <- cf.approx.fun(a.seq)
    a.text <- paste(round(a.CF * 100), '%')
    
    # add to right-hand side of the panel
    unit <- gpar <- NULL
    grid.text(a.text, x=unit(0.99, 'npc'), y=unit(a.seq, 'native'), just='right', gp=gpar(font=3, cex=0.8))  
  }

}


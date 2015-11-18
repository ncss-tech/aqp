## TODO: only a single site-level attribute can be used for sorting
# order profiles by a site-level grouping label
groupedProfilePlot <- function(x, groups, group.name.offset=-5, group.name.cex=0.75, group.line.col='RoyalBlue', group.line.lwd=2, group.line.lty=2, break.style='line', arrow.offset=group.name.offset + 5, arrow.length=0.1, ...) {
  s <- site(x)
  new.order <- order(s[[groups]])
  
  # if our groups are already a factor, keep existing levels
  if(class(s[[groups]]) == 'factor')
    lab <- s[[groups]][new.order]
  else # not a factor, need to convert to factor, inherit default levels
    lab <- factor(s[[groups]][new.order])
  
  # test for NA
  NA.lab <- which(is.na(lab))
  
  # replace with missing label with '<missing>'
  # this requires conversion: factor -> character -> replace NA -> factor with new levels
  if(length(NA.lab) > 0) {
    message('NA in grouping label, filling with `<missing>`')
    o.levels <- levels(lab)
    lab <- as.character(lab)
    lab[NA.lab] <- '<missing>'
    lab <- factor(lab, levels=c(o.levels, '<missing>'))
  }
    
  # get just those levels that are in our data, preserving order of original levels
  unique.lab <- levels(lab)[which(levels(lab) %in% unique(lab))]
  group.lengths <- rle(as.numeric(lab))$lengths
  lab.positions <- (cumsum(group.lengths) - (group.lengths / 2)) + 0.5
  boundary.positions <-  cumsum(group.lengths)[-length(group.lengths)] + 0.5
  
  # setup plot with plot.SoilProfileCollection
  plot(x, plot.order=new.order, ...)
  
  # add group boundaries
  if(break.style == 'line')
    abline(v=boundary.positions, lty=group.line.lty, lwd=group.line.lwd, col=group.line.col)
  if(break.style == 'arrow')
    arrows(x0=c(0.5, boundary.positions), x1=c(boundary.positions, length(x)+0.5), y0=arrow.offset, code=3, length=arrow.length, lty=group.line.lty, lwd=group.line.lwd, col=group.line.col)
  if(break.style == 'both') {
    abline(v=boundary.positions, lty=group.line.lty, lwd=group.line.lwd, col=group.line.col)
    arrows(x0=c(0.5, boundary.positions), x1=c(boundary.positions, length(x)+0.5), y0=arrow.offset, code=3, length=arrow.length, lty=group.line.lty, lwd=group.line.lwd, col=group.line.col)
  }
  
  # annotate with group labels
  text(lab.positions, group.name.offset, unique.lab, cex=group.name.cex, adj=0.5, font=4)
}



## TODO: figure out intellegent recycling of arguments
## TODO: no mechanism for merged legends
plotMultipleSPC <- function(spc.list, group.labels, args=rep(list(NA), times=length(spc.list)), arrow.offset=2, bracket.base.depth=95, ...) {
  
  # compute group stats
  n.groups <- length(spc.list)
  spc.lengths <- sapply(spc.list, length)
  n.pedons <- sum(spc.lengths)
  group.starts <- c(1, 1 + cumsum(spc.lengths[-n.groups]))
  group.ends <- cumsum(spc.lengths)
  
  # get depths + offset to start / end profiles
  yy <- unlist(sapply(spc.list, function(i) profileApply(i, max)))
  tick.heights <- yy[c(group.starts, group.ends)] + arrow.offset
  
  # setup plot with first SPC in list
  do.call(plotSPC, c(x=spc.list[[1]], n=n.pedons, na.omit(args[[1]]), ...))
  
  # iterate over remaining SPC objs
  if(n.groups > 1) {
    for(i in 2:n.groups) {
      this.obj <- spc.list[[i]]
      this.args <- na.omit(args[[i]])
      do.call(plotSPC, c(x=this.obj, x.idx.offset=group.ends[i-1], add=TRUE, plot.depth.axis=FALSE, this.args))
    }
  }
    
  # annotate with group brackets
  profileGroupLabels(x0=group.starts, x1=group.ends, labels=group.labels, y0=bracket.base.depth, y1=tick.heights) 
}


# annotate profile plots with group labels, usually below
profileGroupLabels <- function(x0, x1, labels, y0=100, y1=98, label.offset=2, label.cex=0.75) {
  
  # sanity check: start / stop / label lengths should be equal
  if(! all.equal(length(x0), length(x1), length(labels)) )
    stop('start positions, stop positions, and number of labels must be equal', call. = FALSE)
  
  # pre-compute some elements
  n.groups <- length(x0)
  label.centers <- (x0 + x1) / 2
  
  # add group base lines
  segments(x0=x0, x1=x1, y0=y0, y1=y0)
  # add arrows to first / last group members
  arrows(x0=c(x0, x1), x1=c(x0, x1), y0=c(y0, y0), y1=y1, length=0.1)

  # annotate with group names
  text(x=label.centers, y=y0 + label.offset, labels=labels, cex=label.cex)
}


## TODO: still not completely generalized
# annotate elements from @diagnostic with brackets 
# mostly a helper function for addBracket()
addDiagnosticBracket <- function(s, kind, id=idname(s), top='featdept', bottom='featdepb', ...) {
	
  # get plotting details from aqp environment
  lsp <- get('last_spc_plot', envir=aqp.env)
    
  ## TODO: integrate these
  y.offset <- lsp$y.offset
  scaling.factor <- lsp$scaling.factor
  plot.order <- lsp$plot.order
  
  # extract diagnostic horizon information
  d <- diagnostic_hz(s)
  d <- d[which(d$diag_kind == kind), ]
  
  # generate index linking our top/bottom depths with original ordering
  key <- match(d[[id]], profile_id(s))
  
  # add backets
  addBracket(top=d[[top]], bottom=d[[bottom]], idx=key, ...)
}

## TODO: add proper documentation
## NOTE: this function is vectorized
# internal function for plotting a bracket (usually defines a diagnostic feature or similar)
# idx: (optional) integer index to profile
# top: top depth
# bottom: bottom depth
# tick.length: bracket tick length
# offset: left-hand offset from profile center
addBracket <- function(top, bottom=NULL, idx=NULL, label=NULL, label.cex=0.75, tick.length=0.05, arrow.length=0.05, offset=-0.3, missing.bottom.depth=25, ...) {
  
  # get plotting details from aqp environment
  lsp <- get('last_spc_plot', envir=aqp.env)
  
  # get number of brackets ~ number bracket top boundaries
  n <- length(top)
  
  # if missing an specific index, assume plotting order
  if(is.null(idx))
    plot.order <- lsp$plot.order
  else
    plot.order <- idx
  
  ## TODO: integrate these
  y.offset <- lsp$y.offset
  scaling.factor <- lsp$scaling.factor
  w <- lsp$width
  
	# normal case: both top and bottom defined
	if(!missing(top) & !missing(bottom)) {
    # x-positions
    x.1 <- 1:n + offset
    x.2 <- x.1 + tick.length
		# top tick
		segments(x.1, top[plot.order], x.2, top[plot.order], lend=2, ...)
		# bottom tick
		segments(x.1, bottom[plot.order], x.2, bottom[plot.order], lend=2, ...)
		# vertical bar
		segments(x.1, top[plot.order], x.1, bottom[plot.order], lend=2, ...)
	}
	
	# missing bottom: replace bottom tick with arrow head
	if(!missing(top) & missing(bottom)) {
	  # x-positions
	  x.1 <- 1:n + offset
	  x.2 <- x.1 + tick.length
		# top tick
		segments(x.1, top[plot.order], x.2, top[plot.order], lend=2, ...)
		# vertical bar is now an arrow
		arrows(x.1, top[plot.order], x.1, top[plot.order] + missing.bottom.depth, length=arrow.length, lend=2, ...)
	}
	
  # optionally plot label
  if(!missing(top) & !missing(label)){
    text(x.1 - 0.05, (top[plot.order] + bottom[plot.order])/2, label, srt=90, cex=label.cex, pos=3)
  }
  
}



# simple function to convert horizon boundary distinctness codes into vertical (+/-) offsets
hzDistinctnessCodeToOffset <- function(x, codes=c('A','C','G','D'), offset=c(0.5, 1.5, 5, 10)) {	
	x <- as.character(x)
	x.code <- match(x, codes)
	x.offset <- offset[x.code]
	x.offset <- ifelse(is.na(x.offset), 0, x.offset)
	return(x.offset)
}





# TODO: behavior not defined for horizons with an indefinate lower boundary
# TODO: save important elements of geometry from last plot to aqp.env
# TODO: move some of the processing outside of the main loop: column names, etc.

## basic function
plotSPC <- function(x, color='soil_color', width=0.2, name=NULL, label=idname(x), alt.label=NULL, alt.label.col='black', cex.names=0.5, cex.depth.axis=cex.names, cex.id=cex.names+(0.2*cex.names), print.id=TRUE, id.style='auto', plot.order=1:length(x), add=FALSE, scaling.factor=1, y.offset=0, x.idx.offset=0, n=length(x), max.depth=max(x), n.depth.ticks=5, shrink=FALSE, shrink.cutoff=3, abbr=FALSE, abbr.cutoff=5, divide.hz=TRUE, hz.distinctness.offset=NULL, hz.distinctness.offset.col='black', hz.distinctness.offset.lty=2, axis.line.offset=-2.5, plot.depth.axis=TRUE, density=NULL, col.label=color, col.palette = rev(brewer.pal(10, 'Spectral')), lwd=1, lty=1, default.color=grey(0.95), ...) {
  
  # save arguments to aqp env
  lsp <- list('width'=width, 'plot.order'=plot.order, 'y.offset'=y.offset, 'scaling.factor'=scaling.factor)
  assign('last_spc_plot', lsp, envir=aqp.env)
  
  # get horizons
  h <- horizons(x)
  
  # get column names from horizon dataframe
  nm <- names(h)
  
  # if the user has not specified a column containing horizon designations,
  # attempt to guess
  if(missing(name)) {
    possible.name <- nm[grep('name', nm, ignore.case=TRUE)]
    # use the first valid guess
    if(length(possible.name) > 0) {
      possible.name <- possible.name[1]
      name <- possible.name
      message(paste('guessing horizon designations are stored in `', name, '`', sep=''))
    }
    else {
      message('unable to guess column containing horizon designations')
      name <- NA # set column name to NA, details handled farther down in the function
    }
  }
  
  # setup horizon colors:
  
  # 1. numeric vector, rescale and apply color ramp
  if(is.numeric(h[[color]])) {
    cr <- colorRamp(col.palette)
    # note that this may contain NAs
    c.rgb <- cr(scales::rescale(h[[color]]))
    cc <- which(complete.cases(c.rgb))
    h$.color <- NA
    # convert non-NA values into colors
    h$.color[cc] <- rgb(c.rgb[cc, ], maxColorValue=255)
    # generate range / colors for legend
    pretty.vals <- pretty(h[[color]])
    color.legend.data <- list(legend=pretty.vals, col=rgb(cr(scales::rescale(pretty.vals)), maxColorValue=255))
  }
  # 2. character vector, assume these are valid colors
  if(is.character(h[[color]])) {
    h$.color <- h[[color]]
  }
  
  # if the color column doesn't exist, fill with NA
  if(is.null(h[[color]]))
    h[[".color"]] <- NA
  
  # fill missing colors with a reasonable default
  h$.color <- ifelse(is.na(h$.color), default.color, h$.color)
  
  # get top/bottom column names
  IDcol <- idname(x)
  hzDepthCols <- horizonDepths(x)
  tcol <- hzDepthCols[1]
  bcol <- hzDepthCols[2]
  
  # get profile IDs
  pIDs <- profile_id(x)
  
  # get profile labels from @site
  pLabels <- site(x)[[label]]
  
  # if profile style is auto, determin style based on font metrics
  if(id.style == 'auto') {
  	sum.ID.str.width <- sum(sapply(pLabels, strwidth, units='inches', cex=cex.id, font=2))
  	plot.width <- par('pin')[1]
  	ID.width.ratio <- sum.ID.str.width  / plot.width
#   	print(ID.width.ratio)
  	
  	if(ID.width.ratio > 0.7)
  		id.style <- 'side'
  	else
  		id.style <- 'top'
  	}
  
  
  ## TODO: extra_y_space should be allocated dynamically, as a function of number of profiles
  # fudge factors
  extra_x_space <- 2
  extra_y_space <- 15 # abnout right for n in {1,25}
  
  # pre-compute nice range for depth axis, also used for plot init
  depth_axis_intervals <- pretty(seq(from=0, to=max.depth, by=1), n=n.depth.ticks)
  
  # init plotting region, unless we are appending to an existing plot
  # note that we are using some fudge-factors to get the plotting region just right
  if(!add) {
    # par(mar=c(0.5,1,0,1)) # is it wise to adjust the plotting area?
	  plot(0, 0, type='n', xlim=c(1-(extra_x_space/5), n+(extra_x_space)), 
	       ylim=c(max(depth_axis_intervals), -extra_y_space), 
	       axes=FALSE, xlab='', ylab='')
	}
  
  
  # add horizons in specified order	
  for(i in 1:n) {
	  # convert linear sequence into plotting order
	  profile_i <- plot.order[i]
	  
	  # extract the current profile's horizon data
    this_profile_label <- pLabels[profile_i]
	  this_profile_id <- pIDs[profile_i]
	  this_profile_data <- h[h[IDcol] == this_profile_id, ]
	  
    # extract column names
    cn <- names(this_profile_data)
    
    # extract / generate horizon color
    # note: the ".color" horizon attribute is auto-generated above
    # missing and NA colors have already been dealt with above
    this_profile_colors <- this_profile_data$.color
    
	  # extract / generate horizon fill density
	  if(! missing(density)) {
	  	# if a single number was given, then recylce it over all horizons
	  	if(is.numeric(density))
	  		this_profile_density <- density
	  	# otherwise we have a column name
	  	else {
	  		m <- match(density, cn)
	  		if(! is.na(m))
		  		this_profile_density <- this_profile_data[[m]]
		  	else # user-defined column is missing
			  	this_profile_density <- NULL
	  	}
	  }
	  else # no user-defined density column
	  	this_profile_density <- NULL
	  
    # extract / generate horizon name
    m <- match(name, cn)
    if(! is.na(m))
      this_profile_names <- this_profile_data[[m]]
      # otherwise use an empty string
    else
      this_profile_names <- ''
    
	  
	  # generate rectangle geometry
    
    # get vector of profile indices
    x0 <- x.idx.offset + i
    
	  # get vectors of horizon boundaries, and scale
	  y0 <- (this_profile_data[, bcol] * scaling.factor) + y.offset
	  y1 <- (this_profile_data[, tcol] * scaling.factor) + y.offset
	
	
	##
	## TODO: use horizon boundary type and topography to modify figures
	##
	## i.e. clear-wavy = dashed lines at an angle, based on red book
	  
	# create horizons + colors
    # default are filled rectangles
    if(divide.hz) {
	    rect(x0 - width, y0, x0 + width, y1, col=this_profile_colors, border=NULL, density=this_profile_density, lwd=lwd, lty=lty)
	 
	 # optionally add horizon boundary distinctiveness
	 if(! is.null(hz.distinctness.offset)) {
	 	hz.dist.offset <- this_profile_data[, hz.distinctness.offset]
	 	segments(x0 - width, y0 - hz.dist.offset, x0 + width, y0 - hz.dist.offset, col=hz.distinctness.offset.col, lty=hz.distinctness.offset.lty, lend=2)
		segments(x0 - width, y0 + hz.dist.offset, x0 + width, y0 + hz.dist.offset, col=hz.distinctness.offset.col, lty=hz.distinctness.offset.lty, lend=2)	
     }
	    
	 }
    
    # otherwise, we only draw the left, top, right borders, and then fill
    else {
      rect(x0 - width, y0, x0 + width, y1, col=this_profile_colors, border=NA, density=this_profile_density, lwd=lwd, lty=lty)
      segments(x0 - width, y0, x0 - width, y1, lwd=lwd, lty=lty, lend=2) # left-hand side
      segments(x0 + width, y0, x0 + width, y1, lwd=lwd, lty=lty, lend=2) # right-rand side
      segments(x0 - width, min(y1), x0 + width, min(y1), lwd=lwd, lty=lty, lend=2) # profile top
      segments(x0 - width, max(y0), x0 + width, max(y0), lwd=lwd, lty=lty, lend=2) # profile bottom
    }
      
    
	  # annotate with names
	  # first get the horizon mid-point
	  mid <- ( y1 + y0 )/2
	  
	  # optionally shrink the size of names if they are longer than a given thresh
	  if(shrink) {
		  names.to.shrink <- which(nchar(this_profile_names) > shrink.cutoff)
		  cex.names.shrunk <- rep(cex.names, length(this_profile_data[, tcol]))
		  cex.names.shrunk[names.to.shrink] <- cex.names.shrunk[names.to.shrink] * 0.8
		  text(x0 + width, mid, this_profile_names, pos=4, offset=0.1, cex=cex.names.shrunk)
		  }
	  # standard printing of names, all at the same size
	  else
		  text(x0 + width, mid, this_profile_names, pos=4, offset=0.1, cex=cex.names)		
	  
	  # add the profile ID
	  if(print.id) {
			# optionally abbreviate
			if(abbr)
		  	id.text <- abbreviate(as.character(this_profile_label), abbr.cutoff)
	
			# no abbreviations of th ID
			else
	  		id.text <- as.character(this_profile_label)
		
			# add the text: according to style
			if(id.style == 'top')
				text(x0, y.offset, id.text, pos=3, font=2, cex=cex.id)
	
			if(id.style == 'side')
				text(x0 - (width+0.025), y.offset, id.text, adj=c(1, -width), font=2, cex=cex.id, srt=90)
			}
	  }
  
  # depth axis:
  depth_axis_tick_locations <- (depth_axis_intervals * scaling.factor) + y.offset
  depth_axis_labels <- paste(depth_axis_intervals, depth_units(x))
  if(plot.depth.axis)
  axis(side=4, line=axis.line.offset, las=2, at=depth_axis_tick_locations, labels=depth_axis_labels, cex.axis=cex.depth.axis)
  
  # plot alternate labels
  if(!missing(alt.label)) {
  	al <- site(x)[[alt.label]]
  	al <- al[plot.order]
  	text(1:length(x), y.offset+3, al, srt=90, adj=c(1, 0.5), font=2, cex=cex.id * 1.5, col=alt.label.col)
  }
  
  ## experimental color legend
  if(exists('color.legend.data')) {
    # If no title given, set col.label is set to color
    mtext(side=3, text=col.label, font=2, line=1.6)
    legend('bottom', legend=color.legend.data$legend, col=color.legend.data$col, bty='n', pch=15, horiz=TRUE, xpd=TRUE, inset=c(0, 0.99))
  }
  }



# method dispatch
setMethod("plot", signature("SoilProfileCollection"), definition=plotSPC)



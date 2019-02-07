
# split legend into two rows, and create indices
# any more classes than that and things become impossible to read
# n: total number of classes
.splitLegend <- function(n) {
  
  #  make enough room for even division of odd numbers
  n.per.row <- ceiling(n / 2)
  
  # make indices for first row
  row.1.idx <- seq(from=1, to=n.per.row)
  row.2.idx <- seq(from=n.per.row + 1, to=n)
  
  res <- list(
    row.1=row.1.idx, 
    row.2=row.2.idx
  )
  
  return(res)
}



# simple function to convert horizon boundary distinctness codes into vertical (+/-) offsets
# based on "red book" version 3.0
hzDistinctnessCodeToOffset <- function(x, codes=c('A','C','G','D'), offset=c(0.5, 1.5, 5, 10)) {	
	x <- as.character(x)
	x.code <- match(x, codes)
	x.offset <- offset[x.code]
	x.offset <- ifelse(is.na(x.offset), 0, x.offset)
	return(x.offset)
}



# Function testing the validity of a colour expressed as a character string
# Uses col2rgb() to test the validity
# Adapted from: 
# https://stackoverflow.com/questions/13289009/check-if-character-string-is-a-valid-color-representation
#s
.isColorValid <- function(x) {
  sapply(x, function(i) {
    tryCatch(
      is.matrix(col2rgb(i)), 
      error = function(e) { FALSE }
    )
  })
}

# TODO: behavior not defined for horizons with an indefinate lower boundary
# TODO: move some of the processing outside of the main loop: column names, etc.

## basic function
plotSPC <- function(x, color='soil_color', width=0.2, name=NULL, label=idname(x), alt.label=NULL, alt.label.col='black', cex.names=0.5, cex.depth.axis=cex.names, cex.id=cex.names+(0.2*cex.names), print.id=TRUE, id.style='auto', plot.order=1:length(x), add=FALSE, scaling.factor=1, y.offset=0, x.idx.offset=0, n=length(x), max.depth=ifelse(is.infinite(max(x)), 200, max(x)), n.depth.ticks=5, shrink=FALSE, shrink.cutoff=3, abbr=FALSE, abbr.cutoff=5, divide.hz=TRUE, hz.distinctness.offset=NULL, hz.distinctness.offset.col='black', hz.distinctness.offset.lty=2, axis.line.offset=-2.5, plot.depth.axis=TRUE, density=NULL, col.label=color, col.palette = rev(brewer.pal(10, 'Spectral')), col.legend.cex=1, n.legend=8, lwd=1, lty=1, default.color=grey(0.95), ...) {
  
  ## fudge factors
  # should be adjusted dynamically https://github.com/ncss-tech/aqp/issues/62
  
  # padding along x-axis, prevents crowding
  # dynamic adjustment must also taking into account figure size
  # roughly 10% of length(x)
  extra_x_space <- length(x) * 0.1
  
  # add a little extra x-space when n < 5
  if(length(x) < 5)
    extra_x_space <- extra_x_space + 0.25

  # padding above profiles, ~ 15 is about right for n in {1,25} and max depth near 150cm
  # a sketch of shalllow profiles could benefit from ~ 5
  if(max.depth <=50)
    extra_y_space <- 5
  if(max.depth > 50 & max.depth <= 100)
    extra_y_space <- 10
  if(max.depth > 100)
    extra_y_space <- 15
  
  # get profile IDs
  pIDs <- profile_id(x)
  
  # save arguments to aqp env
  lsp <- list('width'=width, 
              'plot.order'=plot.order, 
              'pIDs'=pIDs[plot.order],
              'idname'=idname(x),
              'y.offset'=y.offset, 
              'scaling.factor'=scaling.factor, 
              'max.depth'=max.depth, 
              'n'=n,
              'extra_x_space'=extra_x_space,
              'extra_y_space'=extra_y_space)
  
  assign('last_spc_plot', lsp, envir=aqp.env, )
  
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
    pretty.vals <- pretty(h[[color]], n = n.legend)
    # truncate to 3 signif vals and convert to character for correct interpretation of floating point values
    leg.pretty.vals <- as.character(signif(pretty.vals, 3))
    # put into a list for later
    color.legend.data <- list(legend=leg.pretty.vals, col=rgb(cr(scales::rescale(pretty.vals)), maxColorValue=255))
  }
  
  # 2. vector of categorical data
  if(is.character(h[[color]]) | is.factor(h[[color]])) {
    # Testing if the data in the column are valid columns
    if( all(.isColorValid(na.omit(h[[color]]))) ) {
      # If this is true this is a column of valid colors
      h$.color <- h[[color]]
    } else {
      # Otherwise that means this is or can be converted into a factor
      if(!is.factor(h[[color]]))
        h[[color]] <- factor(h[[color]])
      
      # get color mapping levels after dropping missing levels
      h[[color]] <- droplevels(h[[color]])
      color.levels <- levels(h[[color]])
      
      # make a color mapping function
      color.mapper <- scales::col_factor(
        palette = colorRampPalette(col.palette)(length(color.levels)),
        domain = color.levels,
        na.color = default.color,
        ordered = TRUE
      )
      
      # apply color mapping
      h$.color <- color.mapper(h[[color]])
      
      # generate colors and labels for legend
      pretty.vals <- color.levels
      color.legend.data <- list(legend = pretty.vals, col = color.mapper(pretty.vals))
      
      # interpret n.legend as max(items) / row
      n.leg.classes <- length(pretty.vals)
      
      # create more room via multiple calls to legend
      if(n.legend < n.leg.classes) {
        
        # make indices to two rows of legends
        # safely accounts for even / odd n.leg.classes
        leg.row.indices <- .splitLegend(n.leg.classes)
        
        # compute max space required for legend items
        # this will ensure that columns line-up
        leg.text.width <- (max(strwidth(pretty.vals, cex = col.legend.cex)))
        
        # set flag for later
        multi.row.legend <- TRUE
      }
      
      
    }
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
  
  # get profile labels from @site
  pLabels <- site(x)[[label]]
  
  # if profile style is auto, determine style based on font metrics
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
  
  
  # pre-compute nice range for depth axis, also used for plot init
  depth_axis_intervals <- pretty(seq(from=0, to=max.depth, by=1), n=n.depth.ticks)
  
  # init plotting region, unless we are appending to an existing plot
  # note that we are using some fudge-factors to get the plotting region just right
  if(!add) {
    # margins are set outside of this function
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
	  # get the horizon mid-point
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
    axis(side=4, line=axis.line.offset, las=2, at=depth_axis_tick_locations, labels=depth_axis_labels, cex.axis=cex.depth.axis, col.axis=par('fg'))
  
  # plot alternate labels
  if(!missing(alt.label)) {
  	al <- site(x)[[alt.label]]
  	al <- al[plot.order]
  	text(1:length(x), y.offset+3, al, srt=90, adj=c(1, 0.5), font=2, cex=cex.id * 1.5, col=alt.label.col)
  }
  
  # add a legend for thematic profile sketch
  if(exists('color.legend.data')) {
    # if no title given, set col.label to name of column containing thematic information
    mtext(side=3, text=col.label, font=2, line=1.6)
    
    
    # possibly split legend across multiple rows
    if(exists('multi.row.legend')) {
      
      # row 1
      legend('bottom', inset=c(0, 0.99),
             legend=color.legend.data$legend[leg.row.indices$row.1], 
             col=color.legend.data$col[leg.row.indices$row.1], 
             text.width = leg.text.width,
             bty='n', pch=15, horiz=TRUE, xpd=TRUE, cex=col.legend.cex, x.intersp=1
             )
      
      # row 2
      legend('bottom', inset=c(0, 0.94),
             legend=color.legend.data$legend[leg.row.indices$row.2], 
             col=color.legend.data$col[leg.row.indices$row.2], 
             text.width = leg.text.width,
             bty='n', pch=15, horiz=TRUE, xpd=TRUE, cex=col.legend.cex, x.intersp=1
      )
      
    } else {
      # standard invocation
      legend('bottom', legend=color.legend.data$legend, col=color.legend.data$col, bty='n', pch=15, horiz=TRUE, xpd=TRUE, inset=c(0, 0.99), cex=col.legend.cex, x.intersp=1)
    }
    
    
  }
  }



# method dispatch
setMethod("plot", signature("SoilProfileCollection"), definition=plotSPC)



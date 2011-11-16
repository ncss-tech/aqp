# generate a soil profile figure, from a generic dataframe
# using top and bottom boundaries, annotating with name
# optionally color with vector that is the same length as number of horizons

# behavior not defined for horizons with an indefinate lower boundary

## basic function
plot.SoilProfileCollection <- function(x, color='soil_color', width=0.2, name='name', cex.names=0.5, cex.depth.axis=cex.names, cex.id=cex.names+(0.2*cex.names), print.id=TRUE, id.style='top', plot.order=1:length(x), add=FALSE, scaling.factor=1, y.offset=0, max.depth=max(x), n.depth.ticks=5, shrink=FALSE, shrink.cutoff=3, abbr=FALSE, abbr.cutoff=5, ...) {
  
  # get horizons
  h <- horizons(x)
  
  # get column names from horizon dataframe
  nm <- names(h)
  
  # get number of profiles
  n <- length(x)
  
  # get top/bottom column names
  IDcol <- idname(x)
  hzDepthCols <- horizonDepths(x)
  tcol <- hzDepthCols[1]
  bcol <- hzDepthCols[2]
  
  # get profile IDs
  pIDs <- profile_id(x)
	  
  # fudge factors
  extra_x_space <- 1
  extra_y_space <- 2
  
  # pre-compute nice range for depth axis, also used for plot init
  depth_axis_intervals <- pretty(seq(from=0, to=max.depth, by=1), n=n.depth.ticks)
  
  # init plotting region, unless we are appending to an existing plot
  if(!add)
	{
  par(mar=c(0.5,1,0,1))
	  plot(0, 0, type='n', xlim=c(1, n+extra_x_space), ylim=c(max(depth_axis_intervals), -4), axes=FALSE)
	}
  
  
  # add horizons in specified order	
  for(i in 1:n)
	  {
	  # convert linear sequence into plotting order
	  profile_i <- plot.order[i]
	  
	  # extract the current profile's horizon data
	  this_profile_id <- pIDs[profile_i]
	  this_profile_data <- h[h[IDcol] == this_profile_id, ]
	  
    ## TODO: allow color to be set via formula interface
    # extract / generate horizon color
    m <- match(color, names(this_profile_data))
    if(! is.na(m))
      this_profile_colors <- this_profile_data[[m]]
    else # no user-defined color column, or it is missing
      this_profile_colors <- 'white'
    
    # extract / generate horizon name
    m <- match(name, names(this_profile_data))
    if(! is.na(m))
      this_profile_names <- this_profile_data[[m]]
    else # no user-defined color column, or it is missing
      this_profile_names <- ''
    
	  # generate rectangle geometry
	  # get vectors of horizon boundaries, and scale
	  y0 <- (this_profile_data[, bcol] * scaling.factor) + y.offset
	  y1 <- (this_profile_data[, tcol] * scaling.factor) + y.offset
	  
	  # make rectangles (horizons)
	  rect(i-width, y0, i + width, y1, col=this_profile_colors)
  
	  # annotate with names
	  # first get the horizon mid-point
	  mid <- ( y1 + y0 )/2
	  
	  # optionally shrink the size of names if they are longer than a given thresh
	  if(shrink) {
		  names.to.shrink <- which(nchar(this_profile_names) > shrink.cutoff)
		  cex.names.shrunk <- rep(cex.names, length(this_profile_data[, tcol]))
		  cex.names.shrunk[names.to.shrink] <- cex.names.shrunk[names.to.shrink] * 0.8
		  text(i + width, mid, this_profile_names, pos=4, offset=0.1, cex=cex.names.shrunk)
		  }
	  # standard printing of names, all at the same size
	  else
		  text(i + width, mid, this_profile_names, pos=4, offset=0.1, cex=cex.names)		
	  
	  # add the profile ID
	  if(print.id)
		{
		# optionally abbreviate
		if(abbr)
		  id.text <- abbreviate(as.character(this_profile_id), abbr.cutoff)
	
	# no abbreviations of th ID
	else
	  id.text <- as.character(this_profile_id)
		
		# add the text: according to style
	if(id.style == 'top')
			text(i, y.offset, id.text, pos=3, font=2, cex=cex.id)
	
	if(id.style == 'side')
		text(i-(width+0.025), y.offset, id.text, adj=c(1, -width), font=2, cex=cex.id, srt=90)
		}
	  }
  
  # axis:
  depth_axis_tick_locations <- (depth_axis_intervals * scaling.factor) + y.offset
  depth_axis_labels <- paste(depth_axis_intervals, units(x))
  axis(side=4, line=-2.5, las=2, at=depth_axis_tick_locations, labels=depth_axis_labels, cex.axis=cex.depth.axis)
  
  }



# method dispatch
setMethod("plot", signature("SoilProfileCollection"), definition=plot.SoilProfileCollection)



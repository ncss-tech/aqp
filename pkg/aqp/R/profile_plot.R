# generate a soil profile figure, from a generic dataframe
# using top and bottom boundaries, annotating with name
# optionally color with vector that is the same length as number of horizons
# behavior not defined for horizons with an indefinate lower boundary


# generic function prototype
profile_plot <- function(x, ...) UseMethod("profile_plot")

# default method, i.e. for a dataframe
# depreciated
profile_plot.default <- function(top, bottom, name, max_depth, cols=NA, width=1, cex.names=0.5, ...)
	{
	
	if(missing(max_depth))
		{
		# get the range of depths
		max_depth <- max(bottom, na.rm=TRUE)
		}
		
	# start a new plot:
	par(mar=c(1,0,0,1))
	plot(0,0, type='n', xlim=c(0,2.5*width), ylim=c(max_depth+2, 0), axes=FALSE, ...)
	
	# add horizons
	rect(0, bottom, width, top, col=cols)
	
	# annotate with names
	mid <- (top+bottom)/2
	text(width, mid, name, pos=4, offset=0.1, cex=cex.names)
	}



# method for a SoilProfile class
# not finished
profile_plot.SoilProfile <- function(d, color='soil_color', width=1, cex.names=0.5, ...)
	{
	# start a new plot:
	par(mar=c(1,0,0,1))
	plot(0,0, type='n', xlim=c(0,2.5*width), ylim=c(d$max_depth+2, 0), axes=FALSE, ...)
	
	# add horizons
	rect(0, d$data$bottom, width, d$data$top, col=d$data[, color_col])
	
	# annotate with names
	mid <- with(d$data, (top+bottom)/2)
	text(width, mid, d$data$name, pos=4, offset=0.1, cex=cex.names)
	
	}
	
	
# method for SoilProfileList class
profile_plot.SoilProfileList <- function(d, color='soil_color', width=0.25, cex.names=0.5, plot.order=1:d$num_profiles, add=FALSE, scaling.factor=1, y.offset=0, max.depth=d$max_depth, ...)
	{
	# fudge factors
	extra_x_space <- 1
	extra_y_space <- 2
	
	# pre-compute nice range for depth axis, also used for plot init
	depth_axis_intervals <- pretty(seq(from=0, to=max.depth, by=10))
	
	# set margins... consider moving outside of function
	par(mar=c(0.5,0,0,1))
	
	# init plotting region, unless we are appending to an existing plot
	if(!add)
		plot(0, 0, type='n', xlim=c(1, d$num_profiles+extra_x_space), ylim=c(max(depth_axis_intervals), -2), axes=FALSE)
	
	# add horizons in specified order	
	for(i in 1:d$num_profiles)
		{
		# convert linear sequence into plotting order
		profile_i <- plot.order[i]
		
		# generate rectangle geometry
		y0 <- (d$data[[profile_i]][,'bottom'] * scaling.factor) + y.offset
		y1 <- (d$data[[profile_i]][,'top'] * scaling.factor) + y.offset
		
		# make rectangles (horizons)
		rect(i-width, y0, i + width, y1, col=d$data[[profile_i]][, color])
	
		# annotate with names
		mid <- ( y1 + y0 )/2
		text(i + width, mid, d$data[[profile_i]][,'name'], pos=4, offset=0.1, cex=cex.names)
		
		# ID
		text(i, y.offset-1, d$data[[profile_i]]$id, pos=3, font=2, cex=cex.names+(0.2*cex.names))
		}
	
	# axis:
	depth_axis_tick_locations <- (depth_axis_intervals * scaling.factor) + y.offset
	depth_axis_labels <- paste(depth_axis_intervals, d$depth_units)
	axis(side=4, line=-2.5, las=2, at=depth_axis_tick_locations, labels=depth_axis_labels, cex.axis=cex.names)
		
 	# debugging:
 	# abline(v=1:d$num_profiles, lty=2)
	}
	
# profile_plot(sp1.list, color='soil_color')	
	
	
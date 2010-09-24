# generate a soil profile figure, from a generic dataframe
# using top and bottom boundaries, annotating with name
# optionally color with vector that is the same length as number of horizons
# behavior not defined for horizons with an indefinate lower boundary


# generic function prototype
profile_plot <- function(...) UseMethod("profile_plot")
	
	
# method for SoilProfileList class
profile_plot.SoilProfileList <- function(d, color='soil_color', width=0.2, name='name', cex.names=0.5, cex.depth.axis=cex.names, cex.id=cex.names+(0.2*cex.names), plot.order=1:d$num_profiles, add=FALSE, scaling.factor=1, y.offset=0, max.depth=d$max_depth, shrink=FALSE, shrink.cutoff=3, abbr=FALSE, abbr.cutoff=5, ...)
	{	
		
	# check for missing / bad soil color column
	# hack: just check the first object in the list
	if(! color %in% names(d$data[[1]]$data))
		{
		stop(paste('Invalid soil color column:', color))
		}
	
	if(! name %in% names(d$data[[1]]$data))
		{
		stop(paste('Invalid horizon name column:', name))
		}
		
	# fudge factors
	extra_x_space <- 1
	extra_y_space <- 2
	
	# pre-compute nice range for depth axis, also used for plot init
	depth_axis_intervals <- pretty(seq(from=0, to=max.depth, by=10))
	
	# set margins... consider moving outside of function
	par(mar=c(0.5,0,0,1))
	
	# init plotting region, unless we are appending to an existing plot
	if(!add)
		plot(0, 0, type='n', xlim=c(1, d$num_profiles+extra_x_space), ylim=c(max(depth_axis_intervals), -4), axes=FALSE)
	
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
		# first get the horizon mid-point
		mid <- ( y1 + y0 )/2
		
		# optionally shrink the size of names if they are longer than a given thresh
		if(shrink) {
			names.to.shrink <- which(nchar(d$data[[profile_i]][, name]) > shrink.cutoff)
			cex.names.shrunk <- rep(cex.names, length(d$data[[profile_i]][, 'top']))
			cex.names.shrunk[names.to.shrink] <- cex.names.shrunk[names.to.shrink] * 0.8
			text(i + width, mid, d$data[[profile_i]][, name], pos=4, offset=0.1, cex=cex.names.shrunk)
			}
		# standard printing of names, all at the same size
		else
			text(i + width, mid, d$data[[profile_i]][, name], pos=4, offset=0.1, cex=cex.names)		
		
		# add the profile ID, optionally abbreviate
		if(abbr)
			{
			text(i, y.offset, abbreviate(as.character(d$data[[profile_i]]$id), abbr.cutoff), pos=3, font=2, cex=cex.id)
			}
		# no abbreviations of th ID	
		else
			text(i, y.offset, as.character(d$data[[profile_i]]$id), pos=3, font=2, cex=cex.id)
		}
	
	# axis:
	depth_axis_tick_locations <- (depth_axis_intervals * scaling.factor) + y.offset
	depth_axis_labels <- paste(depth_axis_intervals, d$depth_units)
	axis(side=4, line=-2.5, las=2, at=depth_axis_tick_locations, labels=depth_axis_labels, cex.axis=cex.depth.axis)
	}
	
	
	
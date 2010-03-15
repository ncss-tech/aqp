##############################################################
## spatial slicing functions ##
##############################################################


# format named depth slices into a list of SPDF objects
format_slices <- function(data, crs=NULL, depths, variable=NULL)
{	

	# check to make sure we have enough data to make an SPDF
	if(is.null(data$x) | is.null(data$y))
		stop('coordinates missing!')
	
	# init empty list
	slices <- list()
	
	for(i in depths)
	{
		# get this depth slice
		d.sub <- subset(data, subset=top==i)
		
		# convert to SPDF: 'x' and 'y' columns must be present
		coordinates(d.sub) <- ~ x+y	
		
		# this isn't quite right
		if(!is.null(crs))
			d.sub@proj4string <- crs
		
		# if a 'variable' is given, slice the data such that no NA
		# in that variable are returned
		if(!is.null(variable))
			{
			# keep only those points that have data
			no.na.idx <- which( ! is.na(d.sub@data[,variable]))
		
			# name each list component
			slices[[paste(i, 'cm', sep='-')]] <- d.sub[no.na.idx, ]
			}
		else # return all data regardless of NA in (un)named variable
			{
			slices[[paste(i, 'cm', sep='-')]] <- d.sub[]
			}	
	}
	return(slices)
}


# plot named slices of a given variable, cut into specified quantiles
plot_slices <- function(slices, region_outline, contours=NA, depths, variable, probs, legend.cex=1, symbol.cex=1.5)
	{
	# lookup quantiles of variable
	# for all slices
	global_quantiles <- quantile(do.call('c', lapply(slices, function(i) i@data[,variable])), na.rm=TRUE, probs)
	
	global_quantiles.names <- levels(cut(probs, breaks=probs))
	
	# setup plot regions: hard-coded for 3x2 layout
	m <- matrix(c(1,1,1,2,3,4,5,6,7,8,8,8), ncol=3, byrow=TRUE)
	layout(m, heights=c(0.1,1,1,0.2))
	
	# plot the main title
	par(mar=c(0,0,0,0))
	plot(1,1, axes=FALSE, type='n')
	text(1,1, variable, cex=1.5)
	
	# loop over slices
	for(i in 1:length(depths))
		{
		# title for each subfig
		title_i <- paste(depths[i], 'cm')
		
		# data cut into global quantiles
		v.quantiles <- cut(slices[[i]]@data[,variable], global_quantiles)
		
		# color scale
		cols <- rev(brewer.pal(n=length(global_quantiles)-1, name='Spectral'))
		
		
		# do the plot
		par(mar=c(1,1,2,1))
		plot(region_outline)
		if(!is.null(contours)) lines(contours, col=grey(0.9))
		points(slices[[i]], pch=21, cex=symbol.cex, col=1, bg=cols[as.numeric(v.quantiles)])
		title(title_i)
		box()
		}
	
	# legend
	ltext <- paste(levels(v.quantiles), global_quantiles.names, sep='\n')
	
	par(mar=c(0,0,0,0))
	plot(1,1, axes=FALSE, type='n')
	legend(1, 1, legend=ltext, col=1, pt.cex=2, pt.bg=cols, pch=21, horiz=TRUE, yjust=0.5, xjust=0.5, cex=legend.cex, bty='n')
	
	}



##############################################################
## data formatting functions ##
##############################################################

# 
# x.re.formatted <- by(x, x$pedon_id, function(d.sub) 
# {
# num_hz <- nrow(d.sub)
# last_hz <- d.sub[num_hz, ]
# last_hz$top <- last_hz$bottom 
# rbind(d.sub, last_hz)
# } 
# )
# 
# x.new <- do.call('rbind', x.re.formatted)








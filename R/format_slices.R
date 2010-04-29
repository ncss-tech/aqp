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
		# 'top' must be a numeric column in the source dataframe
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

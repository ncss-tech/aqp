##
## defines basic soil object and methods
##


# init a SoilProfile class object
initProfile <- function(d, depth_units='cm', idcol="id")
	{
	
	# probably want to do some kind of checking depth logic here
	
	# possibly some automatic color conversion here 
	
	# using a list to store parts
	d.temp <- list(
	id=unique(as.character(d[, idcol])),
	depth_units=depth_units,
	nhz=nrow(d),
	max_depth=max(c(d$top, d$bottom), na.rm=TRUE),
	data=d
	)
	
	# set class membership and inheritance
	class(d.temp) <- c('SoilProfile','data.frame')
	return(d.temp)
	}



# constructor for lists of soil profiles
# could use some work...
initProfileList <- function(d, depth_units='cm', idcol="id")
	{
	# need to 
	if(!require(plyr))
		stop('Please install the "plyr" package.')
	
	# init list of SoilProfile
	d.list <- list()
	d.list$data <- dlply(.data=d, .variables=idcol, .fun=initProfile, depth_units=depth_units, idcol=idcol)
	
	# add the max depth for the entire list
	d.list$max_depth <- max(sapply(d.list$data, max), na.rm=TRUE)
	
	# add the depth units for the entire list
	d.list$depth_units <- depth_units
	
	# keep count of number of profiles for later
	d.list$num_profiles <- length(d.list$data)
	
	# set class membership
	class(d.list) <- c('SoilProfileList','list')
	
	return(d.list)
	}



# default max() operator on SoilProfile class
# just an extractor method for the 
# max depth of the bottom-most horizon property
max.SoilProfile <- function(x, ...)
	{
	return(x$max_depth)
	}


# default square bracket subsetting
# not sure if this is quite right, as the $ operator does not work as expected
"[.SoilProfile" <- function(x, ...)
	{
	"[.data.frame"(x$data, ...)
	}

# interesting effect:
# sp.list[1,] --> returns the first horizon from all objects
# sp.list[,1] --> returns all horizons from the first object
"[.SoilProfileList" <- function(x, ...)
	{
	"[.data.frame"(x$data, ...)
	}


# default print method for a single profile object
print.SoilProfile <- function(x, ...) 
	{
	cat("\nSoilProfile object ID:`", x$id, "` with ", x$nhz, " horizons, ", x$max_depth, " (", x$depth_units, ") deep\n\n", sep='')
	print(x$data)
	}
	
# very basic, could use some work
print.SoilProfileList <- function(x, ...) 
	{
	cat("\nList of ", x$num_profiles , " SoilProfile objects, maximum depth ", x$max_depth, " (", x$depth_units, ")\n\n", sep='')
	}


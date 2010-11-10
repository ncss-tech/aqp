## 
## S4 class defs-- initial steps
##

# class prototype for a single soil profile
setClass('SoilProfile', 
representation(
data='data.frame',
metadata='list',
id='character',
depth_units='character'
), 
prototype=prototype(
metadata=list(),
depth_units='cm'
),
validity=function(object)
	{
	# very basic checking to make sure we have sensible inputs  
	if( nrow(object@data) < 1)
		stop('invalid horizon data')
	else
		return(TRUE)	
		
	}
)

# initializer function for SoilProfile class
setMethod(f='initialize', signature='SoilProfile', 
definition=function(.Object, data, metadata, idcol, depth_units) 
	{	
	# assign data slot
	.Object@data <- data
	
	# call inspector to check for horizon level errors
	validObject(.Object)
	
	# build metadata
	md <- list()
	md$nhz <- nrow(data)
	md$max_depth <- max(c(data$top, data$bottom), na.rm=TRUE)
	
	# assign metadata
	.Object@metadata <- md
	
	# assign id
	.Object@id <- unique(as.character(data[, idcol]))
	
	# assign depth_units
	.Object@depth_units <- depth_units
		
	# done
	return(.Object)
	}
)

# basic printing method for SoilProfile class
setMethod(f='show', signature='SoilProfile',
definition=function(object)
	{
		cat("\nSoilProfile object ID:`", object@id, "` with ", object@metadata$nhz, " horizons, ", object@metadata$max_depth, " (", object@depth_units, ") deep\n\n", sep='')
	print(object@data)
	}
)	


# S3 function for the user, just a wrapper
SoilProfile <- function(data, idcol='id', depth_units='cm')
	{
	# not much to it
	return(new(Class='SoilProfile', data=data, idcol=idcol, depth_units=depth_units))
	}

# testing:
# data(sp1)
# sp <- SoilProfile(data=sp1[sp1$id == 'P001', ], idcol='id', depth_units='cm')
 
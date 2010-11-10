## 
## S4 class defs-- initial steps
##

# class prototype for a single soil profile
setClass('SoilProfile', 
representation(
depths='matrix',
horizon_data='data.frame',
site_data='list',
user_id='character',
depth_units='character',
metadata='list'
), 
prototype=prototype(
site_data=list(),
depth_units='cm'
),
validity=function(object)
	{
	# very basic checking to make sure we have sensible inputs  
	if( nrow(object@horizon_data) < 1)
		stop('invalid horizon data')
	else
		return(TRUE)	
		
	}
)

# initializer function for SoilProfile class
setMethod(f='initialize', signature='SoilProfile', 
definition=function(.Object, depths, horizon_data, site_data, user_id, depth_units) 
	{
		
	# temporary reminder
	print('this is new stuff, use with caution.')	
	
	# assign depths slot
	.Object@depths <- depths
	
	# assign data slot
	.Object@horizon_data <- horizon_data
	
	# assign site data slot
	.Object@site_data <- site_data
	
	# call inspector to check for horizon level errors
	validObject(.Object)
	
	# build metadata
	md <- list()
	md$nhz <- nrow(horizon_data)
	md$max_depth <- max(depths, na.rm=TRUE)
	
	# assign metadata
	.Object@metadata <- md
	
	# assign user_id
	.Object@user_id <- user_id
	
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
		cat("\nSoilProfile object ID:`", object@user_id, "` with ", object@metadata$nhz, " horizons, ", object@metadata$max_depth, " (", object@depth_units, ") deep\n\n", sep='')
	print(object@horizon_data)
	}
)	


# S3 function for the user, just a wrapper
SoilProfile <- function(depths, horizon_data, site_data=list(), user_id, depth_units='cm')
	{
	# not much to it
	return(new(Class='SoilProfile', depths=depths, horizon_data=horizon_data, site_data=site_data, user_id=user_id, depth_units=depth_units))
	}


# setup the depths() generic function
setGeneric('depths<-',package='aqp', def=function(object, value) 
  {
  standardGeneric('depths<-')
  }
)

setGeneric('site_data<-',package='aqp', def=function(object, value) 
  {
  standardGeneric('site_data<-')
  }
)

# init like this: depth(x) <- ~ top + bottom
# based on: getMethod("coordinates<-", "data.frame")
setMethod(f='depths<-', signature='data.frame',
definition=function(object, value)
  {
  depth.names <- NULL
  if (inherits(value, "formula")) 
	{
	# extract components of formula
	mf <- model.frame(value, object)
	
	# generate user ID and depths
	user_id <- unique(as.character(mf[,1]))
	depths <- as.matrix(mf[, 2:3])
	
	# optionally remove id, top, and bottom from the horizon data?
	horizon_data <- object
	}
  else
	stop('invalid initialization for SoilProfile object')
  
  # assemble object
  SoilProfile(depths=depths, horizon_data=horizon_data, user_id=user_id)
  }
)

# # test: works OK
# sp1.1 <- sp1[sp1$id == 'P001', ]
# depths(sp1.1) <- id ~ top + bottom




setMethod(f='site_data<-', signature='SoilProfile',
definition=function(object, value)
  {
  depth.names <- NULL
  if (inherits(value, "formula")) 
	{
	mf <- model.frame(value, object)
	print(cc)
# 	nm <- as.character(as.list(value)[[2]])[2:3]
# 	depth.names <- match(nm, names(object))
	}
   
   SoilProfile(data=object, idcol='id', depth_units='cm')
  }
)




#    
#    return(s)
      
#     else if (is.character(value)) {
#         cc = object[, value]
#         coord.numbers = match(value, names(object))
#     }
#     else if (is.null(dim(value)) && length(value) > 1) {
#         if (any(value != as.integer(value) || any(value < 1))) 
#             stop("coordinate columns should be positive integers")
#         cc = object[, value]
#         coord.numbers = value
#     }
#     else cc = coordinates(value)
#     if (any(is.na(cc))) 
#         stop("coordinates are not allowed to contain missing values")
#     if (!is.null(coord.numbers)) {
#         object = object[, -coord.numbers, drop = FALSE]
#         stripped = coord.numbers
#         if (ncol(object) == 0) 
#             return(SpatialPoints(cc))
#     }
#     else stripped = numeric(0)
#     SpatialPointsDataFrame(coords = cc, data = object, coords.nrs = stripped, 
#         match.ID = FALSE)
  

##
## TODO:
##
## 1. figure out how to deal with: user ids (labels) vs. internal ids
## 2. site_data functions for SoilProfileCollection objects
##
##
##


## 
## S4 class defs-- initial steps
##

# class prototype for a single soil profile
setClass('SoilProfile', 
representation(
depths='matrix',
horizon_data='data.frame',
site_data='data.frame',
user_id='character',
depth_units='character'
), 
prototype=prototype(
site_data=data.frame(),
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


# class prototype for a single soil profile
setClass('SoilProfileCollection', 
representation(
collection='list',
depth_units='character'
),
prototype=prototype(
depth_units='cm'
),
validity=function(object)
	{
	# check for consistent depth units
	du <- sapply(object@collection, function(i) i@depth_units)
	if(length(unique(du)) > 1)
	  warning('inconsisten depth units provided')
	}
)



##
## class initializer functions
##

# initializer function for SoilProfile class
setMethod(f='initialize', signature='SoilProfile', 
definition=function(.Object, depths, horizon_data, site_data, user_id, depth_units) 
	{
	# assign depths slot
	.Object@depths <- depths
	
	# assign data slot
	.Object@horizon_data <- horizon_data
	
	# assign site data slot
	.Object@site_data <- site_data
	
	# call inspector to check for horizon level errors
	validObject(.Object)
	
	# assign user_id
	.Object@user_id <- user_id
	
	# assign depth_units
	.Object@depth_units <- depth_units
		
	# done
	return(.Object)
	}
)



# this needs more work
# initializer function for SoilProfile class
setMethod(f='initialize', signature='SoilProfileCollection', 
definition=function(.Object, collection) 
	{
	# assign slots
	.Object@collection <- collection
		
	# call inspector to check for horizon level errors
	validObject(.Object)
	
	# assign depth_units
	.Object@depth_units <- unique(sapply(.Object@collection, function(i) i@depth_units))
		
	# done
	return(.Object)
	}
)






##
## not yet sure if this is a good idea to overload basic functions
##

# overload length() to give us the number of horizons
setMethod(f='length', signature='SoilProfile',
definition=function(x)
	{
	return(nrow(x@horizon_data))
	}
)

# overload length() to give us the number of profiles
setMethod(f='length', signature='SoilProfileCollection',
definition=function(x)
	{
	return(length(x@collection))
	}
)

# overload max() to give us the profile depth
setMethod(f='max', signature='SoilProfile',
definition=function(x)
	{
	return(max(x@depths, na.rm=TRUE))
	}
)

# overload max() to give us the max depth within a collection
setMethod(f='max', signature='SoilProfileCollection',
definition=function(x)
	{
	return(max(sapply(x@collection, max)))
	}
)

# basic printing method for SoilProfile class
setMethod(f='show', signature='SoilProfile',
definition=function(object)
	{
	cat("SoilProfile object ID:`", object@user_id, "` with ", length(object), " horizons, ", max(object), " (", object@depth_units, ") deep\n", sep='')
	}
)	

setMethod(f='show', signature='SoilProfileCollection',
definition=function(object)
	{
	cat("Collection of ", length(object) , " SoilProfile objects, maximum depth ", max(object), " (", object@depth_units, ")\n", sep='')
	}
)	



##
## S3 helper functions
##


# S3 function for the user, just a wrapper
# TODO: how can we leave out the non-required arguments?
SoilProfile <- function(depths, horizon_data, site_data=data.frame(), user_id, depth_units='cm')
	{
	# not much to it
	return(new(Class='SoilProfile', depths=depths, horizon_data=horizon_data, site_data=site_data, user_id=user_id, depth_units=depth_units))
	}

# S4 function for the user, just a wrapper
# TODO: how can we leave out the non-required arguments?
SoilProfileCollection <- function(collection)
	{
	# not much to it
	return(new(Class='SoilProfileCollection', collection=collection))
	}



##
## depths()<- setter
##
# setup the depths() generic function
setGeneric('depths<-',package='aqp', def=function(object, value) 
  {
  standardGeneric('depths<-')
  }
)

# init like this: depth(x) <- ~ top + bottom
# based on: getMethod("coordinates<-", "data.frame")
setMethod(f='depths<-', signature='data.frame',
definition=function(object, value)
  {
  # make sure inputs are valid
  if (inherits(value, "formula")) 
	{
	# extract components of formula
	# 1. user id
	# 2. top
	# 3. bottom
	mf <- model.frame(value, object)
	
	# get the names and column indices of the id, top, bottom
	# so that we can remove them latter
	nm <- names(mf)
	idx <- match(nm, names(object))
	
	# generate user ID and depths
	user_id <- unique(as.character(mf[, 1]))
	
	# if there is only 1 ID we are generating a SoilProfile objects
	if(length(user_id) == 1)
	  {
	  # extract depths
	  depths <- as.matrix(object[, idx[2:3]])
	
	  # make a copy of the horizon data, with id, top, and bottom removed
	  horizon_data <- object[, -idx]
	  
	  # assemble object and return
	  return(SoilProfile(depths=depths, horizon_data=horizon_data, user_id=user_id))
	  }
	
	# otherwise, we have a collection--> SoilProfileCollection
	else
	  {
	  # check for dependencies
	  if(!require(plyr))
		  stop('Please install the "plyr" package.')
	  
	  # nm contains names for user_id, top, bottom
	  SPC <- dlply(.data=object, .variables=nm[1], .progress='text', .fun=function(profile_i)
		{
		# get current user_id
		user_id_i <- unique(as.character(profile_i[, idx[1]]))
		
		# extract depths
		depths <- as.matrix(profile_i[, idx[2:3]])
	  
		# make a copy of the horizon data, with id, top, and bottom removed
		horizon_data <- profile_i[, -idx]
		
		# assemble object and return
		return(SoilProfile(depths=depths, horizon_data=horizon_data, user_id=user_id_i))
		}
	  )
	  
	  
	  # assemble and return the SoilProfileCollection object
	  return(SoilProfileCollection(collection=SPC))
	  }
	
	
	
	
	}
  else
	stop('invalid initialization for SoilProfile object')
  
  # done
  }
)



##
## site_data() accessor
##
setGeneric('site_data', package='aqp', def=function(object, value) 
  {
  standardGeneric('site_data')
  }
)

# simple case, just a single SoilProfile
setMethod(f='site_data', signature='SoilProfile',
definition=function(object)
  {
  return(object@site_data)
  }
)

# more complex case, collection of SoilProfile objects
setMethod(f='site_data', signature='SoilProfileCollection',
definition=function(object)
  {
  return(object@site_data)
  }
)


##
## site_data()<- setter
##
setGeneric('site_data<-', package='aqp', def=function(object, value) 
  {
  standardGeneric('site_data<-')
  }
)

# experimental assignment of site data
setMethod(f='site_data<-', signature='SoilProfile',
definition=function(object, value)
  {
  depth.names <- NULL
  if (inherits(value, "formula")) 
	{
	mf <- model.frame(value, object@horizon_data)
	
	# get the names and column indices of the site data, 
	# so that we can remove them from horizon_data
	idx <- match(names(mf), names(object@horizon_data))
	
	# assemble site_data, this is a 1-row data.frame
	# since these data are repeated for each horizon, just keep the first
	
	# when there is only one attribute for site data we need to use a different approach
	if(ncol(mf) < 2)
	  {
	  site_data <- data.frame(X1=mf[1,])
	  names(site_data) <- names(mf)
	  }
	  
	# otherwise we just take the first row
	# TODO: check for uniqueness
	else
	  site_data <- mf[1, ]
	
	# remove the named site data from horizon_data
	object@horizon_data <- object@horizon_data[, -idx]
	
	# assign to object's slot
	object@site_data <- site_data
	}
	
  # in this case the user is supplying a 1-row dataframe with information
  else if(inherits(value, 'data.frame'))
	{
	object@site_data <- value
	}
  else
	stop('invalid initialization for SoilProfile object')
   
  # done
  return(object)
  }
)


##
## initialize metadata: object modification in-place
##
if (!isGeneric('metadata<-'))
  setGeneric('metadata<-', function(object, value) 
    standardGeneric('metadata<-'))
    
setReplaceMethod("metadata", "SoilProfileCollection",
  function(object, value) {
	
	# quick sanity check
	if(nrow(value) > 1 | nrow(value) < 1)
	  stop("metadata should be a 1-row data frame")
	
	# otherwise assign
	object@metadata <- value
	
	# done
	return(object)
	}   
)

##
## initialize units: object modification in-place, units stored in @metadata
##
if (!isGeneric('units<-'))
  setGeneric('units<-', function(object, value) 
    standardGeneric('units<-'))
    
setReplaceMethod("units", "SoilProfileCollection",
  function(object, value) {
	
	# quick sanity check: character, length 1
	
	# keep existing metadata
	md <- metadata(object)
	
	# default units are always in metadata
	# replace what ever is there
	md[['units']] <- value
	
	# replace metadata
	metadata(object) <- md
	
	# done
	return(object)
	}   
)


##
## depths<- setter method - to create AQP objects
##
if (!isGeneric('depths<-'))
  setGeneric('depths<-', function(object, value) 
    standardGeneric('depths<-'))

setReplaceMethod("depths", "data.frame",
  function(object, value) {
    if (inherits(value, "formula")) {
      # extract components of formula: 1. user id, 2. top, 3. bottom
      mf <- model.frame(value, object)
      res <- .initSPCfromMF(data=object, mf=mf)
    }	
    else {
      if (inherits(value, "character")) { # initialization by colnames
	mf <- object[,value]
	res <- .initSPCfromMF(data=object, mf=mf)
      }
      else
	stop('invalid initialization for SoilProfile object')
    }
    
    # add default metadata
    metadata(res) <- data.frame(units='', stringsAsFactors=FALSE)
    
    # done 
    return(res)
  }
)


##
## initialize SP/SPC objects from a model.frame
## 
.initSPCfromMF <- function(data, mf){
  # get column names containing id, top, bottom
  nm <- names(mf)
  # create object
  res <- SoilProfileCollection(idcol=nm[1], topcol=nm[2], bottomcol=nm[3], horizons=data)
  # done
  return(res)
}


##
## initialize site data
##
if (!isGeneric('site<-'))
  setGeneric('site<-', function(object, value) 
    standardGeneric('site<-'))

setReplaceMethod("site", "SoilProfileCollection",
  function(object, value) {
	# get the corresponding vector of IDs, will be used to compute distinct site attributes
    ids <- as.character(horizons(object)[[idname(object)]])
  # creation of site data from horizon data
    if (inherits(value, "formula")) {
      mf <- model.frame(value, horizons(object))
      nm <- names(mf)
      mf <- data.frame(ids, mf)
      names(mf) <- c(idname(object), nm)
      res <- .createSiteFromHorizon(object, mf)
      # is this the best approach?
      object <- res
    }
#     else {
#       if (inherits(value, "character")) {
# 	i <- which(names(horizons(object)) %in% value)
# 	mf <- horizons(object)[, i]
# 	if (!is.data.frame(mf)) {
# 	  mf <- data.frame(mf)
# 	  names(mf) <- names(horizons(object))[i]
# 	}
# 	nm <- names(mf)
# 	mf <- data.frame(ids, mf)
# 	names(mf) <- c(idname(object), nm)
# 	res <- .createSiteFromHorizon(object, mf)
# 	object <- SoilProfileCollection(profiles=res$profiles_list, site=res$site_data)
#       }
#   # creation of site data from external data
#       else {
# 	if (inherits(value, "data.frame")) {
# 	# check for a valid site_id
# 	if(is.na(match(object@site_id, names(value)))) {
# 	  warning(paste('there is no column in the site table matching the current site id (', object@site_id, ')', sep=''))
# 	  warning('this is still experimental, use with caution!')
# 	  # stop('please assign a different site id, or add one to the site table')
# 	  }
# 	# if this is a data.frame we are actually adding data
# 	object <- SoilProfileCollection(profiles=as.list(profiles(object)), site=value)
# 	}
# 	else stop('not implemented yet')
#       }
#     }
    object
  }
)

# update an SPC object:
# add site data
# remove named columns from horizons
# return new SPC object
.createSiteFromHorizon <- function(object, mf){
  # create a numeric index for named site columns, as we will remove them 
  # from the horizon data
  names_attr <- names(mf)
  idx <- match(names_attr, names(horizons(object)))
  # remove the index to the ID columnm, as we do not want to remove this from
  # the horizon data !
  idx <- idx[-match(idname(sp1), names_attr)]
  
  # this seems to work fine in all cases, as we keep the ID column
  site_data <- ddply(mf, idname(object), 
      .fun=function(x) {
	df <- subset(x, select=names_attr)
	colwise(unique)(df)
      })

  # if site data is already present in the object, we don't want to erase it
  if (length(site(object)) > 0)
    site_data <- data.frame(site(object), site_data)

  # remove the named site data from horizon_data
  horizons(object) <- horizons(object)[, -idx]
  
  # replace existing site data
  object@site <- site_data
  
  # done
  return(object)
}


##
## horizon data replacement
##
## horizons<- setter method
##
if (!isGeneric('horizons<-'))
  setGeneric('horizons<-', function(object, value) 
    standardGeneric('horizons<-'))

setReplaceMethod("horizons", "SoilProfileCollection",
  function(object, value) {
  # testing the class of the horizon data to add to the object
  if (!inherits(value, "data.frame"))
	stop("value must be a data.frame")
  
  # testing the number of rows of the horizon data
  if (nrow(value) != nrow(horizons(object)))
	stop("inconsistent number of rows")
  
  # basic test of ids:
  if(!idname(object) %in% names(value)) # is there a matching ID column in the replacement?
  	stop("there is no matching ID column in replacement")
  
  if(length(setdiff(unique(as.character(value[[idname(object)]])), profile_id(object))) > 0)
  	stop("there are IDs in the replacement that do not exist in the original data")
  
  # replacement
  object@horizons <- value
  
  # done
  return(object)
  }
)



##
## initialize spatial data
##


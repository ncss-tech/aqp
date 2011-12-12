## init
"SoilProfileCollection" <- function(
idcol='id',
depthcols=c('top','bottom'),
metadata=data.frame(stringsAsFactors=FALSE),
horizons,
site=data.frame(stringsAsFactors=FALSE),
sp=new('SpatialPoints') # this is a bogus place-holder
){
  # creation of the object (includes a validity check)
  new("SoilProfileCollection", idcol=idcol, depthcols=depthcols, metadata=metadata, horizons=horizons, site=site, sp=sp)
}



## show
setMethod(
  f='show',
  signature='SoilProfileCollection',
  definition=function(object){
    cat("Object of class ", class(object), "\n", sep = "")
    cat("Number of profiles: ", length(object), "\n", sep="")
	cat("Depth range: ", min(object), "-", max(object), " ", depth_units(object), "\n", sep="")
	cat("\nHorizon attributes:\n")
	print(head(horizons(object)))

	# in the presence of site data
    if (nrow(site(object)) > 0) {
      cat("\nSampling site attributes:\n")
      print(head(site(object)))
    }

    # presence of spatial data
    if(nrow(coordinates(object)) == length(object)) {
    cat('\nSpatial Data:\n')
    show(object@sp@bbox)
    show(object@sp@proj4string)
    }

  }
)



## summary





##
## accessors
##

## ID column name
if (!isGeneric("idname"))
    setGeneric("idname", function(object, ...) standardGeneric("idname"))

setMethod("idname", "SoilProfileCollection",
  function(object)
    return(object@idcol)
)


## distinct profile IDs
if (!isGeneric("profile_id"))
  setGeneric("profile_id", function(object, ...) standardGeneric("profile_id"))

setMethod("profile_id", "SoilProfileCollection",
  function(object)
    unique(as.character(horizons(object)[[idname(object)]]))
)


## horizon depth column names
if (!isGeneric("horizonDepths"))
    setGeneric("horizonDepths", function(object, ...) standardGeneric("horizonDepths"))

setMethod("horizonDepths", "SoilProfileCollection",
  function(object)
    return(object@depthcols)
)


## spatial data: coordinates
setMethod("coordinates", "SoilProfileCollection",
  function(obj) {
  return(coordinates(obj@sp))
  }
)

## site data
if (!isGeneric("site"))
  setGeneric("site", function(object, ...) standardGeneric("site"))

# retrieves the site data frame
setMethod("site", "SoilProfileCollection",
  function(object) {
  return(object@site)
  }
)

## horizon data
# returns a data.frame aggregating horizons data
if (!isGeneric("horizons"))
  setGeneric("horizons", function(object, ...) standardGeneric("horizons"))

setMethod(f='horizons', signature='SoilProfileCollection',
  function(object){
  return(object@horizons)
  }
)

## metadata
# returns a data.frame
if (!isGeneric("metadata"))
  setGeneric("metadata", function(object, ...) standardGeneric("metadata"))

setMethod(f='metadata', signature='SoilProfileCollection',
  function(object){
  return(object@metadata)
  }
)

## depth_units
# returns a data.frame
if (!isGeneric("depth_units"))
  setGeneric("depth_units", function(object, ...) standardGeneric("depth_units"))

setMethod(f='depth_units', signature='SoilProfileCollection',
  function(object){
	u <- as.character(metadata(object)[['depth_units']])
	  # give a warning if not defined
	if(u == '')
	  message('Note: depth depth_units have not yet been defined.')

	return(u)
  }
)



##
## overloads
##

## column names
## TODO: should this return a named list?
setMethod("names", "SoilProfileCollection",
  function(x) {
  res <- c(names(horizons(x)), names(site(x)))
  return(res)
  }
)

# overload min() to give us the min depth within a collection
setMethod(f='min', signature='SoilProfileCollection',
definition=function(x) {
  # compute depths by ID
  hz_bottom_depths <- horizonDepths(x)[2]
  d <- tapply(unlist(horizons(x)[[hz_bottom_depths]]), unlist(horizons(x)[[idname(x)]]), max, na.rm=TRUE)
  # return the shallowest depth
  return(min(d))
  }
)

# overload max() to give us the max depth within a collection
setMethod(f='max', signature='SoilProfileCollection',
definition=function(x){
  # compute depths by ID
  hz_bottom_depths <- horizonDepths(x)[2]
  d <- tapply(unlist(horizons(x)[[hz_bottom_depths]]), unlist(horizons(x)[[idname(x)]]), max, na.rm=TRUE)
  # return the deepest depth
  return(max(d))
  }
)

# overload length() to give us the number of profiles in the collection
setMethod(f='length', signature='SoilProfileCollection',
  definition=function(x){
  l <- length(profile_id(x))
  return(l)
  }
)



## standard column access: search horizons, then site
setMethod("$", "SoilProfileCollection",
  function(x, name) {

	# get names from site and hz data
	s.names <- names(site(x))
	h.names <- names(horizons(x))

	# when site data are initialized from an external DF, it is possible that
	# there will be duplicate column names
	if(name %in% h.names & name %in% s.names)
		warning('column name is present in horizon and site data, extracting from horizon data only')

	# get column from horizon data
    if (name %in% h.names)
      res <- horizons(x)[[name]]

    # otherwise check site data
    else
      if (name %in% s.names)
		res <- site(x)[[name]]

	  # if still missing return NULL
	  else
		res <- NULL

	return(res)
  }
)


## problem: when making new columns how  can the function determine where to insert the replacement>?
setReplaceMethod("$", "SoilProfileCollection",
  function(x, name, value) {
  	# extract hz and site data
  	h <- horizons(x)
	s <- site(x)

    # working with horizon data
    if (name %in% names(h)) {
      h[[name]] <- value
      horizons(x) <- h
      return(x)
      }
      
    # working with site data  
    if(name %in% names(s)) {
	  s[[name]] <- value
      # TODO: use site(x) <- s
      x@site <- s
      return(x)
      }
    
    # ambiguous: use length of replacement to determing: horizon / site   
    else {
      n.site <- nrow(s)
      n.hz <- nrow(h)
      l <- length(value)

      if(l == n.hz) {
      	h[[name]] <- value
	    horizons(x) <- h
	    return(x)
      	}
      
      if(l == n.site) {
      	s[[name]] <- value
      	# TODO: use site(x) <- s
      	x@site <- s
      	return(x)
      	}
	
	  else
	  	stop('length of replacement must equal number of sites or number of horizons')
    		
    }
  # done  
  }
)



##
## TODO: site + sp data are not returned, should be
## TODO: should return SPC objects not data.frames
##
## matrix / DF style access: only to horizon data
##
## i = profile index
## j = horizon / slice index
setMethod("[", "SoilProfileCollection",
  function(x, i, j, ...) {

    # convert to integer
    if(!missing(i)) {
      i <- as.integer(i)
      if(any(is.na(i)))
        stop('NA not permitted in profile index')
    }
    else # if no index is provided, the user wants all profiles
      i <- 1:length(x)

    # sanity check
    if(!missing(j)) {
      j <- as.integer(j)
      if(any(is.na(j)))
      stop('NA not permitted in horizon/slice index')
    }

    # extract requested profile IDs
    p.ids <- profile_id(x)[i]

    # extract all horizon data
    h <- horizons(x)
	
    # keep only the requested horizon data (filtered by pedon ID)
    h <- h[h[[idname(x)]] %in% p.ids, ]
    
    # site data, with conditional subsetting
    s <- site(x)
    if(nrow(s) > 0)
      s <- s[i, ]   # this will return i-rows of bogus data
	  
    # subset spatial data if exists
    if(nrow(coordinates(x)) == length(x))
      sp <- x@sp[i]
    else
      sp <- x@sp
    
    # subset horizons/slices based on j --> only when j is given
    if(!missing(j))
      h <- ddply(h, idname(x), .fun=function(y) y[j, ])

    # if there is REAL data in @sp, and we only have 1 row of hz per coordinate- return SPDF
    # for now test for our custom dummy SP obj: number of coordinates == number of profiles
    # also need to test that there is only 1 horizon/slice per location
    if(nrow(coordinates(x)) == length(x) & length(p.ids) == nrow(h)) {
      # combine with coordinates
      cat('result is a SpatialPointsDataFrame object\n')
      # note that we are filtering based on 'i' - an index of selected profiles

      # if site data, join hz+site
      if(nrow(s) > 0)
      	return(SpatialPointsDataFrame(coordinates(x)[i, ], data=join(h, s)))
      else # no site data
      	return(SpatialPointsDataFrame(coordinates(x)[i, ], data=h))	
    }

    # in this case there may be missing coordinates, or we have more than 1 slice of hz data
    else {
      SoilProfileCollection(idcol=x@idcol, depthcols=x@depthcols, metadata=x@metadata, horizons=h, site=s, sp=sp)
    }
    
  # done
  }
)



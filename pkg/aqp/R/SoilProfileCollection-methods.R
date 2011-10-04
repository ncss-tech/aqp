## init
"SoilProfileCollection" <- function(
idcol='id',
topcol='top',
bottomcol='bottom',
horizons,
site=data.frame(),
sp=SpatialPoints(matrix(c(1,1), nrow=1)), # this is a bogus place-holder
metadata=data.frame()
){
  # creation of the object (includes a validity check)
  new("SoilProfileCollection", idcol=idcol, topcol=topcol, bottomcol=bottomcol, horizons=horizons, site=site, sp=sp, metadata=metadata)
}



## show
setMethod(
  f='show',
  signature='SoilProfileCollection',
  definition=function(object){
    cat("Object of class ", class(object), "\n", sep = "")
    cat("Number of profiles: ", length(object), "\n", sep="")
	cat("Depth range: ", min(object), "-", max(object), " ", units(object), "\n", sep="")
#       cat("\nAvailable profiles:\n")
#       print(.getProfilesAsList(object))

	# in the presence of site data
    if (nrow(site(object)) > 0) {
      cat("\nSampling site attributes:\n")
      print(head(site(object)))
    }
  }
)



## summary





##
## accessors
##

## ID column name
if (!isGeneric("idname"))
    setGeneric("idname", function(object, ...)
      standardGeneric("idname"))

setMethod("idname", "SoilProfileCollection",
  function(object) 
    return(object@idcol)
)


## distinct profile IDs
if (!isGeneric("profile_id"))
  setGeneric("profile_id", function(object, ...)
    standardGeneric("profile_id"))

setMethod("profile_id", "SoilProfileCollection",
  function(object) 
    unique(as.character(horizons(object)[[idname(object)]]))
)

## for some reason this doesn't work... ?
# ## spatial data: coordinates
# setMethod("coordinates", "SoilProfileCollection",
#   function(object) {
#   return(sp::coordinates(object@sp))
#   }
# )

## site data
if (!isGeneric("site"))
  setGeneric("site", function(object, ...)
    standardGeneric("site"))

# retrieves the site data frame
setMethod("site", "SoilProfileCollection",
  function(object) {
  return(object@site)
  }
)

## horizon data
# returns a data.frame aggregating horizons data
if (!isGeneric("horizons"))
  setGeneric("horizons", function(object, ...)
    standardGeneric("horizons"))

setMethod(f='horizons', signature='SoilProfileCollection',
  function(object){
  return(object@horizons)
  }
)

## metadata
# returns a data.frame
if (!isGeneric("metadata"))
  setGeneric("metadata", function(object, ...)
    standardGeneric("metadata"))

setMethod(f='metadata', signature='SoilProfileCollection',
  function(object){
  return(object@metadata)
  }
)

## units
# returns a data.frame
if (!isGeneric("units"))
  setGeneric("units", function(object, ...)
    standardGeneric("units"))

setMethod(f='units', signature='SoilProfileCollection',
  function(object){
	u <- as.character(metadata(object)[['units']])
	  # give a warning if not defined
	if(u == '')
	  message('Note: depth units have not yet been defined.')
	
	return(u)
  }
)



##
## overloads
##

# overload min() to give us the min depth within a collection
setMethod(f='min', signature='SoilProfileCollection',
definition=function(x) {
  # compute depths by ID
  d <- tapply(unlist(horizons(x)[x@bottomcol]), unlist(horizons(x)[[idname(x)]]), max, na.rm=TRUE)
  # return the shallowest depth
  return(min(d))
  }
)

# overload max() to give us the max depth within a collection
setMethod(f='max', signature='SoilProfileCollection',
definition=function(x){
  # compute depths by ID
  d <- tapply(unlist(horizons(x)[x@bottomcol]), unlist(horizons(x)[[idname(x)]]), max, na.rm=TRUE)
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



# standard column access: search horizons, then site
setMethod("$", "SoilProfileCollection",
  function(x, name) {
    if (name %in% names(horizons(x)))
      res <- horizons(x)[[name]]
    else
      if (name %in% names(site(x)))
	res <- site(x)[[name]]
      else
	res <- NULL
    res
  }
)

## TODO: not sure about this
## TODO: this should check lengths 
## also: how can we create new columns ?
setReplaceMethod("$", "SoilProfileCollection",
  function(x, name, value) {
    if (name %in% names(horizons(x)))
      horizons(x)[[name]] <- value
    else
      site(x)[[name]] <- value
    x
  }
)

##
## TODO: this should return a SPDF when @sp is filled with real data
##
## list/array style access
setMethod("[[", c("SoilProfileCollection", "ANY", "missing"),
  function(x, i, j, ...) {
    if (i %in% names(horizons(x)))
      res <- horizons(x)[[i]]
    else
      if (i %in% names(site(x)))
	res <- site(x)[[i]]
      else
	res <- NULL
    res
  }
)

## TODO: not sure about this
setReplaceMethod("[[", c("SoilProfileCollection", "ANY", "missing", "ANY"),
  function(x, i, j, value) {
    if (i %in% names(horizons(x)))
      horizons(x)[[i]] <- value
    else
      site(x)[[i]] <- value
    x
  }
)

## TODO: should this return a named list?
# getting names
names.SoilProfileCollection <- function(x) c(names(horizons(x)), names(site(x)))




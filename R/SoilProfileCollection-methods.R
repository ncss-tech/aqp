## init
"SoilProfileCollection" <- function(
idcol='id',
depthcols=c('top','bottom'),
metadata=data.frame(),
horizons,
site=data.frame(),
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
	cat("Depth range: ", min(object), "-", max(object), " ", units(object), "\n", sep="")
	cat("\nHorizon attributes:\n")
	print(head(horizons(object)))

	# in the presence of site data
    if (nrow(site(object)) > 0) {
      cat("\nSampling site attributes:\n")
      print(head(site(object)))
    }

    # presence of spatial data
    if(nrow(coordinates(object@sp)) == length(object)) {
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


## for some reason this doesn't work... ?
# ## spatial data: coordinates
# setMethod("coordinates", "SoilProfileCollection",
#   function(object) {
#   return(sp::coordinates(object@sp))
#   }
# )

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

## units
# returns a data.frame
if (!isGeneric("units"))
  setGeneric("units", function(object, ...) standardGeneric("units"))

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



# standard column access: search horizons, then site
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
    if (name %in% names(horizons(x))) {
      h <- horizons(x)
      h[[name]] <- value
      horizons(x) <- h
      }
    else {
	  s <- site(x)
      s[[name]] <- value
      # TODO: site(x) <- should work, and does not
      x@site <- s
      }
    return(x)
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
    
    # extract all horizons
    h <- horizons(x)
    
    # keep only the requested horizons (filtered by pedon ID)
    h <- h[h[[idname(x)]] %in% p.ids, ]
    
    # subset horizons/slices based on j --> only when j is given
    if(!missing(j))
      h <- ddply(h, idname(x), .fun=function(y) y[j, ])

    # if there is REAL data in @sp, return a SPDF
    # for now test for our custom dummy SP obj: number of coordinates == number of profiles
    # also need to test that there is only 1 horizon/slice per location
    if(nrow(coordinates(x@sp)) == length(x) & length(p.ids) == nrow(h)) {
      # combine with coordinates
      cat('result is a SpatialPointsDataFrame object\n')
      # note that we are filtering based on 'i' - an index of selected profiles
      res <- SpatialPointsDataFrame(x@sp[i, ], data=h)
    }

    # no coordinates, return a data.frame for now
    # TODO: return as a SPC + site + sp data
    else {
      cat('result is a data.frame object\n')
      res <- h
    }

  # done
  return(res)
  }
)


##
## TODO: this should return a SPDF when @sp is filled with real data
##
## list / array style access
setMethod("[[", c("SoilProfileCollection", "ANY", "missing"),
  function(x, i, j, ...) {
    if (i %in% names(horizons(x)))
      res <- horizons(x)[[i]]
    else {
      if (i %in% names(site(x)))
	      res <- site(x)[[i]]
      else
	      res <- NULL
    }

  return(res)
  }
)

## TODO: not sure about this
setReplaceMethod("[[", c("SoilProfileCollection", "ANY", "missing", "ANY"),
  function(x, i, j, value) {
    if (i %in% names(horizons(x)))
      horizons(x)[[i]] <- value
    else
      site(x)[[i]] <- value

    return(x)
  }
)


## column names
## TODO: should this return a named list?
setMethod("names", "SoilProfileCollection",
  function(x) {
  res <- c(names(horizons(x)), names(site(x)))
  return(res)
  }

)


##
## neat-stuff
##

# works on a single set of depths + property at a time
# include:
# 'bottom' - bottom boundary is included in the z-slice test
# 'top' - top boundary is included in the z-slice test
get.single.slice <- function(d, top, bottom, z, include='top') {
  # extract pieces
  d.top <- d[[top]]
  d.bottom <- d[[bottom]]
  d.v <- d[['value']]
  d.var.name <- unique(d[['variable']]) # this is repeated for each horizon

  # determine the property at z-slice, based on boundary rule
  if(include == 'bottom')
    res <- d.v[which(z > d.top & z <= d.bottom)]
  if(include == 'top')
    res <- d.v[which(z >= d.top & z < d.bottom)]
  else
    stop('invalid horizon boundary rule')

  # account for no data
  if(length(res) == 0)
    res <- NA

  # name the variable, for nicer column names output from ddply()
  names(res) <- 'slice'
  return(res)
  }

## slice: returns a DF if no @sp, SPDF otherwise
if (!isGeneric("slice"))
  setGeneric("slice", function(object, ...) standardGeneric("slice"))



## TODO: allow for multiple slices... what would be returned ? a new SPC?

## TODO: allow the use of site data (PSC etc.) to determine the z-slice
setMethod(f='slice', signature='SoilProfileCollection',
  function(object, fm, just.the.data=FALSE){

  # test for logical input
  if(! inherits(fm, "formula"))
  	stop('must provide a valid formula: ~ var1 + var2 + ...')

  # extract components of the formula:
  formula <- str_c(deparse(fm, 500), collapse="")
  elements <- str_split(formula, fixed("~"))[[1]]
  formula <- lapply(str_split(elements, "[+*]"), str_trim)

  if (length(formula) > 2)
    stop("please provide a valid formula")

  z <- as.numeric(formula[[1]])
  vars <- formula[[2]]

  # get horizons + depth column names + ID column name
  h <- horizons(object)
  hd <- horizonDepths(object)
  id <- idname(object)

	# check for bogus left/right side problems with the formula
  if(any(z < 1) | any(is.na(z)))
    stop('z-slice must be >= 1')

  ## this will have to be updated for z-slices defined by data in @site
	if(! class(z) %in% c('numeric','integer')) # bogus z-slice
		stop('z-slice must be either numeric or integer')

	if(any(vars %in% names(h)) == FALSE) # bogus column names in right-hand side
		stop('column names in formula do not match any horizon data')

  # melt into long format
  m <- melt(h, measure.vars=vars, id.vars=c(id, hd[1], hd[2]))

  # extract single slice by id/variable
  hd.slice <- ddply(m, c(id, 'variable'), .fun=get.single.slice, top=hd[1], bottom=hd[2], z=z)

  # convert back into wide format
  fm.to.wide <- as.formula(paste(id, 'variable', sep=' ~ '))
  hd.slice <- cast(hd.slice, formula=fm.to.wide, value='slice')

  # if we just want the data:
  if(just.the.data)
    return(hd.slice)

  # if site data: join
  if(nrow(site(object)) > 0 )
    res <- join(hd.slice, site(object))
  else
    res <- hd.slice

  # if spatial data: SPDF
  if(nrow(coordinates(object@sp)) == length(object)) {
    cat('result is a SpatialPointsDataFrame object\n')
    res <- SpatialPointsDataFrame(object@sp, data=res)
    }
  else
    cat('result is a data.frame object\n')

  return(res)
  }
)

## Coercition methods and sp utilities
setAs("SoilProfileCollection", "SpatialPoints", function(from) {
    from@sp
  }
)

setAs("SoilProfileCollection", "SpatialPointsDataFrame", function(from) {
    SpatialPointsDataFrame(from@sp, data = from@site)
  }
)

setMethod(f='proj4string', signature='SoilProfileCollection',
  function(obj){
    proj4string(obj@sp)
  }
)

## clip: spatial clipping of a SPC (requires GEOS)
if (!isGeneric("clip"))
  setGeneric("clip", function(object, ...) standardGeneric("clip"))

setMethod(f='clip', signature='SoilProfileCollection',
  function(object, geom){

    # This functionality require the GEOS bindings
    # provided by rgeos
    require(rgeos)
    spc_intersection <- gIntersects(as(object, "SpatialPoints"), geom, byid = TRUE)
    ids <- which(spc_intersection)

    valid_ids <- site(object)[ids, object@idcol]

    valid_horizons <- which(horizons(object)[, object@idcol] == valid_ids)
    valid_sites <- which(site(object)[, object@idcol] == valid_ids)

    SoilProfileCollection(idcol = object@idcol, depthcols = object@depthcols, horizons = horizons(object)[valid_horizons, ], site = site(object)[valid_sites, ], sp = object@sp[ids,])
  }
)

setMethod(f='proj4string', signature='SoilProfileCollection',
  function(obj){
    proj4string(obj@sp)
  }
)

##
## initialize spatial data
##
setReplaceMethod("coordinates", "SoilProfileCollection",
  function(object, value) {

  # basic sanity check... needs work
  if(! inherits(value, "formula"))
    stop('invalid formula')

  # extract coordinates as matrix
  mf <- data.matrix(model.frame(value, site(object), na.action=na.pass))

  # test for missing coordinates
  mf.missing <- apply(mf, 2, is.na)

  if(any(mf.missing))
	  stop('cannot initialize a SpatialPoints object with missing coordinates')

  # assign to sp slot
  # note that this will clobber any existing spatial data
  object@sp <- SpatialPoints(coords=mf)
  
  # remove coordinates from source data
  # note that mf is a matrix, so we need to access the colnames differently
  coord_names <- dimnames(mf)[[2]]
  idx <- match(coord_names, names(site(object)))
  
  # remove the named site data from site_data
  # TODO we should use a proper setter!
  object@site <- site(object)[, -idx]
  
  # done
  return(object)
  }
)


##
## proj4string setting
##
setReplaceMethod("proj4string", "SoilProfileCollection",
  function(obj, value) {
  proj4string(obj@sp) <- value
  obj
  }
)


## doesn't quite work

## spatial_subset: spatial clipping of a SPC (requires GEOS)
if (!isGeneric("spatial_subset"))
  setGeneric("spatial_subset", function(object, ...) standardGeneric("spatial_subset"))

setMethod(f='spatial_subset', signature='SoilProfileCollection',
  function(object, geom){

    # This functionality require the GEOS bindings
    # provided by rgeos
    if(require(rgeos)) {
      spc_intersection <- gIntersects(as(object, "SpatialPoints"), geom, byid = TRUE)
      ids <- which(spc_intersection)

      valid_ids <- site(object)[ids, object@idcol]

      valid_horizons <- which(horizons(object)[, object@idcol] %in% valid_ids)
      valid_sites <- which(site(object)[, object@idcol] %in% valid_ids)

      SoilProfileCollection(idcol = object@idcol, depthcols = object@depthcols, metadata = object@metadata, horizons = horizons(object)[valid_horizons, ], site = site(object)[valid_sites, ], sp = object@sp[ids,])
    }
    else { # no rgeos, return original
      stop('Spatial subsetting not performed, please install the `rgeos` package.')
    }
  }
)

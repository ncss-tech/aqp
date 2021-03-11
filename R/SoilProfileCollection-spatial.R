
##
## wrappers to spatial operations via sp, rgdal, raster
##

## Note: this should probably be done with proper S4 methods

# spTransform.SoilProfileCollection <- function(spc, ...) {
# 	spc@sp <- spTransform(spc@sp, ...)
# 	return(x)
# }
#
# over.SoilProfileCollection <- function(spc, ...) {
# 	res <- over(spc@sp, ...)
# 	return(res)
# }
#
# extract.SoilProfileCollection <- function(spc, x, ...) {
# 	res <- extract(spc@sp, ...)
# 	return(res)
# }

##
## proj4string setting
##

#' Set PROJ4 string for the SoilProfileCollection
#'
#' @param obj A SoilProfileCollection
setMethod(f = 'proj4string', signature(obj = 'SoilProfileCollection'),
  function(obj){
    suppressWarnings(proj4string(obj@sp))
  }
)
#' Set PROJ4 string for the SoilProfileCollection
#'
#' @param obj A SoilProfileCollection
#' @param value A proj4string
#'
setReplaceMethod("proj4string", signature(obj = 'SoilProfileCollection'),
  function(obj, value) {
    suppressWarnings(proj4string(obj@sp) <- value)
    obj
  }
)

##
## initialize spatial data
##
#' @aliases coordinates<-,SoilProfileCollection-method
#' @param object A SoilProfileCollection
#' @param value A formula specifying columns containing x and y coordinates
#'
#' @rdname coordinates
#'
#' @examples
#'
#' data(sp5)
#'
#' # coordinates are stored in x and y column of site
#' sp5$x <- rnorm(length(sp5))
#' sp5$y <- rnorm(length(sp5))
#'
#' # coordinates takes a formula object as input
#' coordinates(sp5) <- ~ x + y
#'
setReplaceMethod("coordinates", "SoilProfileCollection",
  function(object, value) {

  # basic sanity check
  if(!inherits(value, "formula"))
    stop('invalid formula: ', value, call. = FALSE)

  # extract coordinates as matrix from site 
  mf <- try(data.matrix(model.frame(value, site(object), na.action = na.pass)), silent = TRUE)
  if (inherits(mf, 'try-error')) {
    # for horizons, calculate unique
    mf <- try(unique(data.matrix(model.frame(value, horizons(object), na.action = na.pass))), silent = TRUE)
    # if error, assume it is a variable name 
    if (inherits(mf, 'try-error'))
      stop("unable to find specified coordinates: ", as.character(value), call. = FALSE)
  }
  
  # make sure that "normalization" worked
  if (nrow(mf) != length(object)) {
    stop("coordinates in horizon data are not unique within site: ", as.character(value), call. = FALSE)
  }
  
  # test for missing coordinates
  mf.missing <- apply(mf, 2, is.na)

  if(any(mf.missing))
	  stop('cannot promote to spatial SoilProfileCollection with missing coordinates', call.=FALSE)

  # assign to sp slot
  # note that this will clobber any existing spatial data
  object@sp <- SpatialPoints(coords = mf)

  # remove coordinates from source data
  # note that mf is a matrix, so we need to access the colnames differently
  coord_names <- dimnames(mf)[[2]]
  sn <- siteNames(object)
  hn <- horizonNames(object)

  # @site and @horizons minus coordinates "promoted" to @sp
  object@site <- .data.frame.j(object@site, sn[!sn %in% coord_names], aqp_df_class(object))
  object@horizons <- .data.frame.j(object@horizons, hn[!hn %in% coord_names], aqp_df_class(object))
  
  # done
  return(object)
  }
)

# ### TODO: consider removing this function
#
# ##
# ## spatial_subset: spatial clipping of a SPC (requires GEOS)
# ##
#
# if (!isGeneric("spatial_subset"))
#   setGeneric("spatial_subset", function(object, geom) standardGeneric("spatial_subset"))
#
# setMethod(f='spatial_subset', signature='SoilProfileCollection',
#   function(object, geom){
#
#     # This functionality require the GEOS bindings
#     # provided by rgeos
#     if(require(rgeos)) {
#       spc_intersection <- gIntersects(as(object, "SpatialPoints"), geom, byid = TRUE)
#       ids <- which(spc_intersection)
#
# 	# extract relevant info
# 	s <- site(object)
# 	h <- horizons(object)
# 	d <- diagnostic_hz(object)
#   r <- restrictions(object)
#
# 	# get indexes to valid site, hz, diagnostic data
#   valid_ids <- s[ids, idname(object)]
#   valid_horizons <- which(h[, idname(object)] %in% valid_ids)
#   valid_sites <- which(s[, idname(object)] %in% valid_ids)
#   valid_diagnostic <- which(d[, idname(object)] %in% valid_ids)
# 	valid_restriction <- which(r[, idname(object)] %in% valid_ids)
#
# 	# create a new SPC with subset data
#   ## TODO: copy over diagnostic horizon data
# 	## TODO: use integer profile index to simplify this process
# 	## TODO: @sp bbox may need to be re-computed
#   ## TODO: check diagnostic subset
#       SoilProfileCollection(idcol = object@idcol, depthcols = object@depthcols, metadata = metadata(object), horizons = h[valid_horizons, ], site = s[valid_sites, ], sp = object@sp[ids,], diagnostic = d[valid_diagnostic, ], restrictions=r[valid_restriction,])
#     }
#     else { # no rgeos, return original
#       stop('Spatial subsetting not performed, please install the `rgeos` package.', call.=FALSE)
#     }
#   }
# )

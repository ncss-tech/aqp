# 2018-12-17
# D.E. Beaudette
# A.G. Brown
# 
# 
# This function replaces the previous rbind.SoilProfileCollection function.
# 


# ease the transition to union()
rbind.SoilProfileCollection <- function(...) {
  .Deprecated('please use union()')
  
  # parse dots
  objects <- list(...)
  names(objects) <- NULL
  
  # make compatible
  res <- union(spc=objects)
  return(res)
}



# TODO: https://github.com/ncss-tech/aqp/issues/71
#
# TODO: concatenation of data with duplicated IDs in @site, but unique data in other @site fields, will result in corrupt SPC
# TODO: duplicates in @sp will cause errors
# TODO: duplicates are removed in all other slots... does this make sense?
union <- function(spc=list(), method='all', ...) {
  # setup some defaults
  options(stringsAsFactors=FALSE)
  
  # parse dots
  objects <- spc
  names(objects) <- NULL
  
  # short-circuits
  if(length(objects) == 0)
    return(NULL)
  if(length(objects) == 1)
    return(objects[1])
  
  ## TODO: normalize profile ID, horizon ID, horizon depths
  # profile_id() <- 
  # horizonDepths() <- 
  # idname() <-
  
  
  # combine pieces
  # should have length of 1
  o.idname <- unique(lapply(objects, idname))
  o.depth.units <- unique(lapply(objects, depth_units))
  o.hz.depths <- unique(lapply(objects, horizonDepths))
  o.m <- unique(lapply(objects, aqp::metadata))
  o.coords <- unique(lapply(objects, function(i) ncol(coordinates(i))))
  o.p4s <- unique(lapply(objects, proj4string))
  
  # should have length > 1
  o.h <- lapply(objects, horizons)
  o.s <- lapply(objects, site)
  o.d <- lapply(objects, diagnostic_hz)
  o.sp <- lapply(objects, slot, 'sp')
  
  # sanity checks:
  if(length(o.idname) > 1)
    stop('inconsistent ID names', call.=FALSE)
  if(length(o.depth.units) > 1)
    stop('inconsistent depth units', call.=FALSE)
  if(length(o.hz.depths) > 1)
    stop('inconsistent depth columns', call.=FALSE)
  if(length(o.m) > 1)
    stop('inconsistent metadata', call.=FALSE)
  
  # spatial data may be missing...
  if(length(o.coords) > 1)
    stop('inconsistent spatial data', call.=FALSE)
  if(length(o.p4s) > 1)
    stop('inconsistent CRS', call.=FALSE)
  
  # generate new SPC components
  # using plyr::rbind.fill seems to solve the problem on non-conformal DF
  # https://github.com/ncss-tech/aqp/issues/71
  o.h <- unique(do.call('rbind.fill', o.h)) # horizon data
  o.s <- unique(do.call('rbind.fill', o.s)) # site data
  o.d <- unique(do.call('rbind.fill', o.d)) # diagnostic data, leave as-is
  
  # spatial points require some more effort when spatial data are missing
  o.1.sp <- objects[[1]]@sp
  
  # missing spatial data
  if(ncol(coordinates(o.1.sp)) == 1) {
    o.sp <- o.1.sp # copy the first filler
  } else {
    # not missing spatial data
    # 2015-12-18: added call to specific function: "sp::rbind.SpatialPoints"
    o.sp <- do.call("rbind.SpatialPoints", o.sp) 
  }
  
  
  ## make SPC and return
  res <- SoilProfileCollection(idcol=o.idname[[1]], depthcols=o.hz.depths[[1]], metadata=o.m[[1]], horizons=o.h, site=o.s, sp=o.sp, diagnostic=o.d)
  
  ## reset horizon IDs
  hzID(res) <- 1:nrow(res)
  
  
  ## final sanity checks
  if(length(profile_id(res)) != length(site(res)[[idname(res)]]))
    stop("SPC object corruption. This shouldn't happen and will be fixed in aqp 2.0", call. = FALSE)
  
  if(! all.equal(profile_id(res), site(res)[[idname(res)]]))
    stop("SPC object corruption. This shouldn't happen and will be fixed in aqp 2.0", call. = FALSE)
  
  return(res)
}




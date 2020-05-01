# getSurfaceHorizonDepth

# starting from the surface, match patterns to horizon
# return the last bottom depth of a horizon that is contiguous with surface
# for instance horizon designations: A1-A2-A3-C-Ab , would return A3 bottom depth
#
getSurfaceHorizonDepth <-  function(p, pattern, hzdesgn = hzdesgnname(p)) { 
  hz <- horizons(p)
  depths <- horizonDepths(p)
  shallowest.depth <- min(hz[[depths[1]]], na.rm=TRUE)
  
  if(is.infinite(shallowest.depth)) {
    warning(paste0("Profile (",profile_id(p),") is missing horizon top depths."))
    return(NA)
  }
    
  if(shallowest.depth > 0) {
    warning(paste0("Profile (",profile_id(p),") top depth is greater than zero."))
  }
  
  if(shallowest.depth < 0) {
    warning(paste0("Profile (",profile_id(p),") top depth is less than zero."))
  }
  
  match.idx <- grepl(hz[[hzdesgn]], pattern=pattern)
    
  if(length(which(match.idx)) < 1) {
    return(shallowest.depth)
  }
    
  mod.idx <- c(1, rep(0, length(match.idx) - 1))
  new.idx <- (match.idx + mod.idx) %% 3
  
  who.idx <- numeric(0)
  if(new.idx[1] == 2) {
    who.idx <- (rev(which(new.idx > 0 & new.idx <= 2))[1])
  }
  
  if(!length(who.idx)) {
    return(shallowest.depth)
  }
  
  return(hz[who.idx, depths[2]])
}

getMineralSoilSurfaceDepth <-  function(p, hzdesgn = hzdesgnname(p), pattern=".*O.*") { 
  #assumes OSM is given O designation;
  #TODO: add support for lab-sampled organic measurements
  #      keep organic horizons with andic soil properties
  return(getSurfaceHorizonDepth(p, hzdesgn=hzdesgn, pattern=pattern))
}

getPlowLayerDepth <- function(p, hzdesgn = hzdesgnname(p), pattern="^Ap[^b]") {
  return(getSurfaceHorizonDepth(p, hzdesgn, pattern=pattern))
}

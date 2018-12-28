# getSurfaceHorizonDepth

# starting from the surface, match patterns to horizon
# return the last bottom depth of a horizon that is contiguous with surface
# for instance horizon designations: A1-A2-A3-C-Ab , would return A3 bottom depth
#
getSurfaceHorizonDepth <-  function(p, pattern, hzdesgn='hzname') { 
  hz <- horizons(p)
  match.idx <- grepl(hz[[hzdesgn]], pattern=pattern)
    
  if(length(which(match.idx)) < 1) 
    return(0)
    
  mod.idx <- c(1, rep(0, length(match.idx) - 1))
  new.idx <- (match.idx + mod.idx) %% 3
    
  if(new.idx[1] == 2) {
    who.idx <- (rev(which(new.idx > 0 & new.idx <= 2))[1])
    if(!length(who.idx))
      return(0)
  }
  return(hz[who.idx, horizonDepths(p)[2]])
}

getMineralSoilSurfaceDepth <-  function(p, hzdesgn='hzname', pattern=".*O.*") { 
  #assumes OSM is given O designation;
  #TODO: add support for lab-sampled organic measurements
  #      keep organic horizons with andic soil properties
  return(getSurfaceHorizonDepth(p, hzdesgn=hzdesgn, pattern=pattern))
}

getPlowLayerDepth <- function(p, hzdesgn='hzname', pattern="^Ap[^b]") {
  return(getSurfaceHorizonDepth(p, hzdesgn, pattern=pattern))
}
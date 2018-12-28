# intersect.horizon() 
# returns unique index to all horizons occuring over the depth interval [z1, z2]. 
# z2 is optional, in which case a single horizon with depth range containing z1 is returned
# several wrapper functions around this for hzid

hz.dz <- function(p, z1, z2=NULL, hzid='phiid', as.list = FALSE) {
  #horizons by depth; internal/shorthand alias?
  return(intersect.horizon(p, z1, z2=NULL, hzid='phiid', as.list = FALSE))
}

intersect.horizon <- function(p, z1, z2=NULL, hzid='phiid', as.list = FALSE) {
  top.depth <- horizonDepths(p)[1]
  depthz <- horizons(p)[[top.depth]]
  
  # make two logical vectors reflecting horizon depths being
  #  greater than and less than z1 (and z2?)
  gt1 <- depthz >= z1
  
  idx.top <- which(gt1)
  
  if(!length(idx.top))
    return(NA)
  
  # some high tech reindexing
  idx.top <- idx.top[1] - 1
  
  if(!is.null(z2)) {
    gt2 <- depthz >= z2
    idx.bot <- which(gt2)
    if(!length(idx.bot))
      idx.bot <- length(depthz) + 1
    idx.bot <- idx.bot[1] - 1
    idval <- horizons(p)[idx.top:idx.bot, hzid]
    if(!as.list)
      return(idval)
    return(list(hzid = hzid, hz.idx = idx.top:idx.bot, value = idval))
  }
  
  idval <- horizons(p)[idx.top, hzid]
  
  if(!as.list)
    return(idval)
  
  return(list(hzid = hzid, hz.idx = idx.top, value = idval))
}

intersectPedonHorizon <- function(pedon, z1, z2=NULL) {
  #alias function; default arguments work with pedons (NASIS)
  #returns list of pedon horizon ids (phiid)
  return(intersectHorizon(pedon, z1, z2)) 
}

intersectLabHorizon <- function(pedon, z1, z2=NULL) {
  #alias function for lab pedons (KSSL)
  #returns list of lab sample #'s
  return(intersectHorizon(pedon, z1, z2, hzid='labsampnum')) 
}

intersectComponentHorizon <- function(pedon, z1, z2=NULL) {
  #alias function for components (NASIS)
  #returns list of component horizon ids
  return(intersectHorizon(pedon, z1, z2, hzid='chiid')) 
}


intersect.spc.by.depth <- function(p, top.depth, bottom.depth=NA, hzid='phiid', ...) {
  return(p[, which(horizons(p)[[hzid]] %in% 
                     intersectHorizon(p, top.depth, bottom.depth, hzid, ...))])
}

intersect.hz.by.depth <- function(p, top.depth, bottom.depth=NA, hzid='phiid', ...) {
  return(horizons(p)[horizons(p)[[hzid]] %in% 
                       intersect.horizon(p, top.depth, bottom.depth, hzid, ...),])
}

getHorizonAt50cm <- function(p) {
  return(intersect.hz.by.depth(p, 50))
}

getHorizons25to100cm <- function(p) {
  return(intersect.hz.by.depth(p, 25, 100))
}

getSPCAt50cm <- function(p) {
  return(intersect.spc.by.depth(p, 50))
}

getSPC25to100cm <- function(p, mineral.soil.surface = TRUE) {
  
  return(intersect.spc.by.depth(p, 25, 100))
}
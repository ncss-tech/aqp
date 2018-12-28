# intersect.horizon() 
# returns unique index to all horizons occuring over the depth interval [z1, z2]. 
# z2 is optional, in which case a single horizon with depth range containing z1 is returned
# several wrapper functions around this for hzid

#clod more is a ragged group of soil pedon horizons (each with distinctness, horizons boundaries)
# not resampled like slice or slab.

clod <- function(p, z1, z2=NA, as.list = FALSE) {
  #intersect horizons by depth; internal/shorthand alias? 
  # less typing is good, and i was trying to think of a slice/slab analogy
  hzid <- hzidname(p)
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
  
  if(!is.na(z2)) {
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
  
  return(list(hz.idx = idx.top, value = idval))
}

spc.by.z <- function(p, top.depth, bottom.depth=NA) {
  return(p[, which(horizons(p)[[hzidname(p)]] %in%  hz.dz(p, top.depth, bottom.depth, as.list=F))])
}

hz.by.z <- function(p, top.depth, bottom.depth=NA, ...) {
  hzid <- hzidname(p)
  return(horizons(p)[horizons(p)[[hzidname(p)]] %in% 
                       intersect.horizon(p, top.depth, bottom.depth, ...),])
}

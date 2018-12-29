# glom returns a "clod" of horizons that have a common attribute (currently just depth interval supported)

# returns unique index to all horizons occuring over the depth interval [z1, z2]. 
# z2 is optional, in which case a single horizon with depth range containing z1 is returned
# several wrapper functions around this for hzid

# a 'clod' is a ragged group of soil pedon horizons (each with distinctness, horizons boundaries)
# a clod references the original pedon data, it is not resampled like a slice or slab. 

# the verb/function that creates a clod is "glom" 
# "to glom" is "to steal" or to "become stuck or attached to". it is related to the 
# compound "glomalin", which is a glycoprotein produced by mycorrhizal fungi in soil

# gloms a set of horizons for a single-profile SPC `p`
#  the horizons are aggregated by depth using clod.hz.ids() 
glom <- function(p, z1, z2=NA, as.data.frame = FALSE) {
  # aka glom.by.depth; just one type of glomming of many that we can support
  if(!as.data.frame) {
    return(p[, which(hzID(p) %in% clod.hz.ids(p, z1, z2))]) 
  } else {
    return(horizons(p)[hzID(p) %in% clod.hz.ids(p, z1, z2),])
  }
}

# create a list of horizons comprising a "clod" by intersection of horizons by depth
clod.hz.ids <- function(p, z1, z2=NA, as.list = FALSE) {
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
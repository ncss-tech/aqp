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
    return(p[, which(hzID(p) %in% clod.hz.ids.v2(p, z1, z2))]) 
  } else {
    return(horizons(p)[hzID(p) %in% clod.hz.ids.v2(p, z1, z2),])
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

clod.hz.ids.v2 <- function (p, z1, z2 = NA, as.list = FALSE) 
{
  # access SPC slots to get important info about p
  hz <- horizons(p)
  dz <- horizonDepths(p)
  hzid <- hzidname(p)
  
  # get top and bottom horizon depth vectors
  tdep <- hz[[dz[1]]]
  bdep <- hz[[dz[2]]]
  
  # determine top depths greater than z1
  gt1 <- tdep > z1
  
  # include top depths equal to z1
  gt1[which(tdep == z1)] <- TRUE 
  
  # include horizons whose bottom portion are below z1
  gt1 <- gt1 | (bdep > z1)
  
  # get the index of the first horizon
  idx.top <- which(gt1)
  
  if (!length(idx.top)) 
    stop('Invalid horizon index. Check argument `z1`.')
  
  idx.top <- idx.top[1] # always returns the top horizon
  
  # if a bottom depth of the clod interval is specified
  if (!is.na(z2)) {
    # determine bottom depths less than z2
    lt2 <- bdep < z2 
    
    # include bottom depths equal to z2
    lt2[which(bdep == z2)] <- TRUE 
    
    # include horizons whose top portion are above z2
    lt2 <- lt2 | (tdep < z2)
    
    # get index of last horizon
    idx.bot <- rev(which(lt2))
    
    if (!length(idx.bot)) 
      stop('Invalid horizon index. Check arguments `z1` and `z2`.')
    
    idx.bot <- idx.bot[1]
    
    # not really sure how this could happen ... maybe with wrong depth units for z?
    if(!(all(idx.top:idx.bot %in% 1:nrow(p))))
      stop('Invalid horizon index. Check arguments `z1` and `z2`.')
    
    # get the ID values out of horizon table
    idval <- hz[idx.top:idx.bot, hzid]
    
    # just the horizon IDs
    if (!as.list) 
      return(idval)
    
    # list result.
    return(list(hzid = hzid, hz.idx = idx.top:idx.bot, value = idval))
  }
  
  idval <- hz[idx.top, hzid]
  
  if (!as.list) 
    return(idval)
  
  return(list(hz.idx = idx.top, value = idval))
}

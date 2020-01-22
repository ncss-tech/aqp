# glom returns a "clod" of horizons that have a common attribute (currently just depth interval supported)

# the verb/function that creates a clod is "glom" 
# "to glom" is "to steal" or to "become stuck or attached to". it is related to the 
# compound "glomalin", which is a glycoprotein produced by mycorrhizal fungi in soil

# gloms a set of horizons for a single-profile SPC `p`
#  the horizons are aggregated by depth using 
#  clod.hz.ids() defined below
glom <- function(p, z1, z2=NA, as.data.frame = FALSE) {
  # aka glom.by.depth; 
  if(length(p) > 1)
    stop("glom is intended for single-profile SPCs", call.=FALSE)
  
  idx <- clod.hz.ids(p, z1, z2)
  if(!all(is.na(idx))) {
    if(!as.data.frame) {
      return(p[, which(hzID(p) %in% idx)]) 
    } else {
      return(horizons(p)[hzID(p) %in% idx,])
    }
  } else {
    return(NA)
  }
}

# returns unique index to all horizons occuring over the depth interval [z1, z2]. 
# z2 is optional, in which case a single horizon with depth range containing z1 is returned
clod.hz.ids <- function (p, z1, z2 = NA, as.list = FALSE) 
{
  # access SPC slots to get important info about p
  hz <- horizons(p)
  dz <- horizonDepths(p)
  hzid <- hzidname(p)
  
  # get top and bottom horizon depth vectors
  tdep <- hz[[dz[1]]]
  bdep <- hz[[dz[2]]]
  
  # short circuit test of hz logic
  depthlogic <- .hzTests(tdep, bdep)
  logic_tests <- c('depthLogic','missingDepth','overlapOrGap')
  logic_fail <- as.logical(depthlogic[logic_tests])
  if(any(logic_fail)) {
    # too much could possibly go wrong in presence of overlaps/gaps/missing depths etc
    warning(paste0('Horizon logic error(s) ',
                   paste0(logic_tests[logic_fail], collapse=","),
                   ' found. Returning `NA` for ',idname(p),': ',
                   profile_id(p)), call.=FALSE)
    # returning NA for hz logic errors is too heavy-handed - warnings sufficient
    #return(NA)
  }
  
  # determine top depths greater/equal to z1
  gt1 <- tdep >= z1
  
  # include horizons whose bottom portion are below z1
  gt1 <- gt1 | (bdep > z1)
  
  # get the index of the first horizon
  idx.top <- which(gt1)
  
  if (!length(idx.top)) {
    warning(paste0('Invalid upper bound. Check argument `z1`. Returning `NA` (',idname(p),': ', profile_id(p),')'), call.=FALSE)
    return(NA)
  }
  
  idx.top <- idx.top[1] # always returns the top horizon
  
  if(z1 < tdep[idx.top]) {
    warning(paste0('Upper boundary `z1` (',z1,') shallower than top depth (',tdep[idx.top],') of shallowest horizon in subset. (',idname(p),': ', profile_id(p), ')'), call.=FALSE)
  }
  
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
    
    if (!length(idx.bot)) {
      warning('Invalid lower bound. Check arguments `z1` and `z2`. Returning `NA` (',idname(p),':', profile_id(p),')', call.=FALSE)
      return(NA)
    }
    
    idx.bot <- idx.bot[1]
    
    if(z2 > bdep[idx.bot]) {
      warning(paste0('Lower boundary `z2` (',z2,') is deeper than bottom depth of deepest horizon (', bdep[idx.bot],') in subset. (',idname(p),': ', profile_id(p),")"), call.=FALSE)
    }
    
    # not really sure how this could happen ... maybe with wrong depth units for z?
    if(!(all(idx.top:idx.bot %in% 1:nrow(p)))) {
      warning('Invalid lower bound. Check arguments `z1` and `z2`. Returning `NA` (',idname(p),':', profile_id(p),')', call.=FALSE)
      return(NA)
    }
    
    # warn if incomplete result
    target.thickness <- z2 - z1
    actual.thickness <- sum(bdep[idx.top:idx.bot] - tdep[idx.top:idx.bot])
    
    if(actual.thickness < target.thickness) {
      warning(paste0('Missing data in glom interval (actual/target: ', actual.thickness, '/',target.thickness,' ', depth_units(p), ' (',idname(p),': ', profile_id(p), ')'), call.=FALSE)
      
      if(z1 < tdep[idx.top]) {
        warning(paste0('`z1` (',z1,') shallower than top depth (',tdep[idx.top],') of shallowest horizon. (',idname(p),': ', profile_id(p), ')'), call.=FALSE)
      }
      if(z2 > bdep[idx.bot]) {
        warning(paste0('`z2` (',z2,') deeper than bottom depth of deepest horizon (', bdep[idx.bot],'). (',idname(p),': ', profile_id(p),")"), call.=FALSE)
      }
    }
    
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

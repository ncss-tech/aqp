# glom returns a "clod" of horizons that have a common attribute (currently just depth interval supported)

# the verb/function that creates a clod is "glom" 
# "to glom" is "to steal" or to "become stuck or attached to". it is related to the 
# compound "glomalin", which is a glycoprotein produced by mycorrhizal fungi in soil

# gloms a set of horizons for a single-profile SPC `p`
#  the horizons are aggregated by depth using 
#  clod.hz.ids() defined below
glom <- function(p, z1, z2 = NA, 
                 ids = FALSE, df = FALSE, 
                 truncate = FALSE, invert = FALSE, 
                 modality = "all") {
  # aka glom.by.depth; 
  if(!inherits(p, 'SoilProfileCollection') | length(p) > 1) {
    # TODO: alternately, recursively call glom by invoking glomApply with constant depths z1, z2?
    stop("`p` must be a SoilProfileCollection containing one profile", call.=FALSE)
  }
  
  depthn <- horizonDepths(p)
  
  if(!invert) {
    idx <- clod.hz.ids(p, z1, z2, modality)
  } else {
    idx <- c(clod.hz.ids(p, min(p[[depthn[1]]], na.rm=T), z1, modality),
             clod.hz.ids(p, z2, max(p[[depthn[2]]], na.rm=T), modality))
  }
  
  # short circuit to get hzIDs of result
  if(ids) {
    return(hzID(p)[idx])
  }
  
  # truncate ragged edges to PSCS
  # if we have an interval [z1, z2], option to truncate to it
  if(!invert  & truncate & !is.null(z2)) {
    .top <- p[[depthn[1]]]
    .bottom <- p[[depthn[2]]]
    
    .top[.top < z1] <- z1
    .bottom[.bottom < z1] <- z1
    
    .top[.top > z2] <- z2
    .bottom[.bottom > z2] <- z2
    
    p[[depthn[1]]] <- .top
    p[[depthn[2]]] <- .bottom
    
  } else if (invert & truncate & !is.null(z2)) {
    .top <- p[[depthn[1]]]
    .bottom <- p[[depthn[2]]]
    
    .top[.top > z1 & .top < z2] <- z2
    .bottom[.bottom > z1 & .bottom < z2] <- z1
    
    p[[depthn[1]]] <- .top
    p[[depthn[2]]] <- .bottom
  }
  
  if(!all(is.na(idx))) {
    if(!df) {
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
clod.hz.ids <- function (p, z1, z2 = NA, modality = "all", as.list = FALSE) 
{
  # access SPC slots to get important info about p
  hz <- horizons(p)
  dz <- horizonDepths(p)
  hzid <- hzidname(p)
  
  # get top and bottom horizon depth vectors
  tdep <- hz[[dz[1]]]
  bdep <- hz[[dz[2]]]
  
  # short circuit test of hz logic
  depthlogic <- hzDepthTests(tdep, bdep)
  logic_tests <- c('depthLogic','missingDepth','overlapOrGap')
  logic_fail <- as.logical(depthlogic[logic_tests])
  
  if(any(logic_fail)) {
    warning(paste0('Horizon logic error(s) ',
                   paste0(logic_tests[logic_fail], collapse=","),
                   ' found. (',idname(p),': ',
                   profile_id(p),")"), call.=FALSE)
  }
  
  # determine top depths greater/equal to z1
  gt1 <- tdep >= z1
  
  # include horizons whose bottom portion are below z1
  gt1 <- gt1 | (bdep > z1)
  
  # get the index of the first horizon
  idx.top <- which(gt1)
  
  if (!length(idx.top)) {
    warning(paste0('Invalid upper bound `z1` (',z1,'), returning `NULL` (',
                   idname(p),': ', profile_id(p),')'), call.=FALSE)
    return(NULL)
  }
  
  # always returns the top horizon
  idx.top <- idx.top[1] 
  
  if(z1 < tdep[idx.top]) {
    warning(paste0('Upper boundary `z1` (',z1,') shallower than top depth (',
                   tdep[idx.top],') of shallowest horizon in subset. (',
                   idname(p),': ', profile_id(p), ')'), call.=FALSE)
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
      warning('Invalid bounds (',z1,"-",z2,') returning `NULL` (',
              idname(p),':', profile_id(p),')', call.=FALSE)
      return(NULL)
    }
    
    idx.bot <- idx.bot[1]
    
    if(z2 > bdep[idx.bot]) {
      warning(paste0('Lower boundary `z2` (',z2,
                     ') is deeper than bottom depth of deepest horizon (',
                     bdep[idx.bot],') in subset. (',idname(p),': ',
                     profile_id(p),")"), call.=FALSE)
    }
    
    # not really sure how this could happen ... maybe with wrong depth units for z?
    if(!(all(idx.top:idx.bot %in% 1:nrow(p)))) {
      warning('Invalid bounds (',z1,"-",z2,') returning `NULL` (',
              idname(p),':', profile_id(p),')', call.=FALSE)
      return(NULL)
    }
    
    # warn if incomplete result
    target.thickness <- z2 - z1
    actual.thickness <- sum(bdep[idx.top:idx.bot] - tdep[idx.top:idx.bot])
    
    if(actual.thickness < target.thickness) {
      warning(paste0('Missing data in glom interval (actual/target: ', 
                     actual.thickness,'/',target.thickness,' ',
                     depth_units(p),' (',idname(p),': ', profile_id(p), ')'), call.=FALSE)
      
      if(z1 < tdep[idx.top]) {
        warning(paste0('glom boundary `z1` (',z1,') shallower than top depth (',tdep[idx.top],
                       ') of shallowest horizon. (',idname(p),': ', profile_id(p), ')'), call.=FALSE)
      }
      
      if(z2 > bdep[idx.bot]) {
        warning(paste0('`z2` (',z2,') deeper than bottom depth of deepest horizon (', bdep[idx.bot],
                       '). (',idname(p),': ', profile_id(p),")"), call.=FALSE)
      }
    }
    
    if(modality == "thickest") {
      sub.thk <- bdep[idx.top:idx.bot] - tdep[idx.top:idx.bot]
      max.sub.thk <- max(sub.thk)
      first.thickest.idx <- which(sub.thk == max.sub.thk)[1]
      idx.top <- idx.bot <- idx.top - 1 + first.thickest.idx
    }
    
    # get the ID values out of horizon table
    idval <- hz[idx.top:idx.bot, hzid]
    
    # just the horizon IDs
    if (!as.list) 
      return(idval)
    
    # list result.
    return(list(hzid = hzid, hz.idx = idx.top:idx.bot, value = idval))
  }
  
  if(modality == "thickest") {
    first.thickest.idx <- which(bdep - tdep == max(bdep - tdep))[1]
    idx.top <- first.thickest.idx
  }
  
  idval <- hz[idx.top, hzid]
  
  if (!as.list) {
    return(idval)
  }
  
  return(list(hz.idx = idx.top, value = idval))
}

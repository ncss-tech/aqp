# library(aqp)
# library(soilDB)
# 
# f <- fetchNASIS() 

# this is a helper function to calculate maximum depth of occurence of a specified horizon designation pattern
#  it is used primarily in the place of functions that are capable of reasoning over taxonomic criteria for things
#  like calcic, spodic, natric horizons -- and temporarily for cambic horizons too :*(

depth.of <- function(p, pattern, top = TRUE, hzdesgn = guessHzDesgnName(p), 
                     no.contact.depth = NULL, no.contact.assigned=NULL) {
  hz.match <- horizons(p)[grepl(pattern, p[[hzdesgn]]),]
  
  if(!nrow(hz.match))
    return(no.contact.assigned)
  
  res <- hz.match[[horizonDepths(p)[ifelse(top, 1, 2)]]]
  
  if(any(res > no.contact.depth))
    res <- res[-which(res > no.contact.depth)]
  
  if(all(!is.na(res)) & length(res))
    return(res)
  
  return(no.contact.assigned)
}

max.depth.of <- function(p, pattern, top = TRUE, hzdesgn = guessHzDesgnName(p), 
                         no.contact.depth = NULL, no.contact.assigned=NULL) {
  res <- suppressWarnings(min(depth.of(p, pattern=pattern, top=FALSE, 
                                       no.contact.depth, no.contact.assigned), na.rm=T))
  if(is.infinite(res))
    return(no.contact.assigned)
  return(res)
}

min.depth.of <- function(p, pattern, top = TRUE, hzdesgn = guessHzDesgnName(p), 
                         no.contact.depth = NULL, no.contact.assigned=NULL) {
  res <- suppressWarnings(min(depth.of(p, pattern=pattern, top=FALSE, 
                                       no.contact.depth, no.contact.assigned), na.rm=T))
  if(is.infinite(res))
    return(no.contact.assigned)
  return(res)
}

# calculates the minimum thickness of a mollic epipedon per requirements in criterion 6 of mollic epipedon;
#   keys to soil taxonomy 12th edition
# truncate=TRUE will truncate sliding scale thicknesses to [18,25]
# otherwise returns "most restrictive" thickness which is helpful to see where sliding scale is used

mollic.thickness.requirement <- function(p, truncate=TRUE) {
  
  # determine boundaries  
  # For purposes of identification of minimum thickness of mollic for field descriptions
  #   technically it is not applying the true taxonomic rules b/c it is based on hz desgn
  mss <- getMineralSoilSurfaceDepth(p)
  
  soil_depth <- min.depth.of(p, "Cr|R|Cd|m", 
                         no.contact.depth = 200, 
                         no.contact.assigned = NA)
  
  if(!is.na(soil_depth) & soil_depth == 0) {
    return(10)
  }
  
  #  if criteria aren't met in at least some part of upper 25 of mineral soil material, they won't be met deeper
  #  it is possible there are criteria greater than 25cm that affect total mollic thickness, but here we
  #  are only calculating the thickness _requirement_
  
  # get horizon data within mineral soil surface "critical zone"
  epi <- glom(p, mss, min(soil_depth, 25, na.rm=TRUE), truncate=TRUE)
  if(!inherits(epi, 'SoilProfileCollection'))
    return(NA)
  
  # get horizon data from mineral soil surface to bedrock, physical root restriction, 
  #  or pedogenic cementation >90% OR bottom of profile OR 250
  soil.bottom <- min(soil_depth, max(p[[horizonDepths(p)[1]]], na.rm=TRUE), 
                     250, na.rm = TRUE)
  sol <- glom(p, mss, soil.bottom, truncate=TRUE)
  
  if(!inherits(sol, 'SoilProfileCollection'))
    return(NA)
  
  # 6C1 - TODO: create functions for testing basic field criteria for these diagnostics
  #       and lab data extension to support carbonates/calcic
  
  # Again, for purposes of identification of minimum thickness of mollic this is fast and 
  #   probably fine 95% of the time
  #   but technically it is not applying the true taxonomic rules b/c it is based on hz desgn
  cemented_depth <- depth.of(p, "m", no.contact.depth = 0, no.contact.assigned = NA)
  carbonate_depth <- depth.of(p, "k", no.contact.depth = 0, no.contact.assigned = NA)
  fragipan_depth <- depth.of(p, "x", no.contact.depth = 0, no.contact.assigned = NA)
  
  # calculate "shallowest of secondary carbonates/calcic, petrocalcic, duripan, fragipan" 
  crit6c1 <- suppressWarnings(min(carbonate_depth, 
                                  fragipan_depth, 
                                  cemented_depth, na.rm=TRUE))
  
  # 6C2 - identify argillic boundaries via aqp::getArgillicBounds  
  # TODO: create functions for testing basic field criteria for these diagnostics (color structure)
  #       and lab data extension for spodic materials; can use logic from "sandy cambic" demo -- but more is needed
  argi_bounds <- profileApply(p, getArgillicBounds, 
                              hzdesgn = hzdesgnname(p), 
                              texcl.attr = hztexclname(p))
  argillic_bottom <- NA
  if(is.finite(argi_bounds[2])) {
    argillic_bottom <- argi_bounds[2]
  }
  
  # AGAIN, for purposes of identification of minimum thickness of mollic this is fast and probably fine
  #   but technically it is not applying the true taxonomic rules b/c it is based on hz desgn
  natric_bottom <- max.depth.of(p, pattern="n", top=FALSE, 
                                no.contact.assigned = NA)
  oxic_bottom <- max.depth.of(p, pattern="o", top=FALSE, 
                              no.contact.assigned = NA)
  spodic_bottom <- max.depth.of(p, pattern="h|s", top=FALSE, 
                                no.contact.assigned = NA)
  
  #   the B|w == cambic was particularly egregious 
  cambic_bottom <- max(getPseudoCambicBounds(p, argi_bounds=argi_bounds))
  
  # calculate "deepest of lower boundary of argillic/natric, cambic, oxic, spodic"
  crit6c2 <- suppressWarnings(max(c(argillic_bottom, 
                                    natric_bottom, 
                                    cambic_bottom, 
                                    oxic_bottom, 
                                    spodic_bottom), na.rm=TRUE))
  
  # SHORT CIRCUITS
  sandy.textures <- (grepl("S$", epi[[hztexclname(epi)]], ignore.case = TRUE) & 
                     !grepl("LVFS|LFS$", epi[[hztexclname(epi)]], ignore.case = TRUE))
  
  if(all(sandy.textures)) {
    # 6A - assumes you must check for sandy textures throughout 0-25 to trigger minimum thickness of 25cm
    #      there is an implicit assumption that after mixing any non-sandy texture into sandy texture you would have 
    #      non-sandy. this logic could be altered to be more "restrictive" by changing all() to any() -- forcing the 
    #      25cm requirement. in that case, _any_ sandy texture would imply sandy textures, importantly: precluding 
    #      the 10cm requirement.
      return(25) 
  } else {
    if(max(epi$hzdepb, na.rm=TRUE) < 25) {
    # 6B - if all horizons above a contact are non-sandy and meet all mollic characteristics then
    #      the minimum thickness could be only 10cm, we have filtered out sandy textures, so check where it matters
    #      technically, the 10cm requirement requires knowledge of whether other mollic requirements are met, 
    #      so gives the epipedon a somewhat circular definition. This case applies in deep soils with mollic materials
    #      all the way to contact -- but only practically matters where a contact is less than 25cm depth, 
    #      so that is when we return it.
      return(10)
    }
  }
  
  # If no diagnostics are present...
  if(!is.finite(crit6c1) & !is.finite(crit6c2)) {
    # TODO: "fluventic" soils with no other diagnostics have minimum thickness of 25cm
    if(grepl("fluv|cumulic", p$taxsubgrp)) {
      # for now, hope that the assigned taxonomic class reflects floodplain condition where it matters
      ## for example:
      # tolerance <- 0.1  # set a threshold for "different" carbon contents of 0.1%
      #                   #   carbon-depth "fluctuations" less or equal to tolerance will be ignored
      #                   # for different data sources/levels of horizon detail this number may need to be higher?
           
      # irregular.decrease <- diff(epi$estimated_organic_carbon) < 0
      # if(length(irregular.decrease[irregular.decrease >= tolerance]) {
      #  return(25)
      # }      
      # TODO: most pedons don't have OC measured, develop color-carbon-depth-spatial surrogate model? 
      return(25)
    }
    # this is criterion 6d (if "none of above" apply)
    return(18)
  }
  
  # calculate the most restrictive requirement from 6a, 6b, 6c 
  #   which contain several restatements of the fundamental criteria for a sliding scale thickness
  sixcdepths <- c(crit6c1 / 3, crit6c2 / 3)
  
  # only some diagnostics are present per subcriteria 1 and 2
  no.diag <- which(is.infinite(sixcdepths))
  if(length(no.diag)) {
    sixcdepths <- sixcdepths[-no.diag]
  }
  
  # debug: see "true" (unrestricted 18-25 depth)
  # print(max(sixcdepths))
  
  # the thickness requirement is based on the most restrictive condition
  sixcdepths.t <- sixcdepths
  
  # sliding scale depths based on diagnostics are truncated to [18, 25]
  #  for investigations evaluating particular criteria, may be useful to ignore this limit
  if(truncate) {
    sixcdepths.t[sixcdepths > 25] <- 25
    sixcdepths.t[sixcdepths < 18] <- 18
  }
  most.restrictive <- max(sixcdepths.t)
  
  # this should not happen with properly populated pedons 
  #  (missing horizon designations or bottom depths?)
  if(is.infinite(most.restrictive)) {
    warning(paste0("cannot evaluate mollic miniumum thickness requirement (",
                   idname(p),":",profile_id(p),")"))
  }
  
  return(most.restrictive)
}

getPseudoCambicBounds <- function(p, ..., 
                                  argi_bounds=NULL,
                                  d_value = "d_value", 
                                  m_value = "m_value", 
                                  m_chroma = "m_chroma") {
  depths <- horizonDepths(p)
  
  if(is.null(argi_bounds)) {
    argi_bounds <- getArgillicBounds(p, ...)
  }
  
  cambic_top <- min.depth.of(p, pattern="B", 
                             no.contact.assigned = NA)
  cambic_bottom <- max.depth.of(p, pattern="B", top=FALSE, 
                                no.contact.assigned = NA)
  
  if(any(is.na(cambic_top), is.na(cambic_bottom))) {
    return(NA)
  }
  
  cambic <- glom(p, cambic_top, cambic_bottom, truncate=TRUE)
  
  if(!all(is.na(argi_bounds))) {
    if(all(c(cambic_bottom <= argi_bounds[2], cambic_top >= argi_bounds[1]))) {
      return(NA) 
    }
    # if an argillic is presnt, remove with glom truncate+invert
    non.argillic <- glom(cambic, argi_bounds[1], argi_bounds[2], 
                         truncate=TRUE, invert=TRUE)
  } else {
    non.argillic <- cambic
  }
    
  dark.colors <- has.dark.colors(non.argillic)
  non.argillic$w <- rep(1, nrow(non.argillic))
  
  textures <- non.argillic[[hztexclname(non.argillic)]]
  sandy.textures <- (grepl("S$", textures, ignore.case = TRUE) & 
                       !grepl("LVFS|LFS$", textures, ignore.case = TRUE))
  
  if(!length(sandy.textures) | !length(dark.colors)) {
    return(NA)
  }
  
  # remove horizons that are sandy or have dark colors
  if(any(sandy.textures | dark.colors, na.rm = TRUE)) {
    non.argillic$w[which(sandy.textures | dark.colors)] <- 0
  }
  
  nhz <- horizons(non.argillic)
  
  final <- numeric(0)
  # iterate through combinations of horizons, check topology and thickness
  # finds multiple occurences of cambic horizons, excluding argillics
  for(j in 1:nrow(nhz)) {
    for(i in j:nrow(nhz)) {
      if(any(hzDepthTests(nhz[j:i, depths[1]], nhz[j:i, depths[2]]))) {
        i <- i - 1
        break;
      }
    }
    pcamb.thickness <- nhz[j:i, depths[2]] - nhz[j:i, depths[1]]
    if(sum(pcamb.thickness) >= 15) {
        final <- c(final, c(min(nhz[j:i, depths[1]], na.rm=T), 
                            max(nhz[j:i, depths[2]], na.rm=T)))
    }
  }
  return(final)
}

# default arguments are for mollic/umbric darkness, 
#   results are combined after being applied to both dry and moist colors
has.dark.colors <-  function(p, dvalnm = "d_value", dchrnm="d_chroma", 
                             mvalnm = "m_value", mchrnm = "m_chroma",
                             d_value=5, d_chroma=NA, m_value=3, m_chroma=3) {
  hz <- horizons(p)
  r1 <- hz[[dvalnm]] <= d_value
  r2 <- hz[[dchrnm]] <= d_chroma
  r3 <- hz[[mvalnm]] <= m_value
  r4 <- hz[[mchrnm]] <= m_chroma
  
  r <- cbind(r1, r2, r3, r4)
  
  required <- !is.na(c(d_value, d_chroma, m_value, m_chroma))
  risna <- apply(r, 2, function(x) all(is.na(x)))
  
  # if all data are missing, return NA for each horizon
  if(all(required == risna))
    return(rep(NA, nrow(p)))
  
  r <- r[,required]
  
  # result may contain NA for individual horizons
  return(apply(r, 1, all))
}

# library(aqp)
# library(soilDB)
# 
# f <- fetchNASIS() 

# this is a helper function to calculate maximum depth of occurence of a specified horizon designation pattern
#  it is used primarily in the place of functions that are capable of reasoning over taxonomic criteria for things
#  like calcic, spodic, natric horizons -- and temporarily for cambic horizons too :*(
max.depth.of <- function(p, pattern, top = TRUE, hzdesgn = guessHzDesgnName(p), 
                         no.contact.depth = NULL, 
                         no.contact.assigned = NULL, na.rm = TRUE) {
  res <- depth.of(p, pattern, top, hzdesgn, no.contact.depth, no.contact.assigned)
  return(max(res, na.rm))
}

min.depth.of <- function(p, pattern, top = TRUE, hzdesgn = guessHzDesgnName(p), 
                         no.contact.depth = NULL, 
                         no.contact.assigned = NULL, na.rm = TRUE) {
  res <- depth.of(p, pattern, top, hzdesgn, no.contact.depth, no.contact.assigned)
  return(min(res, na.rm))
}

depth.of <- function(p, pattern, top = TRUE, hzdesgn = guessHzDesgnName(p), 
                     no.contact.depth = NULL, no.contact.assigned=NULL) {
  hz.match <- horizons(p)[grepl(pattern, p[[hzdesgn]]),]
  
  if(!nrow(hz.match))
    return(no.contact.assigned)
  
  res <- hz.match[[horizonDepths(p)[ifelse(top, 1, 2)]]]
  
  if(any(res > no.contact.depth))
    res <- res[-which(res > no.contact.depth)]
  
  if(all(is.na(res)) & length(res))
    return(res[1])
  
  return(no.contact.assigned)
}

# calculates the minimum thickness of a mollic epipedon per requirements in criterion 6 of mollic epipedon;
#   keys to soil taxonomy 12th edition
mollic.thickness.requirement <- function(p) {
  
  # determine boundaries  
  # For purposes of identification of minimum thickness of mollic for field descriptions
  #   technically it is not applying the true taxonomic rules b/c it is based on hz desgn
  mss <- getMineralSoilSurfaceDepth(p)
  
  soil_depth <- depth.of(p, "Cr|R|Cd|m", 
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
  soil.bottom <- min(soil_depth, max(p[[horizonDepths(p)[1]]], na.rm=TRUE), 250, na.rm = TRUE)
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
  natric_bottom <- max.depth.of(p, pattern="n", top=FALSE, no.contact.assigned = NA)
  oxic_bottom <- max.depth.of(p, pattern="o", top=FALSE, no.contact.assigned = NA)
  spodic_bottom <- max.depth.of(p, pattern="h|s", top=FALSE, no.contact.assigned = NA)
  #   the w == cambic is particularly egregious and prone to errors -- fix ASAP
  cambic_bottom <- max.depth.of(p, pattern="w", top=FALSE, no.contact.assigned = NA)
  
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
  sixcdepths.t[sixcdepths > 25] <- 25
  sixcdepths.t[sixcdepths < 18] <- 18
  most.restrictive <- max(sixcdepths.t)
  
  # this should not happen with properly populated pedons (missing horizon designations or bottom depths?)
  if(is.infinite(most.restrictive)) {
    warning(paste0("cannot evaluate mollic miniumum thickness requirement (",idname(p),":",profile_id(p),")"))
  }
  return(most.restrictive)
}

# default arguments are for mollic/umbric darkness, 
#   results are combined after being applied to both dry and moist colors
has.dark.colors <-  function(p, d_value=5, d_chroma=NA, m_value=3, chroma=3) {
  hz <- horizons(p)
  
}

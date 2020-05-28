# finds all horizons that are possibly part of a cambic horizon
# excluding those that are part of an argillic horizon
# (defined either by depth interval or getArgillicBounds)

#' Find all intervals that are potentially part of a Cambic horizon
#' @description Find all intervals that are potentially part of a Cambic horizon excluding those that are part of an argillic horizon (defined either by depth interval or \code{getArgillicBounds()}). 
#' 
#' There may be multiple cambic horizons (indexes) in a profile. Each cambic index has a top and bottom depth associated: cambic_top and cambic_bottom. This result is designed to be used for single profiles, or with \code{profileApply(..., frameify = TRUE)}
#'
#' @param p A single-profile SoilProfileCollection
#' @param hzdesgn Column name containing horizon designations.
#' @param texcl.attr Arguments to \code{getArgillicBounds()}
#' @param clay.attr Arguments to \code{getArgillicBounds()}
#' @param ... Arguments to \code{getArgillicBounds()}
#' @param argi_bounds Optional: numeric vector of length 2 with top and bottom of argillic; (Default: NULL)
#' @param d_value Column name containing dry value. Default: d_value
#' @param m_value Column name containing moist value. Default: m_value
#' @param m_chroma Column name containing moist crhoma. Default: m_chroma
#'
#' @return A \code{data.frame} containing profile, cambic indexes, along with top and bottom depths.
#' 
#' @author Andrew G. Brown
#' 
#' @export
#'
#' @examples 
#' # construct a fake profile
#' spc <- data.frame(id=1, taxsubgrp = "Lithic Haploxerepts",
#'                   hzname   = c("A","AB","Bw","BC","R"),
#'                   hzdept   = c(0,  20, 32, 42,  49), 
#'                   hzdepb   = c(20, 32, 42, 49, 200), 
#'                   clay     = c(19, 22, 22, 21,  NA),
#'                   texcl    = c("l","l","l", "l","br"),
#'                   d_value  = c(5,   5,  5,  6,  NA),
#'                   m_value  = c(2.5, 3,  3,  4,  NA),
#'                   m_chroma = c(2,   3,  4,  4,  NA))
#'                   
#' # promote to SoilProfileCollection
#' depths(spc) <- id ~ hzdept + hzdepb
#' hzdesgnname(spc) <- 'hzname'
#' hztexclname(spc) <- 'texcl'
#' 
#' # print results in table
#' getCambicBounds(spc)
#'
getCambicBounds <- function(p, hzdesgn, texcl.attr, clay.attr,
                            argi_bounds = NULL,
                            d_value = "d_value", 
                            m_value = "m_value", 
                            m_chroma = "m_chroma") {
  # construct data.frame result for no-cambic-found (NA)
  empty_frame <- data.frame(id=profile_id(p),
                            cambic_id=NA, cambic_top=NA, cambic_bottom=NA)
  empty_frame_names <- names(empty_frame)
  empty_frame_names[1] <- idname(p)
  names(empty_frame) <- empty_frame_names
  
  depths <- horizonDepths(p)
  
  if(is.null(argi_bounds)) {
    argi_bounds <- getArgillicBounds(p, hzdesgn, clay.attr, texcl.attr, ...)
  }
  
  cambic_top <- minDepthOf(p, pattern = "B",
                           no.contact.assigned = NA)
  cambic_bottom <- maxDepthOf(p, pattern = "B", top = FALSE, 
                              no.contact.assigned = NA)
  
  if(any(is.na(cambic_top), is.na(cambic_bottom))) {
    return(empty_frame)
  }
  
  cambic <- glom(p, cambic_top, cambic_bottom, truncate=TRUE)
  
  if(all(is.finite(argi_bounds))) {
    if(all(c(cambic_bottom <= argi_bounds[2], cambic_top >= argi_bounds[1]))) {
      return(empty_frame) 
    }
    # if an argillic is presnt, remove with glom truncate+invert
    non.argillic <- suppressWarnings(glom(cambic, argi_bounds[1], argi_bounds[2], 
                         truncate=TRUE, invert=TRUE))
    # commonly, a warning occurs due to argillic bottom depth at contact
  } else {
    non.argillic <- cambic
  }
  
  dark.colors <- hasDarkColors(non.argillic)
  non.argillic$w <- rep(1, nrow(non.argillic))
  
  textures <- non.argillic[[hztexclname(p)]]
  sandy.textures <- (grepl("S$", textures, ignore.case = TRUE) & 
                       !grepl("LVFS|LFS$", textures, ignore.case = TRUE))
  
  if(!length(sandy.textures) | !length(dark.colors)) {
    return(empty_frame)
  }
  
  nhz <- horizons(non.argillic)
  # remove horizons that are sandy or have dark colors
  if(any(sandy.textures | dark.colors, na.rm = TRUE)) {
    nhz <- nhz[-which(sandy.textures | dark.colors),] 
  }
  
  final <- data.frame(cambic_top=NA, cambic_bottom=NA)
  # iterate through combinations of horizons, check topology and thickness
  # finds multiple occurences of cambic horizons, excluding argillics
  for(j in 1:nrow(nhz)) {
    for(i in j:nrow(nhz)) {
      ftop <- nhz[j:i, depths[1]]
      fbot <- nhz[j:i, depths[2]]
      if(any(hzDepthTests(ftop, fbot))) {
        i <- i - 1
        break;
      }
    }
    pcamb.thickness <- ftop - fbot
    if(length(pcamb.thickness) > 0 &
       sum(pcamb.thickness, na.rm = TRUE) >= 15) {
      final <- rbind(final, data.frame(cambic_top = min(ftop, na.rm=T), 
                          cambic_bottom = max(fbot, na.rm=T)))
    }
  }
  # construct data.frame result
  final <- final[complete.cases(final),]
  if(nrow(final) == 0) {
    return(empty_frame)
  }
  iddf <- data.frame(id=profile_id(p), 
                     cambic_index=1:nrow(final))
  colnames(iddf) <- c(idname(p), "cambic_id")
  return(cbind(iddf, final))
}

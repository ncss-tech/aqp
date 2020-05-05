# finds all horizons that are possibly part of a cambic horizon
# excluding those that are part of an argillic horizon
# (defined either by depth interval or getArgillicBounds)

getCambicBounds <- function(p, ..., 
                            argi_bounds=NULL,
                            d_value = "d_value", 
                            m_value = "m_value", 
                            m_chroma = "m_chroma") {
  depths <- horizonDepths(p)
  
  if(is.null(argi_bounds)) {
    argi_bounds <- getArgillicBounds(p, ...)
  }
  
  cambic_top <- minDepthOf(p, pattern="B", 
                           no.contact.assigned = NA)
  cambic_bottom <- maxDepthOf(p, pattern="B", top=FALSE, 
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
  
  dark.colors <- hasDarkColors(non.argillic)
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
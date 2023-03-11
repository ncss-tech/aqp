#' @description  used to implement "drop=FALSE" for various methods that remove horizons from SoilProfileCollection object
#' @noRd
.insert_dropped_horizons <- function(object = SoilProfileCollection(), 
                                     horizons = horizons(object), 
                                     sites = site(object),
                                     pid = idname(object),
                                     depths = horizonDepths(object),
                                     SPC = TRUE) {
  
  s.i <- sites[[pid]]
  i.idx <- unique(horizons[[pid]])
  i.idx2 <- setdiff(s.i, i.idx)
  newid <- sites[[pid]][which(sites[[pid]] %in% i.idx2)]
  
  # create ID-only empty data using original data as templates
  h.empty <- horizons[0, , drop = FALSE][seq_along(i.idx2), , drop = FALSE]
  h.empty[[pid]] <- newid
  s.empty <- sites[0, , drop = FALSE][seq_along(i.idx2), , drop = FALSE]
  s.empty[[pid]] <- newid
  
  # reorder to original id (+ top depth for horizons)
  horizons <- rbind(horizons, h.empty)
  horizons <- horizons[order(horizons[[pid]], horizons[[depths[1]]]),]
  
  sites <- sites[which(!sites[[pid]] %in% h.empty[[pid]]), , drop = FALSE]
  sites <- rbind(sites, s.empty)
  sites <- sites[order(sites[[pid]]), , drop = FALSE]
  
  if (inherits(object, 'SoilProfileCollection') && SPC) {
    object@site <- sites
    replaceHorizons(object) <- horizons
    return(object)
  } else {
    return(list(
      horizons = horizons,
      sites = sites,
      pid = pid,
      depths = depths
    ))
  }
}

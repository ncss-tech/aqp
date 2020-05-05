#' Determine thickness of horizons (continuous from surface) matching a pattern
#' 
#' @description This function is used to find the thickness of arbitrary horizon designations that are continuous from the soil surface (depth = 0).
#' 
#' The horizon designation to match is specified with the regular expression pattern 'pattern'. All horizons matching that pattern, that are continuous from the soil surface, count towards the depth / thickness value that is ultimately returned. For instance: horizon designations: A1-A2-A3-C-Ab , would return A3 bottom depth given \code{pattern = "A"}.
#' 
#' getSurfaceHorizonDepth is used by getPlowLayerDepth for matching Ap horizons; and, it is used by getMineralSoilSurfaceDepth to find the thickness of O horizons in lieu of lab data. 
#' 
#' @param p A single-profile SoilProfileCollection object.
#' @param pattern Regular expression pattern to match for all horizons to be considered part of the "surface". 
#' @param hzdesgn Column name containing horizon designation. Default: \code{guessHzDesgnName(p)}.
#'
#' @return Returns a numeric value corresponding to the bottom depth of the last horizon matching 'pattern' that is contiguous with other matching horizons up to the soil surface (depth = 0).
#' 
#' @author Andrew G. Brown
#' 
#' @aliases getSurfaceHorizon
#' @usage getSurfaceHorizonDepth(p, hzdesgn = guessHzDesgnName(p), pattern)
#' 
#' @aliases getMineralSoilSurfaceDepth
#' @usage getMineralSoilSurfaceDepth(p, hzdesgn = guessHzDesgnName(p), pattern = "O")
#' 
#' @aliases getPlowLayerDepth
#' @usage getPlowLayerDepth(p, hzdesgn = guessHzDesgnName(p), pattern = "^Ap[^b]")
#' 
#' @export getSurfaceHorizonDepth
#' @export getMineralSoilSurfaceDepth
#' @export getPlowLayerDepth
#' 
#' @examples 
#' library(aqp)
#' data(sp1, package = 'aqp')
#' depths(sp1) <- id ~ top + bottom
#' site(sp1) <- ~ group
#' 
#' p <- sp1[1]
#' q <- sp1[2]
#' 
#' # look at horizon designations in p and q
#' p$name
#' q$name
#' 
#' # thickness of all surface horizons containing A
#' getSurfaceHorizonDepth(p, pattern = 'A', hzdesgn = 'name')
#' 
#' # thickness of all surface horizons that start with A
#' getSurfaceHorizonDepth(p, pattern = '^A', hzdesgn = 'name')
#' 
#' # thickness of all surface horizons that start with A, and the A is not followed by B
#' getSurfaceHorizonDepth(p, pattern = '^A[^B]', hzdesgn = 'name')
#' 
#' # thickness of all surface horizons that start with A 
#' #  followed by a number from _2_ to 9 (returns ZERO)
#' getSurfaceHorizonDepth(p, pattern = '^A[2-9]', hzdesgn = 'name')
#' 
#' # getPlowLayerDepth matches first two horizons in fake Ap horizon data with "buried Ap"
#' p$aphorizons <- c("Ap1","Ap2","AB", rep('C', nrow(p) - 4), "Apb")
#' getPlowLayerDepth(p, hzdesgn = 'aphorizons')
#' 
#' # getMineralSoilSurfaceDepthmatches first 3 horizons in fake O horizon data
#' p$ohorizons <- c("Oi1","Oi2","Oe", rep('C', nrow(p) - 4), "2C")
#' getMineralSoilSurfaceDepth(p, hzdesgn='ohorizons')
#' 
#' # matches first Oi horizon with original horizon designations of pedon 2
#' getMineralSoilSurfaceDepth(q, hzdesgn='name')
#' 
# getSurfaceHorizonDepth

# starting from the surface, match patterns to horizon
# return the last bottom depth of a horizon that is contiguous with surface
# for instance horizon designations: A1-A2-A3-C-Ab , would return A3 bottom depth
#
getSurfaceHorizonDepth <-  function(p, pattern, hzdesgn = guessHzDesgnName(p)) { 
  hz <- horizons(p)
  depths <- horizonDepths(p)
  shallowest.depth <- min(hz[[depths[1]]], na.rm=TRUE)
  
  if(is.infinite(shallowest.depth)) {
    warning(paste0("Profile (",profile_id(p),") is missing horizon top depths."))
    return(NA)
  }
    
  if(shallowest.depth > 0) {
    warning(paste0("Profile (",profile_id(p),") top depth is greater than zero."))
  }
  
  if(shallowest.depth < 0) {
    warning(paste0("Profile (",profile_id(p),") top depth is less than zero."))
  }
  
  match.idx <- grepl(hz[[hzdesgn]], pattern=pattern)
    
  if(length(which(match.idx)) < 1) {
    return(shallowest.depth)
  }
    
  mod.idx <- c(1, rep(0, length(match.idx) - 1))
  new.idx <- (match.idx + mod.idx) %% 3
  
  who.idx <- numeric(0)
  if(new.idx[1] == 2) {
    who.idx <- (rev(which(new.idx > 0 & new.idx <= 2))[1])
  }
  
  if(!length(who.idx)) {
    return(shallowest.depth)
  }
  
  return(hz[who.idx, depths[2]])
}
 
getMineralSoilSurfaceDepth <-  function(p, hzdesgn = guessHzDesgnName(p), pattern = "O") { 
  #assumes OSM is given O designation;
  #TODO: add support for lab-sampled organic measurements
  #      keep organic horizons with andic soil properties
  return(getSurfaceHorizonDepth(p, hzdesgn = hzdesgn, pattern = pattern))
}

getPlowLayerDepth <- function(p, hzdesgn = guessHzDesgnName(p), pattern = "^Ap[^b]") {
  return(getSurfaceHorizonDepth(p, hzdesgn = hzdesgn, pattern = pattern))
}

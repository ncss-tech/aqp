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
#' @param simplify logical. Return single profile results as vector (default: `TRUE`) or `data.frame` (`FALSE`)
#' @return Returns a numeric value corresponding to the bottom depth of the last horizon matching 'pattern' that is contiguous with other matching horizons up to the soil surface (depth = 0).
#'
#' @author Andrew G. Brown
#'
#' @aliases getMineralSoilSurfaceDepth
#' @aliases getPlowLayerDepth
#'
#' @export 
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
#' getSurfaceHorizonDepth(p, pattern = '^A[^B]*', hzdesgn = 'name')
#'
#' # thickness of all surface horizons that start with A
#' #  followed by a number from _2_ to 9 (returns ZERO)
#' getSurfaceHorizonDepth(p, pattern = '^A[2-9]*', hzdesgn = 'name')
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
getSurfaceHorizonDepth <- function(p, pattern, hzdesgn = guessHzDesgnName(p), simplify = TRUE) {
  hz <- data.table::as.data.table(horizons(p))
  depthz <- horizonDepths(p)
  
  .FIRST <- NULL
  .SD <- NULL
  
  shallowest.depth <- p[, , .FIRST][[depthz[1]]]
  
  .get_contiguous_surface_hz <- function(h, hzdesgn) {
    # identify horizons matching pattern
    match.idx <- grepl(h[[hzdesgn]], pattern = pattern)
    
    emptyres <- h[1, .SD, .SDcols = c(hzidname(p), depthz[2])] 
    emptyres[[idname(p)]] <- emptyres[[idname(p)]][1]
    emptyres[[hzidname(p)]] <- NA_character_
    emptyres[[depthz[2]]] <- as.double(0)
    emptyres$pattern <- pattern
    emptyres$hzdesgn <- hzdesgn
    
    # no match? return zero or shallowest top depth (the minimum depth)
    if (length(which(match.idx)) < 1) {
      return(emptyres)
    }
    
    # identify surface horizon
    mod.idx <- c(1, rep(0, length(match.idx) - 1))
    
    # identify where matches and surface horizon co-occur
    # matching horizons get a 1, matching surface horizon gets a 2,
    #   0s kick us out
    new.idx <- match.idx + mod.idx
    
    who.idx <- numeric(0)
    # we only have a matching surface if the first value is 2
    #  (meets both above crit)
    if (new.idx[1] == 2) {
      # convert that into logical to identify contiguous matches
      contig <- new.idx > 0 & new.idx <= 2
      
      # calculate difference between contiguous matches/nonmatches
      dcontig <- diff(as.integer(contig))
      
      # max depth is, at first, the bottom depth of last contiguous hz
      max.idx <- length(contig)
      
      if(length(max.idx)) {
        # if we have a negative change at any depth,
        #  we have a discontinuity
        
        # adjust max index (depth) accordingly
        if(any(dcontig < 0)) {
          # take first index of negative dcontig
          # add one to correct for indexing offset due to diff()
          max.idx <- which(dcontig < 0)[1] + 1
        }
        
        # return last value from contig
        # (last contiguous horizon with surface)
        who.idx <- rev(which(contig[1:max.idx]))[1]
      }
    }
    
    if (length(who.idx) == 0)
      return(emptyres)
    
    res <- h[who.idx, .SD, .SDcols = c(hzidname(p), depthz[2])] 
    res[[depthz[2]]] <- as.double(res[[depthz[2]]])
    res$pattern <- pattern
    res$hzdesgn <- hzdesgn
    res
  }
  
  hzids <- list(hz[[idname(p)]])
  names(hzids) <- idname(p)
  
  sitetemplate <- data.table::as.data.table(site(p))[,.SD, .SDcols = idname(p)]
  sitetemplate[[idname(p)]] <- as.character(sitetemplate[[idname(p)]])
  hzsub <- hz[, .get_contiguous_surface_hz(.SD, hzdesgn), by = hzids]
  hzsub[[idname(p)]] <- as.character(hzsub[[idname(p)]])
  out <- hzsub[sitetemplate, on = idname(p)]
  
  # not found
  lnotfound <- shallowest.depth > out[[depthz[2]]]
  if (any(lnotfound)) {
    warning("found %s profiles where pattern did not match and shallowest depth is greater than 0",
            sum(lnotfound), call. = FALSE)
  }
  
  if(length(p) == 1 && simplify) {
    return(out[[depthz[2]]])
  }
  as.data.frame(out)
}

# starting from the surface, match patterns to horizon
# return the last bottom depth of a horizon that is contiguous with surface
# for instance horizon designations: A1-A2-A3-C-Ab , would return A3 bottom depth
# getSurfaceHorizonDepth <- function (p, pattern, hzdesgn = guessHzDesgnName(p)) {
# 
#   # reused variables
#   hz <- horizons(p)
#   depthz <- horizonDepths(p)
#   shallowest.depth <- min(hz[[depthz[1]]], na.rm = TRUE)
# 
#   # warning messages for incomplete profiles
#   if (is.infinite(shallowest.depth)) {
#     warning(paste0("Profile (", profile_id(p), ") is missing horizon top depths."), call. = FALSE)
#     return(NA)
#   }
# 
#   if (shallowest.depth > 0) {
#     warning(paste0("Profile (", profile_id(p), ") top depth is greater than zero."), call. = FALSE)
#   }
# 
#   if (shallowest.depth < 0) {
#     warning(paste0("Profile (", profile_id(p), ") top depth is less than zero."), call. = FALSE)
#   }
# 
#   # identify horizons matching pattern
#   match.idx <- grepl(hz[[hzdesgn]], pattern = pattern)
# 
#   # no match? return zero or shallowest top depth (the minimum depth)
#   if (length(which(match.idx)) < 1) {
#     return(shallowest.depth)
#   }
# 
#   # identify surface horizon
#   mod.idx <- c(1, rep(0, length(match.idx) - 1))
# 
#   # identify where matches and surface horizon co-occur
#   # matching horizons get a 1, matching surface horizon gets a 2,
#   #   0s kick us out
#   new.idx <- match.idx + mod.idx
# 
#   who.idx <- numeric(0)
#   # we only have a matching surface if the first value is 2
#   #  (meets both above crit)
#   if (new.idx[1] == 2) {
#     # convert that into logical to identify contiguous matches
#     contig <- new.idx > 0 & new.idx <= 2
# 
#     # calculate difference between contiguous matches/nonmatches
#     dcontig <- diff(as.integer(contig))
# 
#     # max depth is, at first, the bottom depth of last contiguous hz
#     max.idx <- length(contig)
# 
#     if(length(max.idx)) {
#       # if we have a negative change at any depth,
#       #  we have a discontinuity
# 
#       # adjust max index (depth) accordingly
#       if(any(dcontig < 0)) {
#         # take first index of negative dcontig
#         # add one to correct for indexing offset due to diff()
#         max.idx <- which(dcontig < 0)[1] + 1
#       }
# 
#       # return last value from contig
#       # (last contiguous horizon with surface)
#       who.idx <- rev(which(contig[1:max.idx]))[1]
#     }
#   }
# 
#   # if no horizons are contiguous with surface return the minimum depth
#   if (!length(who.idx)) {
#     return(shallowest.depth)
#   }
# 
#   # get bottom depth from last horizon
#   return(as.numeric(.data.frame.j(hz[who.idx, ], depthz[2], aqp_df_class(p))))
# }

getMineralSoilSurfaceDepth <-  function(p, hzdesgn = guessHzDesgnName(p), pattern = "O") {
  #assumes OSM is given O designation;
  #TODO: add support for lab-sampled organic measurements
  #      keep organic horizons with andic soil properties
  return(getSurfaceHorizonDepth(p, hzdesgn = hzdesgn, pattern = pattern))
}

getPlowLayerDepth <- function(p, hzdesgn = guessHzDesgnName(p), pattern = "^Ap[^b]*") {
  return(getSurfaceHorizonDepth(p, hzdesgn = hzdesgn, pattern = pattern))
}

#' @title Repair Problematic Lower Horizon Depths
#' 
#' @description Attempt a simple repair of horizon bottom depths in the presence of `NA`, or in cases where the horizon shares a common top and bottom depth. Both situations are common in pedon description where "contact" (Cd, Cr, R, etc.) was described without a lower depth. This repair is only applied to the deepest horizon within a profile as identified by [`getLastHorizonID`].
#' 
#' @param x `SoilProfileCollection`
#' 
#' @param adj vertical offset applied to "repair" affected bottom depths
#' 
#' @return `SoilProfileCollection` with a new (logical) horizon-level attribute `.repaired` marking affected horizons
#' 
repairMissingHzDepths <- function(x, adj = 10) {
  
  # sanity checks
  if(!inherits(x, 'SoilProfileCollection')) {
    stop('`x` should be a SoilProfileCollection', call. = FALSE)
  }
  
  # reasonable adj values
  if(adj < 1) {
    stop('`adj` should be >= 1', call. = FALSE)
  }
  
  # hz top, bottom, ID names
  hztb <- horizonDepths(x)
  hzidn <- hzidname(x)
  
  # Setup: get horizon IDs of bottom-most horizons of all profiles
  valid.hzIDs <- getLastHorizonID(x)
  
  
  # Step 1: 
  # find non-NA top AND NA bottom AND (only deepest horizon of each profile)
  idx1 <- which(
    !is.na(x[[hztb[1]]]) & is.na(x[[hztb[2]]]) & (x[[hzidn]] %in% valid.hzIDs)
  )
  
  # make the edit 
  x[[hztb[2]]][idx1] <- x[[hztb[1]]][idx1] + adj
  
  
  # Step 2: 
  # top == bottom AND (only deepest horizon of each profile)
  idx2 <- which(
    ( x[[hztb[1]]] == x[[hztb[2]]] ) & (x[[hzidn]] %in% valid.hzIDs)
  )
  
  # make the edit
  x[[hztb[2]]][idx2] <- x[[hztb[2]]][idx2] + adj
  
  
  # keep track of "repaired" horizons
  idx <- unique(c(idx1, idx2))
  horizons(x)$.repaired <- FALSE
  horizons(x)$.repaired[idx] <- TRUE
  
  
  # repaired SPC
  return(x)
}

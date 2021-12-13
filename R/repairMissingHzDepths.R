#' @title Repair Problematic Lower Horizon Depths
#' 
#' @description Attempt a simple repair of horizon bottom depths in the presence of `NA`, or in cases where the horizon shares a common top and bottom depth. Both situations are common in pedon description where "contact" (Cd, Cr, R, etc.) was described without a lower depth. 
#' 
#' @details This repair is applied to the deepest horizon within a profile as identified by [`getLastHorizonID`], as well as to bottom depths of any horizon that has a horizon below it. Horizon bottom depths are adjusted by adding `adj` (if non-NA). If the resulting value exceeds `max.depth`, the `max.depth` value is returned (if not `NA`).
#' 
#' @param x `SoilProfileCollection`
#' 
#' @param adj vertical offset applied to "repair" missing bottom depths when top and bottom depth are equal or bottom depth is missing. (`NA` to use `max.depth`)
#' 
#' @param max.depth If adj is `NA`, or the resulting offset sum exceeds `max.depth`, `max.depth` is used. 
#' 
#' @return `SoilProfileCollection` with a new (logical) horizon-level attribute `.repaired` marking affected horizons
#' @examples 
#' h <- data.frame(
#'   id = c(1, 1, 1, 2, 2, 2, 2, 3, 3),
#'   top = c(0:2, 0:3, 0:1) * 10,
#'   bottom = c(rep(NA_integer_, 7), c(10, 99))
#' )
#' 
#' # NA depths result in warnings
#' suppressWarnings({
#'   depths(h) <- id ~ top + bottom
#' })
#' 
#' # inspect data before repairs
#' plot(h)
#' 
#' g <- repairMissingHzDepths(h)
#' 
#' # all depth logic now valid
#' all(checkHzDepthLogic(g)$valid)
#' 
#' # inspect 
#' plot(g)
#' 
#' # no adj, max.depth only
#' f <- repairMissingHzDepths(h, adj = NA, max.depth = 200)
#' all(checkHzDepthLogic(f)$valid)
#' plot(f) 
#' 
#' # max.depth defaults to max(x) if too small
#' f$bottom[c(3,7)] <- NA
#' d <- repairMissingHzDepths(f, adj = NA, max.depth = 20)
#' all(checkHzDepthLogic(d)$valid)
#' plot(d)
repairMissingHzDepths <- function(x, adj = 10, max.depth = 200) {
  # define SPC k-keywords as local vars for R CMD CHECK
  .LAST <- NULL; .HZID <- NULL
  
  # sanity checks
  if(!inherits(x, 'SoilProfileCollection')) {
    stop('`x` should be a SoilProfileCollection', call. = FALSE)
  }
  
  if (!is.na(adj)) {
    # adj must be an integer
    adj <- round(adj)
    
    # reasonable adj values
    if(adj < 1) {
      stop('`adj` should be >= 1', call. = FALSE)
    }
  }
  
  if (!is.na(max.depth) && max.depth < max(x)) {
    max.depth <- max(x)
    message("Using max.depth = ", max.depth)
  }
  
  # hz top, bottom, ID names
  hztb <- horizonDepths(x)
  hzidn <- hzidname(x)
  
  # Setup: get horizon IDs of bottom-most horizons of all profiles
  bottom.idx <- x[,, .LAST, .HZID] # which(hzID(h) %in% getLastHorizonID(h))
  valid.hzIDs <- hzID(x)[bottom.idx] # getLastHorizonID(x)
  
  # find NA bottom depths
  na.bottom <- which(is.na(x[[hztb[2]]]))
  
  # # Step 1: 
  # # find non-NA top AND NA bottom AND (only deepest horizon of each profile)
  # fill in the deepest horizon in each profile if it is missing
  idx1 <- intersect(na.bottom, as.numeric(bottom.idx))
  
  # # make the edit 
  x[[hztb[2]]][idx1] <- pmin(x[[hztb[1]]][idx1] + adj, max.depth, na.rm = TRUE)
  
  # Step 2: 
  # calculate the top depths of the underlying horizon for remaining NA bottom depths
  idx2 <- setdiff(na.bottom, bottom.idx)
  
  # replace the bottom depths
  x[[hztb[2]]][is.na(x[[hztb[2]]])] <-  x[[hztb[1]]][spc_horizonOffset(x, hzid = idx2, offset = 1)]
  #                                     x[[hztb[1]]][which(is.na(x[[hztb[2]]])) + 1]
  
  
  # Step 3:
  # top == bottom AND (only deepest horizon of each profile)
  idx3 <- which((x[[hztb[1]]] == x[[hztb[2]]]) & (x[[hzidn]] %in% valid.hzIDs))
  
  # make the edit
  x[[hztb[2]]][idx3] <- pmin(x[[hztb[2]]][idx3] + adj, max.depth, na.rm = TRUE)
  
  # keep track of "repaired" horizons
  idx <- unique(c(idx1, idx2, idx3))
  horizons(x)$.repaired <- FALSE
  horizons(x)$.repaired[idx] <- TRUE
  
  # repaired SPC
  return(x)
}

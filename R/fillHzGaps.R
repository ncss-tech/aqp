#' @title Find and Fill Horizon Gaps
#'
#' @description This function attempt to find "gaps" in the horizon records of a `SoilProfileCollection` object and fill with place holder horizons.
#'
#' Gaps are defined as:
#'  * within each profile, for horizons `i` to `n_hz`:
#'  * `bottom_i != top_i+1 (but only to i = 1:(n_hz - 1)`
#'
#' @param x `SoilProfileCollection` object
#' @param flag logical, flag empty horizons that have been added. default: `FALSE`
#' @param to_top numeric, fill from shallowest top depth in each profile to specified depth? default: `0` 
#' @param to_bottom numeric, fill from deepest bottom depth in each profile to specified depth? default: `aqp::max(x)` 
#' @return `SoilProfileCollection` object
#' @export
#'
#' @examples
#'
#' data(sp4)
#' depths(sp4) <- id ~ top + bottom
#'
#' # introduce depth logic errors
#' idx <- c(2, 6:7, 8, 12)
#' sp4$top[idx] <- NA
#'
#' # check
#' horizons(sp4)[idx, ]
#'
#' # create gaps by removing logic errors
#' x <- HzDepthLogicSubset(sp4, byhz = TRUE)
#'
#' # inspect
#' par(mar = c(0, 0, 0, 1))
#' plotSPC(x, width = 0.3, default.color = 'royalblue', name = 'hzID')
#'
#' z <- fillHzGaps(x, flag = TRUE)
#'
#' # BUG: plotSPC can't use logical data for color
#' z$.filledGap <- as.factor(z$.filledGap)
#' plotSPC(z, width = 0.3, color = '.filledGap', name = 'hzID', show.legend = FALSE)
#' 
#' # fill top to 0 cm
#' z2 <- fillHzGaps(x, flag = TRUE, to_top = 0)
#' z2$.filledGap <- as.factor(z2$.filledGap)
#' plotSPC(z2, width = 0.3, color = '.filledGap', name = 'hzID', show.legend = FALSE)
#' 
#' # fill bottom to max(SPC)
#' z3 <- fillHzGaps(x, flag = TRUE, to_top = 0, to_bottom = max(x))
#' z3$.filledGap <- as.factor(z3$.filledGap)
#' plotSPC(z3, width = 0.3, color = '.filledGap', name = 'hzID', show.legend = FALSE)
#'
fillHzGaps <- function(x, flag = FALSE, to_top = 0, to_bottom = max(x)) {
  idn <- idname(x)
  hzidn <- hzidname(x)

  htb <- horizonDepths(x)

  hznames <- horizonNames(x)
  hcnames <- c(idn, htb)
  
  h <- data.table::as.data.table(horizons(x))

  lead.idx <- 2:nrow(h)
  lag.idx <- 1:(nrow(h) - 1)

  # identify bad horizons
  bad.idx <- which(h[[htb[2]]][lag.idx] != h[[htb[1]]][lead.idx]
                   & h[[idn]][lag.idx] == h[[idn]][lead.idx])
  
  # TODO: data.table more performant than above for large SPC?
  #   bad.idx <- h[, .I[.SD[1:(.N - 1), 3] != .SD[2:.N, 2] &
  #                     .SD[1:(.N - 1), 1] == .SD[2:.N, 1]],
  #                      .SDcols = hcnames]

  # create template data.frame
  hz.template <- h[bad.idx, ]

  if (nrow(hz.template > 0)) {
    # replace non-ID/depth column values with NA
    hz.template[, hznames[!hznames %in% hcnames]] <- NA

    # fill gaps
    hz.template[[htb[1]]] <- h[[htb[2]]][bad.idx]     # replace top with (overlying) bottom
    hz.template[[htb[2]]] <- h[[htb[1]]][bad.idx + 1] # replace bottom with (underlying) top
  }

  # fill from shallowest top depth to X cm
  surface.template <- hz.template[0,]
  
  if (!is.null(to_top) && is.numeric(to_top)) {
    .FIRST <- NULL
    .HZID <-  NULL
    surface.template <- h[x[,, .FIRST, .HZID],]
    surface.template <- surface.template[which(surface.template[[htb[1]]] > to_top)]
    if (nrow(surface.template) > 0) {
      surface.template[, hznames[!hznames %in% hcnames]] <- NA

      surface.template[[htb[2]]] <- surface.template[[htb[1]]] # replace bottom with (underlying) top
      surface.template[[htb[1]]] <- to_top                     # replace top with zero
    }
  }
  
  # fill from deepest bottom depth to X cm
  bottom.template <- hz.template[0,]
  
  if (!is.null(to_bottom) && is.numeric(to_bottom)) {
    .LAST <- NULL
    .HZID <-  NULL
    bottom.template <- h[x[,, .LAST, .HZID],]
    bottom.template <- bottom.template[which(bottom.template[[htb[2]]] < to_bottom)]
    if (nrow(bottom.template) > 0) {
      bottom.template[, hznames[!hznames %in% hcnames]] <- NA
      
      bottom.template[[htb[1]]] <- bottom.template[[htb[2]]] # replace top with (overlying) bottom
      bottom.template[[htb[2]]] <- to_bottom                 # replace bottom with to_bottom
    }
  }
  
  # flag if needed
  if (flag) {
    if (nrow(h) > 0) h[['.filledGap']] <- FALSE
    if (nrow(surface.template) > 0) surface.template[['.filledGap']] <- TRUE
    if (nrow(bottom.template) > 0) bottom.template[['.filledGap']] <- TRUE
    if (nrow(hz.template) > 0) hz.template[['.filledGap']] <- TRUE
  }

  # combine original data with filled data
  res <- rbind(h, hz.template)

  if (nrow(surface.template) > 0) {
    res <- rbind(res, surface.template)
  }
  
  if (nrow(bottom.template) > 0) {
    res <- rbind(res, bottom.template)
  }
  
  # ID + top depth sort
  res <- res[order(res[[idn]], res[[htb[1]]]),]

  # re-calculate unique hzID (note: AFTER reorder)
  res$hzID <- as.character(1:nrow(res))

  # replace horizons (use df class in object x)
  replaceHorizons(x) <- .as.data.frame.aqp(res, aqp_df_class(x))

  # use the autocalculated hzID (in case user had e.g. phiid, chiid set)
  hzidname(x) <- "hzID"

  return(x)
}


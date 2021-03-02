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
#' @param surface logical, fill from shallowest top depth to 0 cm? default: `TRUE`
#' @return `SoilProfileCollection` object
#' @export
#'
#' @examples
#'
#' data(sp4)
#' depths(sp4) <- id ~ top + bottom
#'
#' # introduce gaps
#' idx <- c(2, 8, 12)
#' sp4$top[idx] <- NA
#'
#' # check
#' horizons(sp4)[idx, ]
#'
#' # remove problematic horizons
#' x <- HzDepthLogicSubset(sp4, byhz = TRUE)
#'
#' # gaps and now problematic profiles
#' par(mar = c(0, 0, 0, 1))
#' plotSPC(x, width = 0.3, default.color = 'royalblue', name = 'hzID')
#'
#' z <- fillHzGaps(x, flag = TRUE)
#'
#' # BUG: plotSPC can't use logical data for color
#' z$.filledGap <- as.factor(z$.filledGap)
#' plotSPC(z, width = 0.3, color = '.filledGap', name = 'hzID', show.legend = FALSE)
#'
#'
fillHzGaps <- function(x, flag = FALSE, surface = TRUE) {
  idn <- idname(x)
  hzidn <- hzidname(x)

  htb <- horizonDepths(x)

  hznames <- horizonNames(x)
  hcnames <- c(idn, htb)

  h <- horizons(x)

  lead.idx <- 2:nrow(h)
  lag.idx <- 1:(nrow(h) - 1)

  # fill from shallowest top depth to 0 cm
  surface.template <- h[0,]
  if (surface) {
    h <- data.table::as.data.table(h)
    .FIRST <- NULL
    .HZID <-  NULL
    .I <- NULL
    .SD <- NULL
    surface.template <- horizons(x[h[x[,,.FIRST,.HZID], .I[.SD[[1]] > 0], .SDcols = htb[1]], 1])
    if (nrow(surface.template) > 0) {
      surface.template[, hznames[!hznames %in% hcnames]] <- NA

      surface.template[[htb[2]]] <- surface.template[[htb[1]]] # replace bottom with (underlying) top
      surface.template[[htb[1]]] <- 0                          # replace top with zero
    }
  }

  # identify bad horizons
  bad.idx <- which(h[[htb[2]]][lag.idx] != h[[htb[1]]][lead.idx]
                   & h[[idn]][lag.idx] == h[[idn]][lead.idx])

  # data.table 3x faster than above with 10k profiles
  # .SD <- NULL
  # .N <- NULL
  # h <- data.table::as.data.table(horizons(x))
  #
  # bad.idx <- which(h[, .SD[1:(.N - 1), 3] != .SD[2:.N, 2] &
  #                      .SD[1:(.N - 1), 1] == .SD[2:.N, 1],
  #                    .SDcols = hcnames])

  # create template data.frame
  hz.template <- h[bad.idx, ]

  if (nrow(hz.template > 0)) {
    # replace non-ID/depth column values with NA
    hz.template[, hznames[!hznames %in% hcnames]] <- NA

    # fill gaps
    hz.template[[htb[1]]] <- h[[htb[2]]][bad.idx]     # replace top with (overlying) bottom
    hz.template[[htb[2]]] <- h[[htb[1]]][bad.idx + 1] # replace bottom with (underlying) top
  }

  # flag if needed
  if (flag) {
    if (nrow(h) > 0) h[['.filledGap']] <- FALSE
    if (nrow(hz.template) > 0) hz.template[['.filledGap']] <- TRUE
  }

  # combine original data with filled data
  res <- rbind(surface.template, h, hz.template)

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


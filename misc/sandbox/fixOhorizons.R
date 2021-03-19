# library(aqp)
# 
# # get sample data from @jskovlin; USFS d.phorizon .rda file
# load("E:/workspace/Ofixer/d.phorizon.Rda")
# 
# # apply SPC-style character ID + top depth sort
# d.phorizon <- d.phorizon[order(as.character(d.phorizon$peiidref),
#                                d.phorizon$hzdept),]
# 
# # check logic of sorted data frame
# res <- checkHzDepthLogic(d.phorizon, c("hzdept", "hzdepb"), "peiidref")
# 
# # find the bad depthLogic (either NA depths, or bottom < top)
# bad.peiids <- subset(res, depthLogic)[['peiidref']]
# 
# # "good" set -- leave them alone for now (still may have other errors)
# good.d.phorizon <- subset(d.phorizon, !(peiidref %in% bad.peiids))
# 
# # bad set -- we will try to fix O horizons
# bad.d.phorizon <- subset(d.phorizon, peiidref %in% bad.peiids)
# 
# # add 1cm to R or Cr horizons that have NA hzdepb
# bad.r.idx <- which(with(bad.d.phorizon, grepl("R|Cr", hzname) & is.na(hzdepb)))
# bad.d.phorizon$hzdepb[bad.r.idx] <- bad.d.phorizon$hzdept[bad.r.idx] + 1
# 
# # add 1cm to O horizons that have equal hzdept and hzdepb 
# #  (note: this "creates" old style horizon by adding 1cm to TOP depth)
# bad.thk.idx <- which(with(bad.d.phorizon, grepl("O", hzname) & hzdept == hzdepb))
# bad.d.phorizon[bad.thk.idx, 'hzdept'] <- bad.d.phorizon$hzdept[bad.thk.idx] + 1
# 
# # remove any rows with both depths NA
# 
# # inspect these / fix elsewhere
# bad.d.phorizon_toinspect <- subset(bad.d.phorizon, is.na(hzdept) & is.na(hzdepb))
# which(!is.na(bad.d.phorizon_toinspect$texture))
# 
# 
# bad.peiids2 <- subset(checkHzDepthLogic(bad.d.phorizon, c("hzdept", "hzdepb"), "peiidref"), 
#                       depthLogic)[['peiidref']]
# 
# # "good" set -- leave them alone for now (still may have other errors)
# good.d.phorizon <- subset(d.phorizon, !(peiidref %in% bad.peiids2))
# 
# # bad set -- we will try to fix O horizons
# bad.d.phorizon <- subset(bad.d.phorizon, peiidref %in% bad.peiids2)
# 
# # these we can try to fix (remove all NA rows)
# bad.d.phorizon <- subset(bad.d.phorizon, !is.na(hzdept) & !is.na(hzdepb))
# 
# # look for the specific logic error 
# #  (O horizon with bottom depth shallower than top depth)
# bad.o.idx <- which(with(bad.d.phorizon, (grepl("O", hzname) | 
#                                            (is.na(hzname) & seqnum == 1)) & 
#                           hzdepb < hzdept))
# bad.o.peiids <- bad.d.phorizon[bad.o.idx, 'peiidref']
# 
# # make negative
# bad.d.phorizon[bad.o.idx, c("hzdept","hzdepb")] <- -bad.d.phorizon[bad.o.idx, c("hzdept","hzdepb")]
# 
# # re-order using SPC-style ID+top depth sorting (again!)
# bad.d.phorizon <- bad.d.phorizon[order(as.character(bad.d.phorizon$peiidref),
#                                        bad.d.phorizon$hzdept),]
# 
# # calculate thickness using ordered horizons
# bad.d.phorizon$thk <- bad.d.phorizon$hzdepb - bad.d.phorizon$hzdept
# bad.d.phorizon$thk[is.na(bad.d.phorizon$thk)] <- 0
# 
# # convert to data.table
# bad.d.phorizon_after <- data.table::as.data.table(bad.d.phorizon)
# 
# # cumulative sums of thickness to make new top/bottom depths
# bad.d.phorizon_after <- bad.d.phorizon_after[, list(hzdept = c(min(abs(hzdept)), cumsum(thk[1:(.N - 1)])),
#                                                     hzdepb = min(abs(hzdept)) + cumsum(thk)), 
#                                              by = peiidref]
# 
# # res <- daff::diff_data(bad.d.phorizon[,c("peiidref","hzdept","hzdepb")], 
# #                        as.data.frame(bad.d.phorizon_after)[,c("peiidref","hzdept","hzdepb")])
# # daff::render_diff(res)
# 
# stillbad <- checkHzDepthLogic(bad.d.phorizon_after, c("hzdept", "hzdepb"), "peiidref")
# 
# # View(subset(bad.d.phorizon_after, peiidref %in% stillbad$peiidref[!stillbad$valid]))
# 
# bad.d.phorizon$hzdept <- bad.d.phorizon_after$hzdept
# bad.d.phorizon$hzdepb <- bad.d.phorizon_after$hzdepb
# bad.d.phorizon$thk <- NULL
# 
# final <- rbind(good.d.phorizon, bad.d.phorizon)
# 
# finalres <- checkHzDepthLogic(final, c("hzdept", "hzdepb"), "peiidref")
# 
# # all results pass depthLogic check
# sum(finalres$depthLogic)
# 
# subset(d.phorizon, peiidref %in% finalres$peiidref[finalres$depthLogic])
# 
# # no pedon IDs are missing 
# d.phorizon$peiidref[!d.phorizon$peiidref %in% finalres$peiidref]
# 
# # 177 still have some sort of logic error (it seems)
# sum(!finalres$valid)
# 
# subset(final, peiidref == 157)

# GENERIC FUNCTION for dealing with old-style O horizons and other? abnormalities in datum

#' Accumulate horizon depths, and reflect reversed depths, relative to new datum
#' 
#' Fix old-style organic horizon depths, or depths with a non-standard datum, by the "depth accumulation" method.
#' 
#' The "depth accumulation" method calculates thicknesses of individual horizons and then cumulative sums them after putting them in `id` + top depth order. The routine tries to determine context based on `hzname` and `pattern`. The main transformation is if a top depth is deeper than the bottom depth, the depths are reflected on the Z-axis (made negative). The data are then `id` + top depth sorted again, the thickness calculated and accumulated to replace the old depths.
#' 
#' This function uses several heuristics to adjust data before transformation and thickness calculation:
#' 
#'  - matches of `pattern` where both top and bottom depth `NA` -> `[0,1]` `[top,bottom]` depth
#'  - REMOVE horizons that do not match `pattern` where both top and bottom depths `NA`
#'  - if `seqnum` specified "first record with `NA` `hzname`" is considered a `pattern` match if `seqnum == 1`
#'  - Add 1 cm to bottom-most horizons with `NA` bottom depth
#'  - Add 1 cm thickness to horizons with top and bottom depth equal
#'  - Add 1 cm thickness to horizons with `NA` top depth and bottom depth `0`
#'  
#' @param x A `data.frame` or `SoilProfileCollection`
#' @param id unique profile ID. Default: `NULL`, if `x` is a SoilProfileCollection `idname(x)`
#' @param hzdepths character vector containing horizon top and bottom depth column names. Default: `NULL`, if `x` is a SoilProfileCollection `horizonDepths(x)`
#' @param hzname character vector containing horizon designation or other label column names. Default: `NULL`, if `x` is a SoilProfileCollection `hzdesgnname(x)`
#' @param hzdatum a numeric constant to add to accumulated depths. Default: `0`
#' @param seqnum Optional: character vector containing record "sequence number" column name; used in-lieu of `hzname` (when `NA`) to identify "first" record in a profile
#' @param pattern pattern to search for in `hzname` to identify matching horizons to append the profile to
#' @param fixer apply adjustments to missing (`NA`) depths and expand 0-thickness horizons? Default: `TRUE` 
#'
#' @return A horizon-level `data.frame`, suitable for promoting to SPC with `depths<-`, or a `SoilProfileCollection`, depending on the class of `x`.
#' @export
#'
#' @examples
#' 
#' # example using z_datum argument
#' data(sp4)
#' hz <- .accumulateDepths(sp4, "id", c("top","bottom"), "name", hzdatum = 15)
#' depths(hz) <- id ~ top + bottom
#' plot(hz)
#' 
#' # example using old-style O horizons
#' hz <- read.table(text = "peiidref hzdept hzdepb hzname seqnum phiid
#'                     1        11      0      5      A      2   295
#'                     2        11      1      0     Oe      1   294
#'                     3        11      5     13     C1      3   296
#'                     4        11     13     58     C2      4   297
#'                     5        11     58    152     C3      5   298
#'                     6        13      0      5      A      2   303
#'                     7        13      1      0     Oe      1   302
#'                     8        13      5     25     Bw      3   304
#'                     9        13     25     61      C      4   305
#'                     10       13     61     NA      R      5   306
#'                     11      136      0     13     A1      3   695
#'                     12      136      1      0     Oe      2   694
#'                     13      136      2      1     Oi      1   693
#'                     14      136     13     61     C1      4   696
#'                     15      136     61     76     C2      5   697")
#'                     
#' depths(hz) <- peiidref ~ hzdept + hzdepb
#' 
#' hz_fixed <- .accumulateDepths(hz,
#'                               id = "peiidref",
#'                               hzdepth = c("hzdept", "hzdepb"),
#'                               hzname = "hzname")
#' 
#' is_valid <- checkHzDepthLogic(hz_fixed)$valid
#' 
#' test0 <- subset(hz_fixed, !is_valid)
#' test1 <- subset(hz_fixed, is_valid)
#' 
#' origO <- subset(hz, grepl("O", hzname))
#' fixedO <- subset(hz_fixed, grepl("O", hzname))
#' 
#' par(mfrow=c(2,1), mar=c(0,0,3,2))
#' plotSPC(origO, max.depth = 25)
#' plotSPC(fixedO, max.depth = 25)
#' 
.accumulateDepths <- function(x,
                              id = NULL,
                              hzdepths = NULL,
                              hzname = NULL,
                              hzdatum = 0,
                              seqnum = NULL,
                              pattern = "O",
                              fixer = TRUE) {
  
  if(!length(hzdatum) == 0 && is.numeric(as.numeric(hzdatum)))
    hzdatum <- hzdatum[1]
  else stop("hzdatum must be a constant numeric value", call. = FALSE)
  
  # x (becomes) a data.frame of horizon-level data
  if (inherits(x, 'SoilProfileCollection')) {
    dat <- horizons(x)
    
    if (is.null(id))
      id <- idname(x)
    
    if (is.null(hzname))
      hzname <- hzdesgnname(x)
    
    if (is.null(hzdepths)) 
      hzdepths <- horizonDepths(x)
    
  } else if (inherits(x, 'data.frame')) {
    
    dat <- x    
    
    if (is.null(id) || is.null(hzname) || is.null(hzdepths)) 
      stop("if x is a data.frame `id`, `hzdepths` and `hzname` must be specified!", call. = FALSE)
    
  } else {
    stop("x must be SoilProfileCollection or data.frame", call. = FALSE)
  }
  
  # convert to data.table, set up some safe globals to use
  .internalID <- id
  .N <- NULL
  .SD <- NULL
  .TOP <- hzdepths[1]
  .BOTTOM <- hzdepths[2]
  dat <- data.table::as.data.table(dat)
  
  # set up for fix of matches where both top and bottom NA
  #  the rest of profile is "appended" to them if they match
  match.na.idx <- which(is.na(dat[[.TOP]]) & 
                        is.na(dat[[.BOTTOM]]) &
                        grepl(pattern, dat[[hzname]]))
  
  if (length(match.na.idx) > 0 && fixer)
    dat[[.BOTTOM]][match.na.idx] <- 0
  
  # seqnum specified triggers "first record with NA hzname" fixing
  if (!is.null(seqnum)) {
    bad.seq.idx <- which(is.na(dat[[hzname]]) & dat[[seqnum]] == 1)
    
    if (length(bad.seq.idx) > 0 && fixer)
      dat[[.BOTTOM]][bad.seq.idx] <- 0
  }
  
  # remove rows where both top and bottom depths NA (nontarget horizons)
  not.na.idx <- !(is.na(dat[[.TOP]]) & is.na(dat[[.BOTTOM]])) 
  dat <- dat[not.na.idx,]

  .BYLIST1 <- list(dat[[.internalID]])
  names(.BYLIST1) <- .internalID
  
  # add 1cm to bottom-most horizons with NA bottom depth
  bad.bottom.idx <- na.omit(dat[, .I[.SD[[.TOP]] == suppressWarnings(max(.SD[[.TOP]], 
                                                                                na.rm = TRUE)) &
                             is.na(.SD[[.BOTTOM]])], by = .BYLIST1]$V1)
  
  # 0-thickness O horizon
  
  # matches pattern or no name
  thk1 <- (grepl(pattern, dat[[hzname]]) | is.na(dat[[hzname]]))
  
  # top and bottom depth equal
  thk2 <- dat[[.TOP]] == dat[[.BOTTOM]]
  
  # top depth NA and bottom depth 0
  thk3 <- is.na(dat[[.TOP]]) & dat[[.BOTTOM]] == 0
  bad.thk.idx <- which(thk1 & thk2) 
  bad.thk.idx2 <- which(thk1 & thk3)  
  
  if (fixer) {
    
    # add 1 to bottom most horizons with na bottom depth
    dat[bad.bottom.idx, ][[.BOTTOM]] <- dat[bad.bottom.idx,][[.TOP]] + 1
    
    # add 1 to 0 thickness
    dat[bad.thk.idx, ][[.TOP]] <- dat[[.TOP]][bad.thk.idx] + 1
    
    # this creates "old style" o horizon for missing TOP depth with bottom==0
    dat[bad.thk.idx2, ][[.TOP]] <- dat[[.BOTTOM]][bad.thk.idx2] + 1
  }
  
  # ORDER
  dat <- dat[order(as.character(dat[[.internalID]]), dat[[.TOP]]),]
  
  # calculate index
  bad.ldx <- grepl(pattern, dat[[hzname]]) & dat[[.BOTTOM]] < dat[[.TOP]]
  bad.ldx[is.na(bad.ldx)] <- TRUE
  
  bad.idx <- which(bad.ldx)
  
  # bad.peiids <- unique(dat[bad.idx, id])
  
  # make depths that are top/bottom reversed NEGATIVE
  dat[bad.idx, hzdepths] <- -dat[bad.idx, .SD, .SDcols = hzdepths]
  
  # re-order using SPC-style ID+top depth sorting (again!)
  dat <- dat[order(as.character(dat[[.internalID]]), dat[[.TOP]]),]
  
  # calculate thickness using ordered horizons
  dat$thk <- dat[[.BOTTOM]] - dat[[.TOP]]
  dat$thk[is.na(dat$thk)] <- 0
  
  # need more than 1 record
  test1 <- dat[, .N, by = .BYLIST1]
  dat_sub <- dat[!dat[[.internalID]] %in% test1[[.internalID]][test1$N == 1],]
  
  .BYLIST2 <- list(dat_sub[[.internalID]])
  names(.BYLIST2) <- .internalID
  
  # cumulative sums of thickness to make new top/bottom depths
  dat_after <- dat_sub[, list(top = c(hzdatum + min(abs(.SD[[.TOP]])), 
                                      hzdatum + min(abs(.SD[[.TOP]])) + cumsum(thk[1:(.N - 1)])),
                              bottom = hzdatum + min(abs(.SD[[.TOP]])) + cumsum(thk)), 
                       by = .BYLIST2]
  
  .BYLIST3 <- list(dat_after[[.internalID]])
  names(.BYLIST3) <- .internalID
  
  # calculate number of rows in resulting horizon data
  test2 <- dat_after[, .N, by = .BYLIST3]
  
  # insert into working subset
  dat_sub[[.TOP]] <- dat_after$top
  dat_sub[[.BOTTOM]] <- dat_after$bottom
  
  ndrop <- sum(!dat_sub[[.internalID]] %in% dat[[.internalID]])
  
  if(ndrop > 0)
    message("profile IDs (n=",ndrop,") dropped from set")
  
  if(nrow(dat) != nrow(dat_sub))
    message("horizons (n=",(nrow(dat) - nrow(dat_sub)),") dropped from set")
  
  # return SPC if input was SPC
  if (inherits(x, 'SoilProfileCollection')) {
    
    # subset with [,
    x <- x[which(profile_id(x) %in% dat_sub[[idname(x)]]), ]
    
    # replace horizons
    replaceHorizons(x) <- .as.data.frame.aqp(dat_sub, aqp_df_class(x))
    
    return(x)
  } else {
    # regular data.frame result
    return(data.frame(dat_sub))
  }
}

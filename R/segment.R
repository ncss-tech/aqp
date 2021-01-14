
## TODO: tests https://github.com/ncss-tech/aqp/issues/180

#' @title Segmenting of Soil Horizon Data by Depth Interval
#' 
#' @description This function adds depth interval ("segment") labels to soil horizon data associated with \code{SoilProfileCollection} and \code{data.frame} objects. Additional horizon records are inserted when a segment label does not overlap with a horizon boundary. See examples.
#'
#' @param object either a \code{SoilProfileCollection} or \code{data.frame}
#' @param intervals a vector of integers over which to slice the horizon data (e.g. \code{c(25, 100)} or \code{25:100})
#' @param trim logical, when \code{TRUE} horizons in \code{object} are truncated to the min/max specified in \code{intervals}. When \code{FALSE}, those horizons overlapping an interval are marked as such. Care should be taken when specifying more than one depth interval and \code{trim = FALSE}.
#' @param hzdepcols a character vector of length 2 specifying the names of the horizon depths (e.g. \code{c("hzdept", "hzdepb")}), only necessary if \code{object} is a \code{data.frame}.
#' 
#' 
#' @details This function adds segment labels to soil horizon data according to \code{intgervals} (e.g. c(25, 100) or 25:100). Compared to \code{slice}, \code{slab}, and \code{glom}, \code{segment} performs no aggregation or resampling of the source data, rather, labels are added to horizon records for subsequent aggregation. This makes it possible to process a very large number of records outside of the constraints associated with e.g. \code{slice} or \code{slab}.
#'
#' @return Either a \code{SoilProfileCollection} or \code{data.frame} with the original horizon data segmented by depth intervals. There are usually more records in the resulting object, one for each time a segment interval partially overlaps with a horizon. A new column called \code{segment_id} identifying the depth interval is added.
#' 
#' @author Stephen Roecker
#' 
#' @export
#'
#' @examples
#' 
#' # example data
#' data(sp1)
#' 
#' # upgrade to SPC
#' depths(sp1) <- id ~ top + bottom
#' 
#' # segment and trim
#' z <- segment(sp1, intervals = c(0, 10, 20, 30), trim = TRUE)
#' 
#' # display segment labels
#' # note that there are new horizon boundaries at segments
#' par(mar = c(0, 0, 3, 1))
#' plotSPC(z, color = 'segment_id', width = 0.3)
#' 
#' # highlight new horizon records
#' par(mar = c(0, 0, 2, 1))
#' plotSPC(z, color = NA, default.color = NA, width = 0.3, lwd = 1)
#' plotSPC(sp1, color = NA, default.color = NA, 
#' width = 0.3, lwd = 3, add = TRUE, name = NA, print.id = FALSE)
#' legend('top', horiz = TRUE, 
#' legend = c('original', 'segmented'), 
#' lwd = c(1, 3), cex = 0.85, bty = 'n')
#' 
#' \donttest{
#' # same results as slab()
#' # 10 random profiles
#' s <- lapply(1:10, random_profile, n_prop = 1, SPC = TRUE, method = 'random_walk')
#' s <- combine(s)
#' 
#' a.slab <- slab(s, fm = ~ p1, slab.structure = c(0, 10, 20, 30), slab.fun = mean, na.rm = TRUE)
#' 
#' z <- segment(s, intervals = c(0, 10, 20, 30), trim = TRUE)
#' z <- horizons(z)
#' z$thick <- z$bottom - z$top
#' 
#' a.segment <- sapply(split(z, z$segment_id), function(i) {
#'   weighted.mean(i$p1, i$thick)
#' })
#' 
#' 
#' res <- data.frame(
#'   slab = a.slab$value,
#'   segment = a.segment,
#'   diff = a.slab$value - a.segment
#' )
#'
#' print(res)
#' res$diff < 0.001
#'}
#' 
#' 
#' data(sp5)
#' 
#' # segment by upper 25-cm
#' test1 <- segment(sp5, intervals = c(0, 100))
#' print(test1)
#' nrow(test1)
#' print(object.size(test1), units = "Mb")
#' 
#' # segment by 1-cm increments
#' test2 <- segment(sp5, intervals = 0:100)
#' print(test2)
#' nrow(test2)
#' print(object.size(test2), units = "Mb")
#' 
#' 
#' # segment and aggregate
#' test3 <- segment(horizons(sp5), 
#'                  intervals = c(0, 5, 15, 30, 60, 100, 200), 
#'                  hzdepcols = c("top", "bottom")
#' )
#' test3$hzthk <- test3$bottom - test3$top
#' test3_agg <- by(test3, test3$segment_id, function(x) {
#'   data.frame(
#'     hzID = x$hzID[1],
#'     segment_id = x$segment_id[1],
#'     average = weighted.mean(x$clay, w = x$hzthk)
#'   )
#' })
#' test3_agg <- do.call("rbind", test3_agg)
#' 
#' head(test3_agg)
#' 

segment <- function(object, intervals, trim = TRUE, hzdepcols = NULL) {
  
  # depth interval rules
  dep <- data.frame(
    top = intervals[- length(intervals)],
    bot = intervals[-1],
    stringsAsFactors = FALSE
  )
  dep$id <- paste0(dep$top, "-", dep$bot)
  
  # argument sanity check
  test_spc <- inherits(object, 'SoilProfileCollection')
  test_df  <- inherits(object, 'data.frame')
  test_hd  <- !is.null(hzdepcols) & length(hzdepcols) == 2
  test_dep <- is.numeric(dep$top) & is.numeric(dep$bot) & all(dep$top < dep$bot)
  
  
  if (! any(test_spc, test_df)) {
    stop("the input must be either a SoilProfileCollection or data.frame")
  }
  
  if (!test_spc & (!test_df | !test_hd)) {
    stop("if the input is a data.frame then hzdepcols must not be NULL and length(hzdepcols) == 2")
  }
  
  if (!test_dep) {
    stop("intervals should be numeric and sequential (e.g. c(0, 1, 2, 3) or 0:100)")
  }
  
  
  # standardize inputs
  if (test_spc) {
    peid      <- idname(object)
    hzid      <- hzidname(object)
    hzdepcols <- horizonDepths(object)
    h  <- horizons(object)
    names(h)[names(h) %in% c(peid, hzid)] <- c("peid", "hzid")
  } else {
    h <- object
  }
  names(h)[names(h) %in% hzdepcols] <- c("hzdept", "hzdepb")
  
  # filter horizons and trim
  .slice <- function(h, top = NULL, bot = NULL) {
    idx <- h$hzdept <= bot & h$hzdepb >= top
    h <- h[idx, ]
    
    # causing errors when FALSE
    if (trim == TRUE) {
      h <- within(h, {
        hzdept = ifelse(hzdept < top, top, hzdept)
        hzdepb = ifelse(hzdepb > bot, bot, hzdepb)
        })
      
    h <- h[(h$hzdepb - h$hzdept) > 0, ]
    }
    
    return(h)
  }
  
  # slice spc by intervals
  # dep$df <- lapply(1:nrow(dep), function(x) h[0, ]) # pre-allocate memory
  dep$df <- list(h[0, ])[rep(1, nrow(dep))] # pre-allocate memory faster
  h <- {
    split(dep, dep$id) ->.;
    lapply(., function(x) {
      x$df[[1]] <- cbind(.slice(h, top = x$top, bot = x$bot), segment_id = x$id)
      return(x$df[[1]])
    }) ->.;
    do.call("rbind", .) ->.;
    }
  names(h)[names(h) %in% c("hzdept", "hzdepb")] <- hzdepcols
  
  
  if (test_spc) {
    h <- h[order(h$peid, h[hzdepcols[1]]), ]
    
    # merge to re-add spc with NA
    h_orig <- data.frame(peid = names(table(horizons(object)[peid])), stringsAsFactors = FALSE)
    h <- merge(h_orig, h, by = "peid", all.x = TRUE, sort = FALSE)
    rm(h_orig)
    
    ## TODO: consider adding a flag to indicate "new" horizon records that have been added
    
    # rebuild SPC
    names(h)[names(h) == "peid"] <- peid
    names(h)[names(h) == "hzid"] <- hzid
    h$hzID <- 1:nrow(h)
    
    replaceHorizons(object) <- h
    suppressMessages(hzidname(object) <- "hzID")
  }
  
  # return
  if(test_spc){
    return(object)
  } else {
    return(h)
  }
  
}



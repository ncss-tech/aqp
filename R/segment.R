
## TODO: tests https://github.com/ncss-tech/aqp/issues/180

#' @title Segmenting of Soil Horizon Data by Depth Interval
#' 
#' @description This function adds depth interval ("segment") labels to soil horizon data associated with \code{SoilProfileCollection} and \code{data.frame} objects.
#'
#' @param object either a \code{SoilProfileCollection} or \code{data.frame}
#' @param intervals a vector of integers over which to slice the horizon data (e.g. \code{c(25, 100)} or \code{25:100})
#' @param trim logical, specify whether horizon depths should be clipped to the depth intervals (e.g. 15 -> 25)
#' @param hzdepcols a character vector of length 2 specifying the names of the horizon depths (e.g. \code{c("hzdept", "hzdepb")}), only necessary if \code{object} is a \code{data.frame}.
#' 
#' 
#' @details This function adds segment labels to soil horizon data according to \code{intgervals} (e.g. c(25, 100) or 25:100). Compared to \code{slice}, \code{slab}, and \code{glom}, \code{segment} performs no aggregation or resampling of the source data, rather, labels are added to horizon records for subsequent aggregation. This makes it possible to process a very large number of records outside of the constraints associated with e.g. \code{slice} or \code{slab}.
#'
#' @return Either a \code{SoilProfileCollection} or \code{data.frame} with the original horizon data segmented by depth intervals. A new column called \code{segment_id} identifying the depth interval is added.
#' 
#' @author Stephen Roecker
#' 
#' @export
#'
#' @examples
#' 
#' library(aqp)
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



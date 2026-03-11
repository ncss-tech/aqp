# Here are some ideas I have been considering around indexing horizon data in a SoilProfileCollection based on "spatial predicates" that operate on the "horizon geometry". 
# I want to be able to find the horizons that are above or below arbitrary horizons for the whole collection.

# hzOffset() allows for vertical offset (in terms of j index units) of horizon IDs. Two aliases are `hzAbove()` and `hzBelow()` that just use a different sign on the `offset` argument. 

# I think we could also probably do things like hz_adjacent(), hz_between/within(). Also we could make it more convenient to prepare the vectors of target horizon IDs. 



#' @title Select Horizons Above or Below a Reference Horizon
#' 
#' @description
#' These functions return "selected" horizons offset from "reference" horizon defined by logical expressions which include horizon data elements. Typically, `hzAbove()` and `hzBelow()` are used as a simpler interface to `hzOffset()`. Selected horizons can be returned as a complete `SoilProfileCollection` (`SPC = TRUE`), a list of horizon data row indices (`SPC = FALSE, simplify = FALSE`), or as a numeric vector of horizon data row indices (`SPC = FALSE, simplify = TRUE`).
#' 
#' @details Expressions that match multiple reference horizons will result in the inclusion of these reference horizons in the selected horizons. Interpreting multiple reference horizons as a single horizon (`single = TRUE`), defined by min(top) - max(bottom), will ensure that only selected horizons are returned.
#' 
#' The `offset` argument controls which "horizons above" or "horizons below" reference horizon(s) are selected, using the `SoilProfileCollection` `[, j]` (horizon/slice) notation. Possible values and their interpretation are as follows:
#' 
#'  * `offset = NULL`: select all horizons above or below reference horizon(s)
#'  * `offset = 1`: select the first horizon above or below reference horizon(s)
#'  * `offset = 1:2`: select the first and second horizons above or below reference horizon(s)
#'  * `offset = 1:10`: safely select the 1st -> 10th horizons, even if there are fewer, above or below reference horizon(s)
#' 
#' @param x `SoilProfileCollection`
#' 
#' @param ... comma-separated set of logical expressions, in context of horizon data, length for each expression must match number of horizons in `x`
#' 
#' @param hzidx integer vector of target horizon row indices, typically calculated by `hzAbove()` and `hzBelow()`
#' 
#' @param offset integer vector or NULL, when specified this is the `[, j]` (horizon/slice) index definining the horizons above or below a reference horizon that should be returned. see details
#' 
#' @param SPC logical, return a `SoilProfileCollection` subset to selected horizons
#' 
#' @param simplify logical, flatten list of horizon row indices into a numeric vector
#' 
#' @param single logical, interpret multiple matching reference horizons as a single horizon, defined by max(top) - min(bottom)
#' 
#' @return One of:
#'  * `SoilProfileCollection`: when `SPC = TRUE`
#'  * list of horizon row indices when `SPC = FALSE` and `simplify = FALSE`
#'  * vector of horizon row indices: when `SPC = FALSE` and `simplify = TRUE`
#' 
#' The return value will not include data from profiles which have no matching reference horizons.
#' 
#' @export
#' @rdname hzOffset
#' 
#' @examples
#' 
#' # example data
#' x <- c(
#'   'P1:AAA|BwBwBwBw|CCCCCCC|CdCdCdCd',
#'   'P2:Ap|AA|E|BhsBhs|Bw1Bw1|CCCCC',
#'   'P3:A|Bt1Bt1Bt1|Bt2Bt2Bt2|Bt3|Cr|RRRRR',
#'   'P4:AA|EEE|BhsBhsBhsBhs|BwBw|CCCCC',
#'   'P5:AAAA|ACACACACAC|CCCCCCCCCCC|CdCdCd'
#' )
#' 
#' s <- quickSPC(x)
#' 
#' # single match for most profiles
#' .ex <- grepl('Bt3|Bw', s$name)
#' s$e <- .ex
#' 
#' a <- hzAbove(s, .ex, SPC = FALSE, simplify = TRUE)
#' b <- hzBelow(s, .ex, SPC = FALSE, simplify = TRUE)
#' 
#' op <- par(no.readonly = TRUE)
#' par(mar = c(0, 0, 3, 0))
#' plotSPC(
#'   s, color = 'e', col.label = 'expression', col.palette = c('grey', 'royalblue'), 
#'   name = 'name', hz.depths = TRUE, depth.axis = FALSE, 
#'   name.style = 'center-center', cex.names = 0.75
#' )
#' 
#' # highlight selected horizons above and below with brackets
#' addBracket(
#'   depths(s, hzID = FALSE)[a, ], agg = TRUE,
#'   offset = -0.3, col = 'darkgreen', tick.length = 0, lwd = 3
#' )
#' 
#' addBracket(
#'   depths(s, hzID = FALSE)[b, ], agg = TRUE,
#'   offset = -0.35, col = 'firebrick', tick.length = 0, lwd = 3
#' )
#' 
#' 
#' # multiple matches
#' .ex <- grepl('B', s$name)
#' s$e <- .ex
#' 
#' # default
#' a <- hzAbove(s, .ex, SPC = FALSE, simplify = TRUE)
#' b <- hzBelow(s, .ex, SPC = FALSE, simplify = TRUE)
#' 
#' par(mfcol = c(1, 2))
#' plotSPC(
#'   s,  col.label = 'expression', color = 'e', 
#'   col.palette = c('grey', 'royalblue'), 
#'   name = 'name', hz.depths = TRUE, depth.axis = FALSE, 
#'   name.style = 'center-center', cex.names = 0.75
#' )
#' 
#' addBracket(
#'   depths(s, hzID = FALSE)[a, ], agg = TRUE,
#'   offset = -0.3, col = 'darkgreen', tick.length = 0, lwd = 3
#' )
#' 
#' addBracket(
#'   depths(s, hzID = FALSE)[b, ], agg = TRUE,
#'   offset = -0.35, col = 'firebrick', tick.length = 0, lwd = 3
#' )
#' 
#' mtext('single = FALSE',  side = 1, line = -1.5, at = 0, adj = -0.5)
#' 
#' 
#' # interpret multiple reference hz as a single reference hz
#' a <- hzAbove(s, .ex, SPC = FALSE, simplify = TRUE, single = TRUE)
#' b <- hzBelow(s, .ex, SPC = FALSE, simplify = TRUE, single = TRUE)
#' 
#' 
#' plotSPC(
#'   s, col.label = 'expression', color = 'e', 
#'   col.palette = c('grey', 'royalblue'), name = 'name', 
#'   hz.depths = TRUE, depth.axis = FALSE, 
#'   name.style = 'center-center', cex.names = 0.75
#' )
#' 
#' addBracket(
#'   depths(s, hzID = FALSE)[a, ], agg = TRUE,
#'   offset = -0.3, col = 'darkgreen', tick.length = 0, lwd = 3
#' )
#' 
#' addBracket(
#'   depths(s, hzID = FALSE)[b, ], agg = TRUE,
#'   offset = -0.35, col = 'firebrick', tick.length = 0, lwd = 3
#' )
#' 
#' mtext('single = TRUE',  side = 1, line = -1.5, at = 0, adj = -0.5)
#' 
#' 
#' # demonstrate SPC = TRUE, single = TRUE
#' plotSPC(
#'   s, col.label = 'expression', color = 'e', 
#'   col.palette = c('grey', 'royalblue'), name = 'name', 
#'   hz.depths = TRUE, depth.axis = FALSE, 
#'   name.style = 'center-center', cex.names = 0.75, max.depth = 250
#' )
#' 
#' a <- hzAbove(s, .ex, SPC = TRUE, single = TRUE)
#' 
#' plotSPC(
#'   a, name = 'name', hz.depths = TRUE, depth.axis = FALSE, 
#'   name.style = 'center-center', cex.names = 0.75, max.depth = 250
#' )
#' title('selected profiles/horizons')
#' 
#' par(op)


#' 
hzAbove <- function(x, ..., offset = NULL, SPC = TRUE, simplify = SPC, single = FALSE) {
  .NHZ <- NULL
  
  # "above" is a negative offset in j index
  
  # missing offset indicates "all horizons above target"
  if(is.null(offset)) {
    # use max number of horizons within x
    offset <- seq(from = 1, to = max(x[, , , .NHZ]))
  }
  
  # capture expression(s) at function
  .dots <- substitute(list(...))
  .dots <- .dots[2:length(.dots)]
  
  # create object to facilitate eval
  .data <- horizons(x)
  
  # loop through list of expressions and evaluate
  res <- vector('list', length(.dots))
  for (i in 1:length(.dots)) {
    res[[i]] <- eval(.dots[[i]], .data, parent.frame(n = 2))
  }
  
  subcrit <- Reduce('&', res)
  
  if (!is.logical(subcrit)) {
    badxpr <- paste0("'",paste0(.dots[sapply(.dots, function(x) !is.logical(x))],
                                collapse = ",'"),"'")
    stop(sprintf("%s is not logical", badxpr), call. = FALSE)
  }
  
  # interpret multiple matches as a single reference horizon
  # offsets are applied relative to top-most reference hz
  if(single) {
    .idx <- which(subcrit)
    
    .d <- depths(x, hzID = FALSE)[.idx, ]
    .d$idx <- .idx
    
    # for each profile
    # find the horizon row index of top-most reference hz
    .topmost <- sapply(split(.d, .d[[1]]), function(i) {
      i$idx[which.min(i[[2]])]
    })
  } else {
    .topmost <- which(subcrit)
  }
  
  hzOffset(x, hzidx = .topmost, offset = -offset, SPC = SPC, simplify = simplify)
}

#' @export
#' @rdname hzOffset
hzBelow <- function(x, ..., offset = NULL, SPC = TRUE, simplify = SPC, single = FALSE) {
  .NHZ <- NULL
  
  # "below" is a positive offset in j index
  
  # missing offset indicates "all horizons below target"
  if(is.null(offset)) {
    # use max number of horizons within x
    offset <- seq(from = 1, to = max(x[, , , .NHZ]))
  }
  
  # capture expression(s) at function
  .dots <- substitute(list(...))
  .dots <- .dots[2:length(.dots)]
  
  # create object to facilitate eval
  .data <- horizons(x)
  
  # loop through list of expressions and evaluate
  res <- vector('list', length(.dots))
  for (i in 1:length(.dots)) {
    res[[i]] <- eval(.dots[[i]], .data, parent.frame(n = 2))
  }
  
  subcrit <- Reduce('&', res)
  
  if (!is.logical(subcrit)) {
    badxpr <- paste0("'",paste0(.dots[sapply(.dots, function(x) !is.logical(x))],
                                collapse=",'"),"'")
    stop(sprintf("%s is not logical", badxpr), call. = FALSE)
  }
  
  # interpret multiple matches as a single reference horizon
  # offsets are applied relative to deepest reference hz
  if(single) {
    .idx <- which(subcrit)
    
    .d <- depths(x, hzID = FALSE)[.idx, ]
    .d$idx <- .idx
    
    # for each profile
    # find the horizon row index of deepest reference hz
    .deepest <- sapply(split(.d, .d[[1]]), function(i) {
      i$idx[which.max(i[[3]])]
    })  
  } else {
    .deepest <- which(subcrit)
  }
  
  hzOffset(x, hzidx = .deepest, offset = offset, SPC = SPC, simplify = simplify)
}

#' @export
#' @rdname hzOffset
hzOffset <- function(x, hzidx, offset, SPC = FALSE, simplify = TRUE) {
  # define SPC k-keywords as local vars for R CMD CHECK
  .LAST <- NULL; .HZID <- NULL
  
  if (SPC) {
    simplify <- TRUE
  }
  
  # calculate first and last horizon indices
  hzidl <- x[,, .LAST, .HZID]
  hzidf <- c(1, hzidl[seq_len(pmax(0, length(hzidl) - 1))] + 1)[seq_along(hzidl)]
  
  # determine intersection between each profile horizon index and the target ID + offset
  idx <- lapply(seq_along(hzidf), function(i) {
    haystack <- unique(do.call('c', lapply(offset, function(o) {
      o + hzidx[hzidx >= hzidf[i] & hzidx <= hzidl[i]]
    })))
    intersect(x = seq(hzidf[i], hzidl[i]), y = haystack)
  })
  
  if (simplify) {
    idx <- do.call('c', idx)
  }
  
  if (!SPC) {
    return(idx)
  }
  
  # assemble reduced SPC here
  # profiles with no matching reference horizon are left out
  
  # TODO: subset NSE needs var in the SPC / "needs" rlang here
  .hzldx <- NULL
  x$.hzldx <- seq_len(nrow(x)) %in% idx
  res <- subsetHz(x, .hzldx) 
  res$.hzldx <- NULL
  
  return(res)
}

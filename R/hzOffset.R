# Here are some ideas I have been considering around indexing horizon data in a SoilProfileCollection based on "spatial predicates" that operate on the "horizon geometry". 
# I want to be able to find the horizons that are above or below arbitrary horizons for the whole collection.

# hzOffset() allows for vertical offset (in terms of j index units) of horizon IDs. Two aliases are `hzAbove()` and `hzBelow()` that just use a different sign on the `offset` argument. 

# I think we could also probably do things like hz_adjacent(), hz_between/within(). Also we could make it more convenient to prepare the vectors of target horizon IDs. 


#' Horizons Above or Below 
#'
#' @param x A SoilProfileCollection
#' @param ... Comma-separated set of R expressions that evaluate as `TRUE` or `FALSE` in context of horizon data frame. Length for individual expressions matches number of horizons, in \code{x}.
#' @param hzid A vector of target horizon IDs. These are calculated from `...` for `horizon_*()` methods
#' @param offset Integer offset in terms of SoilProfileCollection `[,j]` (horizon/slice) index
#' @param SPC Return a SoilProfileCollection? Default `TRUE` for `horizon_*` methods.
#' @param simplify If `TRUE` return a vector (all elements combined), or a list (1 element per profile). If `SPC` is `TRUE` then `simplify` is `TRUE`.
#' @return A SoilProfileCollection (when `SPC = TRUE`) or a vector of horizon row indices (when `SPC = FALSE` and `simplify = TRUE`) or a list (when `SPC = FALSE` and `simplify = FALSE`))
#' 
#' @details To minimize likelihood of issues with non-standard evaluation context, especially when using `hzAbove()`/`hzBelow()` inside another function, all expressions used in `...` should be in terms of variables that are in the horizon data frame.
#' @export
#' @rdname hzOffset
#' @examples
#' data(sp4)
#' depths(sp4) <- id ~ top + bottom
#' 
#' # get the horizon above the last horizon (j-index of bottom horizon minus 1)
#' hzAbove(sp4, hzID(sp4) %in% getLastHorizonID(sp4))
#' 
#' # get horizons below the last horizon (none; j-index of bottom horizon plus 1)
#' hzBelow(sp4, hzID(sp4) %in% getLastHorizonID(sp4))
#' 
hzAbove <- function(x, ..., offset = 1, SPC = TRUE, simplify = SPC) {
  # "above" is a negative offset in j index
  
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
  
  hzOffset(x, hzid = which(subcrit), offset = -offset, SPC = SPC, simplify = simplify)
}

#' @export
#' @rdname hzOffset
hzBelow <- function(x, ..., offset = 1, SPC = TRUE, simplify = SPC) {
  # "below" is a positive offset in j index
  
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
  
  hzOffset(x, hzid = which(subcrit), offset = offset, SPC = SPC, simplify = simplify)
}

#' @export
#' @rdname hzOffset
hzOffset <- function(x, hzid, offset, SPC = FALSE, simplify = TRUE) {
  # define SPC k-keywords as local vars for R CMD CHECK
  .LAST <- NULL; .HZID <- NULL
  
  if (SPC) {
    simplify <- TRUE
  }
  
  # calculate first and last horizon indices
  hzidl <- x[,, .LAST, .HZID]
  hzidf <- c(1, hzidl[1:(length(hzidl) - 1)] + 1)[seq_along(hzidl)]
  
  # determine intersection between each profile horizon index and the target ID + offset
  idx <- lapply(1:length(hzidf), function(i) {
    haystack <- unique(do.call('c', lapply(offset, function(o) {
      o + hzid[hzid >= hzidf[i] & hzid <= hzidl[i]]
    })))
    intersect(x = seq(hzidf[i], hzidl[i]), y = haystack)
  })
  
  if (simplify) {
    idx <- do.call('c', idx)
  }
  
  if (!SPC) return(idx)
  # TODO: subset NSE needs var in the SPC / "needs" rlang here
  .hzldx <- NULL
  x$.hzldx <- 1:nrow(x) %in% idx
  res <- subsetHz(x, .hzldx) 
  res$.hzldx <- NULL
  res
}

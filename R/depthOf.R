#' Get top or bottom depths of horizons matching a regular expression pattern
#'
#' @description The \code{depthOf} family of functions calculate depth of occurrence of a horizon designation pattern, or any other value that can be coerced to character and matched with a regular expression.
#'
#' If you need all depths of occurrence for a particular pattern, \code{depthOf} is what you are looking for. \code{minDepthOf} and \code{maxDepthOf} are wrappers around \code{depthOf} that return the minimum and maximum depth. They are all set up to handle missing values and missing "contacts" with the target pattern.
#'
#' @param p a SoilProfileCollection
#' @param pattern a regular expression to match in the horizon designation column. See:\code{hzdesgn}
#' @param FUN a function that returns a single value, and takes argument `na.rm`
#' @param top should the top (TRUE) or bottom (FALSE) depth be returned for matching horizons? Default: \code{TRUE}.
#' @param hzdesgn column name containing horizon designations. Default: \code{guessHzDesgnName(p)}
#' @param no.contact.depth depth to assume that contact did not occur.
#' @param no.contact.assigned depth to assign when a contact did not occur.
#' @param simplify logical. Return single profile results as vector (default: `TRUE`) or `data.frame` (`FALSE`)
#' @param na.rm logical. Remove `NA`? (default: `TRUE`)
#' @return a numeric vector containing specified depth(s) of horizons matching a pattern. If `length(p) > 1` then a _data.frame_ containing profile ID, horizon ID, top or bottom depths, horizon designation and pattern.
#'
#' @author Andrew G. Brown
#'
#' @export depthOf
#' @export minDepthOf
#' @export maxDepthOf
#' @aliases maxDepthOf minDepthOf
#' @examples
#' # construct a fake profile
#' spc <- data.frame(id=1, taxsubgrp = "Lithic Haploxerepts",
#'                   hzname   = c("A","AB","Bw","BC","R"),
#'                   hzdept   = c(0,  20, 32, 42,  49),
#'                   hzdepb   = c(20, 32, 42, 49, 200),
#'                   clay     = c(19, 22, 22, 21,  NA),
#'                   texcl    = c("l","l","l", "l","br"),
#'                   d_value  = c(5,   5,  5,  6,  NA),
#'                   m_value  = c(2.5, 3,  3,  4,  NA),
#'                   m_chroma = c(2,   3,  4,  4,  NA))
#'
#' # promote to SoilProfileCollection
#' depths(spc) <- id ~ hzdept + hzdepb
#' hzdesgnname(spc) <- 'hzname'
#' hztexclname(spc) <- 'texcl'
#'
#' # multiple horizons contain B
#' depthOf(spc, "B")
#'
#' # deepest top depth of horizon containing B
#' maxDepthOf(spc, "B")
#'
#' # shallowest top depth
#' minDepthOf(spc, "B")
#'
#' # deepest bottom depth
#' maxDepthOf(spc, "B", top = FALSE)
#'
#' # deepest bottom depth above 35cm
#' maxDepthOf(spc, "B", top = FALSE, no.contact.depth = 35)
#'
#' # assign infinity (Inf) if B horizon does not start within 10cm
#' minDepthOf(spc, "B", no.contact.depth = 10, no.contact.assigned = Inf)
#'
depthOf <- function(p, 
                    pattern, 
                    FUN = NULL,
                    top = TRUE, 
                    hzdesgn = guessHzDesgnName(p),
                    no.contact.depth = NULL, 
                    no.contact.assigned = NA_real_,
                    na.rm = TRUE,
                    simplify = TRUE) {
  
  if (!inherits(p, 'SoilProfileCollection'))
    stop("`p` must be a SoilProfileCollection")
  
  if (length(pattern) != 1)
    stop("`pattern` must be unit length character vector containing a regular expression")
    
  # pass through FUN argument if specified
  if (!is.null(FUN)) {
    return(.funDepthOf(
      p = p,
      pattern = pattern,
      FUN = FUN,
      top = top,
      hzdesgn = hzdesgn,
      no.contact.depth = no.contact.depth,
      no.contact.assigned = no.contact.assigned,
      na.rm = na.rm,
      simplify = simplify
    ))
  }
  
  id <- idname(p)
  hid <- hzidname(p)
  hznames <- horizonNames(p)

  # if the user has not specified a column containing horizon designations
  if (!hzdesgn %in% hznames) {
    hzdesgn <- guessHzDesgnName(p, required = TRUE)
    if (!hzdesgn %in% hznames) {
      stop("depth estimation relies on a column containing horizon designations")
    }
  }

  # get horizons matching designation pattern
  hz.match <- horizons(p)[grepl(pattern, p[[hzdesgn]]),]
  
  subsite <- data.frame(idn = profile_id(p), stringsAsFactors = FALSE) 
  
  # get top or bottom depth, based on `top` argument
  depthcol <- horizonDepths(p)[ifelse(top, 1, 2)]
  res <- hz.match[[depthcol]]
  
  # remove results greater than the cutoff depth: `no.contact.depth`
  if (any(res > no.contact.depth)) {
    hz.match <- hz.match[-which(res > no.contact.depth),]
    res <- hz.match[[depthcol]]
  }
  
  # if no horizons match, return `no.contact.assigned`
  if (nrow(hz.match) == 0) {    
    if (length(p) == 1 && simplify) {
      return(no.contact.assigned)
    }
    
    # if no horizons in any profile match, make a conformal result
    emptyres <- data.frame(
      idn = as.character(subsite[["idn"]]),
      hidn = NA,
      depth = no.contact.assigned,
      hzname = NA
    )
    colnames(emptyres) <- c(id, hid, depthcol, hzdesgn)
    emptyres$pattern <- pattern
    return(emptyres)
  }

  # if there are non-NA results, return all of them
  if (length(res) > 0) {
    
    # backwards compatible: simplify=TRUE and SPC length is 1
    if (length(p) == 1 && simplify) {
      return(res)
    }
    
    dfres <- data.table::data.table(idn = as.character(hz.match[[id]]), 
                                    hidn = hz.match[[hid]],
                                    depth = res, 
                                    hzname = hz.match[[hzdesgn]])[subsite, on = "idn"]
    
    # filter out horizons that are > no.contact.depth, keep NA
    if (!is.null(no.contact.depth)) {
      dfres <- dfres[which(dfres$depth <= no.contact.depth | is.na(dfres$depth)),]
    }
    
    # replace NA with no.contact.assigned
    dfres$depth[is.na(dfres$depth)] <- no.contact.assigned
    
    colnames(dfres) <- c(id, hid, depthcol, hzdesgn)
    dfres$pattern <- pattern
    
    return(as.data.frame(dfres))
  }

  # otherwise:
  return(no.contact.assigned)
}

.funDepthOf <- function(p,
                        pattern,
                        FUN,
                        top = TRUE,
                        hzdesgn = guessHzDesgnName(p),
                        no.contact.depth = NULL,
                        no.contact.assigned = NA,
                        na.rm = TRUE,
                        simplify = TRUE) {
    
  
  id <- idname(p)
  depthcol <- horizonDepths(p)[ifelse(top, 1, 2)]
  
  # depthOf returns all top or bottom depths of horizons matching `hzdesgn`
  res <- data.table::as.data.table(depthOf(p = p,
                 pattern = pattern,
                 FUN = NULL,
                 top = top,
                 hzdesgn = hzdesgn,
                 no.contact.depth = no.contact.depth,
                 no.contact.assigned = no.contact.assigned,
                 na.rm = na.rm,
                 simplify = FALSE))
  
  # otherwise, return the FUN value)) {
  idx <- numeric(0)
  naldx <- is.na(res[[depthcol]])
  
  if (all(naldx)) {
    naldx <- logical(0)
    idx <- res[, .I[1], by = id]$V1
  } else {
    # handle warnings about e.g. no non-missing arguments to FUN
    idx <- res[, .I[which(.SD[[depthcol]] %in% suppressWarnings(FUN(.SD[[depthcol]], na.rm = na.rm)))][1], by = id, .SDcols = depthcol]$V1
    idx <- idx[!is.na(idx)]
  }
  
  idx2 <- sort(unique(c(which(naldx), idx)))
  res <- res[idx2, .SD[1, ], by = id, .SDcols = c(idname(p), hzidname(p), depthcol, hzdesgn, "pattern")]
  
  if (length(p) == 1 && simplify) {  
    return(res[[depthcol]])
  }
  .as.data.frame.aqp(res, aqp_df_class(p))
}
  
# maxDepthOf is a wrapper around depthOf to return a single, maximum value
#' @rdname depthOf
#' @export
maxDepthOf <- function(p,
                       pattern,
                       top = TRUE,
                       hzdesgn = guessHzDesgnName(p),
                       no.contact.depth = NULL,
                       no.contact.assigned = NA,
                       na.rm = TRUE,
                       simplify = TRUE) {
   .funDepthOf(
      p = p,
      pattern = pattern,
      FUN = max,
      top = top,
      hzdesgn = hzdesgn,
      no.contact.depth = no.contact.depth,
      no.contact.assigned = no.contact.assigned,
      na.rm = na.rm,
      simplify = simplify
    )
    
  }

# minDepthOf is a wrapper around depthOf to return a single, minimum value\
#' @rdname depthOf
#' @export
minDepthOf <- function(p,
                       pattern,
                       top = TRUE,
                       hzdesgn = guessHzDesgnName(p),
                       no.contact.depth = NULL,
                       no.contact.assigned = NA,
                       na.rm = TRUE,
                       simplify = TRUE) {
    .funDepthOf(
      p = p,
      pattern = pattern,
      FUN = min,
      top = top,
      hzdesgn = hzdesgn,
      no.contact.depth = no.contact.depth,
      no.contact.assigned = no.contact.assigned,
      na.rm = na.rm,
      simplify = simplify
    )
    
  }



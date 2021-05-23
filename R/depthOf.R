#' Get top or bottom depths of horizons matching a regular expression pattern
#'
#' @description The \code{depthOf} family of functions calculate depth of occurrence of a horizon designation pattern. They are used primarily in the place of complex qualitative or quantitative data that would confirm taxonomic criteria.
#'
#' Generally, these functions are used to implement assumptions about relationships between diagnostic properties, features and materials and horizon designations commonly used in the field. Particular assumptions may not apply in all localities and/or data sources. Great care should be taken when inspecting results.
#'
#' If you need all depths of occurrence for a particular pattern, \code{depthOf} is what you are looking for. \code{minDepthOf} and \code{maxDepthOf} are wrappers around \code{depthOf} that return the minimum and maximum depth. They are all set up to handle missing values and missing "contacts" with the target pattern.
#'
#' @param p A single-profile SoilProfileCollection.
#' @param pattern A regular expression to match in the horizon designation column. See:\code{hzdesgn}
#' @param FUN a function that returns a single value, and takes argument `na.rm`
#' @param top Should the top (TRUE) or bottom (FALSE) depth be returned for matching horizons? Default: \code{TRUE}.
#' @param hzdesgn Column name containing horizon designations. Default: \code{guessHzDesgnName(p)}
#' @param no.contact.depth Depth to assume that contact did not occur.
#' @param no.contact.assigned Depth to assign when a contact did not occur.
#' @param simplify logical. Return single profile results as vector (default: `TRUE`) or `data.frame` (`FALSE`)
#' @param na.rm logical. Remove `NA`? (default: `TRUE`)
#' @return A numeric vector containing specified depth(s) of horizons matching a pattern. If `length(p) > 1` then a _data.frame_ containing profile ID, top or bottom depths and pattern.
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
                    no.contact.assigned = NA,
                    na.rm = TRUE,
                    simplify = TRUE) {
  
  if (!inherits(p, 'SoilProfileCollection'))
    stop("`p` must be a SoilProfileCollection")
  
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
      na.rm = na.rm
    ))
  }
  
  id <- idname(p)
  hznames <- horizonNames(p)

  # if the user has not specified a column containing horizon designations
  if (!hzdesgn %in% hznames) {
    hzdesgn <- guessHzDesgnName(p)
    if (!hzdesgn %in% hznames) {
      stop("depth estimation relies on a column containing horizon designations")
    }
  }

  # get horizons matching designation pattern
  hz.match <- horizons(p)[grepl(pattern, p[[hzdesgn]]),]

  # if no horizons match, return `no.contact.assigned`
  if (nrow(hz.match) == 0) {
    return(no.contact.assigned)
  }

  # get top or bottom depth, based on `top` argument
  depthcol <- horizonDepths(p)[ifelse(top, 1, 2)]
  res <- hz.match[[depthcol]]

  # remove results greater than the cutoff depth: `no.contact.depth`
  if (any(res > no.contact.depth)) {
    res <- res[-which(res > no.contact.depth)]
  }

  # if there are non-NA results, return all of them
  if (length(res) > 0 & any(!is.na(res))) {
    
    # default simplify and SPC length is 1
    if (length(p) == 1 && simplify) {
      return(res)
    }
    
    dfres <- data.frame(idn = hz.match[[id]], depth = res, pattern = pattern, 
                        stringsAsFactors = FALSE)
    colnames(dfres) <- c(id, depthcol, "pattern")
    return(dfres)
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
                        na.rm = TRUE) {
    
  
  id <- idname(p)
  depthcol <- horizonDepths(p)[ifelse(top, 1, 2)]
  
  # depthOf returns all top or bottom depths of horizons matching `hzdesgn`
  res <- depthOf(p, pattern, FUN = NULL, top, hzdesgn, no.contact.depth, no.contact.assigned)
 
  if (inherits(res, 'data.frame')) {
  # otherwise, return the FUN value)) {

    idx <- data.table::as.data.table(res)[, .I[.SD[[depthcol]] == FUN(.SD[[depthcol]], na.rm = na.rm)],
                                          by = list(res[[id]]), .SDcols = depthcol]$V1

    res2 <- res[idx, c(idname(p), depthcol, "pattern")]
    
  } else {
    res2 <- suppressWarnings(FUN(res, na.rm = na.rm))
    
    # if not found, depth is infinite
    if(is.infinite(res2)) {
      return(no.contact.assigned)
    }
  }
  
  return(res2)
}
  
# maxDepthOf is a wrapper around depthOf to return a single, maximum value
maxDepthOf <- function(p,
                       pattern,
                       top = TRUE,
                       hzdesgn = guessHzDesgnName(p),
                       no.contact.depth = NULL,
                       no.contact.assigned = NA,
                       na.rm = TRUE) {
    .funDepthOf(
      p = p,
      pattern = pattern,
      FUN = max,
      top = top,
      hzdesgn = hzdesgn,
      no.contact.depth = no.contact.depth,
      no.contact.assigned = no.contact.assigned,
      na.rm = na.rm
    )
    
  }

# minDepthOf is a wrapper around depthOf to return a single, minimum value
minDepthOf <- function(p,
                       pattern,
                       top = TRUE,
                       hzdesgn = guessHzDesgnName(p),
                       no.contact.depth = NULL,
                       no.contact.assigned = NA,
                       na.rm = TRUE) {
    .funDepthOf(
      p = p,
      pattern = pattern,
      FUN = min,
      top = top,
      hzdesgn = hzdesgn,
      no.contact.depth = no.contact.depth,
      no.contact.assigned = no.contact.assigned,
      na.rm = na.rm
    )
    
  }



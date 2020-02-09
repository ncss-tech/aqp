# functions tailored for use with magrittr %>% operator / tidyr
# thisisnotapipe.R

#' @title Subset SPC with pattern-matching for text-based attributes
#' @description \code{grepSPC()} is a function used for substting SoilProfileCollections in magrittr "pipelines." It includes some support for "tidy" lexical features -- namely access to the columns of site and horizon slots directly by name. 
#' @param object A SoilProfileCollection
#' @param attr A character vector (column in object) for matching patterns against.
#' @param pattern REGEX pattern to match in \code{attr}
#' @param ... Additional arguments are passed to \code{grep()}
#' @return A SoilProfileCollection.
#' @author Andrew G. Brown.
#' 
#' @rdname grepSPC
#' @export grepSPC
grepSPC <- function(object, attr, pattern, ...) {
  if(requireNamespace("rlang")) {
    # capture expression(s) at function
    x <- rlang::enquo(attr)
    
    # create composite object to facilitate eval_tidy
    # TODO: abstract
    h <- horizons(object)
    s <- as.list(site(object))
    h <- as.list(h[, !horizonNames(object) %in% siteNames(object)])
    .data <- c(s, h)
    
    # do tidy eval of attr
    res <- rlang::eval_tidy(x, data = .data)
    
    # do the pattern matching
    idx <- grep(res, pattern=pattern, ...)
    
    # subset the SPC for result
    return(object[idx, ])
    
  } else {
    stop("package 'rlang' is required", .call=FALSE)
  }
}

#' @title Subset SPC with logical vectors
#' @description \code{subSPC()} is a function used for substting SoilProfileCollections in magrittr "pipelines." It allows the user to specify an arbitrary number of logical vectors (equal in length to site or horizon), separated by commas. It includes some support for "tidy" lexical features -- specifically, access to site and horizon-level variables directly by name.
#' @param object A SoilProfileCollection
#' @param ... Comma-separated set of R expressions that evaluate as TRUE or FALSE. Length for individual expressions matche number of sites OR number of horizons, in \code{object}. 
#' @return A SoilProfileCollection.
#' @author Andrew G. Brown.
#' 
#' @rdname subSPC
#' @export subSPC
subSPC <- function(object, ...) {
  if(requireNamespace("rlang")) {
    # capture expression(s) at function
    x <- rlang::enquos(...)
    
    # create composite object to facilitate eval_tidy
    # TODO: abstract
    h <- horizons(object)
    s <- as.list(site(object))
    h <- as.list(h[,!horizonNames(object) %in% siteNames(object)])
    .data <- c(s, h)
    
    # loop through list of quosures and evaluate
    res <- lapply(x, function(q) {
      r <- rlang::eval_tidy(q, data = .data)
      return(r)
    })    
    res.l <- lapply(res, length)
    
    # distinguish site and horizon level attributes
    # in the expression input
    sitematch <- res[res.l == length(object)]
    horizonmatch <- res[res.l == nrow(object)]
    
    # intersect the multi-prop site constraints
    if(length(sitematch) > 1) {
      sm <- rowSums(do.call('cbind', sitematch))
      sitematch <- (sm == length(sitematch))
    }
    
    # intersect the multi-prop horizon constraints
    if(length(horizonmatch) > 1) {
      hm <- rowSums(do.call('cbind', horizonmatch))
      horizonmatch <- (hm == length(horizonmatch))
    }
    
    # empty value to hold site level index
    idx <- numeric()
    
    # create site level index from site criteria
    if(length(sitematch) == 1 | !is.list(sitematch))
      idx <- which(unlist(sitematch))
    
    # create site level index from matching horizon criteria
    if(length(horizonmatch) == 1 | !is.list(horizonmatch)) {
      peiid.from.hz <- unique(horizons(object)[unlist(horizonmatch), 
                                               idname(object)])
      hz.idx <- match(peiid.from.hz, profile_id(object))
      idx <- c(idx, hz.idx)
    }
    
    # return SPC, subsetted using site level index
    object[na.omit(idx), ]
  } else {
    stop("package 'rlang' is required", .call=FALSE)
  }
}

#' @title Subset SPC after performing logical function on each profile.
#' @description \code{subApply()} is a function used for substting SoilProfileCollections in magrittr "pipelines." It currently does NOT support for "tidy" lexical features in the \code{...} arguments passed to \code{profileApply()}. The expectation is that the function \code{.fun} takes a single-profile SoilProfileCollection and returns a logical value of length one. The use case would be for any logical comparisons that cannot be evaluated inline by \code{subSPC()} because they require more than simple logical operations.
#' @param object A SoilProfileCollection
#' @param .fun, A function that takes a single profile, returns _logical_ of length 1.
#' @param ... Additional arguments are passed to \code{.fun}
#' @return A SoilProfileCollection.
#' @author Andrew G. Brown.
#' 
#' @rdname subApply
#' @export subApply
subApply <- function(object, .fun, ...) {
  if(requireNamespace("rlang")) {
  
    #TODO: figure out how to use eval helpers here
    
    ## capture expression(s) at function
    #.dots <- rlang::enquos(...)

    # apply .fun to elements of x
    res <- profileApply(object, FUN = .fun, ...)
  
    # return subset of x where .fun is true
    return(object[which(res),])
  } else {
    stop("package 'rlang' is required", .call=FALSE)
  }
}

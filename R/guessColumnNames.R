# guessColumnNames.R

#' @title Guess Horizon Slot Column Names
#'
#' @description `guessHzAttrName()`: Guess the horizon column name where possible/preferred formative elements are known. There is a preference for records where more optional requirements are met to handle cases where there will be many matches. For example, working with soil data one might have "low, RV and high" total clay, as well as clay fractions. One could distinguish between these different measurements using standard formative elements for column names from the database of interest. Result is the first match in \code{horizonNames(x)} with the most required plus optional patterns matched.
#'
#' e.g. \code{guessHzAttrName(x, attr="clay", optional=c("total", "_r"))} matches (\code{claytotal_r == totalclay_r}) over (\code{clay_r == claytotal == totalclay}) over \code{clay}.
#'
#' @param x A SoilProfileCollection
#' @param attr A regular expression containing required formative element of attribute name.
#' @param optional A character vector of regular expression(s) containing optional formative elements of attribute name.
#' @param verbose A boolean value for whether to produce message output about guesses.
#'
#' @return Character containing horizon attribute column name. Result is the first match in \code{horizonNames(x)} with the most required plus optional patterns matched.
#'
#' @author Andrew G. Brown
#' @rdname guessHzAttrName
#' @export
#'
#' @examples
#'
#' # a has the required attr pattern, but none of the optional
#' a <- data.frame(id = 1, top = c(0,10), bottom=c(10,40),
#'                 clay=c(18,19))
#' depths(a) <- id ~ top + bottom
#'
#' guessHzAttrName(a, attr="clay", optional=c("total", "_r"))
#'
#' # b has requried attr pattern, and one of the opional patterns
#' #   notice that it also contains "clay" but preferentially matches more optional patterns
#' b <- data.frame(id = 1, top = c(0,10), bottom=c(10,40),
#'                 clay=c(0.18,0.19), clay_r=c(18,19))
#' depths(b) <- id ~ top + bottom
#'
#' guessHzAttrName(b, attr="clay", optional=c("total", "_r"))
#'
#' # c has total and _r (both optional) on either side of clay
#' # having all of the optional patterns plus required is best evidence, and first
#' # column containing that combination will be returned
#' c <- data.frame(id = 1, top = c(0,10), bottom=c(10,40),
#'                 totalclay_r=c(18,19), claytotal_r=c(0.18,0.19))
#' depths(c) <- id ~ top + bottom
#'
#' guessHzAttrName(c, attr="clay", optional=c("total", "_r"))
#'
guessHzAttrName <- function(x, attr, optional = NULL, verbose = TRUE, required = FALSE) {
  nm <- horizonNames(x)
  
  if(!inherits(x, 'SoilProfileCollection')) {
    stop("x must be a SoilProfileCollection")
  }
  
  # possible names include column names with name in the name
  req <- grepl(attr, nm, ignore.case=TRUE)
  
  opt <- lapply(as.list(optional), function(i) grepl(i, nm, ignore.case=TRUE))
  if(is.null(optional) | length(optional) == 0)
    opt <- as.list(req)
  opt <- rowSums(do.call('cbind', opt))
  
  # all optional requirements met
  idx1 <- which(req & opt == length(optional))
  
  # one or more optional requirements met
  idx2 <- which(req & opt > 0)
  
  # basic requirement met
  idx3 <- which(req)
  
  # return first index matching in decreasing precedence
  #  all optional met, some optional met, basic requirement met, no requirement met
  res <- NA
  if(length(idx1)) {
    res <- nm[idx1[1]]
  } else if(length(idx2)) {
    res <- nm[idx2[1]]
  } else if(length(idx3)) {
    res <- nm[idx3[1]]
  }
  if(!is.na(res)) {
    if(verbose)
      message(sprintf('guessing horizon attribute \'%s\' is stored in `%s`', attr, res))
  } else {
    msg <- sprintf('unable to guess column containing horizon attribute \'%s\'', attr)
    if (required)
      stop(msg, .call = FALSE)
    if (verbose)
      message(msg)
  }
  return(res)
}

#' @description `guessHzDesgnName()`: This follows the historic convention used by \code{aqp::plotSPC()} looking for "hzname" or other column names containing the regular expression "name". If the pattern "name" is not found, the pattern "desgn" is searched as a fallback, as "hzdesgn" or "hz_desgn" are other common column naming schemes for horizon designation name.
#'
#' @param x A SoilProfileCollection
#' @param required logical Default: `FALSE`. Is this attribute required? If it is, set to `TRUE` to trigger error on invalid value.
#' 
#' @rdname guessHzAttrName
#'
#' @export
#'
#' @examples
#'
#' a <- data.frame(id = 1, top = c(0,10), bottom=c(10,40), horizonname=c("A","Bw"))
#' depths(a) <- id ~ top + bottom
#'
#' # store guess in metadata
#' hzdesgnname(a) <- guessHzDesgnName(a)
#'
#' # inspect result
#' hzdesgnname(a)
#'
guessHzDesgnName <- function(x, required = FALSE) {
  nm <- horizonNames(x)
  name <- NA

  if(!inherits(x, 'SoilProfileCollection')) {
    stop("x must be a SoilProfileCollection")
  }

  hzd <- hzdesgnname(x)
  if (length(hzd) == 1) {
    if (hzd != "")
      return(hzd)
  }

  # possible names include column names with name in the name
  possible.name <- nm[grep('name', nm, ignore.case=TRUE)]

  # use the first valid guess
  if(length(possible.name) > 0) {
    possible.name <- possible.name[1]
    name <- possible.name
  } else {
    # hail mary
    try.again <- guessHzAttrName(x, "desgn", c("hz"), verbose = FALSE)
    if (!is.na(try.again)) {
      name <- try.again
    } else {
      message('unable to guess column containing horizon designations')
    }
  }
  
  # this implements the required argument 
  # (even though this method does not return an SPC)
  hzdesgnname(x, required = required) <- name
  
  return(name)
}

#' @description `guessHzTexClName()`: This function is used to provide a texture class attribute column name to functions. It will use regular expressions to match "texcl" which is typically the texture of the fine earth fraction, without modifiers or in-lieu textures. Alternately, it will match "texture" for cases where "texcl" is absent (e.g. in NASIS Component Horizon).
#'
#' @rdname guessHzAttrName
#' 
#' @export guessHzTexClName
#' 
#' @examples
#'
#' a <- data.frame(id = 1, top = c(0,10), bottom=c(10,40), texture=c("A","Bw"))
#' depths(a) <- id ~ top + bottom
#'
#' # store guess in metadata
#' hztexclname(a) <- guessHzTexClName(a)
#'
#' # inspect result
#' hztexclname(a)
#'
guessHzTexClName <- function(x, required = FALSE) {
  nm <- horizonNames(x)

  if (!inherits(x, 'SoilProfileCollection')) {
    stop("x must be a SoilProfileCollection")
  }

  if (length(hztexclname(x)) == 1) {
    # ideally use metadata if it contains a value
    name <- hztexclname(x)
    if (name != "")
      return(name)
  }

  # possible names include column names with name in the name
  possible.name1 <- nm[grep('texcl', nm, ignore.case = TRUE)]

  # use the first valid guess matching texcl
  if (length(possible.name1) > 0) {
    possible.name1 <- possible.name1[1]
    
    # this implements the required argument 
    # (even though this method does not return an SPC)
    hztexclname(x, required = required) <- possible.name1
    return(possible.name1)
  }
  
  
  # alternately, try for something called "texture"
  possible.name2 <- nm[grep('texture', nm, ignore.case = TRUE)]
  if (length(possible.name2) > 0) {
    possible.name2 <- possible.name2[1]
    hztexclname(x, required = required) <- possible.name2
    return(possible.name2)
  } else {
    message('unable to guess column containing horizon texture classes')
  }
  return("")
}

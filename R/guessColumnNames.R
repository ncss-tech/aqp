# guessColumnNames.R

#' Guess Horizon Designation Column Name
#'
#' @param x A SoilProfileCollection
#'
#' @return Character containing horizon designation column name. This follows the historic convention used by \code{aqp::plotSPC()} looking for 'hzname' or other column  names containing the regular expression \code{name}.
#' 
#' @author Andrew G. Brown
#' 
#' @export
#' @examples
#' 
#' a <- data.frame(id = 1, top = c(0,10), bottom=c(10,40), horizonname=c("A","Bw"))
#' depths(a) <- id ~ top + bottom
#' 
#' # store guess in hzdesgncol slot (used internally by some aqp functions)
#' hzdesgnname(a) <- guessHzDesgnName(a)
#' 
#' # inspect result
#' hzdesgnname(a)
#' 
guessHzDesgnName <- function(x) {
  nm <- horizonNames(x)
  name <- NA 
  
  if(!inherits(x, 'SoilProfileCollection')) {
    stop("x must be a SoilProfileCollection")
  }
  
  if(length(hzdesgnname(x))) {
    # ideally use @hzdesgncol if it contains a value. if so, no message
    name <- hzdesgnname(x)
  } else {
    # possible names include column names with name in the name
    possible.name <- nm[grep('name', nm, ignore.case=TRUE)]
    
    # use the first valid guess
    if(length(possible.name) > 0) {
      possible.name <- possible.name[1]
      name <- possible.name
    } else {
      # hail mary
      try.again <- guessHzAttrName(x, "desgn", c("hz"))
      if(!is.na(try.again)) {
        name <- possible.name
      } else {
        message('unable to guess column containing horizon designations')
      }
    }
    message(paste('guessing horizon designations are stored in `', name, '`', sep=''))
  }
  
  return(name)
}

#' Guess Horizon Texture Class Column Name
#'
#' @param x A SoilProfileCollection
#'
#' @return Character containing horizon texture class column name. This function is used to provide texture class attribute column name to functions that use it to determine various taxonomic criteria. One of the most common uses of this attribute is for identifying horizons with sandy textures to trigger special criteria related to those materials. It will use regular expressions to match \code{'texcl'} which is typically the texture of the fine earth fraction, without modifiers or in-lieu textures. Alternately, it will match \code{'texture'} for cases where texcl is absent (e.g. NASIS components).
#' 
#' @author Andrew G. Brown
#' 
#' @export guessHzTexClName
#' 
#' @examples
#' 
#' a <- data.frame(id = 1, top = c(0,10), bottom=c(10,40), texture=c("A","Bw"))
#' depths(a) <- id ~ top + bottom
#' 
#' # store guess in hzdesgncol slot (used internally by some aqp functions)
#' hzdesgnname(a) <- guessHzTexClName(a)
#' 
#' # inspect result
#' hzdesgnname(a)
#' 
guessHzTexClName <- function(x) {
  nm <- horizonNames(x)
  name <- NA 
  
  if(!inherits(x, 'SoilProfileCollection')) {
    stop("x must be a SoilProfileCollection")
  }
  
  if(length(hztexclname(x))) {
    # ideally use @hzdesgncol if it contains a value. if so, no message
    name <- hztexclname(x)
  } else {
    # possible names include column names with name in the name
    possible.name <- nm[grep('texcl', nm, ignore.case=TRUE)]
    
    # use the first valid guess matching texcl
    if(length(possible.name) > 0) {
      possible.name <- possible.name[1]
      name <- possible.name
      message(paste('guessing horizon texture classes are stored in `', name, '`', sep=''))
    } else { 
      # alternately, try for something called "texture"
      possible.name <- nm[grep('texture', nm, ignore.case=TRUE)]
      if(length(possible.name) > 0) {
        possible.name <- possible.name[1]
        name <- possible.name
        message(paste('guessing horizon texture classes are stored in `', name, '`', sep=''))
      } else {
        message('unable to guess column containing horizon texture classes')
      }
    }
  }
  
  return(name)
}

# 


#' Guess Arbitrary Horizon Column Name
#'
#' @description This works for arbitrary columns where possible/preferred formative elements are known 
#  there is a preference for records where more optional requirements are met to handle cases where there will be many matchesfor example, working with component data one might have low, RV and high total clay, as well as clay fractions. One could easily distinguish between these for analyses using standard formative elements from the database schema of interest. The 
#'
#' e.g. \code{guessHzAttrName(x, attr="clay", optional=c("total", "_r"))}
#'        matches (claytotal_r == totalclay_r) over (clay_r == claytotal == totalclay) over clay
#'
#' @param x A SoilProfileCollection
#' @param attr A regular expression containing required formative element of attribute name.
#' @param optional A character vector of regular expression(s) containing one or more optional formative elements of attribute name.
#'
#' @return Character containing horizon attribute column name. Result is the first match in \code{horizonNames(x)} with the most required plus optional patterns matched.
#' 
#' @author Andrew G. Brown
#' 
#' @export guessHzAttrName
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
guessHzAttrName <- function(x, attr, optional) {
  nm <- horizonNames(x)
  
  if(!inherits(x, 'SoilProfileCollection')) {
    stop("x must be a SoilProfileCollection")
  }
  
  # possible names include column names with name in the name
  req <- grepl(attr, nm, ignore.case=TRUE)
  opt <- lapply(as.list(optional), function(i) grepl(i, nm, ignore.case=TRUE))
  opt <- rowSums(do.call('cbind', opt))
  
  if(!any(req)) {
    message(sprintf('unable to guess column containing horizon attribute \'%s\'', attr))
  }
  
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
    message(sprintf('guessing horizon attribute \'%s\' is stored in `%s`', attr, res))
  } else {
    message(sprintf('unable to guess column containing horizon attribute \'%s\'', attr))
  }
  return(res)
}

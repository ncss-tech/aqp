#' @title Sieve the Coarse Fraction of Soil
#' 
#' @description Sieve applies thresholds to a numeric vector of fragment diameter values, returning fragment size classes. Particle diameter thresholds are evaluated as `d < threshold`.
#' 
#' @references 
#' 
#' Soil Science Division Staff. 2017. Soil survey manual. C. Ditzler, K. Scheffe, and H.C. Monger (eds.). USDA Handbook 18. Government Printing Office, Washington, D.C.
#' 
#' @param diameter numeric. Vector of diameters of coarse fragments to "sieve". Default `sieves` are specified in millimeters.
#' 
#' @param sieves leave as `NULL` to use fragment class labels and diameters defined by [fragmentClasses()], or a named vector of fragment diameters. See examples.
#' 
#' @param ordered logical. Return as an ordered factor.
#' 
#' @param prefix character. Add a prefix to result names? Default: `""` adds no prefix. For example `"para"` might be used for size classes of pararock fragments.
#' 
#' @param new_names Optional: apply new labels to result classes. Should match length of `sieves`. 
#' 
#' @param ... additional arguments to [fragmentClasses()], such as `sys`, `flat`, and `rounded`, see examples.
#'
#' @return character. Size class labels based on names of `sieves`, `new_names`, and `prefix` (if specified).
#' 
#' @seealso [fragmentClasses()]
#' 
#' @export
#'
#' @examples
#' 
#' # use a simplified version of the USDA system
#' # common within NRCS/SPSD and NCSS
#' sieve(c(30, 125, 180, 500, 1000))
#' 
#' # pararock fragments
#' sieve(c(30, 125, 180, 500, 1000), prefix = 'para')
#' 
#' # result as an ordered factor
#' sieve(c(30, 125, 180, 500, 1000), ordered = TRUE)
#' 
#' # USDA system, flat size classes
#' sieve(c(30, 125, 180, 500, 1000), flat = TRUE)
#' 
#' # alternative classification systems
#' sieve(c(30, 125, 180, 500, 1000), sys = 'usda')
#' sieve(c(30, 125, 180, 500, 1000), sys = 'international')
#' sieve(c(30, 125, 180, 500, 1000), sys = 'unified')
#' sieve(c(30, 125, 180, 500, 1000), sys = 'aashto')
#' sieve(c(30, 125, 180, 500, 1000), sys = 'mod.wentworth')
#' 
#' # custom fragment labels / diameter
#' sieve(
#' c(30, 125, 180, 500, 1000), 
#' sieves = c(clumps = 50, chunks = 300, blocks = 100000)
#' )
#' 
#' # unnamed sieves, generic labels used
#' sieve(c(10, 50), sieves = c(30, 70))
#' 
#' sieve(c(10, 50), sieves = c(30, 70), ordered = TRUE)
#'  
sieve <- function(diameter,
                  sieves = NULL, 
                  ordered = FALSE,
                  prefix = "",
                  new_names = NULL, 
                  ...) {
  
  
  # if not specified as named vector of diameters
  # use fragmentClasses() to lookup one of several systems
  if(is.null(sieves)) {
    sieves <- fragmentClasses(...)
  }
  
  if (!is.null(new_names)) {
    names(sieves) <- new_names
  }
  
  # test for NA, and filter-out
  res <- vector(mode = 'character', length = length(diameter))
  res[which(is.na(diameter))] <- NA
  no.na.idx <- which(!is.na(diameter))
  
  # only assign classes to non-NA diameters
  if (length(no.na.idx) > 0) {
    # pass diameters "through" sieves
    # 2020: latest part 618 uses '<' for all upper values of class range
    # 2022: adjusted gravel upper threshold to 76 mm 
    classes <- t(sapply(diameter[no.na.idx], function(i) i < sieves))
    
    if (length(names(sieves)) == 0) {
      names(sieves) <- paste0("class_", seq_along(sieves))
    }
    
    # determine largest passing sieve name
    res[no.na.idx] <- names(sieves)[apply(classes, 1, which.max)]
    
    # apply prefix if specified, e.g. parafrags
    if (nchar(prefix) > 0) {
      res[no.na.idx] <- paste0(prefix, res[no.na.idx])
    }
  }
  
  # optional conversion to ordered factor
  if(ordered) {
    res <- factor(res, levels = names(sieves), ordered = TRUE)
  }
  
  return(res)
}

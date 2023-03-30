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
#' @param flat logical. Default: `FALSE`. If `TRUE` and `sieves` is not specified use the "flat" fragment classes for sieves.
#' 
#' @param prefix character. Add a prefix to result names? Default: `""` adds no prefix. For example `"para"` might be used for size classes of pararock fragments.
#' 
#' @param sieves numeric, possibly named. Thresholds to separate `diameter` into classes. Default (`flat=FALSE`): `c(fine_gravel = 5, gravel = 76, cobbles = 250, stones = 600, boulders = 1e10)`. Default (`flat=TRUE`): `c(channers = 150, flagstones = 380, stones = 600, boulders = 1e10)`
#' 
#' @param new_names Optional: apply new labels to result classes. Should match length of `sieves`. 
#'
#' @return character. Size class labels based on names of `sieves`, `new_names`, and `prefix` (if specified).
#' @export
#'
#' @examples
#' 
#' # default
#' sieve(c(30, 125, 180, 500, 1000))
#' 
#' # flat size classes
#' sieve(c(30, 125, 180, 500, 1000), flat = TRUE)
#' 
#' # custom limits and names
#' sieve(c(30, 125, 180, 500, 1000), sieves = c(75, 1e10), new_names = LETTERS[1:2])
#' 
#' # unnamed sieves, generic labels used
#' sieve(c(10, 50), sieves = c(30, 70))
#'  
sieve <- function(diameter,
                  flat = FALSE,
                  prefix = "",
                  sieves = if (isFALSE(flat)) {
                    c(gravel = 76,
                      cobbles = 250,
                      stones = 600,
                      boulders = 1e10)
                  } else{
                    c(channers = 150,
                      flagstones = 380,
                      stones = 600,
                      boulders = 1e10)
                  }, 
                  new_names = NULL) {
  
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
  
  return(res)
}

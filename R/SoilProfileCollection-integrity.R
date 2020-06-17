#' Quickly assess relative state of site and horizon slots
#'
#' @description
#' 
#' Determine "state" of SoilProfileCollection before or after major modifications of site or horizon slot contents. 
#' 
#' Three logical checks are performed on the site and horizon tables, and a fourth attribute \code{valid} returns \code{TRUE} when all three checks are \code{TRUE}.
#'
#' Check 1: Site order is reflected in horizons. Which means: mo intermingling of sites within horizon table. Each profile is a block of contiguous rows in the horizon table. Though, they _may_ be out of order within profiles.
#' 
#' Check 2: Site IDs match coalesced profile ID from horizon. This check ensures the same _relative_ ordering, but horizons still may be out of order within profiles
#' 
#' Check 3: Horizon IDs are in order of profile ID in site and within profiles, have correct top-depth sequence.
#' 
#' @param object A SoilProfileCollection 
#' @return data.table
#' @aliases as,SoilProfileCollection-method
#' @docType methods
#' @author Andrew G. Brown
#'
#' data(sp5)
#' 
#' spc_in_sync(sp5)
#' 
spc_in_sync <- function(object) {
  # get site and horizon slot contents
  s <- object@site
  h <- object@horizons
  
  # object profile ID name
  oid <- idname(object)
  hzidnm <- hzidname(object)
  
  # profile ID from site
  sid <- as.character(s[[oid]])
  
  # profile ID from horizon
  hid <- as.character(h[[oid]])
  hzid <- as.character(h[[hzidnm]])
  s.hid <- match(sort(hid), hid)
  s.hzid <- match(sort(hzid), hzid)

  
  if(length(hid) == 0 & length(hzid)  == 0)  {
    # this is an empty soil profile collection
    return(data.frame(siteDepth = TRUE,
                    relativeOrder = TRUE,
                    depthOrder = TRUE,
                    valid = TRUE))
  }
  
  # top depths from horizon
  tdep <- h[[horizonDepths(object)[1]]]
  
  # coalesced horizon IDs 
  # identifies intermingling of profiles within horizon
  cohid <- .coalesce.idx(hid)
  cohzid <- .coalesce.idx(hzid)
  
  # if cohid is longer than sid, horizons from different profiles
  # are mixed or IDs have been corrupted (by e.g. direct edit)
  # in less-common case, a site has been removed or duplicated
  
  # check 1: site order is reflected in horizons 
  #          no intermingling of sites within horizon table
  #          though, they _may_ be out of order within profiles
  one <- (length(sid) == length(cohid))
  
  # check 2: site IDs match coalesced profile ID from horizon
  #          this ensures the same _relative_ ordering, but
  #          horizons still may be out of order within profiles
  two <- ifelse(one, all(sid == cohid), FALSE)
  
  # check 3: horizon IDs are in order of profile ID in site
  #          and, within profiles, have correct top-depth sequence
  three <- TRUE #TODO
  
  return(data.frame(siteDepth = one,
                    relativeOrder = two,
                    depthOrder = three,
                    valid = all(one, two, three)))
}

# Remove duplicate values retaining original order
# 
# .coalesce.idx
# 
# "Coalesce" an un-sorted, non-unique vector to an a vector with contiguous identical elements removed. 
# 
# Designed primarily for integer indices that can be particularly marred by character-based sorting also supports characters (by conversion to factor) and factors (implicitly, by conversion to numeric).
# 
# unique is _slightly_ faster for large vectors where order is assumed or irrelevant, but obscures critical information when the relative order of values in the input vector matters. The result of coalesce on a sorted, unique vector is the same as the input value.
# 
# > .coalesce.idx(1:10)
#[1]  1  2  3  4  5  6  7  8  9 10
#
# > .coalesce.idx(c(17,3,3,3,6,11,3,789,23,11,2))
# [1]  17   3   6  11   3 789  23  11   2
# 
# Andrew G. Brown
# 
.coalesce.idx <- function(x) {
  lut <- x
  if(inherits(x, 'character'))
    lut <- as.integer(factor(x, ordered = TRUE))
  x[which(diff(c(0,lut)) != 0)]
}

if (!isGeneric('reorderHorizons'))
  setGeneric('reorderHorizons', 
             function(object, target.order = NULL)
    standardGeneric('reorderHorizons'))

#' Re-order corrupted horizon data
#' 
#' @name reorderHorizons
#' @description This is a method made available primarily to repair horizon data that have been corrupted relative to their order at time of SoilProfileCollection construction. 
#' 
#' There is an option to specify the target order, but this will not update the corresponding metadata entry tracking the original order. Use this functionality at your own risk.
#' 
#' @param object A SoilProfileCollection
#' @param target.order A numeric vector of equal length to \code{object}. Default value is \code{NULL} which restores the internal order of the collection. 
#' @aliases reorderHorizons,SoilProfileCollection-method
#' @return SoilProfileCollection
#' @docType methods
#' @rdname reorderHorizons
#'
setMethod('reorderHorizons',
          signature(object = 'SoilProfileCollection', target.order = "ANY"),
          function(object, target.order = NULL) {
            
            h <- object@horizons
            
            if (is.null(target.order))
              target.order <- metadata(object)$target.order
              if (is.null(target.order))
                target.order <- 1:nrow(h)
            
            current.order <- match(target.order,
                                   order(as.character(h[[idname(object)]]),
                                         h[[horizonDepths(object)[1]]]))
            
            h <- .as.data.frame.aqp(h[current.order,], 
                                          aqp_df_class(object))
            object@horizons <- h
            return(object)
          })

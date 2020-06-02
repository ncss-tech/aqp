# Quickly assess relative state of site and horizon slots
# 
# .spc_in_sync
# 
# Function to relatively rapidly determine "state" of SoilProfileCollection before or after major modifications of site or horizon slot contents. The speed comes from not using match/unique based logic. Ideally, checking the state of object by this method will allow the match/%in%/unique -based methods to be used more sparingly -- i.e. only when an exact index needs to be made.
# 
# 
# Andrew G. Brown
# 
#  
.spc_in_sync <- function(object) {
  # get site and horizon slot contents
  s <- object@site
  h <- object@horizons
  
  # object profile ID name
  oid <- idname(object)
  
  # profile ID from site
  sid <- s[[oid]]
  
  # profile ID from horizon
  hid <- h[[oid]]
  hzid <- h[[hzidname(object)]]
  s.hid <- match(sort(hid), hid)
  s.hzid <- match(sort(hzid), hzid)
  
  # top depths from horizon
  tdep <- h[[horizonDepths(object)[1]]]
  
  # coalesced horizon IDs 
  # identifies intermingling of profiles within horizon
  cohid <- aqp:::.coalesce.idx(hid)
  
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
  two <- all(sid == cohid)
  
  # check 3: horizon IDs are in order of profile ID in site
  #          and, within profiles, have correct top-depth sequence
  three <- length(aqp:::.coalesce.idx(order(hid, hzid, tdep))) == length(hzid)
  
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

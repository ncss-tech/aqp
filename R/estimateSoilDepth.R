
# get soil depth based on morphology
estimateSoilDepth <- function(f, name='hzname', top='hzdept', bottom='hzdepb', p='Cr|R|Cd', no.contact.depth=NULL, no.contact.assigned=NULL) {
  
  # sanity check: this function will only operate on an SPC
  if(! inherits(f, 'SoilProfileCollection'))
    stop('`f` must be a SoilProfileCollection containing one profile')
  
  depthcols <- horizonDepths(f)
  
  # ease removal of attribute name arguments -- deprecate them later
  # for now, just fix em if the defaults dont match the depthcols slot
  if(any(!(c(top, bottom) %in% horizonNames(f)))) {
    top <- depthcols[1]
    bottom <- depthcols[2]
  }
  
  # sanity check: this function works on a single soil profile
  if(length(f) > 1)
    stop('`f` can contain only one profile, see manual page for details')
  
  # if name is not in horizons, look if it is set in hzdesgncol
  hznames <- horizonNames(f)
  
  # if the user has not specified a column containing horizon designations
  if(!name %in% hznames) {
    name <- guessHzDesgnName(f)
  }
  
  # extract horizons
  h <- horizons(f)
  
  # extract possible contact
  contact.idx <- grep(p, h[[name]], ignore.case=TRUE)
  # everything else
  no.contact.idx <- grep(p, h[[name]], ignore.case=TRUE, invert=TRUE)
  
  # no contact defined, use deepest hz bottom depth
  if(length(contact.idx) < 1) {
    d <- max(h[[bottom]][no.contact.idx], na.rm=TRUE)
    
    # is there a user-specified depth at which we assume a standard depth?
    if(!missing(no.contact.depth) & !missing(no.contact.assigned)) {
      if(d >= no.contact.depth & !is.null(no.contact.assigned))
        res <- no.contact.assigned
      else 
        res <- d
    }
    
    # otherwise use depth of deepest horizon
    else
      res <- d
  }
    
  # contact defined
  else
    res <- min(h[[top]][contact.idx], na.rm=TRUE)
  
  return(res)
}

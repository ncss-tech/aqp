
# get soil depth based on morphology
estimateSoilDepth <- function(f, name='hzname', top='hzdept', bottom='hzdepb', p='Cr|R|Cd', no.contact.depth=NULL, no.contact.assigned=NULL) {
  
  # sanity check: this function will only operate on an SPC
  if(! class(f) == 'SoilProfileCollection')
    stop('`f` must be a SoilProfileCollection object')
  
  # sanity check: this function works on a single soil profile
  if(length(f) > 1)
    stop('This function will only work when applied to a single soil profile, see manual page for details.')
  
  # sanity check: horizon name, top, bottom must be present in source SPC
  # this is less informative than the 
  if(! all(c(name, top, bottom) %in% horizonNames(f)))
     stop('horizon name, top, or bottom column not correctly specified')
  
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

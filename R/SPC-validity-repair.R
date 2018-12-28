
# test for valid SPC, based on presence / absense of slots as compared to 
# class prototype
# likely only used between major versions of aqp where internal structure of SPC has changed
isValidSPC <- function(x) {
  
  # get slot names from prototype
  sn <- slotNames(x)
  
  # test for all slots in the prototype
  s.test <- sapply(sn, function(i) .hasSlot(x, name=i))
  
  # a valid object will have all slots present
  if(all(s.test)) {
    res <- TRUE
  } else {
    res <- FALSE
  }
  
  return(res)
}

# repair an SPC by breaking into pieces and re-assembling
# likely only used to fix outdated SPC objects that are missing slots
repairSPC <- function(x) {
  # break into pieces
  id <- idname(x)
  hd <- horizonDepths(x)
  m <- metadata(x)
  h <- horizons(x)
  s <- site(x)
  # hack, no getter function defined
  sp <- x@sp
  d <- diagnostic_hz(x)
  
  # init SPC from pieces
  # note: using depths<- because it will generate a horizon ID
  fm <- as.formula(sprintf("%s ~ %s + %s", id, hd[1], hd[2]))
  depths(h) <- fm
  
  # add additional pieces
  metadata(h) <- m
  site(h) <- s
  h@sp <- sp
  diagnostic_hz(h) <- d
  
  return(h)
}

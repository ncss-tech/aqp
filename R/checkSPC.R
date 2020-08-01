
# test for valid SPC, based on presence / absense of slots as compared to
# class prototype
# likely only used between major versions of aqp where internal structure of SPC has changed
checkSPC <- function(x) {

  # get slot names from prototype
  sn <- slotNames(x)

  # test for all slots in the prototype
  s.test <- sapply(sn, function(i) .hasSlot(x, name=i))

  res <- FALSE
  # a valid object will have all slots present
  if(all(s.test)) {
    res <- TRUE
  }

  return(res)
}




## TODO: this needs checking / documentation

## missing data fraction computed on a thickness basis
## this is meant to be run on a single profile at a time
# x: single SPC
# v: variables to consider
# n: horizon designations
# p: inverted pattern matching non-soil horizons
.getMissingDataFraction <- function(x, v, n, p) {
  # get horizons
  h <- horizons(x)
  # get top/bottom
  dc <- horizonDepths(x)
  # data fraction based on thickness
  hz.thick <- h[[dc[2]]] - h[[dc[1]]]
  
  # get all "soil" horizons, for named variables
  soil.hz <- grep(p, h[[n]], ignore.case=TRUE, invert=TRUE)
  d <- h[soil.hz, v]
  hz.thick <- hz.thick[soil.hz]
  hz.with.data <- which(complete.cases(d))
  # compute fraction of data based on thickness
  res <- sum(hz.thick[hz.with.data], na.rm=TRUE) / sum(hz.thick, na.rm = TRUE)
  
  return(res)
}


## iterate over profiles
# x: SPC with >=1 profiles
# vars: variables to consider
# name: horizon designations
# p: inverted pattern matching non-soil horizons
evalMissingData <- function(x, vars, name='hzname', p='Cr|R|Cd') {
  # apply missing data fraction eval to each profile
  md <- profileApply(x, .getMissingDataFraction, v=vars, n=name, p=p)
  return(md)
}


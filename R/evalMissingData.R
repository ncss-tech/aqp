
## missing data fraction computed on a thickness basis
## this is meant to be run on a single profile at a time
# x: single SPC
# v: variables to consider
# n: horizon designations
# p: inverted pattern matching non-soil horizons
# m: method, 'relative' or 'absolute'
.getMissingDataFraction <- function(x, v, n, p, m) {
  # get horizons
  h <- horizons(x)
  # get top/bottom
  dc <- horizonDepths(x)
  # data fraction based on thickness
  hz.thick <- h[[dc[2]]] - h[[dc[1]]]
  
  # get all "soil" horizons, for named variables
  soil.hz <- grep(p, h[[n]], ignore.case=TRUE, invert=TRUE)
  
  # extract named columns, but only those from @horizons
  v <- as.vector(na.omit(match(v, horizonNames(x))))
  d <- h[soil.hz, v]
  
  # mask out non-soil horizons
  hz.thick <- hz.thick[soil.hz]
  
  # index hz with full data population
  hz.with.data <- which(complete.cases(d))
  
  # compute fraction of data based on:
  # relative thickness
  # absolute thickness
  total <- switch(m,
                  'relative' = sum(hz.thick, na.rm = TRUE),
                  'absolute' = 1
  )
     
  res <- sum(hz.thick[hz.with.data], na.rm=TRUE) / total
  
  return(res)
}


## iterate over profiles
# x: SPC with >=1 profiles
# vars: variables to consider
# name: horizon designations
# p: inverted pattern matching non-soil horizons
# method: 'relative' or 'absolute' reporting
evalMissingData <- function(x, vars, name='hzname', p='Cr|R|Cd', method='relative') {
  # sanity check
  if(! method %in% c('relative', 'absolute')) {
    stop("method should be one of 'relative' or 'absolute'", call. = FALSE)
  }
  
  hn <- horizonNames(x)
  sdv <- setdiff(vars, hn)
  
  # check for bad hzname
  if(! name %in% hn) {
    stop(sprintf('`%s` not a horizon level attribute', name), call. = FALSE)
  }
  
  # check for bad hz attr
  if(length(sdv > 1)) {
    stop(sprintf('`%s` not horizon level attribute(s)', paste(sdv, collapse = ',')), call. = FALSE)
  }
  
  # apply missing data fraction eval to each profile
  md <- profileApply(x, .getMissingDataFraction, v=vars, n=name, p=p, m=method)
  return(md)
}


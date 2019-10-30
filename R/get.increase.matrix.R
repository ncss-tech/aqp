# get.increase.matrix()

get.increase.matrix <- function(p, attr, threshold.fun, vertical.distance) {
  # p - a SoilProfileCollection, containing a single profile 
  # attr - attribute name to get the "increase" of
  # threshold.fun - a function that returns the threshold (as a function of attr); may return a constant single value
  # vertical distance - the vertical distance (determined from difference SPC top depth variable) within which increase must be met
  topdepth <- horizons(p)[[horizonDepths(p)[1]]]
  increase.var <- horizons(p)[[attr]]
  
  threshold.vector <- threshold.fun(increase.var)
  
  if(length(threshold.vector) == 1) {
    # if threshold.fun() returns a constant value, expand it to match the length of the attribute
    threshold.vector <- rep(threshold.vector, length(increase.var))
    
  } else if(length(threshold.vector) != length(increase.var)) {
    # this function assumes that the threshold.fun() supplied by the user returns either a constant,
    # or a vector of equal length to `increase.var` when called above. otherwise, we cannot calculate the result.
    # note that the threshold.fun() result _is_ allowed to contain NA, but that will result in no output for affected cells
    stop(paste0('Error: getIncreaseDepth() - profile ID:', profile_id(site(p)), 
                " - threshold.fun() result should be length 1 or equal to length of attribute \'",attr,"\' (n=",length(increase.var),")."))
  }
  
  # repeat attr content by horizon in columns; number of columns = number of horizons
  attr.mat <- outer(increase.var, rep(1, length(increase.var)))
  
  # create a matrix of corresponding thrsholds
  thresh.mat <- outer(threshold.vector, rep(1, length(increase.var)))
  
  # calculate a matrix of attr differences (between all horizons, not just adjacent)
  attr.inc.mat <- outer(increase.var, increase.var, `-`)
  
  # calculate a vertical distance matrix (between all horizons)
  vdist.mat <- outer(topdepth, topdepth, `-`)
  
  # crit1 "an increase of at least [thresh.mat]"
  increase.met <- (attr.mat - thresh.mat) > (attr.inc.mat * upper.tri(attr.inc.mat))
  increase.met[is.na(increase.met)] <- FALSE
  
  # crit2 "within a vertical distance of [vertical.distance]"
  vdist.met <- vdist.mat <= vertical.distance
  
  # are crit 1 and crit 2 met?
  criteria.met <- increase.met & vdist.met
  return(criteria.met)
}

get.increase.depths <- function(p, attr, threshold.fun, vertical.distance) {
  topdepth <- horizons(p)[[horizonDepths(p)[1]]]
  #TODO: is there interesting pedogenic information that can be derived
  # from the lower triangle of the criteria.met matrix we omit from the 
  # "increase" matrix?) -- i.e. pale/haplo clay decrease at depth?
  criteria.met <- get.increase.matrix(p, attr, threshold.fun, vertical.distance)
  # get the index of  _first_ column from matrix criteria.met that has nonzero sum, 
  # this column index is the index of horizon where the attr increase is met
  # then return the top depth. 
  return(topdepth[which(colSums(criteria.met) > 0)])
}

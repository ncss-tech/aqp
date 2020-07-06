# computes proper limits when there is an 'upper' and 'lower' bound
# expands x-axis on either side of upper/lower (if present) by 5%
prepanel.depth_function <- function(x, y, upper=NA, lower=NA, subscripts, ...) {

  # composite into a data.frame
  if(!missing(upper) & !missing(lower)) {
    d <- data.frame(yhat=x, top=y, upper=upper[subscripts], lower=lower[subscripts])
  } else {
    # no upper / lower - use central value as place-holder
    d <- data.frame(yhat=x, top=y, upper=x, lower=x)
  }
  
  
  # compute better xlim based on range of upper/lower values
  # requires some data in both x and y
  if (any(!is.na(x)) && any(!is.na(y))) {
    the_range <- c(min(c(d$lower,d$yhat), na.rm=TRUE), max(c(d$upper, d$yhat), na.rm=TRUE))
    
    # expand by 5%
    the_range[1] <- the_range[1] - (the_range[1] * 0.05)
    the_range[2] <- the_range[2] + (the_range[2] * 0.05)
    
    return(list(xlim=the_range))
    
  } else {
    print('this is')
    # if data are missing, well... we did the best we could
    return(list(xlim=c(NA, NA)))
  }
  
}


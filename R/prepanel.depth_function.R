# computes proper limits when there is an 'upper' and 'lower' bound
prepanel.depth_function <- function(x, y, upper, lower, subscripts, ...) {
# composite into a data.frame
d <- data.frame(yhat=x, top=y, upper=upper[subscripts], lower=lower[subscripts])

# compute better xlim based on range of confidence band 
if (any(!is.na(x)) && any(!is.na(y))) {
	the_range <- c(min(c(d$lower,d$yhat), na.rm=TRUE), max(c(d$upper, d$yhat), na.rm=TRUE))
	return(list(xlim=the_range))
}

# if data are missing, well... we did the best we could
else 
	return(list(xlim=c(NA, NA)))
}


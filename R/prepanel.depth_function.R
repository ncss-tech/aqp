# computes proper limits when there is an 'upper' and 'lower' bound
# expands x-axis on either side of upper/lower (if present) by 5%
prepanel.depth_function <- function(x, y, upper=NA, lower=NA, subscripts, groups = NULL, ...) {

  ## borrowed from prepanel.default.xyplot
  ord <- order(as.numeric(x))
  if (!is.null(groups)) {
    gg <- groups[subscripts]
    dx <- unlist(lapply(split(as.numeric(x)[ord], gg[ord]), 
                        diff))
    dy <- unlist(lapply(split(as.numeric(y)[ord], gg[ord]), 
                        diff))
  }
  else {
    dx <- diff(as.numeric(x[ord]))
    dy <- diff(as.numeric(y[ord]))
  }
  
  
  # composite into a data.frame
  if(!missing(upper) & !missing(lower)) {
    d <- data.frame(
      yhat = x, 
      top = y, 
      upper = upper[subscripts],
      lower = lower[subscripts]
    )
  } else {
    # no upper / lower - use central value as place-holder
    d <- data.frame(
      yhat = x, 
      top = y, 
      upper = x, 
      lower = x
    )
  }
  
  
  # compute better xlim based on range of upper/lower values
  # requires some data in both x and y
  if (any(!is.na(x)) & any(!is.na(y))) {

    x_range <- c(
      min(c(d$lower, d$yhat), na.rm = TRUE), 
      max(c(d$upper, d$yhat), na.rm = TRUE)
    )
    
    y_range <- c(
      min(d$top, na.rm = TRUE), 
      max(d$top, na.rm = TRUE)
    )
    
    # expand by 5%
    x_range[1] <- x_range[1] - (x_range[1] * 0.05)
    x_range[2] <- x_range[2] + (x_range[2] * 0.05)
    
    y_range[1] <- y_range[1] - (y_range[1] * 0.05)
    y_range[2] <- y_range[2] + (y_range[2] * 0.05)
    
    # results as expected by xyplot
    res <- list(
      xlim = x_range, 
      ylim = y_range, 
      dx = dx, 
      dy = dy,
      xat = NULL,
      yat = NULL
    )
    
    return(res)
    
  } else {
    # otherwise simulate lattice:::prepanel.null()
    res <- list(
      xlim = c(NA_real_, NA_real_), 
      ylim = c(NA_real_, NA_real_), 
      dx = NA_real_, 
      dy = NA_real_
      )
    
    return(res)
  }
  
}


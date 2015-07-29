## TODO: 'hz' can be either horizon or site level attribute
## TODO: user-defined weight function
## TODO: try with slices
## TODO: try with diagnostic features
aggregateColor <- function(x, hz='genhz', col='soil_color', scaling='horizon') {
  ## hack to make R CMD check --as-cran happy
  top <- bottom <- NULL
  
  # extract pieces
  h <- as(x, 'data.frame')
  
  # keep track of generalized horizon names for later
  hz.names <- levels(h[[hz]])
  
  vars <- c(hz, horizonDepths(x), col)
  
  # remove missing data
  h.no.na <- na.omit(h[, vars])
  
  # re-name for simplicity
  names(h.no.na)[2:4] <- c('top', 'bottom', 'soil_color')
  
  # split by genhz
  # note that some genhz will have 0 records
  s <- dlply(h.no.na, hz, function(i){
    # aggregate depth by unique soil color
    res <- ddply(i, col, summarise, weight=sqrt(sum(bottom - top)) * length(top), n.hz=length(top))
    # sort by thickness-- this is our metric for relevance
    res <- res[order(res$weight, decreasing=TRUE), ]
    # back-calculate the closest Munsell color
    m <- rgb2munsell(t(col2rgb(res$soil_color)) / 255)
    # format as text
    res$munsell <- paste0(m[, 1], ' ', m[, 2], '/', m[, 3])
    return(res)
  })
  
  # rescale using max horizon weight from all horizons
  if(scaling == 'profile') {
    max.weight <- max(sapply(s, function(i) sum(i$weight)))
    s.scaled <- lapply(s, function(i) {
      i$weight <- i$weight / max.weight
      return(i)
    })
  }
  
  # rescale using the sum of the weights within the current horizon
  if(scaling == 'horizon') {
    s.scaled <- lapply(s, function(i) {
      i$weight <- i$weight / sum(i$weight)
      return(i)
    })
  }
  
  ## TODO: LAB is the ideal color space for color averaging
  # compute weighted mean color for each GHL
  s.agg <- ldply(s.scaled, function(i) {
    # convert to RGB
    v <- t(col2rgb(i$soil_color)) / 255
    
    # compute weighted mean via matrix manip
    w <- i$weight
    vw <- sweep(v, 1, STATS = w, FUN = '*')
    wm <- colSums(vw) / sum(w)
    
    # convert result back to R color specification
    wm.col <- rgb(t(wm), maxColorValue = 1)
    
    # get closest Munsell color
    wm.munsell <- rgb2munsell(t(wm))
    res <- data.frame(munsell=wm.munsell, col=wm.col, t(wm), n=nrow(i))
    return(res)
  })
  names(s.agg)[1] <- hz
  
  # return scaled color data
  return(list(scaled.data=s.scaled, aggregate.data=s.agg))
}

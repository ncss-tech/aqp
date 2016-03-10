

# compute LAB coordinates at select percentiles of depth
.pigments.pam <- function(x, k=3) {
  
  # extract just horizons that have color data
  h <- horizons(x)
  idx <- which(!is.na(h$L))
  h <- h[idx, ]
  
  # can't go on without some data
  ## TODO: this will cause an error in the calling function if all results are NULL
  if(nrow(h) == 0)
    return(NULL)
  
  # cluster colors using frequency weights
  # https://stat.ethz.ch/pipermail/r-help/2008-February/153501.html
  hd <- horizonDepths(x)
  top <- min(h[[hd[1]]], na.rm=TRUE)
  bottom <- max(h[[hd[2]]], na.rm=TRUE)
  
  # slice to unwind weighted data into un-weighted data (weights are thickness)
  fm <- as.formula(paste0(top, ':', bottom, ' ~ L + A + B'))
  x.slices <- slice(x, fm, just.the.data = TRUE, strict = FALSE)
  x.slices <- na.omit(x.slices[, c(idname(x), 'L', 'A', 'B')])
  
  # have to have at least 1 more row than k
  ## TODO: this will cause an error in the calling function if all results are NULL
  if(nrow(x.slices) < k+1)
    return(NULL)
  
  # use PAM to cluster
  cl <- pam(x.slices[, -1], k = k, stand = FALSE)
  
  # get data
  x.medoids <- x.slices[cl$id.med, c(idname(x), 'L', 'A', 'B')]
  
  # make IDs
  x.medoids$.ids <- paste0('.', 1:k)
  
  # melt and create new variable names
  m <- melt(x.medoids, id.var=c(idname(x), '.ids'), measure.vars = c('L', 'A', 'B'))
  m$variable <- paste0(m$variable, m$.ids)
  
  # cast to wide format
  fm <- as.formula(paste0(idname(x), ' ~ variable'))
  res <- cast(m, fm, value = 'value')
  
  # don't include the id column
  return(res[, -1])
}


# compute LAB coordinates at select percentiles of depth
.pigments.depths <- function(x, p=c(0.1, 0.5, 0.9)) {
  
  # extract just horizons that have color data
  h <- horizons(x)
  idx <- which(!is.na(h$L))
  h <- h[idx, ]
  
  # TODO finish this
  if(nrow(h) == 0)
    return(NULL)
  
  # determine sampling depths via quantiles
  hd <- horizonDepths(x)
  # get approximate horizon mid-points
  h$.mid <- round(((h[[hd[2]]] + h[[hd[1]]]) / 2))
  
  # use discontinuous style quantiles, to force quantiles to "fit" original data
  sample.depths <- round(quantile(h$.mid, probs=p, na.rm=TRUE, type=1))
  
  # locate the horizons that are as close as possible to these depths
  sample.idx <- sapply(sample.depths, function(i) which.min(abs(i - h$.mid)))
  
  # get data
  x.slices <- h[sample.idx, c(idname(x), 'L', 'A', 'B')]

  # make depth IDs
  x.slices$depth.id <- paste0('.', p)
  
  # melt and create new variable names
  m <- melt(x.slices, id.var=c(idname(x), 'depth.id'), measure.vars = c('L', 'A', 'B'))
  m$variable <- paste0(m$variable, m$depth.id)
  
  # cast to wide format
  fm <- as.formula(paste0(idname(x), ' ~ variable'))
  res <- cast(m, fm, value = 'value')
  
  # don't include the id column
  return(res[, -1])
}


# https://en.wikipedia.org/wiki/Lab_color_space
#
# L - brightness
# A - green | red
# B - blue | yellow
# x: single profile
# requires L, pos.A, neg.A, pos.B, neg.B in @horizon
.pigments.proportions <- function(x, useProportions, pigmentNames) {
  h <- horizons(x)
  dc <- horizonDepths(x)
  hz.thick <- h[[dc[2]]] - h[[dc[1]]]
  h <- h[, c('L', 'pos.A', 'neg.A', 'pos.B', 'neg.B')]
  
  # TODO: this may need to be normalized
  hz.pigments <- sweep(h, MARGIN = 1, STATS = hz.thick, FUN = '*')
  pigment <- colSums(hz.pigments, na.rm = TRUE)
  names(pigment) <- pigmentNames
  
  ## NOTE: this removes the effect of soil depth
  # convert to proportions
  if(useProportions)
    pigment <- pigment / sum(pigment)
  
  return(pigment)
}

## TODO: move method-specific arguments to ...
## TODO: init from sRGB() or RGB() ??
# requires colorspace package
soilColorSignature <- function(spc, r='r', g='g', b='b', method='colorBucket', RescaleLightnessBy=1, useProportions=TRUE, pigmentNames=c('.white.pigment', '.red.pigment', '.green.pigment', '.yellow.pigment', '.blue.pigment')) {
  
  # warn about methods
  if(! method  %in% c('colorBucket', 'depthSlices', 'pam'))
    message('method must be either `colorBucket` or `depthSlices`')
  
  if(!requireNamespace('colorspace'))
    stop('pleast install the `colorspace` package.', call.=FALSE)
  
  # extract horizons
  h <- horizons(spc)
  
  # create LAB colors
  lab.colors <- as(colorspace::RGB(h[['r']], h[['g']], h[['b']]), 'LAB')@coords
  
  ## TODO: does it make sense to normalized based on limited data or entire possible range?
  # normalize the L coordinate
  lab.colors[, 1] <- lab.colors[, 1] / RescaleLightnessBy
  
  
  if(method == 'colorBucket') {
    ## L is always positve
    ## split A/B axes into positive / negative pigments
    pos.A <- ifelse(lab.colors[, 2] > 0, lab.colors[, 2], 0)
    neg.A <- ifelse(lab.colors[, 2] < 0, -lab.colors[, 2], 0)
    
    pos.B <- ifelse(lab.colors[, 3] > 0, lab.colors[, 3], 0)
    neg.B <- ifelse(lab.colors[, 3] < 0, -lab.colors[, 3], 0)
    
    ## TODO: this is sloppy
    # assign back to original SPC
    spc$L <- lab.colors[, 1]
    spc$pos.A <- pos.A
    spc$neg.A <- neg.A
    spc$pos.B <- pos.B
    spc$neg.B <- neg.B
    
    col.data <- profileApply(spc, .pigments.proportions, useProportions=useProportions, pigmentNames=pigmentNames, simplify = FALSE)
    col.data <- ldply(col.data)
    names(col.data)[1] <- idname(spc)
  }
  
  # use horizons at depths proportional to percentiles: 0.1, 0.5, 0.9
  if(method == 'depthSlices') {
    spc$L <- lab.colors[, 1]
    spc$A <- lab.colors[, 2]
    spc$B <- lab.colors[, 3]
    
    # this is slow
    col.data <- profileApply(spc, .pigments.depths, simplify = FALSE)
    col.data <- ldply(col.data)
    names(col.data)[1] <- idname(spc)
  }
  
  if(method == 'pam') {
    spc$L <- lab.colors[, 1]
    spc$A <- lab.colors[, 2]
    spc$B <- lab.colors[, 3]
    
    # this is slow
    col.data <- profileApply(spc, .pigments.pam, simplify = FALSE)
    col.data <- ldply(col.data)
    names(col.data)[1] <- idname(spc)
  }
  
  
  return(col.data)
}


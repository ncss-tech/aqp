
# compute LAB coordinates from clusters of slices
.pigments.pam <- function(x, k) {
  
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
  x.slices <- suppressMessages(dice(x, fm = fm, SPC = FALSE))
  x.slices <- na.omit(x.slices[, c(idname(x), 'L', 'A', 'B')])
  
  # have to have at least 1 more row than k
  ## TODO: this will cause an error in the calling function if all results are NULL
  if(nrow(x.slices) < k+1)
    return(NULL)
  
  # compute perceptually based distance matrix on sliced colors, CIELAB
  dE00 <- farver::compare_colour(x.slices[, -1], x.slices[, -1], from_space='lab', method='CIE2000', white_from='D65')
  dE00 <- as.dist(dE00)
  
  # use PAM to cluster, note `pamonce=5`` used for optimization
  cl <- cluster::pam(dE00, k = k, diss = TRUE, pamonce = 5)
  
  # get data
  x.medoids <- x.slices[cl$id.med, c(idname(x), 'L', 'A', 'B')]
  
  # make IDs
  x.medoids$.ids <- paste0('.', 1:k)
  
  # convert wide -> long and create new variable names
  # using data.table::melt()
  m <- data.table::melt(
    data.table::as.data.table(x.medoids), 
    id.var = c(idname(x), '.ids'), 
    measure.vars = c('L', 'A', 'B')
  )
  
  # leave as data.table for dcast
  
  # new ID
  m$variable <- paste0(m$variable, m$.ids)
  
  # convert long -> wide format
  # using data.table::dcast
  fm <- as.formula(paste0(idname(x), ' ~ variable'))
  res <- dcast(m, formula = fm, value.var = 'value')
  
  # convert back to data.frame
  res <- as.data.frame(res)
  
  # result is a data.frame with profile ID
  return(res)
}


# compute LAB coordinates at select percentiles of depth
.pigments.depths <- function(x, p = c(0.1, 0.5, 0.9)) {
  
  # print(profile_id(x))
  
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
  
  # convert wide -> long and create new variable names
  # using data.table::melt()
  m <- data.table::melt(
    data.table::as.data.table(x.slices),
    id.var = c(idname(x), 'depth.id'), 
    measure.vars = c('L', 'A', 'B')
  )
  
  # leave as data.table for re-shape
  
  # new ID
  m$variable <- paste0(m$variable, m$depth.id)
  
  # convert long -> wide format
  # using data.table::dcast
  fm <- as.formula(paste0(idname(x), ' ~ variable'))
  res <- dcast(m, formula = fm, value.var = 'value')
  
  # convert back to data.frame
  res <- as.data.frame(res)
  
  # result is a data.frame with profile ID
  return(res)
}


## TODO: this doesn't always give the best results compared with PAM

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
  # TODO: this will create NA
  hz.pigments <- sweep(h, MARGIN = 1, STATS = hz.thick, FUN = '*')
  pigment <- colSums(hz.pigments, na.rm = TRUE)
  names(pigment) <- pigmentNames
  
  ## NOTE: this removes the effect of soil depth
  # convert to proportions
  if(useProportions) {
    pigment <- pigment / sum(pigment, na.rm = TRUE)
  }
  
  # results as a data.frame for simpler rbinding  
  res <- data.frame(
    .id = profile_id(x)[1],
    t(pigment),
    stringsAsFactors = FALSE
  )
  
  # reset ID
  names(res)[1] <- idname(x)
  
  return(res)
}

## TODO: 
#   * move method-specific arguments to ...
#   * allow for specification of colors via: hex, sRGB, LAB
#   * data.table optimization
#   * better documentation!

#' @title Soil Profile Color Signatures
#' @description Generate a color signature for each soil profile in a collection.
#'
#' @param spc a `SoilProfileCollection` object
#' @param r horizon level attribute containing soil color (sRGB) red values
#' @param g horizon level attribute containing soil color (sRGB) green values
#' @param b horizon level attribute containing soil color (sRGB) blue values
#' @param method algorithm used to compute color signature, `colorBucket`, `depthSlices`, or `pam`
#' @param pam.k number of classes to request from `cluster::pam()`
#' @param RescaleLightnessBy rescaling factor for CIE LAB L-coordinate
#' @param useProportions use proportions or quantities, see details
#' @param pigmentNames names for resulting pigment proportions or quantities
#' @param apply.fun function passed to `aqp::profileApply(APPLY.FUN)` argument, can be used to add progress bars via `pbapply::pblapply`, or parallel processing with `furrr::future_map` 
#'
#'
#' @details See the [related tutorial](http://ncss-tech.github.io/AQP/aqp/soil-color-signatures.html).
#' 
#' @return
#' 
#' For the `colorBucket` method, a `data.frame` object containing:
#' 
#'  * id column: set according to `idname(spc)`
#'  * `.white.pigment`: proportion or quantity of CIE LAB L-values
#'  * `.red.pigment`: proportion or quantity of CIE LAB positive A-values
#'  * `.green.pigment`: proportion or quantity of CIE LAB negative A-values
#'  * `.yellow.pigment`: proportion or quantity of CIE LAB positive B-values
#'  * `.blue.pigment`: proportion or quantity of CIE LAB negative B-values
#' 
#' Column names can be adjusted with the `pigmentNames` argument.
#' 
#' For the `depthSlices` method ...
#' 
#' For the `pam` method ...
#' 
#' @references https://en.wikipedia.org/wiki/Lab_color_space
#' 
#' @author D.E. Beaudette
#' 
#' @seealso \code{\link{munsell2rgb}}
#' 
#' 
#' @export
#'
#' @examples
#' 
#' # trivial example, not very interesting
#' data(sp1)
#' depths(sp1) <- id ~ top + bottom
#' 
#' # convert Munsell -> sRGB triplets
#' rgb.data <- munsell2rgb(sp1$hue, sp1$value, sp1$chroma, return_triplets = TRUE)
#' sp1$r <- rgb.data$r
#' sp1$g <- rgb.data$g
#' sp1$b <- rgb.data$b
#' 
#' # extract color signature
#' pig <- soilColorSignature(sp1)
#' 
soilColorSignature <- function(spc, r = 'r', g = 'g', b = 'b', method = c('colorBucket', 'depthSlices', 'pam'), pam.k = 3, RescaleLightnessBy = 1, useProportions = TRUE, pigmentNames = c('.white.pigment', '.red.pigment', '.green.pigment', '.yellow.pigment', '.blue.pigment'), apply.fun = lapply) {
  
  # sanity check on method
  method <- match.arg(method)
  
  # farver pkg required for method = pam
  if(method == 'pam') {
    if (!requireNamespace('farver', quietly = TRUE))
      stop('please install the `farver` package.', call.=FALSE)
  }
  
  # extract horizons
  h <- horizons(spc)
  
  ## TODO: consider using farver to do the work, or LAB in `spc`
  # create LAB colors
  # note: source colors are sRGB
  # note: convertColor() expects a matrix
  lab.colors <- convertColor(as.matrix(h[, c(r, g, b)]), from = 'sRGB', to = 'Lab', from.ref.white = 'D65', to.ref.white = 'D65')
  
  ## TODO: only for those methods NOT using dE00!
  ## TODO: does it make sense to normalize based on limited data or entire possible range?
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
    
    # result is a list
    col.data <- profileApply(
      spc, 
      FUN = .pigments.proportions, 
      useProportions = useProportions, 
      pigmentNames = pigmentNames, 
      simplify = FALSE,
      APPLY.FUN = apply.fun
    )
    
  }
  
  # use horizons at depths proportional to percentiles: 0.1, 0.5, 0.9
  if(method == 'depthSlices') {
    spc$L <- lab.colors[, 1]
    spc$A <- lab.colors[, 2]
    spc$B <- lab.colors[, 3]
    
    ## TODO: optimize
    # result is a list of data.frames
    col.data <- profileApply(
      spc, 
      FUN = .pigments.depths, 
      simplify = FALSE,
      APPLY.FUN = apply.fun
    )
    
  }
  
  # PAM
  if(method == 'pam') {
    spc$L <- lab.colors[, 1]
    spc$A <- lab.colors[, 2]
    spc$B <- lab.colors[, 3]
    
    ## TODO: optimize
    # result is a list
    col.data <- profileApply(
      spc, 
      FUN = .pigments.pam, 
      k = pam.k, 
      simplify = FALSE,
      APPLY.FUN = apply.fun
    )
    
  }
  
  # back to data.frame
  col.data <- do.call('rbind', col.data)
  # reset rownames
  row.names(col.data) <- NULL
  
  return(col.data)
}


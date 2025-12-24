

#' @title Interpret a color signature containing color groups of CIELAB coordinates using perceptual distance via CIE dE00
#' 
#' @param pig `data.frame` results from `soilColorSignature(..., method = c('pam', 'depthSlices))`
#' @return dist object
.colorSig2PerceptualDistMat <- function(pig) {
  
  # first column contains profile IDs
  .ids <- pig[, 1]
  row.names(pig) <- .ids
  
  # remaining columns are color groups of L,A,B coordinates
  .n <- ncol(pig) - 1
  .ngroup <- .n / 3
  
  # iterate over color groups, result is a distance matrix (CIE dE00)
  dE00 <- lapply(1:.ngroup, function(i) {
    
    # LAB coordinates are named L.i, A.i, B.i, ...
    v.names <- paste(c('L', 'A', 'B'), i, sep = '.')
    
    # pair-wise delta-E00
    # IDs are preserved via row.names in pig
    # more efficient than compare_colour(x, x, ...)
    # output is transposed relative to `dist` object
    d.i <- farver::compare_colour(
      pig[, v.names], 
      from_space = 'lab', 
      white_from = 'D65', 
      method = 'cie2000'
    )
    
    # convert to dist object, note transpose
    d.i <- as.dist(t(d.i))
    
    return(d.i)
  })
  
  # sum distance matrices over color groups
  d <- Reduce('+', dE00)
  
  return(d)
}



# compute LAB coordinates from clusters of slices
.pigments.pam <- function(x, k) {
  
  # extract just horizons that have color data
  h <- horizons(x)
  idx <- which(!is.na(h$L))
  h <- h[idx, ]
  
  # can't go on without some data
  ## TODO: this will cause an error in the calling function if all results are NULL
  if(nrow(h) == 0) {
    return(NULL)
  }
  
  
  # cluster colors using frequency weights
  # https://stat.ethz.ch/pipermail/r-help/2008-February/153501.html
  hd <- horizonDepths(x)
  top <- min(h[[hd[1]]], na.rm = TRUE)
  bottom <- max(h[[hd[2]]], na.rm = TRUE)
  
  # slice to unwind weighted data into un-weighted data (weights are thickness)
  fm <- as.formula(paste0(top, ':', bottom, ' ~ L + A + B'))
  x.slices <- suppressMessages(dice(x, fm = fm, SPC = FALSE))
  x.slices <- na.omit(x.slices[, c(idname(x), 'L', 'A', 'B')])
  
  # have to have at least 1 more row than k
  ## TODO: this will cause an error in the calling function if all results are NULL
  if(nrow(x.slices) < k + 1) {
    return(NULL)
  }
  
  # compute perceptually based distance matrix on sliced colors, CIELAB
  # IDs are preserved via row.names in pig
  # more efficient than compare_colour(x, x, ...)
  # output is transposed relative to `dist` object
  dE00 <- farver::compare_colour(
    x.slices[, -1], 
    from_space = 'lab',
    method = 'CIE2000', 
    white_from = 'D65'
  )
  
  # convert to dist object, note transpose
  dE00 <- as.dist(t(dE00))
  
  # use PAM to cluster, note `pamonce = 5` used for optimization
  cl <- cluster::pam(dE00, k = k, diss = TRUE, pamonce = 5)
  
  # subset medoids
  x.medoids <- x.slices[cl$id.med, c(idname(x), 'L', 'A', 'B')]
  
  # 2025-12-18
  # medoid order depends on source data
  # approximate a standardization scheme
  # by ordering medoids along L, A, B axes
  x.medoids <- x.medoids[order(x.medoids$L, x.medoids$A, x.medoids$B), ]
  
  # make IDs
  x.medoids$.ids <- paste0('.', 1:k)
  
  # convert wide -> long and create new variable names
  # using data.table::melt()
  m <- data.table::melt(
    data.table::as.data.table(x.medoids), 
    id.var = c(idname(x), '.ids'), 
    measure.vars = c('L', 'A', 'B')
  )
  
  # leave as data.table for dcast()
  
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
.pigments.depths <- function(x, p) {
  
  # index for naming columns in the results
  .seq <- seq_along(p)
  
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
  sample.depths <- round(quantile(h$.mid, probs = p, na.rm = TRUE, type = 1))
  
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
  m$variable <- sprintf("%s.%s", m$variable, .seq)
  
  # convert long -> wide format
  # using data.table::dcast
  fm <- as.formula(paste0(idname(x), ' ~ variable'))
  res <- dcast(m, formula = fm, value.var = 'value')
  
  # convert back to data.frame
  res <- as.data.frame(res)
  
  # percentiles used
  attr(res, 'p') <- p
  
  # result is a data.frame with profile ID
  return(res)
}



# CIELAB axes interpretation
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
  
  # convert to proportions: remove the effect of soil depth
  if(useProportions) {
    pigment <- pigment / sum(pigment, na.rm = TRUE)
  }
  
  # results
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
#   * data.table optimization
#   * demonstrate convergence with NCSP()

#' @title Soil Profile Color Signatures
#' @description Generate a color signature for each soil profile in a collection.
#'
#' @param spc a `SoilProfileCollection` object
#' 
#' @param color horizon-level attributes, either character of length 1 specifying a column containing Munsell or sRGB in hex notation, or character vector of three column names containing either sRGB or CIELAB color coordinates. sRGB color coordinates should be within the range of 0 to 1.
#' @param space character, either 'sRGB' or 'LAB', specifying color space

#' @param method algorithm used to compute color signature, `colorBucket`, `depthSlices`, or `pam`
#' @param perceptualDistMat logical, optionally return a distance matrix based on perceptual color distances, when ``method` is one of 'depthSlices' or 'pam', see Details

#' @param prob numeric vector, requested percentiles for `method = 'depthSlices'`
#' @param pam.k number of color classes for `method = 'pam'`

#' @param useProportions use proportions or quantities, see details
#' @param pigmentNames names for resulting pigment proportions or quantities
#' @param apply.fun function passed to `aqp::profileApply(APPLY.FUN)` argument, can be used to add progress bars via `pbapply::pblapply()`, or parallel processing with `furrr::future_map()` 
#' 
#' @param r deprecated, use `color` argument
#' @param g deprecated, use `color` argument
#' @param b deprecated, use `color` argument
#' @param RescaleLightnessBy deprecated, scaling factor for CIELAB L-coordinate
#'
#'
#' @details 
#' 
#' Interpreation of color signature.
#' 
#' Choices related to weighting, scaling, and distance metric.
#' 
#' Perceptual distances (dE00), summed over color groups.
#' 
#' See the [related tutorial](http://ncss-tech.github.io/AQP/aqp/soil-color-signatures.html).
#' 
#' @return
#' 
#' For the `colorBucket` method, a `data.frame`:
#' 
#'  * id column: set according to `idname(spc)`
#'  * `.white.pigment`: proportion or quantity of CIELAB L-values
#'  * `.red.pigment`: proportion or quantity of CIELAB positive A-values
#'  * `.green.pigment`: proportion or quantity of CIELAB negative A-values
#'  * `.yellow.pigment`: proportion or quantity of CIELAB positive B-values
#'  * `.blue.pigment`: proportion or quantity of CIELAB negative B-values
#' 
#' Column names can be adjusted with the `pigmentNames` argument.
#' 
#' 
#' For the `depthSlices` method, a `data.frame`:
#' 
#'  * id column: set according to `idname(spc)`
#'  * `L.1`, `A.1`, `B.1`: CIELAB color coordinates associated with the first depth slice, at depth percentile given in `prob[1]`
#'  * ...
#'  * `L.n`, `A.n`, `B.n`: CIELAB color coordinates associated with the `n` depth slice, at depth percentile given in `prob[n]`
#' 
#' 
#' For the `pam` method, a `data.frame`:
#' 
#'  * id column: set according to `idname(spc)`
#'  * `L.1`, `A.1`, `B.1`: CIELAB color coordinates associated with the first color cluster, after sorting all clusters in ascending order along L, A, B axes.
#'  * ...
#'  * `L.n`, `A.n`, `B.n`: CIELAB color coordinates associated with the `nth` color cluster, after sorting all clusters in ascending order along L, A, B axes.
#'  
#'  When `perceptualDistMat = TRUE` and `method` is one of 'depthSlices' or 'pam', a distance matrix is returned.
#'  
#' 
#' @references https://en.wikipedia.org/wiki/Lab_color_space
#' 
#' @author D.E. Beaudette
#' 
#' @seealso [plotProfileDendrogram()]
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
#' # Munsell notation
#' sp1$m <- sprintf("%s %s/%s", sp1$hue, sp1$value, sp1$chroma)
#' 
#' # extract color signature
#' pig <- soilColorSignature(sp1, color = 'm')
#' 
soilColorSignature <- function(
    spc, 
    color,
    space = c('sRGB', 'CIELAB'), 
    method = c('colorBucket', 'depthSlices', 'pam'), 
    perceptualDistMat = FALSE,
    pam.k = 3, 
    prob = c(0.1, 0.5, 0.9),
    useProportions = TRUE, 
    pigmentNames = c('.white.pigment', '.red.pigment', '.green.pigment', '.yellow.pigment', '.blue.pigment'), 
    apply.fun = lapply,
    r = NULL, 
    g = NULL, 
    b = NULL, 
    RescaleLightnessBy = NULL
) {
  
  # 2025-12-16: deprecated arguments r, g, b
  if(!is.null(r) || !is.null(g) || !is.null(b)) {
    warning('arguments `r`, `g`, and `b` have been deprecated, please use the `color` argument instead')
    
    # temporarily cobble together the new specification
    color <- c(r, g, b)
  }
  
  # 2025-12-18: deprecated argument RescaleLightnessBy
  if(!is.null(RescaleLightnessBy)) {
    warning('argument `RescaleLightnessBy` has been deprecated, consider weighting or standardizing color signature matrix before computing distances')
  }
  
  
  # sanity checks fixed choice arguments
  method <- match.arg(method)
  space <- match.arg(space)
  
  # extract horizons
  h <- horizons(spc)
  
  # detect color specification
  # complicated because color could be length 1 or 3
  if(length(color) == 1) {
    # single column
    .spec <- .detectColorSpec(h[[color]])
  } else if(length(color) == 3) {
    # three columns
    .spec <- .detectColorSpec(h[, color])
  } else{
    # error condition
    stop('`color` should be of length 1 or 3')
  }
  
  
  # conditionally convert colors to CIELAB
  lab.colors <- switch(
    .spec,
    
    `hex-sRGB` = {
      # hex encoded sRGB color coordinates
      # must rescale to [0,1]
      convertColor(t(col2rgb(h[[color]]) / 255), from = 'sRGB', to = 'Lab', from.ref.white = 'D65', to.ref.white = 'D65')
    },
    
    `color-coordinate-data.frame` = {
      # 3 columns subset from a data.frame
      .m <- as.matrix(h[, color])
      
      # convert to CIELAB
      # data.frame -> matrix
      if(space == 'sRGB') {
        
        # check for mis-specification of color space
        if(any(range(.m, na.rm = TRUE) > 5)) {
          stop('color space coordinates do not appear to be sRGB [0,1]: check `space` argument')
        }
        
        .res <- convertColor(.m, from = 'sRGB', to = 'Lab', from.ref.white = 'D65', to.ref.white = 'D65')
      }
      
      # no colorspace conversion
      if(space == 'CIELAB') {
        .res <- .m
      }
      
      .res
    },
    
    `munsell` = {
      # plain Munsell notation
      parseMunsell(h[[color]], returnLAB = TRUE)
    },
    
    # all others
    stop('unknown color specifcation')
  )
  
  
  # choose method
  if(method == 'colorBucket') {
    ## L is always positive
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
      p = prob,
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
  
  # set attributes as metadata
  attr(col.data, 'colorspec') <- .spec
  
  # optionally convert color signature -> perceptual distance matrix
  if(perceptualDistMat) {
    return(.colorSig2PerceptualDistMat(col.data))
  } else {
    # just the signature
    return(col.data)
  }
  
}


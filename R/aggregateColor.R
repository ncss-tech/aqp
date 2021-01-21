

#' @title Summarize Soil Colors
#' 
#' @description Summarize soil color data, weighted by occurrence and horizon thickness.
#'
#' @param x a `SoilProfileCollection` object
#' @param groups the name of a horizon or site attribute used to group horizons, see examples
#' @param col the name of a horizon-level attribute with soil color specified in hexadecimal (i.e. "#rrggbb")
#' @param colorSpace the name of color space or color distance metric to use for conversion of aggregate colors to Munsell; either CIE2000 (color distance metric), LAB, or sRGB. Default = 'CIE2000'
#' @param k single integer specifying the number of colors discretized via PAM (cluster package), see details
#' @param profile_wt the name of a site-level attribute used to modify weighting, e.g. area
#'
#' @return A list with the following components:
#' 
#' \item{scaled.data}{a `list` of colors and associated weights, one item for each generalized horizon label with at least one color specified in the source data}
#' \item{aggregate.data}{a `data.frame` of weighted-mean colors, one row for each generalized horizon label with at least one color specified in the source data}
#' 
#' @export
#' 
#' @details Weights are computed by:
#' `w_i = sqrt(sum(thickness_i)) * n_i`
#' where `w_i` is the weight associated with color `i`, `thickness_i` is the total thickness of all horizons associated with the color `i`, and `n_i` is the number of horizons associated with color `i`. Weights are computed within groups specified by `groups`.
#' 
#' @author D.E. Beaudette
#' 
#' @seealso \code{\link{generalize.hz}}
#' 
#' 
#' @examples
#' 
#' # load some example data
#' data(sp1, package='aqp')
#' 
#' # upgrade to SoilProfileCollection and convert Munsell colors
#' sp1$soil_color <- with(sp1, munsell2rgb(hue, value, chroma))
#' depths(sp1) <- id ~ top + bottom
#' site(sp1) <- ~ group
#' 
#' # generalize horizon names
#' n <- c('O', 'A', 'B', 'C')
#' p <- c('O', 'A', 'B', 'C')
#' sp1$genhz <- generalize.hz(sp1$name, n, p)
#' 
#' # aggregate colors over horizon-level attribute: 'genhz'
#' a <- aggregateColor(sp1, groups = 'genhz', col = 'soil_color')
#' 
#' # aggregate colors over site-level attribute: 'group'
#' a <- aggregateColor(sp1, groups = 'group', col = 'soil_color')
#' 
#' # aggregate colors over site-level attribute: 'group'
#' # discretize colors to 4 per group
#' a <- aggregateColor(sp1, groups = 'group', col = 'soil_color', k = 4)
#' 
#' # aggregate colors over depth-slices
#' s <- slice(sp1, c(5, 10, 15, 25, 50, 100, 150) ~ soil_color)
#' s$slice <- paste0(s$top, ' cm')
#' s$slice <- factor(s$slice, levels=guessGenHzLevels(s, 'slice')$levels)
#' a <- aggregateColor(s, groups = 'slice', col = 'soil_color')
#' 
#' \dontrun{
#'   # optionally plot with helper function
#'   if(require(sharpshootR))
#'     aggregateColorPlot(a)
#' }
#' 
#' # a more interesting example
#' \dontrun{
#'   data(loafercreek, package = 'soilDB')
#'   
#'   # generalize horizon names using REGEX rules
#'   n <- c('Oi', 'A', 'BA','Bt1','Bt2','Bt3','Cr','R')
#'   p <- c('O', '^A$|Ad|Ap|AB','BA$|Bw', 
#'          'Bt1$|^B$','^Bt$|^Bt2$','^Bt3|^Bt4|CBt$|BCt$|2Bt|2CB$|^C$','Cr','R')
#'   loafercreek$genhz <- generalize.hz(loafercreek$hzname, n, p)
#'   
#'   # remove non-matching generalized horizon names
#'   loafercreek$genhz[loafercreek$genhz == 'not-used'] <- NA
#'   loafercreek$genhz <- factor(loafercreek$genhz)
#'   
#'   a <- aggregateColor(loafercreek, 'genhz')
#'   
#'   # plot results with helper function
#'   par(mar=c(1,4,4,1))
#'   aggregateColorPlot(a, print.n.hz = TRUE)
#'   
#'   # inspect aggregate data
#'   a$aggregate.data
#' }
#' 
aggregateColor <- function(x, groups = 'genhz', col = 'soil_color', colorSpace = 'CIE2000', k = NULL, profile_wt = NULL) {

  # sanity check
  if(!is.null(k)) {
    k <- round(k)

    # sanity check, need this for color distance eval
    if (!requireNamespace('farver', quietly = TRUE))
      stop('please install the `farver` package.', call.=FALSE)

    if(is.na(k)) {
      stop('k must be a single integer > 0')
    }
  }

  # sanity check: profile weight must be a valid site-level attribute
  if(!is.null(profile_wt)) {
    if(! profile_wt %in% siteNames(x)) {
      stop('`profile_wt` must be a site-level attribute of `x`', call. = FALSE)
    }
  }

  # sanity check: groups must be a horizon-level attribute
  if(! groups %in% names(x))
    stop('`groups` must be a site or horizon attribute of `x`', call. = FALSE)

  # sanity check: col must be a horizon-level attribute
  if(! col %in% horizonNames(x))
    stop('`col` must be a horizon-level attribute of `x`', call. = FALSE)

  if(!colorSpace %in% c("CIE2000","LAB","sRGB"))
    stop('colorSpace must be either: CIE2000, LAB or sRGB', call. = FALSE)

  ## hack to make R CMD check --as-cran happy
  top <- bottom <- thick <- NULL

  # extract pieces
  h <- as(x, 'data.frame')

  # keep track of just those variables we are using
  # profile_wt is NULL by default
  vars <- c(groups, horizonDepths(x), col, profile_wt)

  # remove missing data
  # note: missing color data will result in 'white' when converting to sRGB, why?
  h.no.na <- na.omit(h[, vars])

  # re-name depths
  names(h.no.na)[2:3] <- c('top', 'bottom')

  # safely compute thickness
  # abs() used in rare cases where horizon logic is wrong: e.g. old-style O horizons
  h.no.na$thick <- abs(h.no.na$bottom - h.no.na$top)

  # 0-thickness will result in NA weights
  # replace with a 1-unit slice
  idx <- which(h.no.na$thick == 0)
  if(length(idx) > 0) {
    h.no.na$thick[idx] <- 1
  }

  # apply site-level weighting here, if present
  if(!is.null(profile_wt)) {
    # multiply thickness by site-level weight
    h.no.na$thick <- h.no.na$thick * h.no.na[[profile_wt]]
  }

  # drop empty group levels
  h.no.na[[groups]] <- factor(h.no.na[[groups]])

  # split by genhz
  ss <- split(h.no.na, h.no.na[[groups]])
  
  # iterate over groups
  # note that some genhz will have 0 records
  s <- lapply(ss, function(i) {
    
    # current group label
    this.group <- i[[groups]][1]
    
    # optionally reduce to `k` colors via PAM, by group
    # this will only work if there are >= 1 unique colors
    n.cols <- length(unique(i[[col]]))
    
    if(is.numeric(k) & n.cols > 1) {
      
      # condition number of classes on available data
      k.adj <- pmin(k, n.cols - 1)
      
      # work with a unique subset, there is no need to compute distances / cluster all colors
      lut <- data.frame(col=unique(i[[col]]), stringsAsFactors = FALSE)
      
      # same order as LUT
      # convert to sRGB
      # NA will create bogus colors, filtered above
      v <- t(col2rgb(lut$col) / 255)
      
      # convert to LAB
      v <- convertColor(v, from='sRGB', to='Lab', from.ref.white='D65', to.ref.white = 'D65', clip = FALSE)
      
      # compute perceptually based distance matrix on unique colors
      dE00 <- farver::compare_colour(v, v, from_space='lab', method='CIE2000', white_from='D65')
      dE00 <- as.dist(dE00)
      
      
      # clustering from distance matrix
      # TODO: save clustering results for later
      v.pam <- pam(dE00, k = k.adj, diss = TRUE, pamonce=5)
      
      # put clustering vector into LUT
      lut$cluster <- v.pam$clustering
      
      # get medoid colors
      med.col <- lut$col[v.pam$medoids]
      
      # expand original colors to medoid colors based on LUT
      idx <- base::match(i[[col]], lut$col)
      
      # replace original colors with discretized colors
      i[[col]] <- med.col[lut$cluster[idx]]
    }
    
    # aggregate depth by unique soil color
    # this assumes that thickness > 0, otherwise NaN is returned
    # convert to data.table for summary
    i <- as.data.table(i)
    
    # not sure about most readable style
    res <- i[, 
             list(
               weight = sqrt(sum(thick)) * length(thick),
               n.hz = length(thick)
             ), 
             by = col
    ]
    
    # convert back to data.frame
    res <- as.data.frame(res)
    
    # sort by thickness-- this is our metric for relevance
    res <- res[order(res$weight, decreasing=TRUE), ]
    
    # back-calculate the closest Munsell color
    m <- rgb2munsell(t(col2rgb(res[[col]])) / 255, colorSpace = colorSpace)
    
    # format as text
    res$munsell <- paste0(m[, 1], ' ', m[, 2], '/', m[, 3])
    
    # add group ID
    res[['.id']] <- this.group
    
    return(res)
  })
  
  

  # rescale using the sum of the weights within the current horizon
  s.scaled <- lapply(s, function(i) {
    i$weight <- i$weight / sum(i$weight)
    return(i)
  })
  

  # compute weighted mean color for each group, in CIE LAB colorspace
  # TODO: this is similar to soilDB::estimateColorMixture(), consider consolidation
  # TODO: this is the second pass of color conversion, can it be done in a single pass?
  # TODO: should aggregate colors be mixed from the discretized colors? probably
  # TODO: color mixing should be performed using reflectance spectra
  s.agg <- lapply(s.scaled, function(i) {
    # convert to sRGB
    v <- t(col2rgb(i[[col]])) / 255

    # convert to LAB
    v <- convertColor(v, from='sRGB', to='Lab', from.ref.white='D65', to.ref.white = 'D65', clip = FALSE)

    # compute weighted mean via matrix manip
    w <- i$weight
    vw <- sweep(v, 1, STATS = w, FUN = '*')
    wm <- colSums(vw) / sum(w)

    # convert back to sRGB
    wm <- convertColor(wm, from='Lab', to='sRGB', from.ref.white='D65', to.ref.white = 'D65')
    dimnames(wm)[[2]] <- c('red', 'green', 'blue')

    # convert result back to R color specification
    wm.col <- rgb(wm, maxColorValue = 1)

    # get closest Munsell color
    wm.munsell <- rgb2munsell(wm, colorSpace = colorSpace)

    # consolidate and return
    res <- data.frame(
      .id = i[['.id']][1],
      munsell = wm.munsell, 
      col = wm.col, 
      wm, 
      n = nrow(i)
    )
    
    return(res)
  })
  
  s.agg <- do.call('rbind', s.agg)
  names(s.agg)[1] <- groups

  # return scaled color data
  return(list(scaled.data = s.scaled, aggregate.data = s.agg))
}


## TODO: consider using Munsell notation for `col`, this would save an entire round-trip through the conversion process


#' @title Summarize Soil Colors
#' 
#' @description Summarize soil color data, weighted by occurrence and horizon thickness.
#'
#' @param x a `SoilProfileCollection` object
#' @param groups the name of a horizon or site attribute used to group horizons, see examples
#' @param col the name of a horizon-level attribute with soil color specified in hexadecimal (i.e. "#rrggbb")
#' @param k single integer specifying the number of colors discretized via PAM ([cluster::pam()]), see details
#' @param profile_wt the name of a site-level attribute used to modify weighting, e.g. area
#' 
#' @param mixingMethod method used to estimate "aggregate" soil colors, see [mixMunsell()]
#'
#' @return A list with the following components:
#' 
#'  * `scaled.data`: a `list` of colors and associated weights, one item for each generalized horizon label with at least one color specified in the source data
#'  * `aggregate.data`: a `data.frame` of weighted-mean colors, one row for each generalized horizon label with at least one color specified in the source data
#' 
#' @export
#' 
#' @details Weights are computed by:
#' `w_i = sqrt(sum(thickness_i)) * n_i`
#' where `w_i` is the weight associated with color `i`, `thickness_i` is the total thickness of all horizons associated with the color `i`, and `n_i` is the number of horizons associated with color `i`. Weights are computed within groups specified by `groups`.
#' 
#' See the [related tutorial for additional examples](http://ncss-tech.github.io/AQP/sharpshootR/aggregate-soil-color.html).
#' 
#' @author D.E. Beaudette
#' 
#' @seealso [generalize.hz()], [aggregateColorPlot()]
#' 
#' @examples
#' 
#' # keep examples from using more than 2 cores
#' data.table::setDTthreads(Sys.getenv("OMP_THREAD_LIMIT", unset = 2))
#' 
#' # load some example data
#' data(sp1, package = 'aqp')
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
#' # check results
#' str(a)
#' 
#' # simple visualization
#' aggregateColorPlot(a)
#' 
aggregateColor <- function(x, groups = 'genhz', col = 'soil_color', k = NULL, profile_wt = NULL, mixingMethod = c('estimate', 'exact')) {
  
  # sanity check
  mixingMethod <- match.arg(mixingMethod)
  
  # sanity check
  if(!is.null(k)) {
    k <- round(k)
    
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
      v <- convertColor(
        v, 
        from = 'sRGB', 
        to = 'Lab', 
        from.ref.white = 'D65', 
        to.ref.white = 'D65', 
        clip = FALSE
      )
      
      # compute perceptually based distance matrix on unique colors
      dE00 <- farver::compare_colour(v, v, from_space = 'lab', method = 'CIE2000', white_from = 'D65')
      dE00 <- as.dist(dE00)
      
      # clustering from distance matrix
      # TODO: save clustering results for later
      v.pam <- cluster::pam(dE00, k = k.adj, diss = TRUE, pamonce = 5)
      
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
    i <- data.table::as.data.table(i)
    
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
    
    ## TODO: this is wasteful, as we likely "knew" the munsell notation before-hand
    # back-calculate the closest Munsell color
    m <- col2Munsell(res[[col]])
    
    
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
  
  
  # iteration over groups, estimation of:
  # aggregate colors via mixing
  # number of associated colors
  # Shannon H, base 2
  s.agg <- lapply(s.scaled, function(i) {
    
    # estimation of mixture via wt. mean in CIELAB coordinates
    mix <- mixMunsell(x = i$munsell, w = i$weight, mixingMethod = mixingMethod)
    
    # split into pieces for backwards-compatibility
    m.pieces <- parseMunsell(mix$munsell, convertColors = FALSE)
    col <- parseMunsell(mix$munsell)
    
    # consolidate and return
    res <- data.frame(
      .id = i[['.id']][1],
      m.pieces,
      munsell = mix$munsell,
      distance = mix$distance,
      col = col, 
      n = nrow(i),
      H = shannonEntropy(i$weight)
    )
    
    return(res)
  })
  
  s.agg <- do.call('rbind', s.agg)
  names(s.agg)[1] <- groups
  row.names(s.agg) <- NULL
  
  .res <- list(scaled.data = s.scaled, aggregate.data = s.agg)
  
  # return scaled color data
  return(.res)
}

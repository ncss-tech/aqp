## TODO: replace dlply and ddply with base functions

# x: SPC
# groups: groups to aggregate over, can be either site/horizon attribute, need not be a factor
# col: r-compatible hex notation of color
# k: number of groups to reduce colors to (within each group), conditioned on unique colors available
# profile_wt: weighting via site-level attribute
aggregateColor <- function(x, groups='genhz', col='soil_color', colorSpace = 'CIE2000', k=NULL, profile_wt=NULL) {

  # sanity check
  if(!is.null(k)) {
    k <- round(k)

    # sanity check, need this for color distance eval
    if(!requireNamespace('farver', quietly = TRUE))
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
  # note that some genhz will have 0 records
  s <- dlply(h.no.na, groups, function(i) {

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
    res <- ddply(i, col, summarise, weight=sqrt(sum(thick)) * length(thick), n.hz=length(thick))


    # sort by thickness-- this is our metric for relevance
    res <- res[order(res$weight, decreasing=TRUE), ]

    # back-calculate the closest Munsell color
    m <- rgb2munsell(t(col2rgb(res[[col]])) / 255, colorSpace=colorSpace)

    # format as text
    res$munsell <- paste0(m[, 1], ' ', m[, 2], '/', m[, 3])

    return(res)
  })

  # rescale using the sum of the weights within the current horizon
  s.scaled <- lapply(s, function(i) {
    i$weight <- i$weight / sum(i$weight)
    return(i)
  })


  # compute weighted mean color for each GHL, in LAB colorspace
  # TODO: this is similar to soilDB::mix_and_clean_colors(), consider consolidation
  # TODO: this is the second pass of color conversion, can it be done in a single pass?
  # TODO: should aggregate colors be mixed from the discretized colors? probably
  s.agg <- ldply(s.scaled, function(i) {
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
    res <- data.frame(munsell=wm.munsell, col=wm.col, wm, n=nrow(i))
    return(res)
  })
  names(s.agg)[1] <- groups

  # return scaled color data
  return(list(scaled.data=s.scaled, aggregate.data=s.agg))
}

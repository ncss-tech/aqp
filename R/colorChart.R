

#' @title Visualize soil colors in Munsell notation according to within-group frequency.
#' @param m character vector of color in Munsell notation ('10YR 4/6')
#' 
#' @param g factor describing group membership, typically a generalization of horizon designation, default value will generate a fake grouping that covers all of the colors in `m`
#' 
#' @param size logical, encode group-wise frequency with chip size
#' 
#' @param annotate logical, annotate color chip frequency
#' 
#' @param chip.cex scaling factor applied to each color chip
#' 
#' @param chip.cex.min lower limit for color chip frequency depiction
#' 
#' @param chip.cex.max lower limit for color chip frequency depiction
#' 
#' @param chip.border.col color for chip borders (outline)
#' 
#' @param annotate.cex scaling factor for chip frequency annotation
#' 
#' @param annotate.type character, within-group `count` or `percentage`
#'
#' @param threshold numeric within 0-1, color chips with proportion `< threshold` are removed
#'
#' @return a `trellis` object
#' 
#' @export
#' @examples
#' 
#' # required for latticeExtra:useOuterStrips
#' if(!requireNamespace('latticeExtra')) {
#'   
#'   # two hue pages
#'   ric <- expand.grid(
#'     hue = c('5YR', '7.5YR'),
#'     value = 2:8,
#'     chroma = 2:8
#'   )
#'   
#'   # combine hue, value, chroma into standard Munsell notation
#'   ric <- sprintf("%s %s/%s", ric$hue, ric$value, ric$chroma)
#'   
#'   # note that chip frequency-based size is disabled 
#'   # because all chips have equal frequency
#'   colorChart(ric, chip.cex = 4, size = TRUE)
#'   
#'   # annotation of frequency
#'   colorChart(ric, chip.cex = 4, annotate = TRUE)
#'   
#'   # bootstrap to larger size
#'   ric.big <- sample(ric, size = 100, replace = TRUE)
#'   
#'   # frequency can be encoded in size
#'   colorChart(ric.big, chip.cex = 3)
#'   colorChart(ric.big, chip.cex = 5, annotate = TRUE)
#'      
#'   # constant size
#'   colorChart(ric.big, chip.cex = 3, size = FALSE)
#'   colorChart(ric.big, chip.cex = 3, size = FALSE, chip.border.col = 'NA')
#'   
#'   # simulate colors based dE00 thresholding
#'   p <- list(
#'     list(m = '10YR 4/4', thresh = 10, hues = c('10YR', '7.5YR'))
#'   )
#'   
#'   # perform 500 simulations
#'   s <- simulateColor(method = 'dE00', n = 500, parameters = p)
#'   
#'   # result is a list, use the first element
#'   colorChart(s[[1]], chip.cex = 4)
#'   
#'   # increase the possible range of color chip sizes
#'   colorChart(s[[1]], chip.cex = 4, chip.cex.min = 0.01, chip.cex.max = 2)
#'   
#'   # slightly funky support for neutral hues
#'   N <- sprintf('N %s/', 2:8)
#'   cols <- c(rep(N, times = 5), ric.big)
#'   
#'   # note special panel used to show neutral hues
#'   colorChart(cols, size = FALSE, annotate = TRUE)
#'   
#'   # filter proportions below given threshold
#'   colorChart(cols, size = FALSE, annotate = TRUE, threshold = 0.01, 
#'   chip.cex = 4, annotate.type = 'percentage')
#'   
#' }
#' 
colorChart <- function(m, g = factor('All'), size = TRUE, annotate = FALSE, chip.cex = 3, chip.cex.min = 0.1, chip.cex.max = 1.5, chip.border.col = 'black', annotate.cex = chip.cex * 0.25, annotate.type = c('count', 'percentage'), threshold = NULL) {
  
  # custom panel function defined here, so that it can "find" data within the colorChart function scope
  panel.colorChart <- function(x, y, subscripts = subscripts, ...) {
    
    # panel-wise subset of required data
    p.data <- tab[subscripts, ]
    p.data$x <- x
    p.data$y <- y
    
    # grid lines: use original data
    if(nrow(p.data) > 0) {
      panel.abline(
        v = unique(p.data$x), 
        h = unique(p.data$y), 
        col = grey(0.85), 
        lty = 3
      )
    }
    
    
    # plot single instances of each chip, transparency weighted by frequency
    panel.points(
      p.data$x, 
      p.data$y, 
      pch = 22, 
      col = chip.border.col,
      fill = p.data$transformed.col, 
      cex =  p.data$chip.size
    )
    
    # annotate chips
    if(annotate) {
      
      # count or percentage
      # these are computed by group NOT by panel
      ann.txt <- switch(
        annotate.type,
        'count' = round(p.data$count),
        'percentage' = signif(100 * p.data$prop, 2)
      )
      
      # adjust based on lightness
      anno.col <- invertLabelColor(p.data$.color)
      
      # within-group frequency
      panel.text(
        x = p.data$x, 
        y = p.data$y, 
        labels = ann.txt, 
        cex = annotate.cex, 
        col = anno.col
      )
    }
    
  }
  
  
  # requires latticeExtra and scales
  if(!requireNamespace('latticeExtra', quietly = TRUE) | !requireNamespace('scales', quietly = TRUE)) {
    stop('pleast install the `latticeExtra` and `scales` packages.', call.=FALSE)
  }
  
  # annotation type
  if(annotate) {
    annotate.type <- match.arg(annotate.type)
  }
  
  # extract pieces / convert colors
  z <- data.frame(
    .munsell = m,
    .groups = g,
    stringsAsFactors = FALSE
  )
  
  # remove obvious NA in colors or groups
  z <- na.omit(z)
  
  # bogus colors can creep-in when composing Munsell notation from pieces
  # remove anything like 'NA NA/NA'
  idx <- grep('NA', z$.munsell, fixed = TRUE, invert = TRUE)
  z <- z[idx, ]
  
  # within-group counts + proportions
  tab <- as.data.frame(table(z$.groups, z$.munsell))
  names(tab) <- c('.groups', '.munsell', 'count')
  
  # group totals
  group.totals <- as.data.frame(table(z$.groups))
  names(group.totals) <- c('.groups', 'total')
  
  # combine
  tab <- merge(tab, group.totals, by = '.groups', all.x = TRUE, sort = FALSE)
  
  # within-group proportions
  tab$prop <- tab$count / tab$total
  
  # filter 0s
  tab$prop <- ifelse(is.na(tab$prop), 0, tab$prop)
  tab <- tab[which(tab$prop > 0), ]
  
  # apply optional threshold on proportions
  if( !is.null(threshold) ) {
    # sanity check
    if(threshold < 0 && threshold > 1) {
      message('ignoring out of bounds threshold')
    } else {
     # apply threshold
      tab <- tab[which(tab$prop > threshold), ]
    }
  }
  
  # check for 0 rows (too much filtering)
  if(nrow(tab) < 1) {
    stop('all rows have been removed, consider setting `threshold` higher', call. = FALSE)
  }
  
  # convert colors
  tab$.color <- parseMunsell(tab$.munsell)
  
  ## TODO: probably a cleaner way
  pm <- parseMunsell(munsellColor = tab$.munsell, convertColors = FALSE)
  tab$hue <- pm$hue
  tab$value <- pm$value
  tab$chroma <- pm$chroma
  
  # remove missing colors / groups
  idx <- which(complete.cases(tab[, c('hue', 'value', 'chroma', '.groups')]))
  tab <- tab[idx, ]
  
  # encode hue as factor using standard hue order
  # second call to factor() drops unused levels
  # note special argument to include neutral hues
  # N is in position 41, we need it in position 1
  hp <- huePosition(returnHues = TRUE, includeNeutral = TRUE)
  N.idx <- match('N', hp)
  hp <- c(hp[N.idx], hp[-N.idx])
  
  tab$hue <- factor(
    factor(tab$hue, levels = hp)
  )
  
  # disable variable size when all frequencies are the same (no useful information)
  if(length(unique(tab$count)) == 1) {
    no.differeneces <- TRUE
  } else {
    no.differeneces <- FALSE
  }
  

  # variable chip size
  if(size) {
    if(no.differeneces) {
      tab$chip.size <- chip.cex
    } else {
      tab$chip.size <- sqrt(scales::rescale(tab$prop, to = c(chip.cex.min, chip.cex.max))) * chip.cex
    }
  } else {
    tab$chip.size <- chip.cex
  }
  
  ## TODO: thematic coloring based on density
  ## needs to be adjusted a group at a time
  # .f <- scales::col_numeric(viridis::viridis(100), domain = c(0, max(tab$prop)))
  # tab$transformed.col <- .f(tab$prop)
  
  # chip color is not modified for now
  tab$transformed.col <- tab$.color
  
  ## TODO: 
  # * consider reporting Shannon entropy / group
  #   print(shannonEntropy(tab$prop))
  
  # set reasonable limits: 
  # value / chroma: 2-8 unless data extend beyond those limits
  # exception for N hues (chroma 0), min value is chroma of 1
  x.lim <- c(
    pmin(pmax(min(tab$chroma), 1), 2) - 0.5,
    pmax(max(tab$chroma), 8) + 0.5
  )
  
  y.lim <- c(
    pmin(min(tab$value), 2) - 0.5,
    pmax(max(tab$value), 8) + 0.5
  )
  
  # assemble basic plot
  pp <- lattice::xyplot(
    value ~ chroma | hue + .groups, 
    data = tab,
    as.table = TRUE,
    subscripts = TRUE, 
    # this works until we attempt to use neutral hues
    xlim = x.lim,
    ylim = y.lim,
    # simple version when no neutral hues
    scales = list(
      alternating = 3, 
      y = list(rot = 0)
    ), 
    main = '', 
    xlab = 'Chroma', 
    ylab = 'Value', 
    panel = panel.colorChart
  )
  
  ## TODO: simplify this
  ## more complex figure required when neutral hues are present
  ll <- levels(tab$hue)
  if('N' %in% ll) {
    
    # standards x-limits
    x.at <- seq(x.lim[1] + 0.5, x.lim[2] - 0.5, by = 1)
    
    # replicate as many times are there are columns
    # first column (N) gets no axes
    x.at.list <- c(
      list(''), 
      lapply(1:(length(ll)-1), FUN = function(i) {x.at})
    )
    
    # limits for as many columns
    x.limits.list <- c(
      list(c(0,0)), 
      lapply(1:(length(ll)-1), FUN = function(i) {x.lim})
    )
    
    # replicate to cover all columns * rows
    n.groups <- length(levels(tab$.groups))
    x.at.list <- rep(x.at.list, times = n.groups)
    x.limits.list <- rep(x.limits.list, times = n.groups)
    
    pp <- lattice::xyplot(
      value ~ chroma | hue + .groups, 
      data = tab,
      as.table = TRUE,
      subscripts = TRUE, 
      # panel-specific limits gets complicated
      scales = list(
        alternating = 3, 
        y = list(rot = 0, limits = y.lim), 
        x = list(
          relation = 'free', 
          at = x.at.list,
          limits = x.limits.list
        )
      ), 
      main = '', 
      xlab = 'Chroma', 
      ylab = 'Value', 
      panel = panel.colorChart
    )
    
    # proportional panel sizes, neutral gets 20% of normal space
    pp <- latticeExtra::resizePanels(pp, w = c(0.2, rep(1, times = length(ll) - 1)))
  }
  
  # move paneling by groups to outer panels
  pp <- latticeExtra::useOuterStrips(
    pp, 
    strip = strip.custom(bg = grey(0.85), par.strip.text = list(cex=0.85)), 
    strip.left = strip.custom(bg = grey(0.85), par.strip.text = list(rot=0))
  )
  
  if(annotate) {
    sub.txt <- sprintf('chip labels represent %ss', annotate.type)
    pp <- update(pp, sub = list(sub.txt, font = 1))
  }
  
  
  
  
  return(pp)  
}

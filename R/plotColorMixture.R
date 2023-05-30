
#' @title Visualize Spectral Mixing of Munsell Colors
#' 
#' @description Lattice visualization demonstrating subtractive mixtures of colors in Munsell notation and associated spectra.
#' 
#' @details If present, `names` attribute of `x` is used for the figure legend.
#' 
#' @author D.E. Beaudette
#' 
#' @param x vector of colors in Munsell notation, should not contain duplicates
#' 
#' @param w vector of weights, can sum to any number
#' 
#' @param mixingMethod approach used to simulate a mixture: 
#'    
#'    * `exact`: simulate a subtractive mixture of pigments, color conversion via CIE1931 color-matching functions (see [mixMunsell()])
#'    
#'    * `reference`  : simulate a subtractive mixture of pigments, selecting `n` closest reference spectra, requires `gower` package
#' 
#' @param n number of closest mixture candidates when `mixingMethod = 'reference'` (see [mixMunsell()]), results can be hard to interpret when `n > 2`
#' 
#' @param swatch.cex scaling factor for color swatch rectangle width and height, relative to `label.cex`, typically between 1 and 3
#' 
#' @param label.cex scaling factor for swatch labels
#' 
#' @param showMixedSpec show weighted geometric mean (mixed) spectra as dotted line (only when `mixingMethod = 'reference'`)
#' 
#' @param overlapFix attempt to "fix" overlapping chip labels via [fixOverlap()], using `method = 'E'`
#' 
#' @return a `lattice` graphics object
#' 
#' @seealso [mixMunsell()]
#' @export
#' 
#' @examples 
#' 
#' # color chips
#' chips <- c('5B 5/10', '5Y 8/8')
#' names(chips) <- chips
#' 
#' # weights
#' wt <- c(1, 1)
#' 
#' if(requireNamespace("gower")) {
#' plotColorMixture(
#' x = chips, 
#' w = wt, 
#' label.cex = 0.65, 
#' showMixedSpec = TRUE, 
#' mixingMethod = 'reference'
#' )
#'
#'}
#'
#' 
#' plotColorMixture(
#'   x = chips, 
#'   w = wt, 
#'   label.cex = 0.65, 
#'   mixingMethod = 'exact'
#' )
#' 
plotColorMixture <- function(x, w = rep(1, times = length(x)) / length(x), mixingMethod = c('exact', 'reference'), n = 1, swatch.cex = 1.5, label.cex = 0.85, showMixedSpec = FALSE, overlapFix = TRUE) {
  
  # TODO plot will be incorrect if duplicate Munsell chips are specified
  
  # TODO: feedback on spectral distance is required
  
  # TODO: ideas on styling legend (size, placement, etc.)
  
  # mixture method sanity checks
  mixingMethod <- match.arg(mixingMethod)
  
  # 'reference' mixing method requires gower package
  if(mixingMethod == 'reference' & !requireNamespace('gower')) {
    stop('package `gower` is required for `reference` mixingMethod', call. = FALSE)
  }
  
  # can't use n > 1 with mixingMethod = 'exact'
  if(mixingMethod == 'exact') {
    
    if(n > 1 ) {
      stop('cannot request multiple matches with `mixingMethod = "exact"`', call. = FALSE)
    }
    
    # must retain mixed spectra
    showMixedSpec <- TRUE
  }
  
  # mix colors
  mx <- suppressMessages(
    mixMunsell(x = x, w = w, n = n, mixingMethod = mixingMethod, keepMixedSpec = showMixedSpec)
  )
  
  # make local copy of the mixed colors when asking for the mixed spectra too
  if(showMixedSpec) {
    m <- mx$mixed
  } else {
    m <- mx
  }
  
  # sanity check: it could be that reference spectra aren't available for requested colors
  if(all(is.na(m$munsell))) {
    stop('reference spectra not available', call. = FALSE)
  }
  
  # satisfy R CMD check
  munsell.spectra <- NULL
  ID <- NULL
  
  # safely load reference spectra
  load(system.file("data/munsell.spectra.rda", package="aqp")[1])
  
  # vector of colors to mix and result
  colors <- c(x, m$munsell)
  
  ## TODO: this will fail if a color is not in the spectral library
  #        -> possibly when mixingMethod = 'exact'
  #        solution: provide a template data.frame
  
  # select spectra from reference library and assign an ID
  # IDs should use names(x) if !NULL
  # otherwise generate an ID
  nm <- names(x)
  if(is.null(nm)) {
    IDs <- sprintf('color %s', 1:length(x))
  } else {
    IDs <- nm
  }
  
  # iteration over colors to-mix + mixture(s)
  s <- lapply(seq_along(colors), function(i) {
    # select current color + spectra
    if(mixingMethod == 'reference') {
      # all colors selected from library
      z <- munsell.spectra[which(munsell.spectra$munsell == colors[i]), ]
      
    } else {
      # exact mixing, last color is mixed spectrum
      z <- munsell.spectra[which(munsell.spectra$munsell == colors[i]), ]
      
      # last color is the mixture, 
      # replace reference spectra / munsell chip with actual mixture
      if(i == length(colors)) {
        z$reflectance <- mx$spec
        z$munsell <- mx$mixed$munsell
      }
      
    }
    
    
    # assign an ID for plotting
    if( i <= length(x)) {
      # current ID
      z$ID <- IDs[i]
    } else {
      # reset counter to mix color ranks
      z$ID <- sprintf('mix #%s', i - length(x))
    }
    
    return(z)
  })
  
  s <- do.call('rbind', s)
  row.names(s) <- NULL
  
  # create sensible levels for plotting / legend
  # names + mixtures
  all.names <- unique(s$ID)
  
  # mixtures, sorted
  mix.names <- sort(setdiff(all.names, nm))
  
  # original chip names + mixtures
  color.chip.levels <- c(nm, mix.names)
  
  # encode chips names + mixture names as into factor
  s$ID <- factor(s$ID, levels = color.chip.levels)
  
  # convert into colors for plotting
  s$color <- parseMunsell(s$munsell)
  
  # plotting style, colors sorted by mixing logic
  cols <- parseMunsell(colors)
  
  # line style
  #  1: colors-to-mix
  #  4: mixture results (n)
  col.lty <- c(
    rep(1, times = length(x)), 
    rep(4, times = nrow(m))
  )
  
  # compile line styles
  tps <- list(
    superpose.line = list(
      col = cols, 
      lwd = 5, 
      lty = col.lty
    ))
  
  # labels for figure
  #  all colors
  munsell.labels <- colors
  # weights
  wt.labels <- sprintf('%s%%', round((w / sum(w)) * 100))
  # ranked matches
  match.rank <- sprintf("#%s", seq(from = 1, to = nrow(m)))
  # combined labels
  lab.text <- sprintf('%s\n%s', munsell.labels, c(wt.labels, match.rank))
  
  ## TODO: use grid::grid.layout() / grid viewports to manage a multi-panel figure
  
  # final figure
  pp <- xyplot(
    reflectance ~ wavelength, groups = ID, data = s, 
    type = c('l', 'g'),
    ylab = 'Reflectance',
    xlab = 'Wavelength (nm)',
    scales = list(tick.number = 12),
    auto.key = list(lines = TRUE, points = FALSE, cex = 1, space='top', columns = length(colors)),
    par.settings = tps,
    xlim = c(370, 780),
    subscripts = TRUE,
    panel = function(x, y, groups, ...) {
      # setup plot
      lattice::panel.xyplot(x = x, y = y, groups = groups, ...)
      
      # split spectra by groups
      d <- split(y, groups)
      
      # get last y-coordinate by group
      last.y.coords <- sapply(d, function(i) {
        i[length(i)]
      })
      
      # get width / height of labels
      # dummy label is not printed
      .dummyLabel <- grid::grid.text(
        label = '10GY 4/4\n100%',
        x = grid::unit(750, units = 'native'), 
        y = grid::unit(max(y, na.rm = TRUE), units = 'native'),
        gp = grid::gpar(
          cex = label.cex,
          font = 2
        ), draw = FALSE
      )
      
      .labelHeight <- grid::convertHeight(
        grid::unit(1, 'grobheight', .dummyLabel),
        unitTo = 'native'
      )
      
      .labelWidth <- grid::convertWidth(
        grid::unit(1, 'grobwidth', .dummyLabel),, 
        unitTo = 'native'
      )
      
      ## debugging grob geom
      # print(cbind(.labelWidth, .labelHeight))
      
      
      # attempt to fix overlap
      if(overlapFix) {
        
        # vertical threshold is the grob height of color swatch
        ov.thresh <- as.vector(.labelHeight) * swatch.cex
        
        # attempt to find overlap using the above threshold
        ov <- overlapMetrics(last.y.coords, thresh = ov.thresh)
        
        # if present, iteratively find suitable alternatives
        # search is bounded to the min/max of all spectra
        # consider set.seed() for consistent output
        if(length(ov$idx) > 0) {
          message('fixing overlap')
          
          ## TODO: finish electrostatic simulation adjustments before conversion
          
          # init position vector 
          # set boundary conditions for overlap adjustment
          # adding fake min / max values, based on spectra reflectance values
          .pos <- c(min(y, na.rm = TRUE), last.y.coords, max(y, na.rm = TRUE))
          
          last.y.coords <- fixOverlap(
            .pos, method = 'S',
            thresh = ov.thresh * 0.85
          )
          
          # ignore first and last positions
          # those are the boundary conditions
          last.y.coords <- last.y.coords[2:(length(.pos) - 1)]
        }
      }
      
      
      
      # iterate over groups and put munsell chip next to last data point
      for(i in seq_along(d)) {
        # last spectral value
        yy <- d[[i]]
        
        # last y-value in this group
        last.y <- last.y.coords[i]
        
        # current color
        this.col <- tps$superpose.line$col[i]
        # current label
        
        # rectangle color swatch surrounds label
        # label widths and heights are used to size t he rectangle (native units)
        grid::grid.rect(
          x = grid::unit(750, units = 'native'), 
          y = grid::unit(last.y, units = 'native'), 
          width = .labelWidth * swatch.cex,
          height = .labelHeight * swatch.cex,
          default.units = 'native',
          gp = grid::gpar(
            col = this.col,
            fill = this.col
          )
        )
        
        # place label
        grid::grid.text(
          label = lab.text[i],
          x = grid::unit(750, units = 'native'), 
          y = grid::unit(last.y, units = 'native'),
          gp = grid::gpar(
            cex = label.cex,
            col = invertLabelColor(this.col),
            font = 2
          )
          
        )
      }
      
      # the mixed spectra is only shown as a dotted line when mixingMethod = 'reference'
      if(showMixedSpec & mixingMethod != 'exact'){
        panel.lines(x = unique(s$wavelength), y = mx$spec, lty = 3, col = 'black')
      }
      
      
    }
  )
  
  return(pp)
  
}



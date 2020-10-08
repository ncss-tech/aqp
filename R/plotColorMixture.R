
#' @title Visualize Spectral Mixing of Munsell Colors
#' 
#' @description Lattice visualization demonstrating subtractive mixtures of colors in Munsell notation and associated spectra.
#' 
#' @author D.E. Beaudette
#' 
#' @param x vector of colors in Munsell notation
#' 
#' @param w vector of proportions, can sum to any number
#' 
#' @return lattice graphics object
#' 
plotColorMixture <- function(x, w = rep(1, times = length(x)) / length(x), swatch.cex = 6, label.cex = 0.85) {
  
  # sanity checks
  
  # mix colors
  m <- mixMunsell(x = x, w = w)
  
  # satisfy R CMD check
  munsell.spectra <- NULL
  
  # safely load reference spectra
  load(system.file("data/munsell.spectra.rda", package="aqp")[1])
  
  colors <- c(x, m$munsell)
  
  s <- lapply(seq_along(colors), function(i) {
    # select current color + spectra
    z <- munsell.spectra[which(munsell.spectra$munsell == colors[i]), ]
    
    # assign an ID for plotting
    if( i < length(colors)) {
      z$ID <- sprintf('color %s', i)
    } else {
      z$ID <- 'mixture'
    }
    
    return(z)
  })
  
  s <- do.call('rbind', s)

  # set ID factor levels
  # sorting is automatic because "color X" always comes before "mixture"
  s$ID <- factor(s$ID)
  
  # convert into colors for plotting
  s$color <- parseMunsell(s$munsell)
  
  # plotting style, colors sorted by mixing logic
  cols <- parseMunsell(colors)
  
  col.lty <- c(rep(1, times = length(x)), 4)
  tps <- list(superpose.line = list(col = cols, lwd = 5, lty = col.lty))
  
  # labels for figure
  munsell.labels <- colors
  wt.labels <- round((w / sum(w)) * 100)
  lab.text <- sprintf('%s\n%s%%', munsell.labels, c(wt.labels, 100))
  
  # final figure
  pp <- xyplot(
    reflectance ~ wavelength, groups=ID, data=s, 
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
      
      # iterate over groups and put munsell chip next to last data point
      for(i in seq_along(d)) {
        # last spectral value
        yy <- d[[i]]
        last.y <- yy[length(yy)]
        # current color
        this.col <- tps$superpose.line$col[i]
        # current label
        
        # place symbol with appropriate color
        grid::grid.points(
          x = grid::unit(750, units = 'native'), 
          y = grid::unit(last.y, units = 'native'), 
          pch = 15, 
          gp = grid::gpar(
            col = this.col,
            cex = swatch.cex
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
      
      
    }
  )
  
  return(pp)

}



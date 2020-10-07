
# weighted geometric mean
# https://en.wikipedia.org/wiki/Weighted_geometric_mean
.wgm <- function(v, w) {
  r <- sum(w * log(v)) / sum(w)
  r <- exp(r)
  return(r)
}



mixMunsell <- function(m1, m2, w1 = 0.5, w2 = 0.5) {
  
  # safely load reference spectra
  load(system.file("data/munsell.spectra.rda", package="aqp")[1])
  # wide version for fast searches
  load(system.file("data/munsell.spectra.wide.rda", package="aqp")[1])
  
  ## TODO: generalize to more than 2 colors
  
  # subset reference spectra for colors
  idx <- which(munsell.spectra$munsell %in% c(m1, m2))
  s <- munsell.spectra[idx, ]
  
  # long -> wide
  s.wide <- dcast(s, munsell ~ wavelength, value.var = 'reflectance')
  
  # spectra as vectors
  # columns are wavelength
  s.1 <- s.wide[1, -1]
  s.2 <- s.wide[2, -1]
  
  # prepare weights
  wts <- c(w1, w2)
  
  # empty vector for mixture
  mixed <- vector(mode = 'numeric', length = length(s.1[1, ]))
  
  # iterate over wavelength (columns in first spectra)
  for(i in seq_along(s.1)) {
    
    # prepare values
    vals <- c(
      s.1[1, i],
      s.2[1, i]
    )
    
    # mix via weighted geometric mean
    mixed[i] <- .wgm( v = vals, w = wts )
  }
  
  # subtract the mixture spectra, element-wise, from reference library
  # note we are removing the wavelength column
  m.diff <- sweep(munsell.spectra.wide[, -1], MARGIN = 1, STATS = mixed, FUN = '-')
  
  # euclidean distance is sufficient
  # D = sqrt(sum(reference - mixed))
  m.dist <- sqrt(colSums(m.diff^2))
  
  # get the spectra of the closest munsell chip
  m.match <- sort(m.dist)[1]
  
  # compile into data.frame
  res <- data.frame(
    munsell = names(m.match),
    distance = m.match,
    stringsAsFactors = FALSE
  )
  
  # clean-up rownames
  row.names(res) <- as.character(1:nrow(res))
  
  
  ## TODO: this will have to be re-factored for >2 mixes
  # re-subset reference library for plotting: source colors and the mixture
  m.1 <- munsell.spectra[which(munsell.spectra$munsell == m1), ]
  m.1$ID <- 'color 1'
  
  m.2 <- munsell.spectra[which(munsell.spectra$munsell == m2), ]
  m.2$ID <- 'color 2'
  
  m.3 <- munsell.spectra[which(munsell.spectra$munsell == res$munsell), ]
  m.3$ID <- 'mixture'
  
  # combine
  s <- rbind(
    m.1,
    m.2,
    m.3
  )
  
  # set ID factor levels
  s$ID <- factor(s$ID, levels = c('color 1', 'color 2', 'mixture'))
  
  # convert into colors for plotting
  s$color <- parseMunsell(s$munsell)
  
  # plotting style, colors sorted by mixing logic
  cols <- parseMunsell(c(m1, m2, res$munsell))
  tps <- list(superpose.line = list(col = cols, lwd = 5, lty = c(1, 1, 4)))
  
  # labels for figure
  munsell.labels <- c(m1, m2, res$munsell)
  wt.labels <- round((c(w1, w2) / sum(c(w1, w2))) * 100)
  lab.text <- sprintf('%s\n%s%%', munsell.labels, c(wt.labels, 100))
  
  # final figure
  pp <- xyplot(
    reflectance ~ wavelength, groups=ID, data=s, 
    type = c('l', 'g'),
    ylab = 'Reflectance',
    xlab = 'Wavelength (nm)',
    scales = list(tick.number = 12),
    auto.key = list(lines = TRUE, points = FALSE, cex = 1, space='top', columns = 3),
    par.settings = tps,
    xlim = c(370, 780),
    subscripts = TRUE,
    panel = function(x, y, groups, ...) {
      # setup plot
      panel.xyplot(x = x, y = y, groups = groups, ...)
      
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
          x = unit(750, units = 'native'), 
          y = unit(last.y, units = 'native'), 
          pch = 15, 
          gp = gpar(
            col = this.col,
            cex = 6
          )
        )
        
        # place label
        grid::grid.text(
          label = lab.text[i],
          x = unit(750, units = 'native'), 
          y = unit(last.y, units = 'native'),
          gp = gpar(
            cex = 0.85,
            col = invertLabelColor(this.col)
          )
          
        )
      }
      
      
    }
  )
  
  return(list(
    fig = pp,
    mixed = res
  ))
}




#' @title Visualize soil colors in Munsell notation according to within-group frequency.
#' 
#' @description 
#'
#' @param m character vector of color in Munsell notation ('10YR 4/6')
#' @param g factor describing group membership, typically a generalization of horizon designation
#' @param chip.alpha transparency applied to each "chip"
#' @param chip.cex scaling factor applied to each "chip"
#'
#' @return a `trellis` object
#' @export
#'
#' @examples
colorChart <- function(m, g, chip.alpha = 0.25, chip.cex = 2) {
  
  
  # requires latticeExtra and scales
  if(!requireNamespace('latticeExtra', quietly = TRUE) | !requireNamespace('scales', quietly = TRUE)) {
    stop('pleast install the `latticeExtra` and `scales` packages.', call.=FALSE)
  }
  
  # extract pieces / convert colors
  z <- parseMunsell(munsellColor = m, convertColors = FALSE)
  z$.color <- parseMunsell(m)
  z$.groups <- g
  z$.munsell <- m
  
  # remove missing colors / groups
  idx <- which(complete.cases(z[, c('hue', 'value', 'chroma', '.groups')]))
  z <- z[idx, ]
  
  # encode hue as factor using standard hue order
  # second call to factor() drops unused levels
  z$hue <- factor(
    factor(z$hue, levels = huePosition(returnHues = TRUE))
    )
  
  ## TODO: more intelligent setting of limits
  
  x.lim <- c(
    pmin(min(z$chroma), 1) - 0.5,
    pmax(max(z$chroma), 8) + 0.5
  )
  
  y.lim <- c(
    pmin(min(z$value), 1) - 0.5,
    pmax(max(z$value), 8) + 0.5
  )
  
  # assemble basic plot
  pp <- xyplot(
    value ~ chroma | hue + .groups, 
    data = z,
    as.table = TRUE,
    subscripts = TRUE, 
    xlim = x.lim, 
    ylim = y.lim, 
    scales = list(alternating = 3, tick.number = 8, y = list(rot = 0)), 
    main = '', 
    xlab = 'Chroma', 
    ylab = 'Value', 
    panel = function(x, y, subscripts = subscripts, ...) {
      
      p.data <- data.frame(
        x = x, 
        y = y, 
        col = z$.color[subscripts], 
        m = z$.munsell[subscripts], 
        stringsAsFactors = FALSE
      )
      
      
      
      tab <- prop.table(table(p.data$m, useNA = 'always'))
      tab <- as.data.frame(tab)
      names(tab) <- c('m', 'freq')
      tab$m <- as.character(tab$m)
      
      p.data <- merge(x = p.data, y = tab, by = 'm', all.x = TRUE, sort = FALSE)
      p.data <- na.omit(p.data)
      
      panel.abline(v = min(z$chroma):max(z$chroma), h = min(z$value):max(z$value), col = grey(0.85), lty = 3)
      panel.xyplot(p.data$x, p.data$y, pch=15, col = scales::alpha(p.data$col, chip.alpha), cex = chip.cex)
    }
  )
  
  # move paneling by groups to outer panels
  pp <- latticeExtra::useOuterStrips(
    pp, 
    strip = strip.custom(bg = grey(0.85), par.strip.text = list(cex=0.85)), 
    strip.left = strip.custom(bg = grey(0.85), par.strip.text = list(rot=0))
  )
  
  return(pp)  
}

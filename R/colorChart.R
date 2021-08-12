

#' @title Visualize soil colors in Munsell notation according to within-group frequency.
#' 
#'
#' @param m character vector of color in Munsell notation ('10YR 4/6')
#' @param g factor describing group membership, typically a generalization of horizon designation, default value will generat a fake grouping that covers all of the colors in `m`
#' @param chip.cex scaling factor applied to each "chip"
#' @param alpha.wt weighted applied to chip transparency
#' @param alpha.toggle logical, automatically set full opacity (no transparency) when all chips share the same frequency
#' @param annotate logical, annotate chip frequency
#' @param annotate.cex scaling factor for chip frequency annotation
#'
#' @return a `trellis` object
#' @export
#'
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
#'   # note that chip frequency-based transparency is disabled 
#'   # because all chips have equal frequency
#'   colorChart(ric, chip.cex = 3)
#'   
#'   # annotation of frequency
#'   colorChart(ric, chip.cex = 3, annotate = TRUE)
#'   
#'   # disable auto-toggling of transparency when all chips equal
#'   colorChart(ric, chip.cex = 3, alpha.toggle = FALSE)
#'   
#'   
#'   # bootstrap to larger size
#'   ric.big <- sample(ric, size = 100, replace = TRUE)
#'   
#'   # note that chip frequency is encoded in transparency
#'   colorChart(ric.big, chip.cex = 3)
#'   colorChart(ric.big, chip.cex = 3, annotate = TRUE)
#'   
#'   # adjust transparency weighting
#'   colorChart(ric.big, chip.cex = 3, annotate = TRUE, alpha.wt = 20)
#'   
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
#'   colorChart(s[[1]], chip.cex = 3, alpha.wt = 10, annotate = TRUE)
#'   
#' }
#' 
colorChart <- function(m, g = factor('All'), chip.cex = 3, alpha.wt = 10, alpha.toggle = TRUE, annotate = FALSE, annotate.cex = chip.cex * 0.25) {
  
  
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
  
  # set reasonable limits: 
  # value / chroma: 2-8 unless data extend beyond those limits
  x.lim <- c(
    pmin(min(z$chroma), 2) - 0.5,
    pmax(max(z$chroma), 8) + 0.5
  )
  
  y.lim <- c(
    pmin(min(z$value), 2) - 0.5,
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
      
      # panel-wise subset of required data
      p.data <- data.frame(
        x = x, 
        y = y, 
        col = z$.color[subscripts], 
        m = z$.munsell[subscripts], 
        stringsAsFactors = FALSE
      )
      
      # tabulate frequencies of each Munsell chip
      tab <- table(p.data$m, useNA = 'always')
      tab <- as.data.frame(tab)
      names(tab) <- c('m', 'freq')
      tab$m <- as.character(tab$m)
      
      # combine tabulation with unique chips
      p.data <- merge(x = unique(p.data), y = tab, by = 'm', all.x = TRUE, sort = FALSE)
      p.data <- na.omit(p.data)
      
      # grid lines: use original data
      panel.abline(
        v = min(z$chroma):max(z$chroma), 
        h = min(z$value):max(z$value), 
        col = grey(0.85), 
        lty = 3
      )
      
      
      # encode frequency via opacity (freq ~ opacity)
      transformed.col <- scales::alpha(
        colour = p.data$col, 
        alpha = (p.data$freq / sum(p.data$freq)) * alpha.wt
      )
      
      # disable when all frequencies are the same (no useful information)
      if(alpha.toggle) {
        if(length(unique(p.data$freq)) == 1) {
          transformed.col <- p.data$col
        }
      }
      
      
      ## TODO: 
      # * consider reporting Shannon entropy / group
      #   print(shannonEntropy(p.data$freq / sum(p.data$freq)))
      # 
      # * variable scaling of chip size ~ frequency
      # * other chip styling options
      
      # plot single instances of each chip, transparency weighted by frequency
      panel.xyplot(
        p.data$x, 
        p.data$y, 
        pch = 15, 
        col = transformed.col, 
        cex = chip.cex
      )
      
      # annotate chips with frequency
      if(annotate) {
        freq.txt <- round(p.data$freq)
        panel.text(
          x = p.data$x, 
          y = p.data$y, 
          labels = freq.txt, 
          cex = annotate.cex, 
          col = invertLabelColor(p.data$col)
        )
      }
      
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

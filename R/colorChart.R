

#' @title Visualize soil colors in Munsell notation according to within-group frequency.
#' 
#'
#' @param m character vector of color in Munsell notation ('10YR 4/6')
#' 
#' @param g factor describing group membership, typically a generalization of horizon designation, default value will generat a fake grouping that covers all of the colors in `m`
#' 
#' @param size logical, encode group-wise frequency with chip size
#' 
#' @param transparency logical, encode group-wise frequency with chip transparency
#' 
#' @param annotate logical, annotate chip frequency
#' 
#' @param chip.cex scaling factor applied to each "chip"
#' 
#' @param alpha.wt weight applied to chip transparency
#' 
#' @param annotate.cex scaling factor for chip frequency annotation
#'
#' @return a `trellis` object
#' 
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
#'   # note that chip frequency-based size / transparency are disabled 
#'   # because all chips have equal frequency
#'   colorChart(ric, chip.cex = 4, size = TRUE, transparency = TRUE)
#'   
#'   # annotation of frequency
#'   colorChart(ric, chip.cex = 4, annotate = TRUE)
#'   
#'   # bootstrap to larger size
#'   ric.big <- sample(ric, size = 100, replace = TRUE)
#'   
#'   # frequency can be encoded in size and/or transparency
#'   colorChart(ric.big, chip.cex = 3)
#'   colorChart(ric.big, chip.cex = 3, size = FALSE, transparency = TRUE)
#'   colorChart(ric.big, chip.cex = 5, annotate = TRUE)
#'   
#'   # adjust transparency weighting
#'   colorChart(ric.big, chip.cex = 5, annotate = TRUE, transparency = TRUE, alpha.wt = 50)
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
#'   colorChart(s[[1]], chip.cex = 4)
#'   
#' }
#' 
colorChart <- function(m, g = factor('All'), size = TRUE, transparency = FALSE, annotate = FALSE, chip.cex = 3, alpha.wt = 20, annotate.cex = chip.cex * 0.25) {
  
  # requires latticeExtra and scales
  if(!requireNamespace('latticeExtra', quietly = TRUE) | !requireNamespace('scales', quietly = TRUE)) {
    stop('pleast install the `latticeExtra` and `scales` packages.', call.=FALSE)
  }
  
  # extract pieces / convert colors
  z <- data.frame(
    .munsell = m,
    .groups = g,
    stringsAsFactors = FALSE
  )
  
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
  tab$hue <- factor(
    factor(tab$hue, levels = huePosition(returnHues = TRUE))
  )
  
  # disable variable size/transparenct when all frequencies are the same (no useful information)
  if(length(unique(tab$count)) == 1) {
    no.differeneces <- TRUE
  } else {
    no.differeneces <- FALSE
  }
  
  # encode frequency via opacity (freq ~ opacity)
  # transparency is normalized to total number of non-NA colors
  if(transparency) {
    
    if(no.differeneces) {
      tab$transformed.col <- tab$.color
    } else {
      tab$transformed.col <- scales::alpha(
        colour = tab$.color, 
        alpha = tab$prop * alpha.wt
      )
    }
  } else {
    # no transparency
    tab$transformed.col <- tab$.color
  }

  # variable chip size
  if(size) {
    if(no.differeneces) {
      tab$chip.size <- chip.cex
    } else {
      tab$chip.size <- sqrt(scales::rescale(tab$prop, to = c(0.1, 1))) * chip.cex
    }
  } else {
    tab$chip.size <- chip.cex
  }
  
  ## TODO: thematic coloring based on density
  ## needs to be adjusted a group at a time
  # .f <- scales::col_numeric(viridis::viridis(100), domain = c(0, max(tab$prop)))
  # tab$transformed.col <- .f(tab$prop)
  
  ## TODO: 
  # * consider reporting Shannon entropy / group
  #   print(shannonEntropy(tab$prop))
  
  # set reasonable limits: 
  # value / chroma: 2-8 unless data extend beyond those limits
  x.lim <- c(
    pmin(min(tab$chroma), 2) - 0.5,
    pmax(max(tab$chroma), 8) + 0.5
  )
  
  y.lim <- c(
    pmin(min(tab$value), 2) - 0.5,
    pmax(max(tab$value), 8) + 0.5
  )
  
  # assemble basic plot
  pp <- xyplot(
    value ~ chroma | hue + .groups, 
    data = tab,
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
      p.data <- tab[subscripts, ]
      p.data$x <- x
      p.data$y <- y
      
      
      # grid lines: use original data
      panel.abline(
        v = min(tab$chroma):max(tab$chroma), 
        h = min(tab$value):max(tab$value), 
        col = grey(0.85), 
        lty = 3
      )
      
      # plot single instances of each chip, transparency weighted by frequency
      panel.points(
        p.data$x, 
        p.data$y, 
        pch = 22, 
        col = 'black',
        fill = p.data$transformed.col, 
        cex =  p.data$chip.size
      )
      
      # annotate chips with frequency
      if(annotate) {
        freq.txt <- round(p.data$count)
        # adjust based on lightness
        anno.col <- invertLabelColor(p.data$.color)
        
        # revert to 'black' when transparency is high
        if(transparency) {
          anno.col <- ifelse((p.data$prop * alpha.wt) < 0.2, 'black', anno.col)  
        }
        
        # within-group frequency
        panel.text(
          x = p.data$x, 
          y = p.data$y, 
          labels = freq.txt, 
          cex = annotate.cex, 
          col = anno.col
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

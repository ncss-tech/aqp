

#' @name contrastChart
#' @author D.E. Beaudette
#'
#' @title Color Contrast Chart
#'
#' @description Compare one or more pages from a simulated Munsell book of soil colors to a reference color.
#'
#' @param m Munsell representation of a single color for comparison e.g. '10YR 4/3'
#'
#' @param hues vector of one or more Munsell hue pages to display
#'
#' @param ccAbbreviate length of abbreviated contrast classes, use 0 to suppress labels
#'
#' @param style 'hue' or 'CC', see details
#' 
#' @param gridLines logical, add grid lines to the color contrast chart
#' 
#' @param de00.cex character scaling applied to dE00 annotation
#' 
#' @param cc.cex character scaling applied to contrast class annotation
#'
#' @param thresh threshold (<) applied to pair-wise comparisons and resulting color chips
#'
#' @param returnData logical, return lattice figure + data used to generate the figure
#'
#' @details
#' A simulated Munsell color book page or pages are used to demonstrate color contrast between all chips and the reference color \code{m} (highlighted in red). NCSS color contrast class and CIE delta-E00 values are printed below all other color chips. Munsell color chips for chroma 5 and 7 are omitted, but axis labels are retained as a reminder of this fact.
#'
#' Setting \code{style='hue'} emphasizes the contrast classes and CIE delta-E00 of chips adjacent to \code{m}. Setting \code{style='CC'} emphasizes adjacent chips according to respective contrast class via lattice panels.
#'
#' Two-way panels are used when multiple hues are provided and \code{style='CC'}. The default output can be greatly enhanced via:
#'
#'   \code{
#'   latticeExtra::useOuterStrips(...,
#'                                strip = strip.custom(bg=grey(0.85)),
#'                                strip.left = strip.custom(bg=grey(0.85))
#'                               )
#'   }
#'
#' @keywords hplots manip
#' @export
#'
#' @examples
#' 
#' # keep examples from using more than 2 cores
#' data.table::setDTthreads(Sys.getenv("OMP_THREAD_LIMIT", unset = 2))
#' 
#' # single hue page
#' contrastChart(m = '10YR 3/3', hues = '10YR')
#'
#' # multiple hue pages
#' contrastChart(m = '10YR 3/3', hues = c('10YR', '2.5Y'))
#'
#' # contrast class, single hue
#' contrastChart(m = '10YR 3/3', hues = '10YR', style='CC')
#'
#' # contrast class, multiple hues
#' # consider latticeExtra::useOuterStrips()
#' contrastChart(m = '10YR 5/6', hues = c('10YR', '2.5Y'), style='CC')
#'

contrastChart <- function(m, hues, ccAbbreviate = 1, style = 'hue', gridLines = FALSE, de00.cex = 0.6, cc.cex = 0.6, thresh = NULL, returnData = FALSE) {

  # load Munsell LUT
  # safe for CRAN check
  munsell <- NULL
  load(system.file("data/munsell.rda", package = "aqp")[1])

  # plotting details
  y.offset.CC <- 0.08
  y.offset.dE00 <- 0.25

  # safety checks on arguments
  # check style
  if(! style %in% c('hue', 'CC', 'dE00')) {
    stop("style must be one of: 'hue', 'CC'", call. = FALSE)
  }

  # multiHue required when length(hues) > 1
  if(length(hues) > 1 & style == 'hue') {
    style <- 'multiHue'
    
    # move dE00 up if CC is disabled via 0-length abbreviation
    if(ccAbbreviate == 0) {
      y.offset.dE00 <- y.offset.dE00 - 0.1
    }
  }

  # 2. disable CC label when style = CC
  if(style == 'CC') {
    # hack to disable CC label
    ccAbbreviate <- 0
    # move dE00 up
    y.offset.dE00 <- y.offset.dE00 - 0.1

    # multiple hues
    if(length(hues) > 1) {
      style <- 'multiCC'
    }
  }

  # extract just requested hues
  # along with standard value/chroma pairs found on a typical color book page
  chroma.subset <- c(1, 2, 3, 4, 6, 8)
  x <- munsell[which(munsell$value %in% 2:8 & munsell$chroma %in% chroma.subset & munsell$hue %in% hues), ]

  # convert into hex notation for plotting
  x$color <- munsell2rgb(x$hue, x$value, x$chroma)
  x$munsell <- sprintf("%s %s/%s", x$hue, x$value, x$chroma)

  # re-level hues according to color contrast guidance
  hh <- unique(x$hue)
  ll <- hh[order(huePosition(hh))]
  x$hue <- factor(x$hue, levels=ll)

  # setup query color table
  m <- data.frame(
    queryColor = m, 
    parseMunsell(m, convertColors = FALSE),
    stringsAsFactors = FALSE
  )
  m$value <- as.integer(m$value)
  m$chroma <- as.integer(m$chroma)

  # compute all pair-wise contrast classes and dE00
  cc <- colorContrast(x$munsell, rep(m$queryColor, times = nrow(x)))

  # join for plotting
  z <- merge(x, cc, by.x='munsell', by.y='m1', all.x=TRUE, sort=FALSE)

  # dE00 thresholding
  if( ! is.null(thresh) ) {
    z <- z[which(z$dE00 < thresh), ]

  }

  # convert chroma into a factor, in this way we can skip chroma 5/7
  # note that the full levels of chroma have to be set 
  z$chroma <- factor(z$chroma, levels = chroma.subset)
  
  # alternative modes
  fm <- switch(style,
               'hue' = as.formula('value ~ chroma'),
               'multiHue' = as.formula('value ~ chroma | hue'),
               'CC' = as.formula('value ~ chroma | cc'),
               'multiCC' = as.formula('value ~ chroma | cc + hue')
  )


  # custom x-axis labeling based on factors vs. integers
  chroma.axis.at <- c(seq_along(chroma.subset), 4.5, 5.5)
  chroma.subset.labels <- as.character(c(chroma.subset, 5, 7))

  # make plot
  pp <- lattice::xyplot(fm, data = z,
               main = sprintf('Color Contrast Chart: %s', m$queryColor),
               asp = 1.2, xlab='Chroma', ylab='Value',
               xlim = c(0.75, 6.25), 
               ylim = c(1.75, 8.25),
               scales = list(
                 alternating = 1, tick.number = 8, relation = 'free', y = list(
                   rot = 0
                 )
                 ,
                 x = list(
                   at = chroma.axis.at,
                   labels = chroma.subset.labels
                 )
               ),
               as.table = TRUE, strip = strip.custom(bg='grey'),
               subscripts = TRUE,
               panel = function(xx, yy, subscripts, ...) {
                 
                 # note: source `xx` and `yy` aren't actually used for anything
                 #       -> chroma factor levels reset when full levels aren't present
                 
                 # prep data for this panel
                 d <- z[subscripts, ]
                 d$cc <- as.character(d$cc)
                 d$dE00 <- format(d$dE00, digits = 2)
                 
                 # extract from d vs. function arguments
                 xx <- as.integer(d$chroma)
                 yy <- d$value
                 
                 # remove query color contrast and dE00
                 idx <- which(d$munsell == m$queryColor)
                 d$cc[idx] <- ''
                 d$dE00[idx] <- ''
                 
                 # grid system
                 if(gridLines) {
                   panel.abline(h = 2:8, v=1:8, col=grey(0.85), lty=1) 
                 }
                 
                 # offsets, may require additional tinkering
                 bd.side <- 0.3
                 bd.bottom <- 0.2
                 bd.top <- 0.4
                 bd.annot <- 0.05
                 
                 # color chips
                 # border encodes query chip
                 chip.border <- rep('black', times=nrow(d))
                 chip.lwd <- rep(1, times=nrow(d))
                 
                 # update border colors and thickness for the query color
                 border.idx <- which(d$hue == m$hue & d$value == m$value & d$chroma == m$chroma)
                 chip.border[border.idx] <- 'red'
                 chip.lwd[border.idx] <- 3
                 
                 panel.rect(
                   xleft = xx - bd.side,
                   ybottom = yy - bd.bottom,
                   xright = xx + bd.side,
                   ytop = yy + bd.top,
                   col = d$color,
                   border = chip.border,
                   lwd = chip.lwd
                 )
                 
                 # optionally annotate contrast class and abbreviate
                 if(ccAbbreviate >= 1) {
                   panel.text(
                     x = xx,
                     y = yy - (bd.bottom + y.offset.CC),
                     as.character(abbreviate(d$cc, minlength = ccAbbreviate)),
                     cex = cc.cex,
                     font = 4
                   )
                 }
                 
                 
                 # annotate dE00
                 panel.text(
                   x = xx,
                   y = yy - (bd.bottom + y.offset.dE00),
                   d$dE00,
                   cex = de00.cex
                 )
               }
  )

  # return list containing figure and data
  # figure is first so that it is printed to the graphics device
  if(returnData) {
    return(list(fig = pp, data = z))
  } else {
    # just the figure
    return(pp)
  }

}





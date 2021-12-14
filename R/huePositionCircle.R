

#' @title Visual Description of Munsell Hue Ordering
#' 
#' @description Munsell hues are arranged on the unit circle with "neutral" at the center.
#' 
#' @param hues vector of Munsell hues, commonly derived from `huePosition()`
#' @param value single integer, Munsell value used to create an actual color
#' @param chroma single integer, Munsell chroma used to create an actual color 
#' @param chip.cex numeric, scaling for color chips
#' @param label.cex numeric, scaling labels
#' @param seg.adj numeric, scaling for line segment cues
#' @param seg.col single color, color used for line segment cues
#' @param plot logical, generate output on the current graphics device
#'
#' @note The best results are obtained when setting margins to zero, and inverting foreground / background colors. For example: `par(mar = c(0, 0, 0, 0), fg = 'white', bg = 'black')`.
#' 
#' @references 
#' Munsell book of color. 1976. Macbeth, a Division of Kollmorgen Corp., Baltimore, MD.
#' 
#' 
#' @return an invisible `data.frame` of data used to create the figure
#' @export
#'
#' @examples
#' 
#' # better graphics defaults
#' op <- par(mar = c(0, 0, 0, 0), fg = 'white', bg = 'black')
#' 
#' # full set of hues, as generated by huePosition(returnHues = TRUE)
#' huePositionCircle()
#' 
#' # subset
#' huePositionCircle(hues = c('5R', '5Y', '5G', '5B', '5P'))
#' 
#' # reset graphics state
#' par(op)
#' 
huePositionCircle <- function(hues = huePosition(returnHues = TRUE), value = 6, chroma = 10, chip.cex = 5.5, label.cex = 0.66, seg.adj = 0.8, seg.col = 'grey', plot = TRUE) {
  
  # sacrifice to CRAN deity
  munsellHuePosition <- NULL
  
  # note: this is incompatible with LazyData: true
  # load look-up table from our package
  load(system.file("data/munsellHuePosition.rda", package="aqp")[1])
  
  # re-order LABELS according to vector of presented hues
  idx <- match(hues, munsellHuePosition$hue)
  d <- munsellHuePosition[idx, ]
  
  # retain sequence
  d$s <- 1:nrow(d)
  
  # convert colors
  d$cols <- parseMunsell(sprintf('%s %s/%s', d$hues, value, chroma))
  
  # neutral color at center
  n.col <- parseMunsell(sprintf('N %s/', value))
  
  # foreground / background colors
  fg.col <- par()$fg
  bg.col <- par()$bg
  
  if(plot) {
    # init plot with full set of hues and place-holder symbols
    plot(x = munsellHuePosition$x, y = munsellHuePosition$y, asp = 1, pch = 21, cex = chip.cex * 0.5, col = fg.col, xlab = '', ylab = '', axes = FALSE)
    
    # hue chips + annotation
    points(x = d$x, y = d$y, pch = 21, cex = chip.cex, bg = d$cols, col = fg.col)
    text(x = d$x, y = d$y, labels = d$hues, col = invertLabelColor(d$cols), cex = label.cex, font = 2)
    
    # ordering / direction sequence and visual cues
    text(x = d$x * 0.88, y = d$y * 0.88, labels = d$s, col = fg.col, cex = label.cex)
    segments(x0 = 0, x1 = d$x * seg.adj, y0 = 0, y1 = d$y * seg.adj, col = seg.col)
    
    # neutral at the center
    points(x = 0, y = 0, bg = n.col, col = fg.col, pch = 21, cex = chip.cex * 1.25)
    text(x = 0, y = 0, labels = 'N', col = invertLabelColor(d$cols), cex = label.cex * 1.5, font = 2)
  }
  
  # remove original row names
  row.names(d) <- NULL
  
  # in case this information is helpful
  invisible(d)
  
}

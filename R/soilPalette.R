

## TODO: this may not scale to > 5 colors
## TODO: this should have more options for title / subtitle


#' @title Soil Color Palette
#' 
#' @description A very simple function for generating labeled swatches of soil colors. Largely based on \code{colorspace::swatchplot}.
#' 
#' @param colors vector of hex colors (e.g. #A66E46FF)
#' 
#' @param lab vector of labels
#' 
#' @param lab.cex character scaling for labels
#' 
#' @param dynamic.labels logical, adjust label colors for maximum contrast via \code{invertLabelColor}
#' 
#' @param x.inset horizontal adjustment for labels
#' 
#' @param y.inset vertical adjustment for labels
#' 
#' @param \dots further arguments to \code{colorspace::swatchplot}
#' 
#' @author D.E. Beaudette
#' 
#' @note The result is a simple figure on the active plotting device.
#' 
#' @keywords hplots
#' 
#' @examples 
#' 
#' # maybe useful for teaching about soil color
#' 
#' par(mfrow=c(2,1), mar=c(1,1,1,1))
#' 
#' # demonstrate range of Munsell value
#' m <- sprintf('10YR %s/4', 2:8)
#' # convert to hex representation
#' cols <- parseMunsell(m)
#' # plot
#' soilPalette(cols, m)
#' 
#' # demonstrate range of Munsell chroma
#' m <- sprintf('10YR 4/%s', 2:8)
#' # convert to hex representation
#' cols <- parseMunsell(m)
#' # plot
#' soilPalette(cols, m)
#' 

soilPalette <- function(colors, lab = colors, lab.cex=0.75, dynamic.labels=TRUE, x.inset = 0.01, y.inset = 0.01, ...) {
  # basic plot
  colorspace::swatchplot(colors, ...)
  
  ## TODO: this doesn't scale to more than a single row of colors
  ## use code from colorContrastPlot to dynamically adjust for multiple rows
  ## possibly share code
  
  # annotation
  nx <- length(colors)
  x.pos <- seq(from = 0, to = 1, by = 1/nx)[1:nx] + x.inset
  y.pos <- rep(y.inset, times = nx)
  
  # label color varies as a function of value
  if(dynamic.labels) {
    label.color <- invertLabelColor(colors)
  } else {
    # use device background color
    label.color <- par()$bg
  }
  
  # annotate
  text(x.pos, y.pos, labels = lab, cex=lab.cex, col = label.color, font = 2, adj = c(0, 0))
}



## TODO: this may not scale to > 5 colors

## tiny function for a quick swatch with annotation
# colors: vector of hex colors
# lab: vector of labels
# lab.cex: scaling factor for labels
# dynamic.lab.cols: adjust label based on HSV-value
soilPalette <- function(colors, lab, lab.cex=0.75, dynamic.labels=TRUE, ...) {
  # basic plot
  colorspace::swatchplot(colors, ...)
  
  ## TODO: this doesn't scale to more than a single row of colors
  ## use code from colorContrastPlot to dynamically adjust for multiple rows
  ## possibly share code
  
  # annotation
  nx <- length(colors)
  x.pos <- seq(from = 0, to = 1, by = 1/nx)[1:nx]
  y.pos <- rep(0.01, times = nx)
  
  # label color varies as a function of value
  if(dynamic.labels) {
    label.color <- invertLabelColor(colors)
  } else {
    # use device background color
    label.color <- par()$bg
  }
  
  # annotate
  text(x.pos, y.pos, labels = lab, cex=lab.cex, col = label.color, font = 2, adj = c(-0.125, -0.33))
}

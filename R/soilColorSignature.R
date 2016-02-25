

# https://en.wikipedia.org/wiki/Lab_color_space
#
# L - brightness
# A - green | red
# B - blue | yellow
# x: single profile
# requires L, pos.A, neg.A, pos.B, neg.B in @horizon
.pigments <- function(x, useProportions, pigmentNames) {
  h <- horizons(x)
  dc <- horizonDepths(x)
  hz.thick <- h[[dc[2]]] - h[[dc[1]]]
  h <- h[, c('L', 'pos.A', 'neg.A', 'pos.B', 'neg.B')]
  
  # TODO: this may need to be normalized
  hz.pigments <- sweep(h, MARGIN = 1, STATS = hz.thick, FUN = '*')
  pigment <- colSums(hz.pigments, na.rm = TRUE)
  names(pigment) <- pigmentNames
  
  ## NOTE: this removes the effect of soil depth
  # convert to proportions
  if(useProportions)
    pigment <- pigment / sum(pigment)
  
  return(pigment)
}

## TODO: implement other methods, break out into own functions
## TODO: init from sRGB() or RGB() ??
# requires colorspace package
soilColorSignature <- function(spc, r='r', g='g', b='b', method='colorBucket', RescaleLightnessBy=1, useProportions=TRUE, pigmentNames=c('.white.pigment', '.red.pigment', '.green.pigment', '.yellow.pigment', '.blue.pigment')) {
  
  # warn about methods
  if(method != 'colorBucket')
    message('`colorBucket` is the only available method at this time')
  
  if(!requireNamespace('colorspace'))
    stop('pleast install the `colorspace` package.', call.=FALSE)
  
  # extract horizons
  h <- horizons(spc)
  
  # create LAB colors
  lab.colors <- as(colorspace::RGB(h[['r']], h[['g']], h[['b']]), 'LAB')@coords
  
  ## TODO: does it make sense to normalized based on limited data or entire possible range?
  # normalize the L coordinate
  lab.colors[, 1] <- lab.colors[, 1] / RescaleLightnessBy
  
  ## L is always positve
  ## split A/B axes into positive / negative pigments
  pos.A <- ifelse(lab.colors[, 2] > 0, lab.colors[, 2], 0)
  neg.A <- ifelse(lab.colors[, 2] < 0, -lab.colors[, 2], 0)
  
  pos.B <- ifelse(lab.colors[, 3] > 0, lab.colors[, 3], 0)
  neg.B <- ifelse(lab.colors[, 3] < 0, -lab.colors[, 3], 0)
  
  ## TODO: this is sloppy
  # assign back to original SPC
  spc$L <- lab.colors[, 1]
  spc$pos.A <- pos.A
  spc$neg.A <- neg.A
  spc$pos.B <- pos.B
  spc$neg.B <- neg.B
  
  col.data <- profileApply(spc, .pigments, useProportions=useProportions, pigmentNames=pigmentNames, simplify = FALSE)
  col.data <- ldply(col.data)
  names(col.data)[1] <- idname(spc)
  
  return(col.data)
}


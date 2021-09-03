
#' @title Make High Contrast Label Colors
#' @description Generate a vector of white or black label colors conditioned on a vector of colors to maximize label contrast.
#' 
#' @param colors vector of colors
#' @param threshold black | white threshold
#' 
#' @return vector of label colors
#' 
#' @author D.E. Beaudette
#' 
#' @examples 
#' 
#' # test with shades of grey
#' s <- seq(0, 1, by = 0.05)
#' cols <- grey(s)
#' soilPalette(cols, lab = as.character(s))
#' 
#' # test with 10YR x/3
#' m <- sprintf('10YR %s/3', 1:8)
#' cols <- parseMunsell(m)
#' soilPalette(cols, lab = m)
#' 
#' 
invertLabelColor <- function(colors, threshold = 0.65) {
  
  # NA is converted to white, resulting in black label
  # convert colors -> sRGB -> HSV
  hsv.cols <- t(rgb2hsv(col2rgb(colors)))
  
  # truncate hue and saturation
  hsv.cols[, 1] <- 0
  hsv.cols[, 2] <- 0
  
  # conditionally set value according to threshold
  # V > thresh -> black | else white
  hsv.cols[, 3] <- ifelse(hsv.cols[, 3] > threshold, 0, 1)
  
  # new label color: white | black
  label.color <- hsv(hsv.cols[, 1], hsv.cols[, 2], hsv.cols[, 3])
  
  return(label.color)
}


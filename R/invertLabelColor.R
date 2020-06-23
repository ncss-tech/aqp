
#' @description Generate a vector of white or black label colors conditioned on a vector of colors to maximize label contrast.
#' 
#' @param colors vector of colors
#' @param threshold black | white threshold
#' 
#' @return vector of label colors
#' 
#' @author D.E. Beaudette
invertLabelColor <- function(colors, threshold=0.6) {
  
  ## TODO: not NA-safe!!
  # convert colors -> sRGB -> HSV
  hsv.cols <- t(rgb2hsv(col2rgb(colors)))
  
  # truncate hue and saturation
  hsv.cols[, 1] <- 0
  hsv.cols[, 2] <- 0
  
  # conditionally set value according to thresold
  hsv.cols[, 3] <- ifelse(hsv.cols[, 3] > threshold, 0, 1)
  
  # new label color: white | black
  label.color <- hsv(hsv.cols[, 1], hsv.cols[, 2], hsv.cols[, 3])
  
  return(label.color)
}


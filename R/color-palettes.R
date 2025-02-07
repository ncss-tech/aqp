

#' @title Soil Texture Color Palettes
#' 
#' @description
#' Suggested color palettes for USDA soil texture classes, ranked according to average plant-available water holding capacity. The default color mapping schema is based on a palette used by SoilWeb applications.
#' 
#' @param simplify logical, return the base 12 (`TRUE`) or full 21 (`FALSE`) soil texture classes
#' 
#' @param schema select mapping between soil texture classes, and colors, currently limited to 'soilweb'
#'
#' @return `data.frame` from soil texture class codes and colors
#' 
#' @author D.E. Beaudette, Mike Walkinshaw, A.T. O'Geen
#' 
#' @export
#' 
#' @rdname soilTextureColorPal
#' 
#' @examplesIf requireNamespace("colorspace", quietly = TRUE)
#'  
#' # base 12 soil texture classes
#' # ranked by plant available water-holding capacity
#' d <- soilTextureColorPal(simplify = TRUE)
#' soilPalette(d$color, lab = d$class, lab.cex = 1)
#' 
#' # full 21 soil texture classes
#' # ranked by plant available water-holding capacity
#' d <- soilTextureColorPal(simplify = FALSE)
#' soilPalette(d$color, lab = d$class, lab.cex = 1)
#' 
soilTextureColorPal <- function(simplify = FALSE, schema = 'soilweb') {
  
  # SoilWeb soil texture ordering, based on ranking of PAWS of 
  .l <- c("s", "ls", "sl", "scl", "l", "sc", "c", "sic", "cl", "sil", "sicl", "si")
  
  .cols <- c(
    "#BEBEBE", "#FDFD9E", "#ebd834", "#92C158", "#307431", "#4C5323", 
    "#AF4732", "#E93F4A", "#EA6996", "#CD94EA", "#6D94E5", "#546BC3"
  )
  
  # expanded soil texture ordering to include all 21 classes, 
  # from SoilWeb color palette of 12 base classes
  .le <- c("cos", "s", "fs", "vfs", "lcos", "ls", "lfs", "lvfs", "cosl", "sl", "fsl", "vfsl", "scl", "l", "sc", "c", "sic", "cl", "sil", "sicl", "si")
  
  # linear interpolation required to prevent wild "blue" colors on the sandy side
  # bias required as we are increasing the resolution on the coarse side of the scalee
  .cols_e <- colorRampPalette(
    colors = .cols, 
    space = 'Lab', 
    interpolate = 'linear',  
    bias = 0.45
  )(21)
  
  if(simplify) {
    res <- data.frame(class = .l, color = .cols)
  } else {
    res <- data.frame(class = .le, color = .cols_e)
  }
  
  return(res)
  
}


##
##

## TODO: consider various sorting strategies: WMPD, AWC, {PWP,FC,SAT}
## http://ncss-tech.github.io/AQP/aqp/water-retention-curves.html

## TODO: move ranking system definitions into internal function or environment
## TODO: ensure that creating ranking code is easily repeatable

# .USDA_soil_texture_ranking_systems <- function(sys) {
#   
#   # 1. Field Book (what is the original authority?)
#   
#   # 2. ranked AWC
#   
#   # 3. clustered (SAT, FC, PWP) / sorted (AWC)
#   
# }


#' @title Ranking Systems for USDS Soil Texture Classes
#' 
#' @description Generate a vector of USDA soil texture codes or class names, sorted according to approximate particle size
#'
#' @references \href{https://www.nrcs.usda.gov/wps/portal/nrcs/detail/soils/research/guide/?cid=nrcs142p2_054184}{Field Book for Describing and Sampling Soils, version 3.0}
#' 
#' @param which 'codes' (texture codes) or 'names' (texture class names)
#' 
#' @return an ordered factor
#' 
#' @examples
#' 
#' # class codes
#' SoilTextureLevels()
#' 
#' # class names
#' SoilTextureLevels(which = 'names')
#'  
SoilTextureLevels <- function(which = 'codes') {
  
  # sanity check
  if(! which %in% c('codes', 'names')) {
    stop('must specify `codes` (texture codes) or `names` (texture class names)', call. = FALSE)
  }
  
  # from the Field Book version 3.0
  # sorted by approximate particle size
  tx <- structure(
    list(
      texture = c("coarse sand", "sand", "fine sand", 
                  "very fine sand", "loamy coarse sand", "loamy sand", "loamy fine sandy", 
                  "loamy very fine sand", "coarse sandy loam", "sandy loam", "fine sandy loam", 
                  "very fine sandy loam", "loam", "silt loam", "silt", "sandy clay loam", 
                  "clay loam", "silty clay loam", "sandy clay", "silty clay", "clay"
      ), 
      txtcl = c("cos", "s", "fs", "vfs", "lcos", "ls", "lfs", "lvfs", 
                "cosl", "sl", "fsl", "vfsl", "l", "sil", "si", "scl", "cl", "sicl", 
                "sc", "sic", "c")), 
    class = "data.frame", 
    row.names = c(NA, -21L
    ))

  # set levels
  tx$texture <- factor(tx$texture, levels = tx$texture, ordered = TRUE)
  tx$txtcl <- factor(tx$txtcl, levels = tx$txtcl, ordered = TRUE)
  
  # return what was requested
  switch(
    which,
    codes = return(tx$txtcl),
    names = return(tx$texture)
  )
  
}


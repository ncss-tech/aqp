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
#' @param simplify Return 12-class factor levels (`TRUE`) or 21-class factor levels (default: `FALSE`)? The 12-class system does not separate sands, loamy sands and sandy loams into sand fraction variants (e.g. "very fine sandy loam" in the 21-class system is "sandy loam" in 12-class system)
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
#' # simpler class names
#' SoilTextureLevels(which = 'names', simplify = TRUE)
#'  
SoilTextureLevels <- function(which = 'codes', simplify = FALSE) {
  
  # sanity check
  if(! which %in% c('codes', 'names')) {
    stop('must specify `codes` (texture codes) or `names` (texture class names)', call. = FALSE)
  }
  
  # sorted by approximate particle size
  if (!simplify) { # from the Field Book version 3.0
    tx <- data.frame(texture = c("coarse sand", "sand", "fine sand", 
                                 "very fine sand", "loamy coarse sand", "loamy sand", "loamy fine sandy", 
                                 "loamy very fine sand", "coarse sandy loam", "sandy loam", "fine sandy loam", 
                                 "very fine sandy loam", "loam", "silt loam", "silt", "sandy clay loam", 
                                 "clay loam", "silty clay loam", "sandy clay", "silty clay", "clay"), 
                     texcl = c("cos", "s", "fs", "vfs", "lcos", "ls", "lfs", "lvfs", 
                               "cosl", "sl", "fsl", "vfsl", "l", "sil", "si", "scl", "cl", "sicl", 
                               "sc", "sic", "c")) 
  } else { # From Soil Survey Manual (1951) p. 210
    tx <- data.frame(texture = c("sand", "loamy sand", "sandy loam",
                                 "loam", "silt loam", "silt", "sandy clay loam",
                                 "clay loam", "silty clay loam",
                                 "sandy clay", "silty clay", "clay"),
                     texcl = c("s", "ls", "sl", "l", "sil", "si",
                               "scl", "cl", "sicl", "sc", "sic", "c"))
  }
  
  # set levels
  tx$texture <- factor(tx$texture, levels = tx$texture, ordered = TRUE)
  tx$texcl <- factor(tx$texcl, levels = tx$texcl, ordered = TRUE)
  
  # return what was requested
  switch(
    which,
    codes = return(tx$texcl),
    names = return(tx$texture)
  )
  
}


#' Convert Texture Class to Class Limits
#'
#' @param x _character_ or _list_. A vector of texture class codes (e.g. `"l"` for "loam", `"sicl"` for silty clay loam) without texture class modifiers. If the input is a list, multiple texture classes within each list element are aggregated to create combined class limits.
#' @details Logic derived from NASIS validation NASIS calculation "Textural Class versus Particle Size Separates" written by Cathy Seybold (last updated 4/07/14)
#' 
#' @return A _data.frame_ with column names "texcl", "clay_l", "clay_m", "clay_h", "sand_l", "sand_m", "sand_h", "silt_l", "silt_m", "silt_h"
#' 
#' @export
#'
#' @examples
#' 
#' texcl_to_classlimit(c("l", "sicl", "cl"))
#' 
#' texcl_to_classlimit(list(c("l", "sicl", "cl")))
#' 
texcl_to_classlimit <- function(x) {
  x <- lapply(x, function(y) tolower(trimws(y)))
  xout <- sapply(x, paste0, collapse = ",")
  res <- as.data.frame(t(sapply(x, function(y) {
    xcl <- as.integer(factor(y, levels = SoilTextureLevels()))
    data.frame(
      # texcl = y,
      clay_l = .getClayLow(xcl),
      clay_h = .getClayHigh(xcl),
      sand_l = .getSandLow(xcl),
      sand_h = .getSandHigh(xcl),
      silt_l = .getSiltLow(xcl),
      silt_h = .getSiltHigh(xcl)
    )
  })))
  res[] <- lapply(res, unlist)
  res$clay_m <- apply(res[,c("clay_l","clay_h")], MARGIN = 1, mean)
  res$sand_m <- apply(res[,c("sand_l","sand_h")], MARGIN = 1, mean)
  res$silt_m <- apply(res[,c("silt_l","silt_h")], MARGIN = 1, mean)
  res <- res[c("clay_l", "clay_m", "clay_h",
               "sand_l", "sand_m", "sand_h",
               "silt_l", "silt_m", "silt_h")]
  res <- cbind(data.frame(texcl = xout), res)
  rownames(res) <- NULL
  res
}

####
# LOGIC DERIVED FROM TEXTURAL CLASS VERSUS PARTICLE SIZE SEPARATES
####
.getClayHigh <- function(texcl) {
  if (any(texcl == 21)) return(100) #clay
  else if (any(texcl == 20)) return(60)  #sic
  else if (any(texcl == 19)) return(55)  #sc
  else if (any(texcl == 18 | texcl == 17)) return(40)  #cl, sicl
  else if (any(texcl == 16)) return(35)  #scl
  else if (any(texcl == 14 | texcl == 13)) return(27)  #l, sil
  else if (any(texcl == 12 | texcl == 11 | texcl == 10 | texcl == 9)) return(20)  #sl, fsl, vfsl, cosl
  else if (any(texcl == 8 | texcl == 7 | texcl == 6 | texcl == 5)) return(15)  #ls, lvfs, lfs, lcos
  else if (any(texcl == 15)) return(12) #si
  else if (any(texcl == 4 | texcl == 3 | texcl == 2 | texcl == 1)) return(10)
  return(NA)
}

.getClayLow <- function(texcl) {
  if (any(texcl <= 12 & texcl >= 1) | any(texcl == 14 | texcl == 15)) return(0)
  else if (any(texcl == 13)) return(7)
  else if (any(texcl == 16)) return(20)
  else if (any(texcl == 17 | texcl == 18)) return(27)
  else if (any(texcl == 19)) return(35)
  else if (any(texcl == 20 | texcl == 21)) return(40)
  return(NA)
}

.getSiltHigh <- function(texcl) {
  if (any(texcl == 15)) return(100)
  else if (any(texcl == 14)) return(88)
  else if (any(texcl == 18)) return(73)
  else if (any(texcl == 20)) return(60)
  else if (any(texcl == 17)) return(53)
  else if (any(texcl <= 13 & texcl >= 9)) return(50)
  else if (any(texcl == 21)) return(40)
  else if (any(texcl <= 8 & texcl >= 5)) return(30)
  else if (any(texcl == 16)) return(28)
  else if (any(texcl == 19)) return(20)
  else if (any(texcl == 4 | texcl == 3 | texcl == 2 | texcl == 1)) return(15)
  return(NA)
}

.getSiltLow <- function(texcl) {
  if (any(texcl >= 1 & texcl <= 12) | any(texcl == 16 | texcl == 19 | texcl == 21)) return(0)
  else if (any(texcl == 17)) return(15)
  else if (any(texcl == 13)) return(28)
  else if (any(texcl == 20 | texcl == 18)) return(40)
  else if (any(texcl == 14)) return(50)
  else if (any(texcl == 15)) return(80)
  return(NA)
}

.getSandHigh <- function(texcl) {
  if (any(texcl == 4 | texcl == 3 | texcl == 2 | texcl == 1)) return(100)
  else if (any(texcl >= 5 & texcl <= 8)) return(90)
  else if (any(texcl >= 9 & texcl <= 12)) return(85)
  else if (any(texcl == 16)) return(80)
  else if (any(texcl == 19)) return(65)
  else if (any(texcl == 13)) return(52)
  else if (any(texcl == 14)) return(50)
  else if (any(texcl == 17 | texcl == 21)) return(45)
  else if (any(texcl == 15 | texcl == 18 | texcl == 20)) return(20)
  return(NA)
}

.getSandLow <- function(texcl) {
  if (any(texcl == 21 | texcl == 20 | texcl == 18 | texcl == 14 | texcl == 15)) return(0)
  else if (any(texcl == 17)) return(20)
  else if (any(texcl == 13)) return(23)
  else if (any(texcl >= 9 & texcl <= 12)) return(43)
  else if (any(texcl == 16 | texcl == 19)) return(45)
  else if (any(texcl >= 5 & texcl <= 8)) return(70)
  else if (any(texcl >= 1 & texcl <= 4)) return(85)
  return(NA)
}

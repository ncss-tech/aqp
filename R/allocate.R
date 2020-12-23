allocate <- function(..., system = "salt severity", droplevels = TRUE) {
  
  if (system == "salt severity") {
    .rank_salts(..., droplevels = droplevels)
  }
}
    
    
.rank_salts <- function(EC = NULL, pH = NULL, ESP = NULL, system = "salt severity", method = "FAO", droplevels = TRUE) {
  
  lev <- c(
    c("nonsaline", "slightly saline", "moderately saline", "strongly saline", "very strongly saline", "extremely saline"),
    c("none", "slightly sodic", "moderately sodic", "strongly sodic", "very strongly sodic")
    )
  
  sc <- rep("none", times = length(EC))
  # saline soils
  sc <- ifelse(ESP <= 15, # & EC > 4 & pH <= 8.5, 
               as.character(
                 cut(EC,
                     breaks = c(-1, 0.75, 2, 4, 8, 15, 1000), 
                     labels = lev[1:6],
                     right  = FALSE
                     )),
               sc
               )
  # sodic soils
  sc <- ifelse(EC <= 4 & (ESP > 15 | pH > 8.2),  
               as.character(
                 cut(ESP,
                     # breaks = c(0, 15, 30, 50, 70, 100),
                     breaks = c(-2, 30, 50, 70, 102),
                     # labels = lev[7:11],
                     labels = lev[8:11],
                     right  = FALSE
                     )),
               sc
               )
  # saline-sodic soils
  sc <- ifelse(EC > 4 & ESP > 15, "saline-sodic", sc)
  
  
  # convert to factor
  sc <- factor(sc, levels = c(lev[1:6], "saline-sodic", lev[8:11]))

  
  # droplevels
  if (droplevels == TRUE) {
    sc <- droplevels(sc)
  }
  
  return(sc)
}



codify <- function(x, system = "salt severity", droplevels = TRUE) {
  
  if (system == "salt severity") {
    
    .codify_salt_severity(x, droplevels = droplevels)
    }
  }


.codify_salt_severity <- function(x, droplevels = TRUE) {
  
  # set levels
  lev <- c(
    c("nonsaline", "slightly saline", "moderately saline", "strongly saline", "very strongly saline", "extremely saline"),
    c("none", "slightly sodic", "moderately sodic", "strongly sodic", "very strongly sodic")
  )
  
  # test
  if (!is.integer(x)) stop("x is not an integer")
  if (!all(unique(x) %in% c(1:11, NA))) warning("some x values do not match the lookup table")

  sc <- factor(x, levels = 1:11, labels = c(lev[1:6], "saline-sodic", lev[8:11]))
  
  if (droplevels == TRUE) {
    sc <- droplevels(sc)
  }
  
  return(sc)
}
  

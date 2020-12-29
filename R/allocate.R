allocate <- function(..., to = NULL, droplevels = TRUE) {
  
  tos <- c(ss = "FAO Salt Severity", bs = "FAO Black Soil")
  
  # test
  if (all(is.null(to))) {
    stop(paste('the "to" argument should equal one of the following:', paste0('"', tos, '"', collapse = ', ')))
  }
  if (length(to) > 1) {
    stop('the length of the "to" argument should equal 1')
  }
  if (! to %in% tos) {
    stop(paste('the argument "to" currently supports allocating soil properties to one of the following classification schemes\\:', paste0('"', tos, '"', collapse = ', ')))
  }
    
  
  # allocate
  if (to == "FAO Salt Severity") {
    a <- .rank_salts(..., system = to, droplevels = droplevels)
  }
  
  if (to == "FAO Black Soil") {
    a <- .black_horizon(...)
  }

  return(a)  
}
    

# To do add USDA and other salt classes
.rank_salts <- function(EC = NULL, pH = NULL, ESP = NULL, system = "FAO Salt Severity", droplevels = TRUE) {
  
  # EC = 1; pH = 3; ESP = 50
  l <- list(EC = EC, pH = pH, ESP = ESP)
  
  # tests
  # minimum dataset
  if (any(sapply(l, is.null))) {
    warning("the minimum dataset of soil properites for allocating to the Salt Severity classes are: EC (aka Electrial Conductivity), pH, ESP (aka Exchangable Sodium Percentage")
  }
  # length
  n <- sapply(l, length)
  if (! all(max(n) == n)) {
    stop("all arguments must have the same length")
  }
  
  
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
  


.black_horizon <- function(OC = NULL, chroma_moist = NULL, value_moist = NULL, value_dry = NULL, thickness = NULL, CEC = NULL, BS = NULL, tropical = FALSE, horizon = TRUE) {
  
  # OC = 1; chroma_moist = 3; value_moist = 3; value_dry = 5; thickness = 25; CEC = 20; BS = 50
  l <- list(OC = OC, chroma_moist = chroma_moist, value_moist = value_moist, value_dry = value_dry, thickness = thickness, CEC = CEC, BS = BS)
  
  # tests
  # minimum dataset
  if (any(sapply(l[1:4], is.null))) {
    stop("the minimum dataset of soil properites for allocating to the 2nd category of Black Soils are: OC (aka Organic Carbon), chroma_moist, value_moist, and value_dry") # and thickness
  }
  # length
  n <- sapply(l[1:5], length)
  if (! all(max(n) == n)) {
    stop("all arguments must have the same length")
  }
  
  
  # criteria
  # 2nd category of Black Soils
  bs2 <- 
    (OC <= 20 & (OC >= 1.2 | (tropical == TRUE & OC >= 0.6))) & 
    chroma_moist <= 3 & 
    (value_moist <= 3 & value_dry <= 5) & 
    (thickness >= 25 | horizon == TRUE) # thickness should only be applied to profiles
  
  # 1st category of Black Soils
  if (!is.null(l$CEC) & !is.null(l$BS)) {
    # test length
    n <- sapply(l, length)
    if (! all(max(n) == n)) {
      stop("all arguments must have the same length")
    }
    
    bs1 <- bs2 & CEC >=25 & BS >= 50
    
  } else bs1 <- NA[1:seq_along(max(n))]
  
  return(data.frame(BS1 = bs1, BS2 = bs2))
  
}




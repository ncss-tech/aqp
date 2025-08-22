



#' @param sub logical, return class subdivisions
#' @param flat logical, use flat coarse fragment classes
#' @param .upper_limit numeric, upper limit for particle size
#' @return vector of particle classes and associated diameters in mm
.particles.usda <- function(sub, flat, .upper_limit = 1e5, ...) {
  
  if(flat) {
    # flat, top-level classes
    .p <- c(
      clay = 0.002,
      silt = 0.05,
      sand = 2,
      channers = 150,
      flagstones = 380,
      stones = 600,
      boulders = .upper_limit
    )
  } else {
    if(sub) {
      # non-flat, subdivisions
      .p <- c(
        fine_clay = 0.0002,
        coarse_clay = 0.002,
        fine_silt = 0.02,
        coarse_silt = 0.05,
        very_fine_sand = 0.1,
        fine_sand = 0.25,
        medium_sand = 0.5,
        coarse_sand = 1,
        very_coarse_sand = 2,
        fine_gravel = 5,
        medium_gravel = 20,
        coarse_gravel = 76,
        cobbles = 250,
        stones = 600,
        boulders = .upper_limit
      )    
    } else {
      # non-flat, top-level classes
      .p <- c(
        clay = 0.002,
        silt = 0.05,
        sand = 2,
        gravel = 76,
        cobbles = 250,
        stones = 600,
        boulders = .upper_limit
      )
    }
  }
  
  return(.p)
}



#' @param sub logical, return class subdivisions
#' @param .upper_limit numeric, upper limit for particle size
#' @return vector of particle classes and associated diameters in mm
.particles.international <- function(sub, .upper_limit = 1e5, ...) {
  
  if(sub) {
    .p <- c(
      clay = 0.002,
      silt = 0.02,
      fine_sand = 0.2,
      coarse_sand = 2,
      gravel = 20,
      stones = .upper_limit
    )
  } else {
    .p <- c(
      clay = 0.002,
      silt = 0.02,
      sand = 2,
      gravel = 20,
      stones = .upper_limit
    )
  }
  
  return(.p)
}




#' @param sub logical, return class subdivisions
#' @param .upper_limit numeric, upper limit for particle size
#' @return vector of particle classes and associated diameters in mm
.particles.unified <- function(sub, .upper_limit = 1e5, ...) {
  
  if(sub) {
    .p <- c(
      silt_or_clay = 0.074,
      fine_sand = 0.42,
      medium_sand = 2,
      coarse_sand = 4.8,
      fine_gravel = 19,
      coarse_gravel = 76,
      cobbles = 300,
      boulders = .upper_limit
    )
  } else {
    .p <- c(
      silt_or_clay = 0.074,
      sand = 4.8,
      gravel = 76,
      cobbles = 300,
      boulders = .upper_limit
    )
  }
  
  return(.p)
}



#' @param sub logical, return class subdivisions
#' @param .upper_limit numeric, upper limit for particle size
#' @return vector of particle classes and associated diameters in mm
.particles.aashto <- function(sub, rounded, .upper_limit = 1e5, ...) {
  
  if(sub) {
    .p <- c(
      clay = 0.005,
      silt = 0.74,
      fine_sand = 0.42,
      coarse_sand = 2,
      fine_gravel = 9.5,
      medium_gravel = 25,
      coarse_gravel = 75,
      big = .upper_limit
    )
  } else {
    .p <- c(
      clay = 0.005,
      silt = 0.74,
      sand = 2,
      gravel = 75,
      big = .upper_limit
    )
  }
  
  # adjust largest particle size class based on roundness
  if(rounded) {
    .bigstuff <- 'broken rock'  
  } else {
    .bigstuff <- 'boulders'
  }
  
  names(.p)[length(.p)] <- 'bigstuff'
  
  return(.p)
}




#' @param sub logical, return class subdivisions
#' @param .upper_limit numeric, upper limit for particle size
#' @return vector of particle classes and associated diameters in mm
.particles.mod.wentworth <- function(.upper_limit = 1e5, ...) {
  
  if(sub) {
    .p <- c(
      clay = 0.004,
      silt = 0.062,
      sand = 2,
      pebbles = 64,
      cobbles = 256,
      boulders = .upper_limit
    )
  } else {
    .p <- c(
      clay = 0.004,
      silt = 0.062,
      sand = 2,
      pebbles = 64,
      cobbles = 256,
      boulders = .upper_limit
    )
  }
  
  
  return(.p)
}

particleClasses <- function(sys = c('usda', 'international', 'unified', 'aashto', 'mod.wentworth'), sub = TRUE, flat = FALSE, rounded = FALSE, .upper_limit = 1e5) {
  
  # normalize to lower case
  sys <- tolower(sys)
  
  # most frequent option, usually not specified
  if(missing(sys)) {
    sys <- 'usda'
  } else {
    sys <- match.arg(sys)
  }
  
  .pc <- switch(sys, 
                'usda' = {
                  .particles.usda(sub = sub, flat = flat, .upper_limit = .upper_limit)
                },
                'international' = {
                  .particles.international(sub = sub, .upper_limit = .upper_limit)
                },
                'unified' = {
                  .particles.unified(sub = sub, .upper_limit = .upper_limit)
                },
                'aashto' = {
                  .particles.aashto(sub = sub, rounded = rounded, .upper_limit = .upper_limit)
                },
                'mod.wentworth' = {
                  .particles.mod.wentworth(sub = sub, .upper_limit = .upper_limit)
                }
  )
  
  return(.pc)
  
}





particleSieve <- function() {
  
}






## TODO:
##  * NA handling
##  * vectorization
##  * distance-based matching -> is this sufficient
##  * other systems


convertParticleDiameterUnits <- function(x, from, to) {
  
  ## enumerate possible from/to conversions
  .possibilities <- combn(
    x = c('mm', 'sieve', 'phi'),
    m = 2
  )
  
  # combine forward and reverse conversions
  .possibilities <- t(
    cbind(
      .possibilities, 
      apply(.possibilities, 2, rev)
    )
  )
  
  # simplify to data.frame
  .possibilities <- data.frame(.possibilities)
  names(.possibilities) <- c('from', 'to')
  
  # setup conversion rules
  .possibilities$rule <- paste(.possibilities$from, .possibilities$to, sep = '-')
  
  # select conversion rule
  .rule <- .possibilities$rule[.possibilities$from == from & .possibilities$to == to]
  
  
  ## US standard sieve sizes
  # .lut <- read.table(textConnection(readClipboard()))
  # names(.lut) <- c('mm', 'in', 'no')
  
  .us_sieve_size_lut <- structure(
    list(
      `mm` = c(11.2, 6.35, 5.6, 4.75, 4, 3.35, 2.8, 2.36, 
               2, 1.7, 1.4, 1.18, 1, 0.841, 0.71, 0.595, 0.5, 0.4, 0.355, 0.3, 
               0.25, 0.21, 0.177, 0.149, 0.125, 0.105, 0.088, 0.074, 0.063, 
               0.053, 0.044, 0.037, 0.025, 0.02), 
      `in` = c(0.438, 0.25, 0.223, 
               0.187, 0.157, 0.132, 0.11, 0.0937, 0.0787, 0.0661, 0.0555, 0.0469, 
               0.0394, 0.0331, 0.0278, 0.0232, 0.0197, 0.0165, 0.0139, 0.0117, 
               0.0098, 0.0083, 0.007, 0.0059, 0.0049, 0.0041, 0.0035, 0.0029, 
               0.0024, 0.0021, 0.0017, 0.0015, 0.001, 8e-04), 
      `no` = c("7/16", 
               "1/4", "3.5", "4", "5", "6", "7", "8", "10", "12", "14", "16", 
               "18", "20", "25", "30", "35", "40", "45", "50", "60", "70", "80", 
               "100", "120", "140", "170", "200", "230", "270", "325", "400", 
               "500", "632")), 
    class = "data.frame", 
    row.names = c(NA, -34L)
  )
  
  ## phi numbers, used with modified wentworth
  .phi_lut <- data.frame(
    phi = c(12:1, 0, -c(1:12)),
    mm = c(
      1 / 2^c(12:1),
      2^c(0:12)
    )
  )
  
  
  
  ## process rules
  if(.rule == 'mm-sieve') {
    .idx <- which.min(outer(x, .us_sieve_size_lut$mm, FUN = '-')^2)
    .us_sieve_size_lut$no[.idx]
    
  }
  
  if(.rule == 'sieve-mm') {
    
  }
  
  if(.rule == 'mm-phi') {
    
  }
  
  if(.rule == 'phi-mm') {
    
  }
  
  if(.rule == 'sieve-phi') {
    
  }
  
  if(.rule == 'phi-sieve') {
    
  }
  
  
  return(.res)
  
}





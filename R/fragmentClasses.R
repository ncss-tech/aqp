

#' @title Coarse Fragment Class Labels and Diameter
#' @description This is a convenience function for accessing coarse fragment class labels and associated diameter (mm), as defined in various classification systems such as USDA, Unified, and AASHTO.
#'
#' @param sys character, length 1. This is an abbreviated name used to select class labels and fragment diameter.
#' @param flat logical. Fragments are flat, only used by USDA systems.
#' @param rounded logical. Fragments are rounded, only used by AASHTO system.
#' 
#' @references 
#' Schoeneberger, P.J., D.A. Wysocki, E.C. Benham, and Soil Survey Staff. 2012. Field book for describing and sampling soils, Version 3.0. Natural Resources Conservation Service, National Soil Survey Center, Lincoln, NE.
#' 
#' @return named vector of fragment diameter in mm
#' @export
#' 
#' @seealso [sieve()]
#'
#' @examples
#' 
#' # use default system: "usda_simplified"
#' fragmentClasses()
#' fragmentClasses(flat = TRUE)
#' 
#' fragmentClasses(sys = 'usda')
#' fragmentClasses(sys = 'USDA', flat = TRUE)
#' 
#' fragmentClasses(sys = 'international')
#' 
#' fragmentClasses(sys = 'unified')
#' 
#' fragmentClasses(sys = 'aashto')
#' fragmentClasses(sys = 'aashto', rounded = TRUE)
#' 
#' fragmentClasses(sys = 'mod.wentworth')
#' 
fragmentClasses <- function(sys = c('usda_simplified', 'usda', 'international', 'unified', 'aashto', 'mod.wentworth'), flat = FALSE, rounded = FALSE) {
  
  # normalize to lower case
  sys <- tolower(sys)
  
  # most frequent option, usually not specified
  if(missing(sys)) {
    sys <- 'usda_simplified'
  } else {
    sys <- match.arg(sys)
  }
  
  # upper fragment diameter, some large number (mm)
  .upper_limit <- 1e5
  
  # select a specification 
  .spec <- switch(sys, 
                  'usda_simplified' = {
                    
                    if(flat) {
                      c(
                        channers = 150,
                        flagstones = 380,
                        stones = 600,
                        boulders = .upper_limit
                      )
                      
                    } else {
                      c(
                        gravel = 76,
                        cobbles = 250,
                        stones = 600,
                        boulders = .upper_limit
                      )
                    }
                    
                  },
                  
                  'usda' = {
                    
                    if(flat) {
                      c(
                        channers = 150,
                        flagstones = 380,
                        stones = 600,
                        boulders = .upper_limit
                      )
                      
                    } else {
                      c(
                        fine_gravel = 5,
                        medium_gravel = 20,
                        coarse_gravel = 76,
                        cobbles = 250,
                        stones = 600,
                        boulders = .upper_limit
                      )
                    }
                    
                  },
                  
                  'international' = {
                    
                    c(
                      gravel = 20,
                      stones = .upper_limit
                    )
                    
                  },
                  
                  'unified' = {
                    
                    c(
                      fine_gravel = 19,
                      coarse_gravel = 76,
                      cobbles = 300,
                      boulders = .upper_limit
                    )
                    
                  },
                  
                  'aashto' = {
                    
                    if(rounded) {
                      c(
                        fine_gravel = 9.5,
                        medium_gravel = 25,
                        coarse_gravel = 75,
                        boulders = .upper_limit
                      )
                      
                    } else {
                      c(
                        fine_gravel = 9.5,
                        medium_gravel = 25,
                        coarse_gravel = 75,
                        broken_rock = .upper_limit
                      )
                    }
                    
                  },
                  
                  'mod.wentworth' = {
                    
                    c(
                      pebbles = 64,
                      cobbles = 256,
                      boulders = .upper_limit
                    )
                    
                  }
                  
  )
  

  return(.spec)
}






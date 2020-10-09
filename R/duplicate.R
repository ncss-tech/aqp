

#' @title Duplicte Profiles of a SoilProfileColection
#' @description A simple function to duplicate the contents of a \code{SoilProfileColection} object. Old profile IDs are saved as a site-level attribute (\code{oldID}) and new IDs are generated using a numeric serial number.
#' 
#' @author D.E. Beaudette
#' 
#' 
#' @param x a \code{SoilProfileCollection} object with 1 or more profiles
#' @param times requested number of copies
#' @param oldID site-level attribute used to store the original profile IDs
#' 
#' @return a \code{SoilProfileCollection} object
#' 
#' @note Duplication is also possible with \code{permute_profile} and \code{sim} by simulation from 0-variance distributions. This is a simpler, likely faster, alternative when duplication vs. simulation is required.
#' 
#' @keywords manip
#' 
#' 
duplicate <- function(x, times = 3, oldID = '.oldID') {
  
  res <- lapply(1:times, function(i) {
    # local copy of original object
    .x <- x
    
    # save old ID just in case
    site(.x)[[oldID]] <- profile_id(.x)
    
    # overwrite with new ID
    profile_id(.x) <- sprintf('%s-%02d', profile_id(.x), i)
    
    # done
    return(.x)
  })
  
  # safely combine list -> SPC
  s <- combine(res)
}

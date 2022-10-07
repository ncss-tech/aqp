

#' @title Duplicate Profiles of a SoilProfileCollection
#' @description A simple function to duplicate the contents of a \code{SoilProfileCollection} object. Old profile IDs are saved as a site-level attribute (\code{oldID}) and new IDs are generated using a numeric serial number.
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
#' @keywords manip
#' 
#' @examples 
#' 
#' # sample data
#' data('sp4')
#' 
#' # promote to SPC
#' depths(sp4) <- id ~ top + bottom
#' 
#' # duplicate each profile 2 times
#' d <- duplicate(sp4, times = 2)
#' 
#' # graphical check
#' par(mar = c(0, 0, 3, 1))
#' plotSPC(d, color = 'Ca', width = 0.25)
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

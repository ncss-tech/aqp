#' Return upper boundary of argillic horizon
#' 
#' Returns the top depth of the argillic horizon as a numeric vector.
#' 
#' Uses \code{crit.clay.argillic} to determine threshold clay increase, and
#' \code{get.increase.matrix} to determine where increase is met within a
#' vertical distance of 30 cm.
#' 
#' 
#' @param p A single-profile \code{SoilProfileCollection} object.
#' @param clay.attr OPTIONAL: horizon attribute name referring to clay content.
#' default: `clay`
#' @return A numeric vector containing top depth of argillic horizon, if
#' present, or NA.
#' @author Andrew Gene Brown
#' @seealso \code{getArgillicBounds}, \code{get.increase.matrix},
#' \code{crit.clay.argillic}
#' @keywords manip
#' @examples
#' 
#' data(sp1, package = 'aqp')
#' depths(sp1) <- id ~ top + bottom
#' site(sp1) <- ~ group
#' 
#' p <- sp1[1]
#' attr <- 'prop' # clay contents 
#' foo <- argillic.clay.increase.depth(p, clay.attr = attr)
#' foo
#' 

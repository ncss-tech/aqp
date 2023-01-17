
#' @title Munsell Hue Reference and Position Searching
#'
#' @description The 40 Munsell hues are typically arranged from 5R to 2.5R moving clock wise on the unit circle. This function matches a vector of hues to positions on that circle, with options for setting a custom origin or search direction.
#'
#' This function is fully vectorized.
#'
#' @param x character vector of hues, e.g. c('10YR', '5YR'), optional if `returnHues = TRUE`
#' 
#' @param returnHues logical, should the full set of Munsell hues be returned? See details.
#' 
#' @param includeNeutral logical, add 'N' to the end of the full set of Munsell hues 
#' 
#' @param origin hue to be used as the starting point for position searches (position 1)
#' 
#' @param direction indexing direction, should be `cw` (clock wise) or `ccw` (counter-clock wise)
#' 
#' @return A vector of integer hue positions is returned, of the same
#' length and order as `x`. If `returnHues = TRUE`, then all hue
#' names and ordering are returned and `x` is ignored.
#' 
#' @author D.E. Beaudette
#' 
#' @seealso [colorContrast], [huePositionCircle]
#' 
#' @references
#'  - Soil Survey Technical Note 2 [wayback machine URL](https://web.archive.org/web/20220704214918/https://www.nrcs.usda.gov/wps/portal/nrcs/detail/soils/ref/?cid=nrcs142p2_053569)
#' 
#'  - Munsell book of color. 1976. Macbeth, a Division of Kollmorgen Corp., Baltimore, MD.
#' 
#' @keywords manip
#' @export
#' @examples
#'
#' # get hue ordering for setting levels of a factor
#' huePosition(returnHues = TRUE)
#' 
#' # get hue ordering including N (neutral)
#' huePosition(returnHues = TRUE, includeNeutral = TRUE)
#'
#' # get position of the '10YR' hue, relative to standard origin of '5R'
#' # should be 7
#' huePosition(x = '10YR')
#' 
#' # get position of the '10YR' hue, relative to standard origin of '5YR'
#' # should be 3
#' huePosition(x = '10YR', origin = '5YR')
#' 
#' # visualize
#' op <- par(mar = c(0, 0, 0, 0), fg = 'white', bg = 'black')
#' 
#' huePositionCircle(huePosition(returnHues = TRUE, origin = '5YR'))
#' 
#' par(op)
#'
huePosition <- function(x, returnHues = FALSE, includeNeutral = FALSE, origin = '5R', direction = c('cw', 'ccw')) {
  # ordering via Tech Note #2
  # Soil Survey Technical Note 2 [wayback machine URL](https://web.archive.org/web/20220704214918/https://www.nrcs.usda.gov/wps/portal/nrcs/detail/soils/ref/?cid=nrcs142p2_053569)
  
  # sacrifice to CRAN deity
  munsellHuePosition <- NULL
  
  # note: this is incompatible with LazyData: true
  # load look-up table from our package
  load(system.file("data/munsellHuePosition.rda", package="aqp")[1])
  
  ## basic error checking / argument processing
  
  # returnHues must be logical
  if(! inherits(returnHues, 'logical')) {
    stop('returnHues must be TRUE or FALSE', call. = FALSE)
  }
  
  # origin must be a valid hue
  if(! origin %in% munsellHuePosition$hues) {
    stop('invalid hue', call. = FALSE)
  }
  
  ## indexing direction 
  direction <- match.arg(direction)
  
  ## extract hues
  hues <- munsellHuePosition$hues
  
  ## optionally retain neutral, not typically useful
  if(! includeNeutral) {
    hues <- hues[which(hues != 'N')]
  }
  
  ## adjust origin as needed
  start.idx <- match(origin, hues)
  
  # anything other than 5R (position 1) requires an adjustment
  if(start.idx > 1) {
    # wrap-around
    before.idx <- 1:(start.idx - 1)
    hues <- hues[c(start.idx:length(hues), before.idx)]
  } else {
    # no change
    # hues <- hues[start.idx:length(hues)]
  }
  
  # enforce search direction
  if(direction == 'ccw') {
    hues <- c(hues[1], rev(hues[-1]))
  }

  # just the hues
  if(returnHues) {
    return(hues)
  } else {
    # convert hue into position
    res <- match(x, hues)
    return(res)
  }

}

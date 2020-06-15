#' Calculate quasi-Gower's Distance for all profiles in a SoilProfileCollection
#'
#' @description This experimental function calculates pairwise distances between profiles in a SoilProfileCollection by a method related to Gower's Distance metric. One or more site or horizon level attributes are supplied as arguments for use in the distance calculation. Both numeric and categorical attributes are allowed -- with "difference" values calculated for numeric values and  for "different" and "equal" categories \code{1} and \code{0} are assigned respectively.
#' 
#' This is a SoilProfileCollection-specific implementation of the concept. The distance calculations specified in the method are first applied to horizon data, and then to the site data, to produce a "final" distance matrix. 
#' 
#' The pairwise distance of all horizons in the collection is reduced to a single value for each profile-level comparison. The horizon data distance matrix is scaled by both the range within individual variables (first), and the total thickness of each horizon (second). 
#' 
#' An aggregated horizon-to-site level value is added to a site-level distance calculation applied by the same method. Default weighting is assigns all site-level attributes equal weight, which has a tendency to favor site level attributes over the aggregate horizon attribute, so take care when mixing these two.
#' 
#' @param spc A SoilProfileCollection
#' @param attr A character vector containing site or horizon attribute names to use in calculating distance.
#'
#' @return A numeric n x n distance matrix where n is \code{length(spc)}. May contain NaN.
#' 
#' @author Andrew G. Brown
#' 
#' @export gower_distance
#'
#' @examples
#' 
#' library(aqp)
#' 
#' data(sp2)
#' 
#' depths(sp2) <- id ~ top + bottom
#' site(sp2) <-  ~ surface
#' hzdesgnname(sp2) <- "name"
#' 
#' # update soil colors that have data for plot
#' sp2$soil_color <- munsell2rgb(sp2$hue, sp2$value, sp2$chroma)
#'
#' # calculate some site attributes
#' sp2$depth <- profileApply(sp2, estimateSoilDepth, p = "^C|2C|3C")
#' sp2$darkness <- profileApply(sp2, thompson.bell.darkness, pattern = "^A|^2A",
#'                              value = "value", chroma = "chroma")
#' 
#' # calculate a horizon attribute
#' sp2$redness <- hurst.redness(sp2$hue, sp2$value, sp2$chroma)
#' 
#' # default order
#' plot(sp2[1:10,], label = "surface")
#' 
#' # ordered by quasi-gower distance
#' #  reasonably convincing separation of parent materials
#' #  using depth -- in this case this is pattern for ~solum thickness
#' #  also, thompson-bell profile darkness index (site-level)
#' #  also, hurst redness index (by horizon, traditionally for dry colors)
#' #  finally, texture and clay content from horizon
#'
#' plot(sp2[1:10,], 
#'      label = "surface",
#'      plot.order = order(gower.distance(sp2[1:10,], 
#'                         c('depth','darkness','redness','texture',
#'                           'prop'))[1, ]))
#'
#' ## another example with random data
#' spc <- do.call('rbind', lapply(letters[1:3], random_profile))
#' depths(spc) <- id ~ top + bottom
#' 
#' ## make a numeric site attribute (something like slope gradient?)
#' spc$siteval1 <- profileApply(spc, function(x) {
#'                               runif(1, 3, 65)
#'                              })
#'                              
#' ## make a horizon level category (something like structure grade?)
#' spc$catval1 <- letters[round(runif(nrow(spc), 5, 8))]
#' 
#' ## all numeric, 1 site + 3 horizon attributes
#' gower_distance(spc, c("siteval1","p1","p2","p3"))
#'
#' ## 1 site + 4 horizon, 1 horizon attribute categorical
#' gower_distance(spc, c("siteval1","catval1","p1","p2","p3"))
#'
#' ## without site attribute
#' gower_distance(spc, c("catval1","p1","p2","p3"))
#' 
#' ## just numeric horizon data
#' gower_distance(spc, c("p1","p2","p3"))
#'

gower_distance <- function(spc, attr, sitewt = 0.5) {
  h <- horizons(spc)
  s <- site(spc)
  hid <- h[[idname(spc)]]
  hzvars <- attr[attr %in% horizonNames(spc)]
  
  # calculate pairwise difference matrix for horizon attribute
  hdmat <- Reduce('+', lapply(hzvars, function(v) {
    var <- h[[v]]
    if (is.numeric(var)) {
      ov <- abs(outer(var, var, "-")) / diff(range(var, na.rm = TRUE))
    } else {
      ov <- outer(var, var, "!=")
    }
    return(ov)
  }))
  
  # perform horizon-distance weighting
  hdepths <- horizonDepths(spc)
  h$.marginDist <- colSums(hdmat, na.rm = TRUE) / (h[[hdepths[2]]] - h[[hdepths[1]]])
  
  # aggregate the horizon-level distances for all attributes to a single site var
  s$.hzDist <- as.numeric(aggregate(h$.marginDist, by = list(hid),
              sum, na.rm = TRUE)$x)
  
  # calculate pairwise difference matrix for aggregate horizon + site attributes
  svars <- attr[attr %in% siteNames(spc)]
  
  svarmat <- lapply(c('.hzDist', svars),
                    function(v) {
                      var <- s[[v]]
                      if (is.numeric(var)) {
                        ov <- abs(outer(var, var, "-")) / 
                           diff(range(var, na.rm = TRUE))
                      } else {
                        ov <- outer(var, var, "!=")
                      }
                      return(ov)
                    })
  
  sdmat <- Reduce('+', svarmat[2:length(svarmat)])
  
  # if there were any site variables in the attr vector...
  if (length(sdmat) > 0) {
    sdmat <- (sdmat * sitewt) + (svarmat[[1]] * (1 - sitewt))
  } else {
    sdmat <- svarmat[[1]]
  }
  
  # the final result is an n x n distance metric
  return(sdmat)
}


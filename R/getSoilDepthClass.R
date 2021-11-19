## TODO: 
# * data.table safety
# * data.table optimization -> no iteration over profiles

#' @title Generate Soil Depth Class Matrix
#'
#' @description Generate a boolean matrix of soil depth classes, actual soil depth class, and estimate of soil depth from a `SoilProfileCollection` object. Soil depths are estimated using pattern matching applied to horizon designations, by [`estimateSoilDepth()`]. The default REGEX pattern (`p = 'Cr|R|Cd'`) will match most "contacts" described using the USDA / Soil Taxonomy horizon designation conventions.
#'
#'
#' @param f a SoilProfileCollection object
#' @param depth.classes a named vector of classes and depth breaks
#' @param \dots arguments passed to \code{\link{estimateSoilDepth}}
#' @return a \code{data.frame} containing soil depth and depth class for each
#' profile, see examples
#' @author D.E. Beaudette and J.M. Skovlin
#' @seealso \code{\link{estimateSoilDepth}}
#' @keywords manip
#' @examples
#'
#' data(sp1)
#' depths(sp1) <- id ~ top + bottom
#'
#' # generate depth-class matrix
#' sdc <- getSoilDepthClass(sp1, name = 'name')
#'
#' # inspect
#' head(sdc)
#'
#' # join back into sp1 as site-level data
#' site(sp1) <- sdc
#'
#' \dontrun{
#' # sample data
#' data(gopheridge, package='soilDB')
#'
#' getSoilDepthClass(gopheridge, name = 'hzname')
#' }
#'
getSoilDepthClass <- function(f, depth.classes=c('very.shallow'=25, 'shallow'=50, 'mod.deep'=100, 'deep'=150, 'very.deep'=1000), ...) {

  # apply soil-depth finding function
  # horizon top/bottom depths automatically extracted
  soil.depth <- profileApply(f, estimateSoilDepth, ...)

  # convert to 1-column matrix
  soil.depth <- matrix(soil.depth)

  # evaluate depth-class rules
  depth.class.matrix <- t(sapply(soil.depth, function(i) i < depth.classes))

  # determine shallowest matching rule by-row
  # safely deal with NA or inf
  best.match <- apply(
    depth.class.matrix, 
    MARGIN = 1, FUN = function(i) {
      if(all(is.na(i)) | all(is.infinite(i))) {
        NA
      } else {
        names(which.min(which(i)))      
      }
    }
  )

  # zero-out the depth class matrix
  depth.class.matrix[] <- FALSE

  # load best matching depth class by row
  for(i in 1:nrow(depth.class.matrix)) {
    # safely account for NA
    if(is.na(best.match[i])) {
      depth.class.matrix[i, ] <- NA
    } else {
      depth.class.matrix[i, best.match[i]] <- TRUE
    }
  }

  
  # add-in ID and actual depth
  d <- data.frame(
    profile_id(f), 
    depth = soil.depth, 
    depth.class.matrix, 
    depth.class = best.match, 
    stringsAsFactors=FALSE
  )

  # fix ID name
  names(d)[1] <- idname(f)

  # set factor levels
  d$depth.class <- factor(d$depth.class, levels = names(depth.classes))

  return(d)
}

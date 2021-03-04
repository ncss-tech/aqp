#' @title Subset an SPC by applying glom to each profile
#' @name glomApply
#' @aliases glomApply,SoilProfileCollection-method
#'
#' @description \code{glomApply()} is a function used for subsetting SoilProfileCollection objects by depth. It is a wrapper around \code{glom} which is intended to subset single-profile SPCs based on depth intervals/intersection.
#'
#' \code{glomApply} works by accepting a function \code{.fun} as argument. This function is used on each profile to process a multi-profile SPC for input to \code{glom} (via \code{profileApply}). For each profile, \code{.fun} returns a 2-length numeric vector of top and bottom boundaries \code{glom} arguments: \code{z1}, \code{z2}.
#'
#' \code{glomApply} provides the option to generate profile-specific glom depths for a large SPC and handles iteration and rebuilding of a subset SPC object. Optional arguments include: \code{truncate} to cut the boundaries to specified \code{[z1, z2]}; \code{invert} to the portion outside \code{[z1, z2]}, \code{modality} to either \code{"all"} horizons or \code{"thickest"} horizon in the \code{glom} interval. \code{...} are various expressions you can run on the individual profiles using NSE, similar to \code{mutate}.
#'
#' @param object A SoilProfileCollection
#'
#' @param .fun A function that returns vector with top and bottom depth (\code{z1} and \code{z2} arguments to \code{glom}) for a single profile \code{p} (as passed by \code{profileApply})
#'
#' @param truncate Truncate horizon top and bottom depths to \code{[z1, z2]}
#'
#' @param invert Truncate horizon top and bottom depths to \code{[z1, z2]} and then invert result?
#'
#' @param modality Aggregation method for glom result. Default \code{"all"}: return all horizons; \code{"thickest"}: return (shallowest) thickest horizon
#'
#' @param ... A set of comma-delimited R expressions that resolve to a transformation to be applied to a single profile e.g \code{glomApply(hzdept = max(hzdept) - hzdept)} like \code{aqp::mutate}
#'
#' @param chunk.size Chunk size parameter for \code{profileApply}
#'
#' @seealso \code{\link{glom}}  \code{\link{trunc}}
#'
#' @return A SoilProfileCollection.
#' @author Andrew G. Brown.
#'
#' @rdname glomApply
#' @export glomApply
#'
#' @seealso \code{\link{glom}}  \code{\link{glomApply}}
#' @examples
#'
#' data(sp3)
#' depths(sp3) <- id ~ top + bottom
#'
#' # constant depths, whole horizon returns by default
#' plot(glomApply(sp3, function(p) c(25,100)))
#'
#' # constant depths, truncated
#' #(see aqp::trunc for helper function)
#' plot(glomApply(sp3, function(p) c(25,30), truncate = TRUE))
#'
#' # constant depths, inverted
#' plot(glomApply(sp3, function(p) c(25,100), invert = TRUE))
#'
#' # constant depths, inverted + truncated (same as above)
#' plot(glomApply(sp3, function(p) c(25,30), invert = TRUE, truncate=TRUE))
#'
#' # random boundaries in each profile
#' plot(glomApply(sp3, function(p) round(sort(runif(2, 0, max(sp3))))))
#'
#' # random boundaries in each profile (truncated)
#' plot(glomApply(sp3, function(p) round(sort(runif(2, 0, max(sp3)))), truncate = TRUE))
#'
#' # calculate some boundaries as site level attribtes
#' sp3$glom_top <- profileApply(sp3, getMineralSoilSurfaceDepth)
#' sp3$glom_bottom <- profileApply(sp3, estimateSoilDepth)
#'
#' # use site level attributes for glom intervals for each profile
#' plot(glomApply(sp3, function(p) return(c(p$glom_top, p$glom_bottom))))
#'
glomApply <- function(object, .fun = NULL, truncate = FALSE, invert = FALSE,
                      modality = "all", ..., chunk.size = 100) {
  if (is.null(.fun) | !inherits(.fun, 'function'))
    stop("function `.fun`` to return glom boundaries for profiles is missing", call. = FALSE)
  pbindlist(profileApply(object, function(p, ...) {
    dep <- .fun(p, ...)
    return(glom(p, dep[1], dep[2], truncate = truncate, modality = modality, invert = invert))
  }, simplify = FALSE, chunk.size = chunk.size))
}

#' Truncate a SoilProfileCollection to specified top and bottom depth
#'
#' \code{trunc} is a wrapper method around \code{glomApply} for the case when the same top and bottom depth is required for all profiles in a collection. In contrast, \code{glomApply} allows for arbitrary functions to be run on each profile to calculate a unique set of depths.
#' @param x A SoilProfileCollection
#' @param z1 Upper boundary
#' @param z2 Lower boundary
#'
#' @return A SoilProfileCollection truncated to interval \code{[z1, z2]}
#' @export trunc
#' @aliases trunc
#' @method trunc SoilProfileCollection
#' @examples
#'
#' # load sample data
#' data("sp3")
#'
#' # promote to SPC
#' depths(sp3) <- id ~ top + bottom
#'
#' ### TRUNCATE all profiles in sp3 to [0,25]
#'
#' # set up plot parameters
#' par(mfrow=c(2,1), mar=c(0,0,0,0))
#'
#' # full profiles
#' plot(sp3)
#'
#' # trunc'd profiles
#' plot(trunc(sp3, 0, 25))

# note, we are using the default method signature: trunc(x, ...)
#       so this will not show up as being "masked" from base
#setGeneric("trunc", function(x, ...)
#  standardGeneric("trunc"))

setMethod(f = 'trunc', signature(x = 'SoilProfileCollection'),
          function(x, z1, z2) {
            return(glom(x, z1, z2, invert = FALSE, truncate = TRUE))
          })


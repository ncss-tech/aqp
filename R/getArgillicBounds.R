# functions for argillic horizon detection in SPCs
# getArgillicBounds - upper and lower boundary of argillic horizon
# crit.clay.argillic - argillic horizon threshold function
# argillic.clay.increase.depth() - top depth of argillic

#' Estimate upper and lower boundary of argillic diagnostic subsurface horizon
#'
#' @param p A single-profile SoilProfileCollection
#' @param hzdesgn the name of the column/attribute containing the horizon designation; default="hzname"
#' @param clay.attr the name of the column/attribute containing the clay content; default="clay"
#' @param texcl.attr the name of the column/attribute containing the textural class (used for finding sandy horizons); default="texcl"
#' @param require_t require a "t" subscript for positive identification of upper and lower bound of argillic? default: TRUE
#' @param bottom.pattern regular expression passed to \code{estimateSoilDepth} to match the lower boundary of the soil. default is "Cr|R|Cd" which approximately matches paralithic, lithic and densic contacts.
#' @param lower.grad.pattern this is a pattern for adjusting the bottom depth of the argillic horizon upwards from the bottom depth of the soil. The absence of illuviation is used as a final control on horizon pattern matching.
#' @param sandy.texture.pattern this is a pattern for matching sandy textural classes: `-S$|^S$|COS$|L[^V]FS$|[^L]VFS$|LS$|LFS$`
#' @param verbose Print out information about 't' subscripts, sandy textures, plow layers and lower gradational horizons?
#' @param vertical.distance Vertical distance in which clay increase must be met. Default `30` cm
#' @param simplify Return a length 2 vector with upper and lower boundary when `p` has length 1? Default `TRUE`.
#'
#' @description \code{getArgillicBounds} estimates the upper and lower boundary of argillic diagnostic subsurface horizon for a profile in a single-profile SoilProfileCollection object (`p`).
#'
#' The upper boundary is where the clay increase threshold is met. The function uses \code{crit.clay.argillic} as the threshold function for determining whether a clay increase occurs and \code{get.increase.matrix} to determine whether the increase is met, whether vertical distance of increase is sufficiently small, and in which horizon.
#'
#'@details The lower boundary is first approximated as the depth to a lithic/paralithic/densic contact, or some other horizon matchable by a custom regular expression pattern. Subsequently, that boundary is extended upwards to the end of "evidence of illuviation."
#'
#' The depth to contact is estimated using 'bottom.pattern' "Cr|R|Cd" by default. It matches anything containing Cr, R or Cd.
#'
#' The lower gradational horizon regular expression ‘lower.grad.pattern' default is `^[2-9]*B*CB*[^rtd]*[1-9]*$}`. It matches anything that starts with a lithologic discontinuity (or none) and a C master horizon designation. May contain B as second horizon designation in transitional horizon. May not contain 'r' or 't' subscript.
#'
#' The minimum thickness of the argillic horizon is dependent on whether all subhorizons are "sandy" or not. The \code{sandy.texture.pattern} default `-S$|^S$|COS$|L[^V]FS$|[^L]VFS$|LS$|LFS$` captures USDA textural class fine earth fractions that meet "sandy" particle size class criteria.
#'
#' There also is an option ‘require_t' to omit the requirement for evidence of eluviation in form of 't' subscript in 'hzdesgn'. Even if "t" subscript is not required for positive identification, the presence of lower gradational C horizons lacking 't' will still be used to modify the lower boundary upward from a detected contact, if needed. If this behavior is not desired, just set 'lower.grad.pattern' to something that will not match any horizons in your data.
#'
#' @author Andrew G. Brown
#'
#' @return Returns a numeric vector; first value is top depth, second value is bottom depth. If as.list is TRUE, returns a list with top depth named "ubound" and bottom depth named "lbound". If `p` has more than one profile or if `simplify = FALSE` the result is a data.frame containing profile ID, upper and lower boundary columns.
#'
#' @export
#'
#' @examples
#' data(sp1, package = 'aqp')
#' depths(sp1) <- id ~ top + bottom
#' site(sp1) <- ~ group
#'
#' p <- sp1[1]
#' attr <- 'prop' # clay contents
#' foo <- getArgillicBounds(p, hzdesgn='name', clay.attr = attr, texcl.attr="texture")
#' foo
#'
getArgillicBounds <- function(p,
                              hzdesgn = 'hzname',
                              clay.attr = 'clay',
                              texcl.attr = 'texcl',
                              require_t = TRUE,
                              bottom.pattern = "Cr|R|Cd",
                              lower.grad.pattern = "^[2-9]*B*CB*[^rtd]*[1-9]*$",
                              sandy.texture.pattern = "-S$|^S$|COS$|L[^V]FS$|[^L]VFS$|LS$|LFS$",
                              vertical.distance = 30,
                              simplify = TRUE,
                              verbose = FALSE) {
  hz <- horizons(p)
  hzd <- horizonDepths(p)

  # ease removal of attribute name arguments -- deprecate them later
  # for now, just fix em if the defaults dont match the hzdesgn/texcl.attr
  if (!hzdesgn %in% horizonNames(p)) {
    hzdesgn <- guessHzDesgnName(p)
    if (is.na(hzdesgn))
      stop("horizon designation column not correctly specified")
  }

  if (!clay.attr %in% horizonNames(p)) {
    clay.attr <- guessHzAttrName(p, attr = "clay", optional = c("total","_r"))
    if (is.na(clay.attr))
      stop("horizon clay content column not correctly specified")
  }

  if (!texcl.attr %in% horizonNames(p)) {
    texcl.attr <- guessHzTexClName(p)
    if (is.na(texcl.attr))
      stop("horizon texture class column not correctly specified")
  }

  # get upper bound...
  mss <- getMineralSoilSurfaceDepth(p, hzdesgn = hzdesgn, simplify = FALSE)
  pld <- getPlowLayerDepth(p, hzdesgn = hzdesgn, simplify = FALSE)

  # estimate the thickness of the soil profile
  # (you will need to specify alternate pattern if Cr|R|Cd
  # doesn't match your contacts)
  soil.depth <- minDepthOf(p, pattern = bottom.pattern, hzdesgn = hzdesgn, simplify = FALSE)

  # find all horizons with t subscripts; some old/converted horizons have all capital letters
  has_t <- grepl(as.character(hz[[hzdesgn]]), pattern = "[Tt]")
  
  # clay attribute and clay thresholds
  ivar <- hz[[clay.attr]]
  thresh <- crit.clay.argillic(ivar) # threshold.fun()
  
  # in lieu of plow layer and LD, the clay increase depth determines upper bound
  # upper.bound <- argillic.clay.increase.depth(p, clay.attr)
  .N <- NULL; .SD <- NULL; increase.var <- NULL; threshold.vector <- NULL; vdist <- NULL; middepth <- NULL; .hzID <- NULL
  dt <- data.table::data.table(
    id = hz[[idname(p)]],
    middepth = hz[[hzd[1]]] + (hz[[hzd[2]]] - hz[[hzd[1]]]) / 2, # TODO: evaluate midpoint
    increase.var = ivar,
    threshold.vector = thresh,
    .hzID = 1:nrow(hz)
  )
  
  .dmax <- function(x, n) {
    res <- c(0, diff(c(0, x[-n])))
    if(length(res) == 0) 
      return(NA_real_)
    res
  }
  
  dt$vdist <- dt[, list(vdist = .dmax(middepth, .N)), by = "id"]$vdist
  increase.met <- dt[, list(increase.met = .hzID[which(increase.var[2:.N] > threshold.vector[1:(.N - 1)] &
                                                         vdist[2:.N] <= vertical.distance)[1]]), 
                     by = "id"]$increase.met
  upper.bound <- hz[increase.met + 1, ][[hzd[1]]]
  lower.bound <- rep(NA, length(upper.bound))
  
  # handle case where Ap disturbs upper bound of argillic
  shallowest_t <- minDepthOf(p, pattern = "[Tt]", hzdesgn = hzdesgn, simplify = FALSE)
  idx2 <- which(pld[[hzd[2]]] == shallowest_t[[hzd[1]]] & pld[[hzd[2]]] != 0)
  upper.bound[idx2] <- shallowest_t[[hzd[1]]][idx2]
  
  # or a lithologic discontinuity overrides the clay increase req
  depth_ld <- depthOf(p, pattern = "^[2-9].*[Tt]", hzdesgn = hzdesgn, simplify = FALSE)
  idx3 <- which(shallowest_t[[hzd[1]]] %in% depth_ld[[hzd[1]]])
  upper.bound[idx3] <- shallowest_t[[hzd[1]]][idx3]

  deepest_t <- maxDepthOf(p, pattern = "[Tt]", hzdesgn = hzdesgn, top = FALSE, simplify = FALSE)
  c_idx <- minDepthOf(p, pattern = lower.grad.pattern, hzdesgn = hzdesgn, simplify = FALSE)
  
  idx4 <- which(!is.na(upper.bound))
  lower.bound[idx4] <- pmin(deepest_t[[hzd[2]]], c_idx[[hzd[1]]], na.rm = TRUE)[idx4]
  
  # calculate baseline minimum rhickness
  min.thickness <- pmax(7.5, soil.depth[[hzd[1]]] / 10)
  textures <- data.table::data.table(glom(p, upper.bound, lower.bound, df = TRUE))
  
  # sandy textures require 15cm minimum thickness
  idx5 <- which(textures[, list(is_sandy = all(grepl(sandy.texture.pattern, .SD[[texcl.attr]]))),
                   by = eval(idname(p))]$is_sandy)
  min.thickness[idx5] <- 15
  
  idx6 <- which((lower.bound - upper.bound ) < min.thickness)
  upper.bound[idx6] <- NA
  lower.bound[idx6] <- NA
  
  if (length(p) == 1 && simplify) {
    return(c(ubound = upper.bound, lbound = lower.bound))
  }
  res <- data.frame(id = profile_id(p), ubound = upper.bound, lbound = lower.bound)
  colnames(res)[1] <- idname(p)
  .as.data.frame.aqp(res, aqp_df_class(p))
}
#' Determines threshold (minimum) clay content for argillic upper bound
#'
#' Given a vector or matrix of "eluvial" horizon clay contents (\%),
#' \code{crit.clay.argillic()} returns a vector or matrix of minimum clay
#' contents (thresholds) that must be met for an argillic horizon clay
#' increase.
#'
#' Uses the standard equations for clay contents less than 15 \%, between 15
#' and 40 \%, and greater than 40 \%. Based on the clay increase criteria in
#' the definition of the argillic horizon from 12th Edition Keys to Soil
#' Taxonomy (Soil Survey Staff, 2014).
#'
#'
#' @param eluvial_clay_content A numeric vector or matrix containing clay
#' contents of potential "eluvial" horizons. May contain \code{NA}.
#' @return A vector or matrix (input-dependent) containing minimum "illuvial"
#' horizon clay contents (thresholds) to be met for argillic horizon clay
#' increase.
#' @note This function is intended for identifying clay content threshold
#' required for an argillic horizon. These thresholds may not apply depending
#' on the specifics of your soil. E.g. if the upper part of argillic has been
#' plowed (has Ap immediately over upper boundary) the clay increase
#' requirement can be waived (Soil Survey Staff, 2014).
#' @author Andrew Gene Brown
#' @seealso \code{\link{getArgillicBounds}}, \code{\link{get.increase.matrix}}
#' @references Soil Survey Staff. 2014. Keys to Soil Taxonomy, 12th ed.
#' USDA-Natural Resources Conservation Service, Washington, DC.
#' @keywords manip
#' @examples
#'
#' # crit.clay.argillic uses different equations for clay content
#' # less than 15 %, between 15 and 40 %, and >40 %
#'
#' crit.clay.argillic(eluvial_clay_content=c(5, 20, 45))
#'
crit.clay.argillic <- function(eluvial_clay_content) {
  # eluvial clay content is a numeric vector or matrix subsettable with logical vectors based on clay (NA omitted)
  buf <- eluvial_clay_content
  idx.mask <- is.na(buf)
  buf[idx.mask] <- 0

  idx.lt15 <- buf < 15
  idx.lt15[idx.mask] <- FALSE

  idx.gt40 <- buf >= 40
  idx.gt40[idx.mask] <- FALSE

  idx.other <- (buf >= 15) & (buf < 40)
  idx.other[idx.mask] <- FALSE

  buf[idx.lt15] <- eluvial_clay_content[idx.lt15] + 3
  buf[idx.gt40] <- eluvial_clay_content[idx.gt40] + 8
  buf[idx.other] <- 1.2*eluvial_clay_content[idx.other]
  return(round(buf))
}

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
argillic.clay.increase.depth <- function(p, clay.attr = 'clay') {
  vd <- 30
  return(get.increase.depths(p, attr = clay.attr,
                             threshold.fun = crit.clay.argillic,
                             vertical.distance = vd)[1])
}


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
#' @param sandy.texture.pattern this is a pattern for matching sandy textural classes: "-S$|^S$|COS$|L[^V]FS$|[^L]VFS$|LS$|LFS$"
#' @param verbose Print out information about 't' subscripts, sandy textures, plow layers and lower gradational horizons?
#'
#' @description \code{getArgillicBounds} estimates the upper and lower boundary of argillic diagnostic subsurface horizon for a profile in a single-profile SoilProfileCollection object ('p').
#'
#' The upper boundary is where the clay increase threshold is met. The function uses \code{crit.clay.argillic} as the threshold function for determining whether a clay increase occurs and \code{get.increase.matrix} to determine whether the increase is met, whether vertical distance of increase is sufficiently small, and in which horizon.
#'
#'@details The lower boundary is first approximated as the depth to a lithic/paralithic/densic contact, or some other horizon matchable by a custom regular expression pattern. Subsequently, that boundary is extended upwards to the end of "evidence of illuviation."
#'
#' The depth to contact is estimated using 'bottom.pattern' "Cr|R|Cd" by default. It matches anything containing Cr, R or Cd.
#'
#' The lower gradational horizon regular expression ‘lower.grad.pattern' default is \code{^[2-9]*B*CB*[^rtd]*[1-9]*$}. It matches anything that starts with a lithologic discontinuity (or none) and a C master horizon designation. May contain B as second horizon designation in transitional horizon. May not contain 'r' or 't' subscript.
#'
#' The minimum thickness of the argillic horizon is dependent on whether all subhorizons are "sandy" or not. The \code{sandy.texture.pattern} default \code{-S$|^S$|COS$|L[^V]FS$|[^L]VFS$|LS$|LFS$} captures USDA textural class fine earth fractions that meet "sandy" particle size class criteria.
#'
#' There also is an option ‘require_t' to omit the requirement for evidence of eluviation in form of 't' subscript in 'hzdesgn'. Even if "t" subscript is not required for positive identification, the presence of lower gradational C horizons lacking 't' will still be used to modify the lower boundary upward from a detected contact, if needed. If this behavior is not desired, just set 'lower.grad.pattern' to something that will not match any horizons in your data.
#'
#' @author Andrew G. Brown
#'
#' @return Returns a numeric vector; first value is top depth, second value is bottom depth. If as.list is TRUE, returns a list with top depth named "ubound" and bottom depth named "lbound"
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
                              verbose = FALSE) {

  if (length(p) != 1)
   stop("`p` must be a SoilProfileCollection containing one profile", call.=FALSE)

  hz <- horizons(p)
  depthcol <- horizonDepths(p)

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
  mss <- getMineralSoilSurfaceDepth(p, hzdesgn = hzdesgn)
  pld <- getPlowLayerDepth(p, hzdesgn = hzdesgn)

  # estimate the thickness of the soil profile
  # (you will need to specify alternate pattern if Cr|R|Cd
  # doesn't match your contacts)
  soil.depth <- estimateSoilDepth(p, name = hzdesgn,
                                  p = bottom.pattern)

  # find all horizons with t subscripts; some old/converted horizons have all capital letters
  has_t <- grepl(as.character(hz[[hzdesgn]]), pattern = "[Tt]")

  # in lieu of plow layer and LD, the clay increase depth determines upper bound
  upper.bound <- argillic.clay.increase.depth(p, clay.attr)
  lower.bound <- -Inf

  # handle case where Ap disturbs upper bound of argillic
  # or a lithologic discontinuity overrides the clay increase req
  #  eliminating evidence of accumulation
  if (is.na(upper.bound)) {
    shallowest_t <- minDepthOf(p, pattern = "[Tt]", hzdesgn = hzdesgn)
    depth_ld <- depthOf(p, pattern = "^[2-9].*[Tt]", hzdesgn = hzdesgn)
    if (!is.na(shallowest_t)) {
      if (pld == shallowest_t)
        upper.bound <- shallowest_t

      if (!all(is.na(depth_ld)))
        if (any(shallowest_t %in% depth_ld))
          upper.bound <- shallowest_t
    }
  }

  # if upper.bound is non-NA, we might have argillic, because clay increase is met at some depth
  if (!is.na(upper.bound)) {

    ########
    # TODO: allow evidence of illuviation from lab data fine clay ratios etc? how?
    #
    # TODO: if `require_t` is set to `FALSE`... or in a future getKandicBounds()...
    #       how do you detect the bottom of a argillic or kandic horizon (which need not have clay films)
    #          in e.g. saprolite ?
    #       could you look for C in master horizon designation for e.g. rock structure / parent material?
    #       in lieu of checking for t and a cemented bedrock contact... is that how lower.bound should be determined?
    ########
    if (sum(has_t) | !require_t) {
      # get index of last horizon with t
      idx.last <- rev(which(has_t))[1]

      ## Partial fix for TODO #2? seems reasnable for the require_t=FALSE lower.bound case
      # take _very_ last horizon depth first (will be truncated to contact depth if needed)
      depth.last <- as.numeric(.data.frame.j(hz[nrow(hz),], depthcol[2], aqp_df_class(p)))

      # take the top depth of any B or C horizon  without t subscript above "depth.last"
      c.idx <- which(grepl(hz[[hzdesgn]], pattern = lower.grad.pattern))
      if (length(c.idx)) {
        c.horizon <- as.numeric(.data.frame.j(hz[c.idx[1], ], depthcol[1], aqp_df_class(p)))

        # if the _shallowest C horizon_ top depth is above the _last horizon_ bottom depth (could be top depth of same hz)
        if (c.horizon < depth.last)  {
          # use the top depth of the first C horizon that matched the pattern
          if (verbose)
            message(paste0("Found ",paste0(hz[[hzdesgn]][c.idx], collapse = ","),
                         " below argillic, adjusting lower bound (",
                         idname(p),": ", profile_id(p),")"))
          # plot(p)
          # print(c.idx)
          depth.last <- c.horizon
        }
      }

      # get the bottom depth of the last horizon with a t (this could be same as c.horizon above)
      if (require_t)
        depth.last <- as.numeric(.data.frame.j(hz[idx.last,],
                                               depthcol[2],
                                               aqp_df_class(p)))

      # in rare cases, the bottom depth of the bedrock/contact is not populated
      # step back until we find one that is not NA
      idx.last.i <- idx.last
      while (is.na(depth.last) & idx.last.i >= 0) {
        depth.last <- as.numeric(.data.frame.j(hz[idx.last.i,],
                                               depthcol[2],
                                               aqp_df_class(p)))
        idx.last.i <- idx.last.i - 1
      }

      # if the last horizon with a t is below the contact (Crt or Rt) or some other weird reason
      if (soil.depth < depth.last) {
        # return the soil depth to contact
        lower.bound <- soil.depth

      } else {
        #otherwise, return the bottom depth of the last horizon with a t
        lower.bound <- depth.last
      }
    } else {
      if (verbose)
        message(paste0("Profile (",profile_id(p),") has clay increase with no evidence of illuviation (t)."))
      lower.bound <- NA
      upper.bound <- NA
    }
  } else {

    # if the upper bound is NA, return NA for the lower bound
    lower.bound <- NA
  }

  if (!is.finite(lower.bound))
    lower.bound <- NA

  if (is.na(upper.bound))
    return(c(ubound = NA, lbound = NA))

  bdepthspc <- glom(p, mss, upper.bound, df = TRUE)

  # if there are no overlying horizons, return NA
  if (is.null(bdepthspc) | all(is.na(bdepthspc))) {
    if (verbose)
      message(paste0("Profile (",profile_id(p),
                     ") has no horizons overlying a [possible] argillic."))
    return(c(ubound = NA, lbound = NA))
  }

  bdepths <- bdepthspc[[depthcol[2]]]
  if (all(!is.na(c(upper.bound, lower.bound)))) {

    # if argi bounds are found check that minimum thickness requirements are met
    min.thickness <- max(7.5, max(bdepths, na.rm = TRUE) / 10)

    textures <- glom(p, upper.bound, lower.bound, df = TRUE)[[texcl.attr]]
    is_sandy <- all(grepl(sandy.texture.pattern, textures, ignore.case = TRUE))

    if (is_sandy) {
      min.thickness <- 15
    }

    if (lower.bound - upper.bound < min.thickness) {
      if (verbose)
        message(paste0("Profile (",profile_id(p),
                     ") does not meet thickness requirement."))
      return(c(ubound = NA, lbound = NA))
    }
  }

  if (!is.na(upper.bound)) {
    # it is possible that a subhorizon of the Ap horizon meets the clay increase
    if (pld > upper.bound) {
      upper.bound <- pld
      if (verbose)
        message(paste0("Profile (",profile_id(p),
                       ") meets clay increase within plowed layer."))
    }
  }
  return(c(ubound = upper.bound, lbound = lower.bound))
}

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

# returns the top and bottom depth of the argillic horizon as a numeric vector.
# applies get.increase.depth() identify the argillic horizon upper bound
# threshold fun()=`crit.clay.argillic` defines the clay increase that must be met within 30 cm vertical distance
# the default horizon attribute name is `clay`, but it can be adjusted as needed
argillic.clay.increase.depth <- function(p, clay.attr = 'clay') {
  vd <- 30
  return(get.increase.depths(p, attr = clay.attr,
                          threshold.fun = crit.clay.argillic,
                          vertical.distance = vd)[1])
}


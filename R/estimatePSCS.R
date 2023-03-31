#estimatePSCS()
#' Estimate boundaries of the particle size control section (U.S Soil Taxonomy;
#' 12th edition)
#'
#' Estimates the upper and lower boundary of the particle size control section
#' by applying a programmatic version of the particle size control section key
#' from the Keys to Soil Taxonomy (12th edition).
#'
#' Requires information to identify argillic horizons (clay contents, horizon
#' designations) with \code{getArgillicBounds()} as well as the presence of
#' plow layers and surface organic soil material. Any
#' \code{getArgillicBounds()} arguments may be passed to \code{estimatePSCS}.
#'
#' Requires information on taxonomic order (to handle andisols).
#'
#' WARNING: Soils in arenic or grossarenic subgroups, with fragipans, or with
#' strongly contrasting PSCs may not be classified correctly. The author would
#' welcome a dataset to develop this functionality for.
#'
#' @param p A SoilProfileCollection 
#' @param clay.attr Name of the horizon attribute containing clay contents.
#' Default 'clay'
#' @param texcl.attr Name of the horizon attribute containing textural class
#' (used for finding sandy textures). Default 'texcl'
#' @param hzdesgn Name of the horizon attribute containing the horizon
#' designation. Default 'hzname'
#' @param tax_order_field Name of the site attribute containing taxonomic
#' order; for handling PSCS rules for Andisols in lieu of lab data. May be NA
#' or column missing altogether, in which case Andisol PSC possibility is
#' ignored.
#' @param bottom.pattern Regular expression pattern to match a root-restrictive
#' contact. Default matches Cr, R or Cd. This argument is passed to both
#' estimateSoilDepth and getArgillicBounds.
#' @param simplify Return a length 2 vector with upper and lower boundary when p has length 1? Default TRUE.
#' @param ...  additional arguments are passed to getArgillicBounds()
#' @return A numeric vector (when `simplify=TRUE`) containing the top and bottom depth of the particle
#' size control section. First value is top, second value is bottom. 
#' If `p` contains more than one profile, the result is a data.frame with profile ID plus PSCS top and bottom depths.
#' @author Andrew Gene Brown
#' @seealso \code{getArgillicBounds}, \code{getSurfaceHorizonDepth}
#' @references Soil Survey Staff. 2014. Keys to Soil Taxonomy, 12th ed.
#' USDA-Natural Resources Conservation Service, Washington, DC.
#' @keywords manip
#' @export 
#' @examples
#'
#' data(sp1, package = 'aqp')
#' depths(sp1) <- id ~ top + bottom
#' site(sp1) <- ~ group
#'
#' p <- sp1
#' attr <- 'prop' # clay contents
#' foo <- estimatePSCS(p, hzdesgn='name', clay.attr = attr, texcl.attr="texture")
#' foo
#'
#'
estimatePSCS = function(p, hzdesgn = "hzname", clay.attr = "clay",
                        texcl.attr = "texcl", tax_order_field = "tax_order",
                        bottom.pattern='Cr|R|Cd', simplify = TRUE, ...) {
  
  .LAST <- NULL
  hz.depths <- horizonDepths(p)

  attr.len <- unlist(lapply(c(hzdesgn, clay.attr, texcl.attr), length))
  if (any(attr.len > 1))
    stop("horizon designation, clay attribute or texture class attribute must have length 1")

  if (is.null(hzdesgn) | (!hzdesgn %in% horizonNames(p))) {
    hzdesgn <- guessHzDesgnName(p, required = TRUE)
    if (hzdesgn == "")
      stop("horizon designation column not correctly specified")
  }

  if (is.null(clay.attr) | (!clay.attr %in% horizonNames(p))) {
    clay.attr <- guessHzAttrName(p, attr = "clay", optional = c("total","_r"))
    if (clay.attr == "")
      stop("horizon clay content column not correctly specified")
  }

  if (is.null(texcl.attr) | (!texcl.attr %in% horizonNames(p))) {
    texcl.attr <- guessHzTexClName(p)
    if (texcl.attr == "")
      stop("horizon texture class column not correctly specified")
  }

  soildepth <- minDepthOf(p, hzdesgn = hzdesgn, pattern = bottom.pattern, simplify = FALSE)[[hz.depths[1]]]
  nobottomidx <- is.na(soildepth)
  if (any(nobottomidx)) {
    soildepth[nobottomidx] <- p[which(nobottomidx), , .LAST][[hz.depths[2]]]
  }
  
  andisols_flag <- rep(FALSE, length(soildepth))
  shallow_flag <- rep(FALSE, length(soildepth))

  # Parts D (argillic starts >100cm  depth) and F (all other mineral soils)
  default_t <- rep(25, length(soildepth))
  default_b <- rep(100, length(soildepth))
  
  # Key part A (soils with restriction in shallow depth)
  lt36idx <- which(soildepth <= 36)
  if (length(lt36idx) > 0) {
    default_t[lt36idx] <- 0
    default_b[lt36idx] <- soildepth[lt36idx]
    shallow_flag[lt36idx] <- TRUE
  }
  
  # Key part B (Andisols)
  if (tax_order_field %in% siteNames(p)) {
    andidx <- grep("[Aa]ndisols", p[[tax_order_field]])
    if (length(andidx) > 0) {
      default_t[andidx] <- 0
      default_b[andidx] <- 100
      andisols_flag[andidx] <- TRUE
    }
  }

  # Adjust PSCS range downward if organic soil material is present at surface (i.e. mineral soil surface depth > 0)
  odepth <- getMineralSoilSurfaceDepth(p, hzdesgn, simplify = FALSE)[[hz.depths[2]]]
  ohzidx <- which(odepth > 0)
  if (length(ohzidx) > 0) {
    
    default_t[ohzidx] <- default_t[ohzidx] + odepth[ohzidx]
    maxdtruncidx <- which(default_b != soildepth)
    
    if (length(maxdtruncidx) > 0) {
      default_b[maxdtruncidx] <- default_b[maxdtruncidx] + odepth[maxdtruncidx]
    }
    
    # TODO: get logic for the classification of histosol/histel control section in here?
    # handle thick o horizons that cause the mineral PSCS to be greater than soil depth
    bad.idx <- default_t[ohzidx] >= soildepth[ohzidx]
    default_t[bad.idx] <- NA_real_
    default_b[bad.idx] <- NA_real_
  }

  # Key parts C and E (has argillic/kandic/natric WITHIN 100CM)

  if (any(!andisols_flag)) {
    argillic_bounds <- getArgillicBounds(
      p,
      clay.attr = clay.attr,
      texcl.attr = texcl.attr,
      hzdesgn = hzdesgn,
      bottom.pattern = bottom.pattern,
      ...,
      simplify = FALSE # always return data.frame
    )
    
    lt100argidx <- which(argillic_bounds$ubound < 100)
    thinargidx <- argillic_bounds$lbound - argillic_bounds$ubound <= 50
    shallowargidx <- which(argillic_bounds$lbound <= 25)
    
    if (length(lt100argidx) > 0) {
      default_t[lt100argidx] <- argillic_bounds$ubound[lt100argidx]
    }
    
    if (length(which(thinargidx)) > 0) {
      default_b[which(thinargidx)] <- argillic_bounds$lbound[which(thinargidx)]
    }
    
    if (length(which(!thinargidx)) > 0) {
      default_b[which(!thinargidx)] <- argillic_bounds$ubound[which(!thinargidx)] + 50
    }
    
    if (length(shallowargidx) > 0) {
      default_b[shallowargidx] <- 100
    }
  }

  # Adjust PSCS top depth to bottom of plow layer (if appropriate)
  plow_layer_depth <- getPlowLayerDepth(p, hzdesgn, simplify = FALSE)[[hz.depths[2]]]
  plowidx <- which(plow_layer_depth >= 25 + odepth)
  if (length(plowidx) > 0) {
    default_t[plowidx] <- plow_layer_depth[plowidx]
  }
  
  # Adjust PSCS top depth to mineral soil surface for soils <36cm to restriction
  shallowoidx <- which(shallow_flag & default_t != 0)
  if (length(shallowoidx) > 0) {
    default_t[shallowoidx] <- odepth[shallowoidx]
  }

  # Adjust PSCS bottom depth to restriction depth, if appropriate
  restridx <- which(soildepth < default_b)
  if (length(restridx) > 0) { #truncate to restriction
    default_b[restridx] <- soildepth[restridx]
  }
  
  if (length(default_t) == 1 && simplify) {
    return(as.numeric(c(default_t, default_b)))
  }
  
  res <- data.frame(id = profile_id(p), pscs_top = default_t, pscs_bottom = default_b)
  names(res)[1] <- idname(p)
  res
}

# calculate control section based on materials in surface tier, root limiting layers, permafrost and water
histosol.control.section <- function(p) {
  
  # control section stops at Cr|R|Cd
  rll.depth <- estimateSoilDepth(p, no.contact.depth = 0, no.contact.assigned = 200)
  
  # control section stops at permafrost + 25cm
  permafrost.depth <- minDepthOf(p, "f|ff",  no.contact.assigned = Inf)
  
  # control section stops at water bottom depth  greater than either 130 or 160 cm, as applicable
  water.depth <- minDepthOf(p, "W", top = FALSE, no.contact.assigned = Inf)
  
  # get surface tier thickness for p
  hst <- histosol.surface.tier(p)$hst_b
  
  if (hst == 60) {
    # low bulk density OSM
    if (water.depth > 160) {
      water.depth <- minDepthOf(p, "W", no.contact.depth = 0, no.contact.assigned = Inf)
    }
    return(data.frame(peiid = profile_id(p), 
                      hcs_t = 0, 
                      hcs_b = min(rll.depth, permafrost.depth + 25, min(water.depth, 160))))
  } else if (hst == 30) {
    # other cases
    if (water.depth > 130) {
      water.depth <- minDepthOf(p, "W", no.contact.depth = 0, no.contact.assigned = Inf)
    }
    return(data.frame(peiid = profile_id(p), 
                      hcs_t = 0, 
                      hcs_b = min(rll.depth, permafrost.depth + 25, min(water.depth, 130))))
  }
  
  # error in finding surface tier
  return(data.frame(
    peiid = profile_id(p),
    hcs_t = NA,
    hcs_b = NA
  ))
  
}

histosol.surface.tier <- function(p, lieutex = "lieutex") {
  
  depthz <- horizonDepths(p)
  
  if (!lieutex %in% names(p)) {
    lieutex <- guessHzAttrName(p, "lieu", c("tex", "in"))
    if (is.na(lieutex))
      stop("could not guess in-lieu texture class; specify name with `lieutex` argument")
  }
  
  rll <- estimateSoilDepth(p, no.contact.depth = 0, no.contact.assigned = 200)
  
  mss <- getMineralSoilSurfaceDepth(p)
  
  # if there is a mineral surface, the surface tier is always 30cm thick
  if (mss == 0) {
    return(data.frame(peiid = profile_id(p), 
                      hst_t = 0, 
                      hst_b = 30))
  }
  
  p60 <- glom(p, 0, min(rll, 60), truncate = TRUE)
  
  # allow for multiple lines of evidence for fibric/low density OM
  #  either lieutex of peat or "i" suffix throughout all horizons in upper 60
  idx <- which(p60[[lieutex]] %in% c("peat", "spm") | grepl("i", p60[[hzdesgnname(p)]]))
  
  if (length(idx)) {
    hz <- horizons(p)[idx,]
    
    # calculate sum of fibric material thickness within upper 60cm, compare to total hz thickness
    fibric.thickness <- sum(hz[[depthz[2]]] - hz[[depthz[1]]], na.rm = TRUE)
    total.hz.thickness <- 1 * (max(p60[[depthz[2]]], na.rm = TRUE) - min(p60[[depthz[1]]], na.rm =
                                                       TRUE))
    if (fibric.thickness >= total.hz.thickness) {
      return(data.frame(
        peiid = profile_id(p),
        hst_t = 0,
        hst_b = 60
      ))
    }
  }
  
  return(data.frame(
    peiid = profile_id(p),
    hst_t = 0,
    hst_b = 30
  ))
}

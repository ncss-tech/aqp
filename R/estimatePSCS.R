#estimatePSCS()

estimatePSCS = function(p, hzdesgn = "hzname", clay.attr = "clay",
                        texcl.attr = "texcl", tax_order_field = "tax_order",
                        bottom.pattern='Cr|R|Cd', ...) {

  hz.depths <- horizonDepths(p)

  attr.len <- unlist(lapply(c(hzdesgn, clay.attr, texcl.attr), length))
  if (any(attr.len > 1))
    stop("horizon designation, clay attribute or texture class attribute must have length 1")

  if (is.null(hzdesgn) | (!hzdesgn %in% horizonNames(p))) {
    hzdesgn <- guessHzDesgnName(p)
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

  soildepth <- estimateSoilDepth(f = p, name = hzdesgn,
                                 top = hz.depths[1], bottom = hz.depths[2],
                                 p = bottom.pattern)
  andisols_flag <- FALSE
  shallow_flag <- FALSE

  # Parts D (argillic starts >100cm  depth) and F (all other mineral soils)
  default_t <- 25
  default_b <- 100

  # Key part A (soils with restriction in shallow depth)
  if (soildepth <= 36) {
    default_t <- 0
    default_b <- soildepth
    shallow_flag <- TRUE
  }

  # Key part B (Andisols)
  if (tax_order_field %in% siteNames(p)) {
    if (length(site(p)[[tax_order_field]])) {
      if (!is.na(site(p)[[tax_order_field]])) {
        if (all(grepl("[Aa]ndisols", site(p)[[tax_order_field]]))) {
          default_t <- 0
          default_b <- 100
          andisols_flag <- TRUE
        }
      }
    }
  }

  # Adjust PSCS range downward if organic soil material is present at surface (i.e. mineral soil surface depth > 0)
  odepth <- getMineralSoilSurfaceDepth(p, hzdesgn)
  if (odepth > 0) {
    default_t <- default_t + odepth
    if (default_b != soildepth)
      default_b <- default_b + odepth
  }

  # Key parts C and E (has argillic/kandic/natric WITHIN 100CM)
  if (!andisols_flag) {
    argillic_bounds <- getArgillicBounds(p, clay.attr = clay.attr,
                                        texcl.attr = texcl.attr,
                                        hzdesgn = hzdesgn,
                                        bottom.pattern = bottom.pattern,
                                        ...)

    if (!any(is.na(argillic_bounds))) {
      if (argillic_bounds[1] < 100) {
        default_t <- argillic_bounds[1]

        # Part C - argillic near surface
        if (argillic_bounds[1] <= 100) {
          # TODO: check arenic and grossarenic subgroups, fragipan depths, strongly contrasting PSCs... should work fine for CA630 though
          if (argillic_bounds[2] - argillic_bounds[1] <= 50) {
            default_b <- argillic_bounds[2]
          } else {
            default_b <- argillic_bounds[1] + 50
          }
          if (argillic_bounds[2] <= 25) {
            default_b <- 100
          }
        }
      }
    }
  }

  # Adjust PSCS top depth to bottom of plow layer (if appropriate)
  plow_layer_depth <- getPlowLayerDepth(p, hzdesgn)
  if (plow_layer_depth)
    if (plow_layer_depth >= 25 + odepth)
      default_t <- plow_layer_depth

  # Adjust PSCS top depth to mineral soil surface for soils <36cm to restriction
  if (shallow_flag & default_t != 0) {
    default_t <- odepth
  }

  # Adjust PSCS bottom depth to restriction depth, if appropriate
  if (soildepth < default_b) { #truncate to restriction
    default_b <- soildepth
  }

  return(as.numeric(c(default_t, default_b)))
}

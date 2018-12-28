#estimatePSCS()

estimatePSCS = function(p, attr = 'clay', require_t = TRUE, tax_order_field="tax_order", hzdesgn = "hzname") {
  hz.depths <- horizonDepths(p)
  soildepth <- estimateSoilDepth(f = p, name = hzdesgn, top = hz.depths[1], bottom = hz.depths[2])
  
  # Parts D (argillic starts >100cm  depth) and F (all other mineral soils)
  default_t = 25
  default_b = 100
  
  # Key part A (soils with restrictio in shallow depth)
  if(soildepth <= 36) {
    default_t = 0
    default_b = soildepth
  }
  
  # Key part B (Andisols)
  if(tax_order_field %in% siteNames(p)) {
    if(length(site(p)[[tax_order_field]])) {
      if(!is.na(site(p)[[tax_order_field]])) {
        if(site(p)[[tax_order_field]] == "andisols") {
          default_t = 0
          default_b = 100
        }  
      }
    }
  }
  
  # Adjust PSCS range downward if organic soil material is present at surface (i.e. mineral soil surface depth > 0)
  odepth <- getMineralSoilSurfaceDepth(p, hzdesgn) 
  if(odepth > 0) {
    default_t = default_t + odepth
    if(default_b != soildepth)
      default_b = default_b + odepth
  }
  
  # Key parts C and E (has argillic/kandic/natric WITHIN 100CM)
  #if(is.na(site(p)[[tax_order_field]]) | site(p)[[tax_order_field]] != "andisols") {
  argillic_bounds = getArgillicBounds(p, attr = attr, hzdesgn = hzdesgn, require_t = require_t)
  if(!any(is.na(argillic_bounds))) { 
    if(argillic_bounds[1] < 100) {
      default_t <- argillic_bounds[1]
      # Part C - argillic near surface
      if(argillic_bounds[1] <= 100) {
        # TODO: check arenic and grossarenic subgroups, fragipan depths, strongly contrasting PSCs... should work fine for CA630 though
        if(argillic_bounds[2] - argillic_bounds[1] <= 50)
          default_b <- argillic_bounds[2]
        else
          default_b <- argillic_bounds[1] + 50 
      } else if(argillic_bounds[2] <= 25) {
        default_b = 100
      } 
    }
  }  
  #}
  
  # Adjust PSCS top depth to bottom of plow layer (if appropriate)
  plow_layer_depth = getPlowLayerDepth(p, hzdesgn)
  if(plow_layer_depth)
    if(plow_layer_depth >= 25 + odepth) 
      default_t = plow_layer_depth
  
  # Adjust PSCS bottom depth to restriction depth, if appropriate
  if(soildepth < default_b) {#truncate to restriction
    default_b = soildepth
  }
  
  return(as.numeric(c(default_t, default_b)))
}





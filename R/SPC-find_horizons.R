## TODO: this should be 1 of a series of functions built in to aqp
## find_horizons_by_points: locate horizons at distinct depths
## find_horizons_by_interval: locate horizons that overlap with a defined interval
# spc: SPC of single profile
# z: vector of depths to search for
find_horizons_by_points <- function(spc, z) {
  # sanity check
  if(length(s) > 1)
    stop('this function can only operate on a SoilProfileCollection containing 1 profile', call. = FALSE)
  # get top/bottom names
  hdc <- horizonDepths(spc)
  h <- horizons(spc)[, hdc]
  
  # implicit iteration over elements of 'z'
  idx <- sapply(z, function(i) {
    which(i >= h[, 1] & i <= h[, 2])
  })
  
  # index to horizons containing depths 'z'
  return(idx)
}

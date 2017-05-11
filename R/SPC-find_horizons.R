
### this isn't ready for prime time, better to use slice() for now

### 

## TODO: this should be 1 of a series of functions built in to aqp
## find_horizons_by_points: locate horizons at distinct depths
## find_horizons_by_interval: locate horizons that overlap with a defined interval

# 
# 
# # spc: SoilProfileCollection
# # z: vector of depths to search for
# find_horizons_by_points <- function(spc, z) {
#   # get top/bottom names
#   hdc <- horizonDepths(spc)
#   h <- horizons(spc)[, hdc]
#   
#   # implicit iteration over elements of 'z'
#   idx <- sapply(z, function(i) {
#     res <- i >= h[, 1] & i <= h[, 2]
#     res[!res] <- NA
#     return(res)
#   })
#   
#   # unique index to horizons containing depths 'z'
#   # this index is relative to the current soil profile in the collection
#   return(unique(as.vector(idx)))
# }
# 
# 

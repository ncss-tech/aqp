## TODO: documentation / generalization
# note source data must be "normalized" via dice() first; assumes each profile has the same number of horizons

# generate labels for slabs
genSlabLabels <- function(slab.structure = 1, max.d, n.profiles) {
  
  # fixed-size slabs
  if (length(slab.structure) == 1) {
    # generate sequence of segment labels
    seg.label <- rep(1:ceiling(max.d / slab.structure), each = slab.structure, length = max.d)
    
    # general segment labels
    seg.label.levels <- tapply(1:max.d, seg.label, 
                               function(i) {
                                 r <- range(i, na.rm = TRUE)
                                 paste(c(r[1] - 1, r[2]), collapse = '-')
                               })
  }
  
  # user-defined slabs
  if (length(slab.structure) > 1) {
    if (slab.structure[1] == 0 & length(slab.structure) > 2) {
      # trivial case where segments start from 0
      
      
      seg.label <- rep(slab.structure[-1], times = diff(slab.structure))[1:max.d]
    
    } else {
      # other case: user defines an arbitrary lower and upper limit
      if (length(slab.structure) != 2)
        stop('user-defined slab boundaries must either start from 0, or contain two values between 0 and the max soil depth')
      
      # calculate thickness of each slab
      slab.thickness <- diff(slab.structure)
      
      # make a new label for the slab
      new.label <- paste(slab.structure, collapse = '-')
      
      # generate an index for the slab
      slab.idx <- rep(new.label, times = slab.thickness)
      seg.label <- slab.idx 
    }
    
    # generate segment labels	
    seg.label.levels <- sapply(1:(length(slab.structure) - 1), function(i) {
      paste(c(slab.structure[i], slab.structure[i + 1]), collapse = '-')
    })
  }
  
  # covert into a factor that can be used to split profiles into slabs
  res <- factor(rep(seg.label, times = n.profiles), labels = seg.label.levels)
  
  return(res)
}

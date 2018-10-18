## TODO: merge this into slab()

# generate labels for slabs
genSlabLabels <- function(slab.structure=1, max.d, n.profiles) {
  
  # fixed-size slabs
  if(length(slab.structure) == 1) {
    # generate sequence of segment labels
    seg.label <- rep(1:ceiling(max.d / slab.structure), each=slab.structure, length=max.d)
    # general segment labels
    seg.label.levels <- tapply(1:max.d, seg.label, function(i) {r <- range(i); paste(c(r[1]-1, r[2]), collapse='-') } )
  }
  
  # user-defined slabs
  if(length(slab.structure) > 1) {
    # trival case where segments start from 0
    if(slab.structure[1] == 0 & length(slab.structure) > 2)
      seg.label <- rep(slab.structure[-1], times=diff(slab.structure))[1:max.d]
    
    # other case: user defines an arbitrary lower and upper limit
    else {
      if(length(slab.structure) != 2)
        stop('user-defined slab boundaries must either start from 0, or contain two values between 0 and the max soil depth')
      
      # proceed
      slab.thickness <- diff(slab.structure)
      # how many slices of NA before the slab?
      padding.before <- rep(NA, times=slab.structure[1])
      # how many slices of NA afer the slab
      padding.after <- rep(NA, times=max.d - slab.structure[2])
      # make a new label for the slab
      new.label <- paste(slab.structure, collapse='-')
      # generate an index for the slab
      slab.idx <- rep(new.label, times=slab.thickness)
      # generate the entire index: padding+slab+padding = total number of slices (max_d)
      # seg.label <- c(padding.before, slab.idx, padding.after)
      seg.label <- slab.idx 
    }
    
    # generate segment labels	
    seg.label.levels <- sapply(1:(length(slab.structure)-1), function(i) paste(c(slab.structure[i], slab.structure[i+1]), collapse='-'))
  }
  
  # covert into a factor that can be used to split profiles into slabs
  res <- factor(rep(seg.label, times=n.profiles), labels=seg.label.levels)
  
  return(res)
}






#' Generate Labels for Slabs 
#' 
#' This method is used by [slab()] for generating labels that assign IDs to layers in a SoilProfileCollection
#' 
#' The new routine used in aqp 2.0 requires that, at a minimum, the `spc` and `slab.structure` arguments be specified.
#' 
#' @param slab.structure A user-defined slab thickness (defined by an integer), or user-defined structure (numeric vector). See details for `slab()`.
#' @param max.d Maximum depth
#' @param n.profiles Number of profiles
#' @param spc Optional: A SoilProfileCollection
#' @param diced Optional: The `dice()`-ed horizon-level data.frame corresponding to `spc`
#' @return factor. slab IDs, labels are the segment top and bottom depth separated by `"-"`
#' @param ... Additional arguments passed to `dice()` when `spc` is specified.
#' @seealso [slab()]
#' @export
genSlabLabels <- function(slab.structure = 1, max.d = NULL, n.profiles = NULL, spc = NULL, diced = NULL, ...) {

  # new method in aqp 2.x slab() requires SPC input
  if (!is.null(spc)) {
    if (is.null(diced)) {
      diced <- dice(spc, ..., SPC = FALSE)
    }
    
    # call method now used internally in slab()
    return(.genSlabLabels2(spc = spc, diced = diced, slab.structure = slab.structure))
  }
  
  if (is.null(max.d)) {
    stop("Must specify `max.d` when `spc` is NULL")
  }
  
  if (is.null(n.profiles)) {
    stop("Must specify `n.profiles` when `spc` is NULL")
  }
  
  # legacy method from aqp 1.x slab():
  
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
      
      # calculate thickness of slab
      slab.thickness <- diff(slab.structure)
      
      # make a new label for the slab
      new.label <- paste(slab.structure, collapse = '-')
      
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

# does not assume logical horizons (may overlap, resulting in different # of slices per profile)
.genSlabLabels2 <- function(spc, diced, slab.structure = 1) {
  
  # handle various allowed slab.structure specifications
  if (length(slab.structure) == 1) {
    i <- seq(from = 0, length.out = (max(spc) / slab.structure) + 1) * slab.structure
  } else if (length(slab.structure) > 1) {
    i <- slab.structure
  } else {
    stop("empty slab.structure", call. = FALSE)
  }
  
  # calculate a sequence of cumulative depths and corresponding indices for each slab
  j <- diff(i)
  if (length(j) == 0) {
    stop("Empty slab.structure", call. = FALSE)
  } else if(length(j) == 1 && j == 0) {
    stop("Invalid slab.structure", call. = FALSE)
  } else {
    idx1 <- cumsum(do.call('c', lapply(seq_along(j), function(x) rep(1, j[x]))))
    idx2 <- do.call('c', lapply(seq_along(j), function(x) rep(x, j[x])))
    mt <- data.frame(idx1, slab_id = idx2, slab_label = paste0(i[idx2], "-", i[idx2 + 1]))
  }
  hzdepb <- horizonDepths(spc)[2]
  colnames(mt) <- c(hzdepb, "slab_id", "slab_label")
  
  # merge the dice'd data.frame result with the slab ID and labels
  res <- merge(diced, mt, by = hzdepb, all.x = TRUE, sort = FALSE)
  
  # order by horizon ID and depth
  res <- res[order(res[[idname(spc)]], res[[hzdepb]]),]
  
  # return the slab IDs as a factor, with labels denoting each segment top and bottom
  factor(res$slab_id, labels = na.omit(unique(res$slab_label)))
}

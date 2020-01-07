# return a vector of generalized horizon names, based on:
# x: a vector of new names
# x: a vector of regex patterns
# non.matching.code: code for any non-matching hz designations
# hzdepm: vector of hz mid-points
# dots: additional arguments passed to grep (e.g. perl=TRUE)
generalize.hz <- function(x, new, pat, non.matching.code='not-used', hzdepm = NA, ...) {
	
	# init vector of 'other', same length as original horizon name vector
	g <- rep(non.matching.code, times=length(x))
	
	# iterate over new new names, match with patterns, assign new hz names
	for(i in seq_along(new)) {
		g[grep(pat[i], x, ...)] <- new[i]
	}
	
	# convert to factor with levels set by median depth
	# TODO: this will fail if a given genhz label is missing hz mid-points
	if(! all(is.na(hzdepm))) {
	  new_sort <- names(sort(tapply(hzdepm, g, median)))
	  new_sort <- new_sort[new_sort != non.matching.code]
	  g <- factor(g, levels = c(new_sort, non.matching.code))
	  
	  return(g)
	}
	
	# no adjustment of factor levels based on hz mid-points
	g <- factor(g, levels = c(new, non.matching.code))
	return(g)
	
}


# convert a cross-tabulation: {original, genhz} to adjacency matrix
genhzTableToAdjMat <- function(tab) {
  tab <- as.matrix(tab)
  # extract unique set of names
  nm <- sort(unique(unlist(dimnames(tab))))
  # generate full matrix with named dimensions
  m <- matrix(nrow=length(nm), ncol=length(nm), data=0)
  dimnames(m) <- list(nm, nm)
  # set adjacency information via sub-matrix containing the original cross-tab
  m[rownames(tab), colnames(tab)] <- tab
  return(m)
}

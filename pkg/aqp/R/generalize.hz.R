# return a vector of generalized horizon names, based on:
# a vector of new names
# a vector of patterns
generalize.hz <- function(x, new, pat) {
	# init vector of NA, same length as original horizon name vector
	g <- rep(NA, times=length(x))
	# iterate over new new names, match with patterns, assign new hz names
	for(i in seq_along(new)) {
		g[grep(pat[i], x)] <- new[i]
	}
	# convert to factor, and re-level
	g <- factor(g, levels=new)
	return(g)
}

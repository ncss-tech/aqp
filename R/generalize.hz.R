# return a vector of generalized horizon names, based on:
# x: a vector of new names
# x: a vector of regex patterns
# non.matching.code: code for any non-matching hz designations
# hzdepm: vector of hz mid-points
# dots: additional arguments passed to grep (e.g. perl=TRUE)

#' Generalize Horizon Names
#'
#' Generalize a vector of horizon names, based on new classes, and REGEX
#' patterns.
#'
#'
#' @param x a character vector of horizon names
#' @param new a character vector of new horizon classes
#' @param pat a character vector of REGEX, same length as \code{x}
#' @param non.matching.code text used to describe any horizon not matching any
#' item in \code{pat}
#' @param hzdepm a numeric vector of horizon mid-points, must not contain NA,
#' same length as \code{x}
#' @param \dots additional arguments passed to \code{grep()} such as
#' \code{perl=TRUE} for advanced REGEX
#' @return factor of the same length as \code{x}
#' @author D.E. Beaudette
#' @keywords manip
#' @examples
#'
#' \dontrun{
#' 
#' data(sp1)
#' 
#' # check original distribution of hz designations
#' table(sp1$name)
#' 
#' # generalize
#' sp1$genhz <- generalize.hz(sp1$name,
#'                            new=c('O','A','B','C','R'),
#'                            pat=c('O', '^A','^B','C','R'))
#' 
#' # see how we did / what we missed
#' table(sp1$genhz, sp1$name)
#' 
#' 
#' ## a more advanced example, requries perl=TRUE
#' # example data
#' x <- c('A', 'AC', 'Bt1', '^AC', 'C', 'BC', 'CB')
#' 
#' # new labels
#' n <- c('A', '^AC', 'C')
#' # patterns:
#' # "A anywhere in the name"
#' # "literal '^A' anywhere in the name"
#' # "C anywhere in name, but without preceding A"
#' p <- c('A', '^A', '(?<!A)C')
#' 
#' # note additional argument
#' res <- generalize.hz(x, new = n, pat=p, perl=TRUE)
#' 
#' # double-check: OK
#' table(res, x)
#' 
#' }
#'
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

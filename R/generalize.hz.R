# return a vector of generalized horizon names, based on:
# x: a vector of new names
# x: a vector of regex patterns
# non.matching.code: code for any non-matching hz designations
# hzdepm: vector of hz mid-points
# dots: additional arguments passed to grep (e.g. perl=TRUE)

#' Generalize Horizon Names
#'
#' Generalize a vector of horizon names, based on new classes, and REGEX
#' patterns. Or create a new column `gendesgnname` in a SoilProfileCollection (requires a horizon designation name to be defined for the collection, see details)
#'
#' @param x a character vector of horizon names or a `SoilProfileCollection`
#' @param new a character vector of new horizon classes
#' @param pattern a character vector of REGEX patterns, same length as `new`
#' @param non.matching.code label used for any horizon not matching any `pattern`
#' @param hzdepm a numeric vector of horizon mid-points; `NA` values in `hzdepm` will result in `non.matching.code` (or `NA` if not defined) in result
#' @param ordered by default, the result is an ordered factor when `hzdepm` is defined. 
#' @param ... additional arguments passed to `grep()` such as `perl=TRUE` for advanced REGEX
#' @return (ordered) factor of the same length as `x` (if character) or as number of horizons in `x` (if SoilProfileCollection)
#' @details When `x` is a SoilProfileCollection the `gendesgnname` column will be updated with the factor results. This requires that the "horizon designation name" metadata be defined for the collection to set the column for input designations. See `hzdesgnname()`.
#' @seealso `hzdesgnname()`
#' @author D.E. Beaudette
#' @keywords manip
#' @examples
#' 
#' data(sp1)
#' 
#' # check original distribution of hz designations
#' table(sp1$name)
#' 
#' # generalize
#' sp1$genhz <- generalize.hz(sp1$name,
#'                            new=c('O','A','B','C','R'),
#'                            pattern=c('O', '^A','^B','C','R'))
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
#' res <- generalize.hz(x, new = n, pattern=p, perl=TRUE)
#' 
#' # double-check: OK
#' table(res, x)
#' 
#'
generalize.hz <- function(x, new, pattern, non.matching.code = 'not-used', hzdepm = NULL, ordered = !missing(hzdepm), ...) {

	# init vector of 'other', same length as original horizon name vector
	g <- rep(non.matching.code, times = length(x))

	# iterate over new new names, match with patterns, assign new hz names
	for (i in seq_along(new)) {
		g[grep(pattern[i], x, ...)] <- new[i]
	}

	# convert to (ordered) factor with levels set by median depth
	if (!missing(hzdepm) && !is.null(hzdepm) && !all(is.na(hzdepm))) {
	  # more stringent NA handling:
	  # stopifnot(`Number of non-NA \`hzdepm\` does match number of new levels` =
	  #           { sum(!is.na(hzdepm)) == length(new) })
	  
	  # less stringent:
	  # any NA hzdepm will return NA factor level, even when pattern is matched
	  new_sort <- names(sort(tapply(hzdepm, g, median)))
	  new_sort <- new_sort[new_sort != non.matching.code]
	  
	  # use an ordered factor (may be overridden w/ ordered = FALSE)
	  g <- factor(g, levels = c(new_sort, non.matching.code), ordered = ordered)
	  
	  # if any are not matched (i.e. hzdepm is NA), replace with non.matching.code (if defined)
	  if (!is.null(non.matching.code)) {
	    g[is.na(g)] <- non.matching.code
	  }
	  g
	} else {
  	# no adjustment of factor levels based on hz mid-points
  	# no order implied (by default; override with ordered=TRUE)
  	factor(g, levels = c(new, non.matching.code), ordered = ordered)
	}
}

setGeneric("generalizeHz",  function(x, new, pattern, non.matching.code, hzdepm, ordered, ...) standardGeneric("generalizeHz"))

#' @export
#' @rdname generalize.hz
setMethod("generalizeHz", signature(x = "character"), function(x, new, pattern, non.matching.code = 'not-used', hzdepm = NULL, ordered = !missing(hzdepm), ...) {
 generalize.hz(
    x,
    new = new,
    pattern = pattern,
    non.matching.code = 'not-used',
    hzdepm = hzdepm,
    ordered = ordered, 
    ...
  )
})

# SoilProfileColletion method
#' @param gendesgnname Generalized Horizon Designation column name (to be created/updated when `x` is a `SoilProfileCollection`)
#' @export
#' @rdname generalize.hz
setMethod("generalizeHz", signature(x = "SoilProfileCollection"), function(x, new, pattern, non.matching.code = 'not-used', hzdepm = NULL, ordered = !missing(hzdepm), gendesgnname = "genhz", ...) {
  hzdesgn <- hzdesgnname(x, required = TRUE)
  
  x[[gendesgnname]] <- generalize.hz(
    x[[hzdesgn]],
    new = new,
    pattern = pattern,
    non.matching.code = 'not-used',
    hzdepm = hzdepm,
    ordered = ordered, 
    ...
  )
  x
})

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

# default slab function for categorical variables
# returns a named vector of results
# this type of function is compatible with aggregate()
.slab.fun.factor.default <- function(values, cpm, ...) {

  if (cpm == 1) {
	  # probabilities are relative to number of contributing profiles
    tb <- table(values, useNA = 'no')
		# convert to proportions
		pt <- prop.table(tb)
	} else if (cpm == 2) {
	  # probabilities are relative to total number of profiles
	  tb <- table(values, useNA = 'always')
		# convert to proportions,
		# the last column will be named 'NA', and contains the tally of NAs --> remove it
		pt <- prop.table(tb)
		pt <- pt[-length(pt)]
	}

  ## NOTE: this will corrupt hz designations associated with lithologic discontinuities
  ## ---> 2Bt will become X2Bt
	# assign safe names to the vector of probabilities
	names(pt) <- make.names(levels(values))

	return(pt)
}

.slab.fun.factor.weighted <- function(values, w, na.rm = TRUE, ...) {
  # TODO
}
  
# default slab function for continuous variables
# returns a named vector of results
# this type of function is compatible with aggregate()
.slab.fun.numeric.default <- function(values, probs = c(0.05, 0.25, 0.5, 0.75, 0.95), na.rm = TRUE, ...) {
  .slab.fun.numeric.fast(values, probs = probs, na.rm = na.rm, ...)
}

# for a site or horizon level weight column, use Hmisc::wtd.quantile
#   note that "frequency weights" are assumed to be most common use case, so normwt=FALSE by default, 
#   normwt=TRUE can be passed as optional argument for slab.fun from slab() interface
.slab.fun.numeric.weighted <- function(values, w, probs = c(0.05, 0.25, 0.5, 0.75, 0.95), normwt = FALSE, na.rm = TRUE, ...) {
  if (!requireNamespace('Hmisc', quietly = TRUE))
    stop('please install the `Hmisc` package to use `wtd.quantile()` method', call. = FALSE)
  res <- try(Hmisc::wtd.quantile(values, w, probs = probs, na.rm = na.rm, normwt = normwt), silent = TRUE)
  if (inherits(res, 'try-error') && grepl("zero non-NA points", res[1])) {
    res <- rep(NA_real_, length(probs))
  } else {
    names(res) <- paste('p.q', round(probs * 100), sep = '')
  }
  return(res)
}

# easy specification of Hmisc::hdquantile if available
.slab.fun.numeric.HD <- function(values, probs = c(0.05, 0.25, 0.5, 0.75, 0.95), na.rm = TRUE, ...) {
  # sanity check, need this for color distance eval
  if (!requireNamespace('Hmisc', quietly = TRUE))
    stop('please install the `Hmisc` package to use `hdquantile()` method', call. = FALSE)
  
  res <- Hmisc::hdquantile(values, probs = probs, na.rm = na.rm)
  
  names(res) <- paste('p.q', round(probs * 100), sep = '')
  return(res)
}

# basic quantile evaluation, better for large data sets
.slab.fun.numeric.fast <- function(values, probs = c(0.05, 0.25, 0.5, 0.75, 0.95), na.rm = TRUE, ...) {
  res <- quantile(values, probs = probs, na.rm = na.rm)
  names(res) <- paste('p.q', round(probs * 100), sep = '')
  return(res)
}

# SoilProfileCollection method
# object: SoilProfileCollection
# fm: formula defining aggregation
# slab.structure: either regular segment interval, or user-defined segment boundaries {starting from 0, or length of 2}
# progress: plyr-progress display
# slab.fun: aggregate function applied to data chunks (must return a single row / chunk)
# cpm: class probability normalization mode
# weights: character vector naming column in site slot containing weights
# ... : extra arguments passed on to slab.fun
.slab <- function(object,
                  fm,
                  slab.structure = 1,
                  strict = FALSE,
                  byhz = TRUE,
                  slab.fun = slab_function(method = "numeric"),
                  cpm = 1,
                  weights = NULL,
                  ...) {

  # define keywords for data.table
  .SD <- NULL; .N <- NULL; value <- NULL

	# get unique list of names in object
	object.names <- unique(unlist(names(object)))

	# get horizon depth column names
	hzd <- horizonDepths(object)
	
	# get name of ID column in original object for later
	object.ID <- idname(object)

  # if we are aggregating a single categorical variable, we need to keep track of the original levels
	original.levels <- NULL

	# extract components of the formula:
	g <- all.vars(update(fm, . ~ 0)) # left-hand side
	vars <- all.vars(update(fm, 0 ~ .)) # right-hand side
	
	# sanity check:
	if (!inherits(fm, "formula"))
	  stop('must provide a valid formula: groups ~ var1 + var2 + ...', call. = FALSE)
	
	# check for bogus left/right side problems with the formula
	if (length(g) > 0 && !any(g %in% object.names) && g != '.') # bogus grouping column
	  stop('group name either missing from formula, or does match any columns in data.frame', call. = FALSE)

	if (any(vars %in% object.names) == FALSE) # bogus column names in right-hand side
	  stop('column names in formula do not match column names in data.frame', call. = FALSE)
	
	# make formula for slicing
	if (length(slab.structure) == 2) {
  	# user-defined single slab
	  fm.slice <- formula(paste(slab.structure[1], ':', slab.structure[2], ' ~ ', paste(vars, collapse = ' + '), sep = ''))
	} else {
	  fm.slice <- formula(paste('0:', max(object), ' ~ ', paste(vars, collapse = ' + '), sep = ''))
	}
	
	# slice into 1cm increments, result is a data.frame
	data <- dice(x = object, fm = fm.slice, strict = strict, byhz = byhz, SPC = FALSE, pctMissing = TRUE)
  
	# Note: in this case we need to subtract the extra slice included by slice()/dice()
  # do it to sliced result so that the genSlabLabels have the correct length
  ldx <- data[[horizonDepths(object)[2]]] > slab.structure[2]
	if (length(slab.structure) == 2 && any(ldx, na.rm = TRUE)) {
	  data <- data[which(!ldx), ]  
	}
   
	# extract site data
	site.data <- site(object)

	# check groups for NA, if grouping variable was given
	if (g != '.' && any(is.na(site.data[, g]))) {
		stop('grouping variable must not contain NA', call. = FALSE)
	}
	
	# merge site data back into the result, this would include site-level weights
	data <- merge(x = data, y = site.data, by = object.ID, all.x = TRUE, sort = FALSE)

	# check variable classes
	if (length(vars) > 1) {
	  vars.numeric.test <- sapply(data[, vars], is.numeric)
	}	else {
	  vars.numeric.test <- is.numeric(data[[vars]])
	}

	# # sanity check: all numeric, or single character/factor
	if (any(!vars.numeric.test) & length(vars) > 1) {
	  stop('mixed variable types and multiple categorical variables are not currently supported in the same call to slab', call. = FALSE)
	}

	# check for single categorical variable, and convert to factor
	if (length(vars) == 1 & inherits(data[, vars], c('character', 'factor'))) {

		# if we have a character, then convert to factor
		if (inherits(data[[vars]],'character')) {
			message('Note: converting categorical variable to factor.')
			data[[vars]] <- factor(data[[vars]])
		}

		# check for weights
		if (!missing(weights)) {
		  slab.fun <- .slab.fun.factor.weighted
		} else {
  		# re-set default function, currently no user-supply-able option
  		slab.fun <- .slab.fun.factor.default
		}

    # save factor levels for later
    original.levels <- levels(data[[vars]])

    # set a flag for post data.table.melt -> factor level setting
    .factorFlag <- TRUE
	} else {
	  .factorFlag <- FALSE
	}

	# add segmenting label to data
 	data$seg.label <- .genSlabLabels2(object, data, slab.structure = slab.structure)

	# if there is no left-hand component in the formula, we are aggregating all data in the collection
	if (g == '.') {
		g <- 'all.profiles' # add new grouping variable to horizons
		data[, g] <- 1
	}
 	
 	if (length(weights) > 1) {
 	  stop("`weights` argument should be a character vector of length one containing (site-level) weight column name", call. = FALSE)
 	}
 	
 	if (!is.null(weights) && !weights %in% siteNames(object)) {
 	  stop("column '", weights, "' not present in site data", call. = FALSE)
 	}
 	
 	# convert wide -> long format; warnings will occur when not all columns are e.g. double
 	d.long <- suppressWarnings(data.table::melt(
 	  data.table::as.data.table(data[which(!is.na(data$seg.label)), ]),
 	  id.vars = unique(c(object.ID, 'seg.label', g, weights)),
 	  measure.vars = vars
 	))
 	
	# check for weights
	if (!missing(weights)) {
	  if (missing(slab.fun)) {
	    # default _weighted_ aggregation fun is .slab.fun.numeric.weighted
	    FUN <- .slab.fun.numeric.weighted
	    # custom weighted aggregation fun should take first argument as values 
	    # second argument is "weights" argument
	  } else {
	    FUN <- slab.fun
	  }
	} else {
	  if (missing(slab.fun) || !inherits(slab.fun, 'function')) {
	    # default numeric aggregation fun is .slab.fun.numeric.default
	    FUN <- .slab.fun.numeric.default
	  } else {
	    FUN <- slab.fun
	  }
	}
 	
 	.internal_wt <- eval(weights)
 	
 	# calculate summary function FUN for each variable*groups*slablabel
 	#  FUN returns a (named) vector of summary statistics, which are converted to "wide" data.frame
 	
 	if (.factorFlag) {
 	  d.long[['value']] <- factor(d.long[['value']], levels = original.levels)
 	}
 	
 	if (!missing(weights)) {
   	d.slabbed <- as.data.frame(d.long[, as.data.frame(t(FUN(value, .SD[[.internal_wt]], ...))),
   	                                  by = c('variable', g, 'seg.label')])
 	} else {
 	  if (!missing(cpm)) {
 	    d.slabbed <- as.data.frame(d.long[, as.data.frame(t(unclass(FUN(value, cpm = cpm, ...)))),
 	                                      by = c('variable', g, 'seg.label')])
 	  } else {
   	  d.slabbed <- as.data.frame(d.long[, as.data.frame(t(FUN(value, ...))),
   	                                    by = c('variable', g, 'seg.label')])
 	  }
 	}
 	
 	# calculate contributing fraction for each variable*groups*slablabel
 	d.slabbed$contributing_fraction <- d.long[, sum(!is.na(.SD[["value"]])) / .N,
 	                                          by = c('variable', g, 'seg.label')]$V1
 	
	# ensure that the names returned from slab.fun are legal
	names(d.slabbed) <- make.names(names(d.slabbed)) 
	  
 	# rename default data.table value column names
 	unn <- grepl("^V\\d+$", colnames(d.slabbed))
 	if (any(unn)) {
 	  colnames(d.slabbed)[unn] <- paste0("value", ifelse(sum(unn) == 1, "", 1:sum(unn)))
 	}
 	
 	# convert the slab.label column into top/bottom as integers
 	slab.depths <- strsplit(as.character(d.slabbed$seg.label), '-')
 	d.slabbed$top <- as.integer(lapply(slab.depths, function(i) i[1]))
 	d.slabbed$bottom <- as.integer(lapply(slab.depths, function(i) i[2]))
 	
	# remove seg.label from result
	d.slabbed$seg.label <- NULL

  # if categorical data have been aggregated, set an attribute with the original levels
  # this allows one to recover values that are not legal column names
  # e.g. 2Bt is corrupted to X2Bt
  if (!is.null(original.levels)) {
    attr(d.slabbed, 'original.levels') <- original.levels
  }
	
	d.slabbed
}

# setup generic function
setGeneric("slab", function(object,
                              fm,
                              slab.structure = 1,
                              strict = FALSE,
                              byhz = TRUE,
                              slab.fun = slab_function(method = "numeric"),
                              cpm = 1,
                              weights = NULL,
                              ...)
    standardGeneric("slab"))

#' Slab-Wise Aggregation of SoilProfileCollection Objects
#'
#' Aggregate soil properties along user-defined `slabs`, and optionally within
#' groups.
#'
#' Multiple continuous variables OR a single categorical (factor) variable can
#' be aggregated within a call to \code{slab}. Basic error checking is
#' performed to make sure that top and bottom horizon boundaries make sense.
#' User-defined aggregate functions (\code{slab.fun}) should return a named
#' vector of results. A new, named column will appear in the results of
#' \code{slab} for every named element of a vector returned by \code{slab.fun}.
#' See examples below for a simple example of a slab function that computes
#' mean, mean-1SD and mean+1SD. The default slab function wraps
#' \code{stats::quantile} from the Hmisc package, which requires at least 2
#' observations per chunk. Note that if `group` is a factor it must not contain
#' NAs.
#' 
#' `slab()` uses `dice()` to "resample" profiles to 1cm slices from depth 0 to `max(x)` (or `slab.structure[2]`, if defined).
#'
#' Sometimes \code{slab} is used to conveniently re-arrange data vs. aggregate.
#' This is performed by specifying \code{identity} in \code{slab.fun}. See
#' examples beflow for a demonstration of this functionality.
#'
#' The default \code{slab.fun} was changed 2019-10-30 from a wrapper around
#' \code{Hmisc::hdquantile} to a wrapper around \code{stats::quantile}. See
#' examples below for a simple way to switch to the HD quantile estimator.
#'
#' Execution time scales linearly (slower) with the total number of profiles in
#' \code{object}, and exponentially (faster) as the number of profiles / group
#' is increased. \code{slab} and \code{slice} are much faster and require less
#' memory if input data are either numeric or character.
#'
#' There are several possible ways to define slabs, using
#' \code{slab.structure}:
#'
#' \describe{ \item{a single integer}{e.g. 10: data are aggregated over a
#' regular sequence of 10-unit thickness slabs} \item{a vector of 2
#' integers}{e.g. c(50, 60): data are aggregated over depths spanning 50--60
#' units} \item{a vector of 3 or more integers}{e.g. c(0, 5, 10, 50, 100): data
#' are aggregated over the depths spanning 0--5, 5--10, 10--50, 50--100 units}
#' }
#'
#' @name slab-methods
#' @aliases slab slab2 genSlabLabels slab,SoilProfileCollection-method
#' @docType methods
#' @param object a SoilProfileCollection
#' @param fm A formula: either `groups ~ var1 + var2 + var3` where named
#' variables are aggregated within `groups' OR where named variables are
#' aggregated across the entire collection ` ~ var1 + var2 + var3`. If `groups`
#' is a factor it must not contain `NA`
#' @param slab.structure A user-defined slab thickness (defined by an integer),
#' or user-defined structure (numeric vector). See details below.
#' @param strict logical: should horizons be strictly checked for
#' self-consistency?
#' @param byhz logical: should horizons or whole profiles be removed by logic checks in `strict`? Default `TRUE` removes only offending horizons, `FALSE` removes whole profiles with one or more illogical horizons.
#' @param slab.fun Function used to process each 'slab' of data, ideally
#' returning a vector with names attribute. Defaults to a wrapper function
#' around `stats::quantile()`. See details.
#' @param cpm Strategy for normalizing slice-wise probabilities, dividing by
#' either: number of profiles with data at the current slice (`cpm=1`), or by the
#' number of profiles in the collection (cpm=2). Mode 1 values will always sum
#' to the contributing fraction, while mode 2 values will always sum to 1.
#' @param weights Column name containing site-level weights
#' @param \dots further arguments passed to `slab.fun`
#' @return Output is returned in long format, such that slice-wise aggregates
#' are returned once for each combination of grouping level (optional),
#' variable described in the `fm` argument, and depth-wise 'slab'.
#'
#' Aggregation of numeric variables, using the default slab function:
#' \describe{ \item{variable}{The names of variables included in the call to
#' \code{slab}.} \item{groupname}{The name of the grouping variable when
#' provided, otherwise a fake grouping variable named 'all.profiles'.}
#' \item{p.q5}{The slice-wise 5th percentile.} \item{p.q25}{The slice-wise 25th
#' percentile} \item{p.q50}{The slice-wise 50th percentile (median)}
#' \item{p.q75}{The slice-wise 75th percentile} \item{p.q95}{The slice-wise
#' 95th percentile} \item{top}{The slab top boundary.} \item{bottom}{The slab
#' bottom boundary.} \item{contributing_fraction}{The fraction of profiles
#' contributing to the aggregate value, ranges from 1/n_profiles to 1.} }
#'
#' When a single factor variable is used, slice-wise probabilities for each
#' level of that factor are returned as: \describe{ \item{variable}{The names
#' of variables included in the call to \code{slab}.} \item{groupname}{The name
#' of the grouping variable when provided, otherwise a fake grouping variable
#' named 'all.profiles'.} \item{A}{The slice-wise probability of level A}
#' \item{B}{The slice-wise probability of level B} \item{list()}{} \item{n}{The
#' slice-wise probability of level n} \item{top}{The slab top boundary.}
#' \item{bottom}{The slab bottom boundary.} \item{contributing_fraction}{The
#' fraction of profiles contributing to the aggregate value, ranges from
#' 1/n_profiles to 1.}
#'
#' }
#' @note Arguments to \code{slab} have changed with \code{aqp} 1.5 (2012-12-29)
#' as part of a code clean-up and optimization. Calculation of
#' weighted-summaries was broken in \code{aqp} 1.2-6 (2012-06-26), and removed
#' as of \code{aqp} 1.5 (2012-12-29).  \code{slab} replaced the previously
#' defined \code{soil.slot.multiple} function as of \code{aqp} 0.98-8.58
#' (2011-12-21).
#' @section Methods: \describe{ \item{data = "SoilProfileCollection"}{Typical
#' usage, where input is a \code{\link{SoilProfileCollection}}.} }
#' @author D.E. Beaudette
#' @seealso \code{\link{slice}, \link{quantile}}
#' @references D.E. Beaudette, P. Roudier, A.T. O'Geen, Algorithms for
#' quantitative pedology: A toolkit for soil scientists, Computers &
#' Geosciences, Volume 52, March 2013, Pages 258-268,
#' 10.1016/j.cageo.2012.10.020.
#'
#' Harrell FE, Davis CE (1982): A new distribution-free quantile estimator.
#' Biometrika 69:635-640.
#' @keywords methods manip
#' @export
#' @examples
#'
#' ##
#' ## basic examples
#' ##
#' library(lattice)
#' library(grid)
#' library(data.table)
#'
#' # load sample data, upgrade to SoilProfileCollection
#' data(sp1)
#' depths(sp1) <- id ~ top + bottom
#'
#' hzdesgnname(sp1) <- "name"
#'
#' # aggregate entire collection with two different segment sizes
#' a <- slab(sp1, fm = ~ prop)
#' b <- slab(sp1, fm = ~ prop, slab.structure=5)
#'
#' # check output
#' str(a)
#'
#' # stack into long format
#' ab <- make.groups(a, b)
#' ab$which <- factor(ab$which, levels=c('a','b'),
#' labels=c('1-cm Interval', '5-cm Interval'))
#'
#' # plot median and IQR
#' # custom plotting function for uncertainty viz.
#' xyplot(top ~ p.q50 | which, data=ab, ylab='Depth',
#' 			 xlab='median bounded by 25th and 75th percentiles',
#' 			 lower=ab$p.q25, upper=ab$p.q75, ylim=c(250,-5),
#' 			 panel=panel.depth_function,
#' 			 prepanel=prepanel.depth_function,
#' 			 cf=ab$contributing_fraction,
#' 			 alpha=0.5,
#' 			 layout=c(2,1), scales=list(x=list(alternating=1))
#' 			 )
#'
#'
#' ###
#' ### re-arrange data / no aggregation
#' ###
#'
#' # load sample data, upgrade to SoilProfileCollection
#' data(sp1)
#' depths(sp1) <- id ~ top + bottom
#'
#' # arrange data by ID
#' a <- slab(sp1, fm = id ~ prop, slab.fun=identity)
#'
#' # convert id to a factor for plotting
#' a$id <- factor(a$id)
#'
#' # check output
#' str(a)
#'
#' # plot via step function
#' xyplot(top ~ value | id, data=a, ylab='Depth',
#'        ylim=c(250, -5), as.table=TRUE,
#'        panel=panel.depth_function,
#'        prepanel=prepanel.depth_function,
#'        scales=list(x=list(alternating=1))
#' )
#'
#' ##
#' ## categorical variable example
#' ##
#'
#' data(sp1)
#' depths(sp1) <- id ~ top + bottom
#'
#' # normalize horizon names: result is a factor
#' sp1$name <- generalize.hz(
#'   sp1$name,
#'   new = c('O','A','B','C'),
#'   pat = c('O', '^A','^B','C')
#'   )
#'
#' # compute slice-wise probability so that it sums to contributing fraction, from 0-150
#' a <- slab(sp1, fm= ~ name, cpm=1, slab.structure=0:150)
#'
#' # convert wide -> long for plotting
#' # result is a data.table
#' # genhz factor levels are set by order in `measure.vars`
#' a.long <- data.table::melt(
#'   data.table::as.data.table(a),
#'   id.vars = c('top','bottom'),
#'   measure.vars = c('O', 'A', 'B', 'C'),
#'   )
#'
#'
#' # plot horizon type proportions using panels
#' xyplot(top ~ value | variable,
#'        data = a.long, subset=value > 0,
#'        col = 1, lwd = 2,
#'        xlab = 'Class Probability',
#'        ylab = 'Depth (cm)',
#'        strip = strip.custom(bg = grey(0.85)),
#'        scales = list(x = list(alternating = FALSE)),
#'        ylim = c(150, -5), type=c('S','g'),
#'        horizontal = TRUE, layout = c(4,1)
#'        )
#'
#' # again, this time using groups
#' xyplot(top ~ value,
#'        data = a.long,
#'        groups = variable,
#'        subset = value > 0,
#'        ylim = c(150, -5),
#'        type = c('S','g'),
#'        horizontal = TRUE,
#'        asp = 2,
#'        lwd = 2,
#'        auto.key = list(
#'          lines = TRUE,
#'          points = FALSE,
#'          cex = 0.8,
#'          columns = 1,
#'          space = 'right'
#'        )
#' )
#'
#' # adjust probability to size of collection, from 0-150
#' a.1 <- slab(sp1, fm= ~ name, cpm = 2, slab.structure = 0:150)
#'
#' # convert wide -> long for plotting
#' # result is a data.table
#' # genhz factor levels are set by order in `measure.vars`
#' a.1.long <- data.table::melt(
#'   data.table::as.data.table(a.1),
#'   id.vars = c('top','bottom'),
#'   measure.vars = c('O','A','B','C')
#' )
#'
#' # combine aggregation from `cpm` modes 1 and 2
#' g <- make.groups(cmp.mode.1 = a.long, cmp.mode.2 = a.1.long)
#'
#' # plot horizon type proportions
#' xyplot(top ~ value | variable,
#'        groups = which,
#'        data = g, subset = value > 0,
#'        ylim = c(240, -5),
#'        type = c('S','g'),
#'        horizontal = TRUE,
#'        layout = c(4,1),
#'        auto.key = list(lines = TRUE, points = FALSE, columns = 2),
#'        par.settings = list(superpose.line = list(col = c(1, 2), lwd = 2)),
#'        scales = list(alternating = 3),
#'        xlab = 'Class Probability',
#'        ylab = 'Depth (cm)',
#'        strip = strip.custom(bg = grey(0.85))
#' )
#'
#'
#' # apply slice-wise evaluation of max probability, and assign ML-horizon at each slice
#' gen.hz.ml <- get.ml.hz(a, "variable", c('O','A','B','C'))
#'
#'
#' \dontrun{
#' ##
#' ## HD quantile estimator
#' ##
#'
#' library(soilDB)
#' library(lattice)
#' library(data.table)
#'
#' # sample data
#' data('loafercreek', package = 'soilDB')
#'
#' # defaul slab.fun wraps stats::quantile()
#' a <- slab(loafercreek, fm = ~ total_frags_pct + clay)
#'
#' # use HD quantile estimator from Hmisc package instead
#' a.HD <- slab(loafercreek, fm = ~ total_frags_pct + clay, slab.fun = aqp:::.slab.fun.numeric.HD)
#'
#' # combine
#' g <- make.groups(standard=a, HD=a.HD)
#'
#' # note differences
#' densityplot(~ p.q50 | variable, data=g, groups=which,
#'             scales=list(relation='free', alternating=3, tick.number=10, y=list(rot=0)),
#'             xlab='50th Percentile', pch=NA, main='Loafercreek',
#'             auto.key=list(columns=2, points=FALSE, lines=TRUE),
#'             par.settings=list(superpose.line=list(lwd=2, col=c('RoyalBlue', 'Orange2')))
#' )
#'
#' # differences are slight but important
#' xyplot(
#'   top ~ p.q50 | variable, data=g, groups=which,
#'   xlab='Value', ylab='Depth (cm)',
#'   asp=1.5, main='Loafercreek',
#'   lower=g$p.q25, upper=g$p.q75,
#'   sync.colors=TRUE, alpha=0.25, cf=g$contributing_fraction,
#'   ylim=c(115,-5), layout=c(2,1), scales=list(x=list(relation='free')),
#'   par.settings=list(superpose.line=list(lwd=2, col=c('RoyalBlue', 'Orange2'))),
#'   strip=strip.custom(bg=grey(0.85)),
#'   panel=panel.depth_function,
#'   prepanel=prepanel.depth_function,
#'   auto.key=list(columns=2, lines=TRUE, points=FALSE)
#' )
#'
#' ##
#' ## multivariate examples
#' ##
#' data(sp3)
#'
#' # add new grouping factor
#' sp3$group <- 'group 1'
#' sp3$group[as.numeric(sp3$id) > 5] <- 'group 2'
#' sp3$group <- factor(sp3$group)
#'
#' # upgrade to SPC
#' depths(sp3) <- id ~ top + bottom
#' site(sp3) <- ~ group
#'
#' # custom 'slab' function, returning mean +/- 1SD
#' mean.and.sd <- function(values) {
#'   m <- mean(values, na.rm=TRUE)
#'   s <- sd(values, na.rm=TRUE)
#'   upper <- m + s
#'   lower <- m - s
#'   res <- c(mean=m, lower=lower, upper=upper)
#'   return(res)
#' }
#'
#' # aggregate several variables at once, within 'group'
#' a <- slab(sp3, fm = group ~ L + A + B, slab.fun = mean.and.sd)
#'
#' # check the results:
#' # note that 'group' is the column containing group labels
#' xyplot(
#'   top ~ mean | variable, data=a, groups=group,
#'   lower=a$lower, upper=a$upper,
#'   sync.colors=TRUE, alpha=0.5,
#'   cf = a$contributing_fraction,
#'   xlab = 'Mean Bounded by +/- 1SD',
#'   ylab = 'Depth (cm)',
#'   ylim=c(125,-5), layout=c(3,1),
#'   scales=list(x=list(relation='free')),
#'   par.settings = list(superpose.line=list(lwd=2, col=c('RoyalBlue', 'Orange2'))),
#'   panel = panel.depth_function,
#'   prepanel = prepanel.depth_function,
#'   strip = strip.custom(bg=grey(0.85)),
#'   auto.key = list(columns=2, lines=TRUE, points=FALSE)
#' )
#'
#'
#' # compare a single profile to the group-level aggregate values
#' a.1 <- slab(sp3[1, ], fm = group ~ L + A + B, slab.fun = mean.and.sd)
#'
#' # manually update the group column
#' a.1$group <- 'profile 1'
#'
#' # combine into a single data.frame:
#' g <- rbind(a, a.1)
#'
#' # plot with customized line styles
#' xyplot(
#'   top ~ mean | variable, data=g, groups=group, subscripts=TRUE,
#'   lower=a$lower, upper=a$upper, ylim=c(125,-5),
#'   layout=c(3,1), scales=list(x=list(relation='free')),
#'   xlab = 'Mean Bounded by +/- 1SD',
#'   ylab = 'Depth (cm)',
#'   panel=panel.depth_function,
#'   prepanel=prepanel.depth_function,
#'   sync.colors = TRUE, alpha = 0.25,
#'   par.settings = list(
#'     superpose.line = list(
#'       col = c('orange', 'royalblue', 'black'),
#'       lwd = 2, lty = c(1,1,2)
#'     )
#'   ),
#'   strip = strip.custom(bg=grey(0.85)),
#'   auto.key = list(columns=3, lines=TRUE, points=FALSE)
#' )
#'
#'
#'
#'
#' ## again, this time for a user-defined slab from 40-60 cm
#' a <- slab(sp3,
#'           fm = group ~ L + A + B,
#'           slab.structure = c(40,60),
#'           slab.fun = mean.and.sd
#' )
#'
#' # now we have weighted average properties (within the defined slab)
#' # for each variable, and each group
#' # convert long -> wide
#' data.table::dcast(
#'   data.table::as.data.table(a),
#'   formula = group + top + bottom ~ variable,
#'   value.var = 'mean'
#' )
#'
#' ## this time, compute the weighted mean of selected properties, by profile ID
#' a <- slab(sp3,
#'           fm = id ~ L + A + B,
#'           slab.structure = c(40,60),
#'           slab.fun = mean.and.sd
#' )
#'
#' # convert long -> wide
#' data.table::dcast(
#'   data.table::as.data.table(a),
#'   formula = id + top + bottom ~ variable,
#'   value.var = 'mean'
#' )
#'
#'
#' ## aggregate the entire collection, using default slab function (hdquantile)
#' ## note the missing left-hand side of the formula
#' a <- slab(sp3, fm= ~ L + A + B)
#'
#'
#'
# # TODO: update this when weights are implemented
#' ## weighted-aggregation -- NOT YET IMPLEMENTED --
#' # load sample data, upgrade to SoilProfileCollection
#' data(sp1)
#' depths(sp1) <- id ~ top + bottom
#'
#' # generate pretend weights as site-level attribute
#' set.seed(10101)
#' sp1$site.wts <- runif(n=length(sp1), min=20, max=100)
#' }
#'
setMethod(f = 'slab',
          signature = 'SoilProfileCollection',
          definition = .slab)

#' @param method one of `"numeric"`, `"factor"`, `"hd"`, `"weighted.numeric"`, `"weighted.factor"`, `"fast"`
#' @details `slab_function()`: The default `"numeric"` aggregation method is the `"fast"` numeric (quantile) method. Additional methods include `"factor"` for categorical data, `"hd"` to use the Harrell-Davis Distribution-Free Quantile Estimator from the Hmisc package, and "`weighted`" to use a weighted quantile method from the Hmisc package
#' @return `slab_function()`: return an aggregation function based on the `method` argument
#' @rdname slab-methods
#' @export
slab_function <- function(method = c("numeric", "factor", "hd", "weighted.numeric", "weighted.factor", "fast")) {
  switch(method,
         numeric = .slab.fun.numeric.default, # uses "fast" numeric method
         factor = .slab.fun.factor.default, 
         weighted.factor = .slab.fun.factor.weighted, 
         hd = .slab.fun.numeric.HD, 
         weighted.numeric = .slab.fun.numeric.weighted,
         weighted.factor = .slab.fun.numeric.weighted,
         fast = .slab.fun.numeric.fast,
         .slab.fun.numeric.default
  )
}

# for debugging;
.slabinternal <- function(...) {
  .slab(...)
}

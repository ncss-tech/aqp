

# default slab function for categorical variables
# returns a named vector of results
# this type of function is compatible with aggregate()
.slab.fun.factor.default <- function(values, cpm) {

	# probabilities are relative to number of contributing profiles
	if(cpm == 1) {
		tb <- table(values, useNA='no')
		# convert to proportions
		pt <- prop.table(tb)
	}

	# probabilities are relative to total number of profiles
	else if(cpm == 2) {
		tb <- table(values, useNA='always')
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


## TODO: make these exported functions with documentation (https://github.com/ncss-tech/aqp/issues/99)


# default slab function for continuous variables
# returns a named vector of results
# this type of function is compatible with aggregate()
.slab.fun.numeric.default <- function(values) {
	q.probs <- c(0.05, 0.25, 0.5, 0.75, 0.95)
	## 2019-10-30: dropping Hmisc as a suggested package
	# res <- Hmisc::hdquantile(values, probs=q.probs, na.rm=TRUE)
	res <- quantile(values, probs=q.probs, na.rm=TRUE)
	names(res) <- paste('p.q', round(q.probs * 100), sep='')
	return(res)
}

# easy specification of Hmisc::hdquantile if available
.slab.fun.numeric.HD <- function(values) {
  # sanity check, need this for color distance eval
  if(!requireNamespace('Hmisc', quietly = TRUE))
    stop('pleast install the `Hmisc` package.', call.=FALSE)

  q.probs <- c(0.05, 0.25, 0.5, 0.75, 0.95)

  res <- Hmisc::hdquantile(values, probs=q.probs, na.rm=TRUE)

  names(res) <- paste('p.q', round(q.probs * 100), sep='')
  return(res)
}

# basic quantile evaluation, better for large datasets
.slab.fun.numeric.fast <- function(values) {
	q.probs <- c(0.05, 0.25, 0.5, 0.75, 0.95)
	res <- quantile(values, probs=q.probs, na.rm=TRUE)
	names(res) <- paste('p.q', round(q.probs * 100), sep='')
	return(res)
}


# ## TODO: not yet implemented
# # default slab function for weighted, continuous variables
# # returns a named vector of results
# # this type of function is compatible with aggregate()
# # NOTE: nw (normalize-weights) argument will affect the results!
# .slab.fun.wtd.numeric.default <- function(values, w, nw=TRUE) {
# 	q.probs <- c(0.05, 0.25, 0.5, 0.75, 0.95)
# 	res <- wtd.quantile(values, weights=w, probs=q.probs, na.rm=TRUE, normwt=nw)
# 	names(res) <- paste('p.q', round(q.probs * 100), sep='')
# 	return(res)
# }


## note: slab() uses slice() to resample to 1cm slices, to max(x) or slab.structure[2] if defined

# SoilProfileCollection method
# object: SoilProfileCollection
# fm: formula defining aggregation
# slab.structure: either regular segment interval, or user-defined segment boundaries {starting from 0, or length of 2}
# progress: plyr-progress display
# slab.fun: aggregate function applied to data chunks (must return a single row / chunk)
# cpm: class probability normalization mode
# weights: character vector naming column containing weights
# ... : extra arguments passed on to slab.fun

# this is about 40% slower than the old method
.slab <- function(object, fm, slab.structure=1, strict=FALSE, slab.fun=.slab.fun.numeric.default, cpm=1, weights=NULL, ...){
	# issue a message for now that things have changed
	#	message('usage of slab() has changed considerably, please see the manual page for details')

	# get extra arguments: length of 0 if no extra arguments
	extra.args <- list(...)

	## important: change the default behavior of data.frame and melt
	opt.original <- options(stringsAsFactors = FALSE)

	# get unique list of names in object
	object.names <- unique(unlist(names(object)))

	# get number of profiles
	n.profiles <- length(object)

	# max depth
	max.d <- max(object)

	# get name of ID column in original object for later
	object.ID <- idname(object)

  # if we are aggregating a single categorical variable, we need to keep track of the original levels
	original.levels <- NULL

	# extract components of the formula:
	g <- all.vars(update(fm, .~0)) # left-hand side
	vars <- all.vars(update(fm, 0~.)) # right-hand side

	# sanity check:
	if(! inherits(fm, "formula"))
		stop('must provide a valid formula: groups ~ var1 + var2 + ...', call.=FALSE)

	# check for bogus left/right side problems with the formula
	if(any(g %in% object.names) == FALSE & g != '.') # bogus grouping column
		stop('group name either missing from formula, or does match any columns in dataframe', call.=FALSE)

	if(any(vars %in% object.names) == FALSE) # bogus column names in right-hand side
		stop('column names in formula do not match column names in dataframe', call.=FALSE)

	
	## old slice() version ###############################
	
	# make formula for slicing
	## TODO: slice() returns an extra row, so only request slices to max-1
	fm.slice <- formula(paste('0:', max(object)-1, ' ~ ', paste(vars, collapse=' + '), sep=''))

	# short-cut for user-defined slab
	## TODO: slice() returns an extra row, so only request slices to max-1
	if(length(slab.structure) == 2 )
		fm.slice <- formula(paste(slab.structure[1], ':', slab.structure[2]-1, ' ~ ', paste(vars, collapse=' + '), sep=''))

	# slice into 1cm increments, result is a data.frame
	data <- slice(object, fm.slice, strict=strict, just.the.data=TRUE)

	#######################################################
	
	
	## dice() version  ####################################
	# https://github.com/ncss-tech/aqp/issues/115
	
	## TODO:
	#  * enforce filling / check assumptions about length
	#  * consider total re-write vs. adaptation
	
	# # make formula for slicing
	# fm.slice <- formula(paste('0:', max(object), ' ~ ', paste(vars, collapse=' + '), sep=''))
	# 
	# # short-cut for user-defined slab
	# if(length(slab.structure) == 2 )
	#   fm.slice <- formula(paste(slab.structure[1], ':', slab.structure[2], ' ~ ', paste(vars, collapse=' + '), sep=''))
	# 
	# # slice into 1cm increments, result is a data.frame
	# data <- dice(x = object, fm = fm.slice, strict = strict, SPC = FALSE, pctMissing = TRUE)
	
	#######################################################
	
	
	
	
	# extract site data
	site.data <- site(object)

	# check groups for NA, if grouping variable was given
	if(g != '.')
		if(any(is.na(site.data[, g])))
			stop('grouping variable must not contain NA', call.=FALSE)

	### !!! this runs out of memory when groups contains NA !!!
	## this is generating a lot of extra objects and possibly wasting memory
	# merge site data back into the result, this would include site-level weights
	data <- merge(x = data, y = site.data, by=object.ID, all.x=TRUE, sort=FALSE)

	# clean-up
	rm(object, site.data)
	gc()

	# check variable classes
	if(length(vars) > 1) {
	  vars.numeric.test <- sapply(data[, vars], is.numeric)
	}	else {
	  vars.numeric.test <- is.numeric(data[[vars]])
	}


	# sanity check: all numeric, or single character/factor
	if(any(! vars.numeric.test) & length(vars) > 1) {
	  stop('mixed variable types and multiple categorical variables are not currently supported in the same call to slab', call. = FALSE)
	}


	# check for single categorical variable, and convert to factor
	if(length(vars) == 1 & inherits(data[, vars], c('character', 'factor'))) {

		# if we have a character, then convert to factor
		if(inherits(data[[vars]],'character')) {
			message('Note: converting categorical variable to factor.')
			data[[vars]] <- factor(data[[vars]])
		}

		# check for weights
		if(!missing(weights)) {
		  stop('weighted aggregation of categorical variables not yet implemented', call.=FALSE)
		}


		# re-set default function, currently no user-supply-able option
		slab.fun <- .slab.fun.factor.default

		# add extra arguments required by this function
		# note that we cannot accept additional arguments when processing categorical values
		extra.args <- list(cpm = cpm)

    # save factor levels for later
    original.levels <- levels(data[[vars]])

    # set a flag for post data.table.melt -> factor level setting
    .factorFlag <- TRUE
	} else {
	  .factorFlag <- FALSE
	}


	####
	#### optimization note: use of factor labels could be slowing things down...
	####
	## Note: this assumes ordering is correct in source data / segment labels
	## TODO: make sure that nrow(data) == genSlabLabels(slab.structure = slab.structure, max.d = max.d, n.profiles = n.profiles)
	## TODO: investigate use of split() to speed things up, no need to keep everything in the safe DF:
	##
	##         l <- split(data, data$seg.label, drop=FALSE)
	##
	## ... parallel processing with furrr

	# add segmenting label to data
 	data$seg.label <- genSlabLabels(slab.structure = slab.structure, max.d = max.d, n.profiles = n.profiles)

	# if there is no left-hand component in the formula, we are aggregating all data in the collection
	if(g == '.') {
		g <- 'all.profiles' # add new grouping variable to horizons
		data[, g] <- 1
	}


	##
	## TODO: adding weighted-aggregate functionality here
	## we can't use aggregate() for this
	## we can use data.table methods

	# check for weights
	if(!missing(weights))
		stop('weighted aggregation is not yet supported', call.=FALSE)

 	## TODO: why would this ever happen?
	# throwing out those rows with an NA segment label
	seg.label.is.not.NA <- which(!is.na(data$seg.label))

	# convert into long format
	# d.long.df <- reshape::melt(data[seg.label.is.not.NA, ], id.vars=c(object.ID, 'seg.label', g), measure.vars=vars)

	# convert wide -> long format
	# using data.table::melt()
	# note that this will not preserve factor levels when 'vars' is categorical
	# must call unique() on `id.vars`
	d.long <- melt(
	  as.data.table(data[seg.label.is.not.NA, ]),
	  id.vars = unique(c(object.ID, 'seg.label', g)),
	  measure.vars = vars,
	  )

	# convert back to data.frame
	d.long <- as.data.frame(d.long)

  # reset factor levels in d.long[[value]]
	if(.factorFlag) {
	  d.long[['value']] <- factor(d.long[['value']], levels = original.levels)
	}

	# make a formula for aggregate()
	aggregate.fm <- as.formula(paste('value ~ seg.label + variable + ', g, sep=''))

	##
	## TODO: this might be the place to implement parallel code in furrr
	##       1. split into a list based on number of cores/cpus available
	##       2. aggregate using seg.label + variable in parallel
	##       3. combine results (a list of data.frames)
	##
	## NOPE: use data.table which will automatically scale to multiple threads
	##



	# process chunks according to group -> variable -> segment
	# NA values are not explicitly dropped
	if(length(extra.args) == 0)
		d.slabbed <- aggregate(aggregate.fm, data=d.long, na.action=na.pass, FUN=slab.fun)

	# optionally account for extra arguments
	else {
		the.args <- c(list(formula=aggregate.fm, data=d.long, na.action=na.pass, FUN=slab.fun), extra.args)
		d.slabbed <- do.call(what='aggregate', args=the.args)
	}


	# if slab.fun returns a vector of length > 1 we must:
	# convert the complex data.frame returned by aggregate into a regular data.frame
	# the column 'value' is a matrix with the results of slab.fun
	if(inherits(d.slabbed$value, 'matrix'))
		d.slabbed <- cbind(d.slabbed[, 1:3], d.slabbed$value)

  # ensure that the names returned from slab.fun are legal
  names(d.slabbed) <- make.names(names(d.slabbed))

	# convert the slab.label column into top/bottom as integers
	slab.depths <- strsplit(as.character(d.slabbed$seg.label), '-')
	d.slabbed$top <- as.integer(lapply(slab.depths, function(i) i[1]))
	d.slabbed$bottom <- as.integer(lapply(slab.depths, function(i) i[2]))

	# estimate contributing fraction
	d.slabbed$contributing_fraction <- aggregate(aggregate.fm, data=d.long, na.action=na.pass, FUN=function(i) {length(na.omit(i)) / length(i)})$value


	# remove seg.label from result
	d.slabbed$seg.label <- NULL

  # if categorical data have been aggregated, set an attribute with the original levels
  # this allows one to recover values that are not legal column names
  # e.g. 2Bt is corrupted to X2Bt
  if(! is.null(original.levels))
    attr(d.slabbed, 'original.levels') <- original.levels

	# reset options:
	options(opt.original)

	# done
	return(d.slabbed)
}

# setup generic function
# if (!isGeneric("slab"))
	setGeneric("slab", function(object, fm, slab.structure=1, strict=FALSE, slab.fun=.slab.fun.numeric.default, cpm=1, weights=NULL, ...) standardGeneric("slab"))

# method dispatch
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
#' @param fm A formula: either `groups ~ var1 + var2 + var3' where named
#' variables are aggregated within `groups' OR where named variables are
#' aggregated across the entire collection ` ~ var1 + var2 + var3'. If `groups`
#' is a factor it must not contain NA.
#' @param slab.structure A user-defined slab thickness (defined by an integer),
#' or user-defined structure (numeric vector). See details below.
#' @param strict logical: should horizons be strictly checked for
#' self-consistency?
#' @param slab.fun Function used to process each 'slab' of data, ideally
#' returning a vector with names attribute. Defaults to a wrapper function
#' around \code{stats::quantile}. See details.
#' @param cpm Strategy for normalizing slice-wise probabilities, dividing by
#' either: number of profiles with data at the current slice (cpm=1), or by the
#' number of profiles in the collection (cpm=2). Mode 1 values will always sum
#' to the contributing fraction, while mode 2 values will always sum to 1.
#' @param weights Column name containing weights. NOT YET IMPLEMENTED
#' @param \dots further arguments passed to \code{slab.fun}
#' @return Output is returned in long format, such that slice-wise aggregates
#' are returned once for each combination of grouping level (optional),
#' variable described in the \code{fm} argument, and depth-wise 'slab'.
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
#' a.long <- melt(
#'   as.data.table(a),
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
#' a.1.long <- melt(
#'   as.data.table(a.1),
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
#' (gen.hz.ml <- get.ml.hz(a, c('O','A','B','C')))
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
#' dcast(
#'   as.data.table(a),
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
#' dcast(
#'   as.data.table(a),
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
setMethod(f='slab', signature='SoilProfileCollection', definition=.slab)

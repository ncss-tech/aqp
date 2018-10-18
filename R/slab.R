

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


## Note: this function requires at least _2_ observations
## Note: this function can be slow when N is large
# default slab function for continuous variables
# returns a named vector of results
# this type of function is compatible with aggregate()
.slab.fun.numeric.default <- function(values) {
	q.probs <- c(0.05, 0.25, 0.5, 0.75, 0.95)
	res <- hdquantile(values, probs=q.probs, na.rm=TRUE)
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


## TODO: not yet implemented
# default slab function for weighted, continuous variables
# returns a named vector of results
# this type of function is compatible with aggregate()
# NOTE: nw (normalize-weights) argument will affect the results!
.slab.fun.wtd.numeric.default <- function(values, w, nw=TRUE) {
	q.probs <- c(0.05, 0.25, 0.5, 0.75, 0.95)
	res <- wtd.quantile(values, weights=w, probs=q.probs, na.rm=TRUE, normwt=nw)
	names(res) <- paste('p.q', round(q.probs * 100), sep='')
	return(res)
}


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
	
	# make formula for slicing
	## TODO: slice() returns an extra row, so only request slices to max-1
	fm.slice <- formula(paste('0:', max(object)-1, ' ~ ', paste(vars, collapse=' + '), sep=''))
	
	# short-cut for user-defined slab
	## TODO: slice() returns an extra row, so only request slices to max-1
	if(length(slab.structure) == 2 )
		fm.slice <- formula(paste(slab.structure[1], ':', slab.structure[2]-1, ' ~ ', paste(vars, collapse=' + '), sep=''))
	
	# slice into 1cm increments, result is a data.frame
	data <- slice(object, fm.slice, strict=strict, just.the.data=TRUE)
	
	# extract site data
	site.data <- site(object)
	
	# check groups for NA, if grouping variable was given
	if(g != '.')
		if(any(is.na(site.data[, g])))
			stop('grouping variable must not contain NA', call.=FALSE)
	
	### !!! this runs out of memory when groups contains NA !!!
	## this is generating a lot of extra objects and possibly wasting memory
	# merge site data back into the result, this would include site-level weights
	data <- join(data, site.data, by=object.ID)
	
	# clean-up
	rm(object, site.data)
	gc()
	
	# check variable classes
	if(length(vars) > 1)
		vars.numeric.test <- sapply(data[, vars], is.numeric)
	else
		vars.numeric.test <- is.numeric(data[[vars]])
		
	# sanity check: all numeric, or single character/factor
	if(any(! vars.numeric.test) & length(vars) > 1)
		stop('mixed variable types and multiple categorical variables are not currently supported in the same call to slab', call.=FALSE)
	
	# check for single categorical variable, and convert to factor
	if(length(vars) == 1 & class(data[, vars]) %in% c('character', 'factor')) {
		
		# if we have a character, then convert to factor
		if(class(data[[vars]]) == 'character') {
			message('Note: converting categorical variable to factor.')
			data[[vars]] <- factor(data[[vars]])
		}
		
		# check for weights
		if(!missing(weights))
			stop('weighted aggregation of categorical variables not yet implemented', call.=FALSE)
		
		# re-set default function, currently no user-supply-able option
		slab.fun <- .slab.fun.factor.default
		
		# add extra arguments required by this function
		# note that we cannot accept additional arguments when processing categorical values
		extra.args <- list(cpm=cpm)
    
    # save factor levels for later
    original.levels <- levels(data[[vars]])
		}
		
	
	####
	#### optimization note: use of factor labels could be slowing things down...
	####
	## TODO: this assumes ordering is correct in source data / segment labels 
	## TODO: make sure that nrow(data) == length(factor(rep(seg.label, times=n.profiles), labels=seg.label.levels))
	## TODO: investigate use of split() to speed things up, no need to keep everything in the safe DF:
	##         seg.label <- factor(rep(seg.label, times=n.profiles), labels=seg.label.levels)
	##         l <- split(data, seg.label, drop=FALSE)
	
	# add segmenting label to data
 	data$seg.label <- genSlabLabels(slab.structure = slab.structure, max.d = max.d, n.profiles = n.profiles)
	
	# if there is no left-hand component in the formula, we are aggregating all data in the collection
	if(g == '.') { 
		g <- 'all.profiles' # add new grouping variable to horizons
		data[, g] <- 1
	}
	
# 	# determine number of profiles / group
# 	profiles.per.group <- tapply(data[, object.ID], data[, g], function(i) length(unique(i)))
# 	
# 	# deliver some feedback:
# 	message(paste('number of profiles:', n.profiles))
# 	message(paste('profiles / ', g, ':', sep=''), appendLF=TRUE)
# 	print(profiles.per.group)
	
	##
	## TODO: adding weighted-aggregate functionality here
	## we can't use aggregate() for this
	##
	
	# check for weights
	if(!missing(weights))
		stop('weighted aggregation is not yet supported', call.=FALSE)
	
	# convert into long format
	# throwing out those rows with an NA segment label
	seg.label.is.not.NA <- which(!is.na(data$seg.label))
	d.long <- melt(data[seg.label.is.not.NA, ], id.vars=c(object.ID, 'seg.label', g), measure.vars=vars)
	
	# make a formula for aggregate()
	aggregate.fm <- as.formula(paste('value ~ seg.label + variable + ', g, sep=''))
	
	##
	## TODO: this might be the place to implement parallel code: 
	##       1. split into a list based on number of cores/cpus available
	##       2. aggregate using seg.label + variable in parallel
	##       3. combine results (a list of data.frames)
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
	if(class(d.slabbed$value) == 'matrix')
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
if (!isGeneric("slab"))
	setGeneric("slab", function(object, fm, slab.structure=1, strict=FALSE, slab.fun=.slab.fun.numeric.default, cpm=1, weights=NULL, ...) standardGeneric("slab"))

# method dispatch
setMethod(f='slab', signature='SoilProfileCollection', definition=.slab)

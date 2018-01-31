## updated to use aggregate(), now >10x faster than ddply() version
## slice() can be further optimized, possibly through the use of idata.frame() or data.table()
## scales linearly with an increas in num. profiles
## scales exponentially (faster) with an increase in num. profiles / group
## keep checking on other options:
## http://stackoverflow.com/questions/11533438/why-is-plyr-so-slow
## 
## missing horizons:
## slab will run out of memory when trying to aggregate profiles that are missing horizons-- why?

## groups is a factor and contains NAs:
## with large data sets join() will run out of memory

## weighted aggregation:
## 1. not currently possible with aggregate() as it can only operate on vectors
## 2. possible with other functions: https://stat.ethz.ch/pipermail/r-help/2003-June/035321.html
## 3. switching to data.table may be required
#### basic syntax looks like this: d[, list(wm=wtd.mean(x, w)), by=g]



# SoilProfileCollection method
# object: SoilProfileCollection 
# fm: formula defining aggregation
# slab.structure: either regular segment interval, or user-defined segment boundaries {starting from 0, or length of 2}
# progress: plyr-progress display
# slab.fun: aggregate function applied to data chunks (must return a single row / chunk)
# cpm: class probability normalization mode
# weights: character vector naming column containing weights
# ... : extra arguments passed on to slab.fun




# data.table implementation
.slab2 <- function(object, fm, slab.structure=1, strict=FALSE, slab.fun=NULL, cpm=1, weights=NULL, ...){
	# issue a message for now that things have changed
	#	message('usage of slab() has changed considerably, please see the manual page for details')
	
  library(data.table)
  
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
		}
		
	# generate labels for slabs
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
	
	
	####
	#### optimization note: use of factor labels could be slowing things down...
	####
	## TODO: make sure that nrow(data) == length(factor(rep(seg.label, times=n.profiles), labels=seg.label.levels))
	## TODO: investigate use of split() to speed things up, no need to keep everything in the safe DF:
	##         seg.label <- factor(rep(seg.label, times=n.profiles), labels=seg.label.levels)
	##         l <- split(data, seg.label, drop=FALSE)
	# add segmenting label to data
 	data$seg.label <- factor(rep(seg.label, times=n.profiles), labels=seg.label.levels)
	  
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
	
	
	# convert into long format
	# throwing out those rows with an NA segment label
	seg.label.is.not.NA <- which(!is.na(data$seg.label))
	d.long <- melt(data[seg.label.is.not.NA, ], id.vars=c(object.ID, 'seg.label', g), measure.vars=vars)
	
	## data.table optimization
  d.long <- data.table(d.long)
  
  # init keys
  setkeyv(d.long, cols=c('variable', g, 'seg.label'))
  
  ## what about NA values...?
	if(length(extra.args) == 0)
		d.slabbed <- data.frame(d.long[!is.na(value), as.list(slab.fun(value)), by=key(d.long)])
	
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
	
	
	# convert the slab.label column into top/bottom as integers
	slab.depths <- strsplit(as.character(d.slabbed$seg.label), '-')
	d.slabbed$top <- as.integer(lapply(slab.depths, function(i) i[1]))
	d.slabbed$bottom <- as.integer(lapply(slab.depths, function(i) i[2]))
	
	# estimate contributing fraction
# 	d.slabbed$contributing_fraction <- aggregate(aggregate.fm, data=d.long, na.action=na.pass, FUN=function(i) {length(na.omit(i)) / length(i)})$value

	
	# remove seg.label from result
	d.slabbed$seg.label <- NULL
		
	# reset options:
	options(opt.original)
	
	# done
	return(d.slabbed)
}

# setup generic function
if (!isGeneric("slab2"))
	setGeneric("slab2", function(object, fm, slab.structure=1, strict=FALSE, slab.fun=.slab.fun.numeric.default, cpm=1, weights=NULL, ...) standardGeneric("slab2"))

# method dispatch
setMethod(f='slab2', signature='SoilProfileCollection', definition=.slab2)

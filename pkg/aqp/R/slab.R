
# default slab function for categorical variables
# returns data.frame with a single row
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
	
	# convert table into a data.frame
	p.prop <- data.frame(matrix(pt, nrow=1))
	# assign safe names to the resulting columns: each column is a level of values
	names(p.prop) <- make.names(levels(values))
	
	return(p.prop)
	}

# default slab function for continuous variables
# returns data.frame with a single row
.slab.fun.numeric.default <- function(values) {
	q.probs <- c(0.05, 0.25, 0.5, 0.75, 0.95)
	q.df <- data.frame(t(hdquantile(values, probs=q.probs, na.rm=TRUE)))
	names(q.df) <- paste('p.q', round(q.probs * 100), sep='')
	return(q.df)
	}

# this function is internally used to process chunks
.process.slab.chunk <- function(chunk, slab.fun, ...) {
	# process summary
	chunk.summary <- slab.fun(chunk$value, ...)
	# parse chunk label and re-create depths
	chunk.depths <- strsplit(as.character(chunk$seg.label[1]), '-')[[1]]
	# estimate contributing fraction
	cf <- length(na.omit(chunk$value)) / length(chunk$value)
	# compose result
	res <- data.frame(top=chunk.depths[1], bottom=chunk.depths[2], contributing_fraction=cf, chunk.summary)
	return(res)
	}


# SoilProfileCollection method
# object: SoilProfileCollection 
# fm: formula defining aggregation
# slab.structure: either regular segment interval, or user-defined segment boundaries {starting from 0, or length of 2}
# progress: plyr-progress display
# slab.fun: aggregate function applied to data chunks (must return a single row / chunk)
# cpm: class probability normalization mode
# ... : extra arguments passed on to slab.fun

# this is about 40% slower than the old method
.slab <- function(object, fm, slab.structure=1, progress='none', strict=FALSE, slab.fun=.slab.fun.numeric.default, cpm=1, ...){
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
	
	# slice into 1cm increments, result is a SPC
	data <- slice(object, fm.slice, strict=strict, just.the.data=TRUE)
	
	# merge site data back into the result
	data <- join(data, site(object), by=idname(object))
	
	# convert sliced SPC into a data.frame, merging-in site-level data
	# data <- as(data, 'data.frame')
	
	# check variable classes
	## TODO: there must be a cleaner way to check on this
	vars.numeric.test <- sapply(data.frame(data[, vars]), function(i) inherits(i, 'numeric') | inherits(i, 'integer'))
	
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
	
		if(class(data[[vars]]) ==  'factor') {
			# save the levels of our categorical variable
			prop.levels <- levels(data[[vars]]) 
		}
		
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
	
	
	# add segmenting label to data
	## TODO: check to make sure sorting assumptions are correct
	data$seg.label <- factor(rep(seg.label, times=n.profiles), labels=seg.label.levels)
	
	# if there is no left-hand component in the formula, we are aggregating all data in the collection
	if(g == '.') { 
		g <- 'all.profiles' # add new grouping variable to horizons
		data[, g] <- 1
	}
	
	# convert into long format
	# throwing out those rows with an NA segment label
	seg.label.is.not.NA <- which(!is.na(data$seg.label))
	d.long <- melt(data[seg.label.is.not.NA, ], id.vars=c(idname(object), 'seg.label', g), measure.vars=vars)
	
	# process chunks according to group -> variable -> segment
	# optionally account for extra arguments
	if(length(extra.args) == 0)
		d.slabbed <- ddply(d.long, .variables=c(g, 'variable', 'seg.label'), .progress=progress, .fun=.process.slab.chunk, slab.fun=slab.fun) 
	
	else
		d.slabbed <- do.call(what='ddply', args=c(list(d.long, .variables=c(g, 'variable', 'seg.label'), .progress=progress,  .fun=.process.slab.chunk, slab.fun=slab.fun), extra.args))
	
	# remove seg.label from result
	d.slabbed$seg.label <- NULL
	
	# convert depths back into integers
	d.slabbed$top <- as.integer(d.slabbed$top)
	d.slabbed$bottom <- as.integer(d.slabbed$bottom)
	
	# reset options:
	options(opt.original)
	
	# done
	return(d.slabbed)
}

# setup generic function
if (!isGeneric("slab"))
	setGeneric("slab", function(object, fm, slab.structure=1, progress='none', strict=FALSE, slab.fun=.slab.fun.numeric.default, cpm=1, ...) standardGeneric("slab"))

# method dispatch
setMethod(f='slab', signature='SoilProfileCollection', definition=.slab)

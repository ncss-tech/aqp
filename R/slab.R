##############################################################
## slotting functions ##
##############################################################

# default slab function for categorical variables
slab.fun.factor.default <- function(values, prop.levels) {
	# get a vector of all possible categories
	# note that l.recon contains factor codes
	p.unique.classes <- as.vector(na.omit(unique(values)))
		
	# tabular frequences for complete set of possible categories
	p.table <- sapply(values, function(i, cpm=class_prob_mode, wts=l.recon.wts) {
			tf <- factor(l.recon[[i]], levels=p.unique.classes, labels=prop.levels[p.unique.classes])
			
			# probabilities are relative to number of contributing profiles
			if(cpm == 1) {
			  tb <- table(tf, useNA='no')
			  # convert to proportions
			  pt <- prop.table(tb)
			  }
			
			# probabilities are relative to total number of profiles
			else if(cpm == 2) {
			  tb <- table(tf, useNA='always')
			  # convert to proportions, 
			  # the last column will be named 'NA', and contains the tally of NAs --> remove it
			  pt <- prop.table(tb)
			  pt <- pt[-length(pt)]
			  }
			  
			return(pt)
			} 
		)
		
		# convert into a dataframe: if there are > 1 classes, 
		# then we must transpose p.table when converting to a data.frame
		if(length(p.unique.classes) > 1)
			p.prop <- data.frame(t(p.table))
			
		# when there is only 1 class, things are more complicated:
		# 1. no need to transpose p.table
		# 2. need to manually add the class name
		else {
			p.prop <- data.frame(p.table)
			names(p.prop) <- prop.levels[p.unique.classes]
			}
	}

# default slab function for continuous variables
slab.fun.numeric.default <- function(values) {
	q.probs <- c(0.05, 0.25, 0.5, 0.75, 0.95)
	q.df <- data.frame(t(hdquantile(values, probs=q.probs, na.rm=TRUE)))
	names(q.df) <- paste('p.q', round(q.probs * 100), sep='')
	return(q.df)
	}

# this function is internally used
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
# seg.size: either regular segment interval, or user-defined segment boundaries {starting from 0, or length of 2}
# progress: plyr-progress display
# slab.fun: aggregate function applied to data chunks
# ... : extra arguments passed on to slab.fun

# custom segmentation 
slab2 <- function(object, fm, seg.size=1, progress='none', strict=FALSE, slab.fun=slab.fun.numeric.default, ...){
	## important: change the default behavior of data.frame and melt
	opt.original <- options(stringsAsFactors = FALSE)
	
	# get unique list of names in object
	object.names <- unique(unlist(names(object)))
	
	# get number of profiles
	n.profiles <- length(object)
	
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
	if(length(seg.size) == 2 )
		fm.slice <- formula(paste(seg.size[1], ':', seg.size[2]-1, ' ~ ', paste(vars, collapse=' + '), sep=''))
	
	# slice into 1cm increments, result is a SPC
	data <- slice(object, fm.slice, strict=strict)
	
	# max depth
	max.d <- max(data)
	
	# convert sliced SPC into a data.frame, merging-in site-level data
	data <- as(data, 'data.frame')
	
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
		slab.fun <- slab.fun.factor.default	
		stop('TODO: finish implementation and testing')	
		}
		
	
	
	# generate labels for slabs
	# fixed-size slabs
	if(length(seg.size) == 1) {
		# generate sequence of segment labels
		seg.label <- rep(1:ceiling(max.d / seg.size), each=seg.size, length=max.d)
		# general segment labels
		seg.label.levels <- tapply(1:max.d, seg.label, function(i) {r <- range(i); paste(c(r[1]-1, r[2]), collapse='-') } )
	}
	
	# user-defined slabs
	if(length(seg.size) > 1) {
		# trival case where segments start from 0
		if(seg.size[1] == 0 & length(seg.size) > 2)
			seg.label <- rep(seg.size[-1], times=diff(seg.size))[1:max.d]
		
		# other case: user defines an arbitrary lower and upper limit
		else {
			if(length(seg.size) != 2)
				stop('user-defined slab boundaries must either start from 0, or contain two values between 0 and the max soil depth')
			
			# proceed
			slab.thickness <- diff(seg.size)
			# how many slices of NA before the slab?
			padding.before <- rep(NA, times=seg.size[1])
			# how many slices of NA afer the slab
			padding.after <- rep(NA, times=max.d - seg.size[2])
			# make a new label for the slab
			new.label <- paste(seg.size, collapse='-')
			# generate an index for the slab
			slab.idx <- rep(new.label, times=slab.thickness)
			# generate the entire index: padding+slab+padding = total number of slices (max_d)
			# seg.label <- c(padding.before, slab.idx, padding.after)
			seg.label <- slab.idx 
			}
		
		# generate segment labels	
		seg.label.levels <- sapply(1:(length(seg.size)-1), function(i) paste(c(seg.size[i], seg.size[i+1]), collapse='-'))
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
	d.long <- melt(data[seg.label.is.not.NA, ], id.vars=c('id', 'seg.label', g), measure.vars=vars)
	
	# process chunks according to group -> variable -> segment
	d.slabbed <- ddply(d.long, .variables=c(g, 'variable', 'seg.label'), .progress=progress, .parallel=getOption('AQP_parallel', default=FALSE), .fun=.process.slab.chunk, slab.fun=slab.fun, ...) 
	
	# remove seg.label from result
	d.slabbed$seg.label <- NULL
	
	# reset options:
	options(opt.original)
	
	# done
	return(d.slabbed)
}


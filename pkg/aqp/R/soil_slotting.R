##############################################################
## slotting functions ##
##############################################################




#
# as of R 2.7 calling var() or anything that calls var()
# results in an error when there are not enough values
# previously NA was returned
#
conditional.sd <- function(x)
	{
	
	l <- length(na.omit(x))
	if(l >= 3)
		{
		x.sd <- sd(x, na.rm=TRUE)
		}
	else
		{
		x.sd <- NA	
		}
	
	return(x.sd)
	}


## TODO: seg_vect does not work
# input dataframe must have an id column identifing each profile
# note: this only works with numeric variables
soil.slot.multiple <- function(data, g, vars, seg_size=NA, strict=FALSE, user.fun=NULL)
	{
	# check for dependencies
	if(!require(plyr) | !require(reshape))
		stop('Please install the "plyr" and "reshape" packages.')
		
	## this is still experimental
	# check for ability to use parallel computations:
	# parallel <- checkMC()
		
		
	# currently this will only work with integer depths
	if(any( !as.integer(data$top[data$top != 0]) == data$top[data$top != 0] ) | any( !as.integer(data$bottom) == data$bottom))
		stop('this function can only accept integer horizon depths')
	
	## TODO: is there a better way to do this?
	# capture arguments
	ss <- seg_size
	s <- strict
	uf <- user.fun
	
	# convert into long forma
	d.long <- melt(data, id.vars=c('id','top','bottom', g), measure.vars=vars)
	
	# apply slotting group-wise and return in long format
	# note that we are passing in additional arguments to soil.slot 
	# from the calling function
	d.slotted <- ddply(d.long, .(variable), .progress='text', .fun=function(i, groups=g, seg_size=ss, strict=s, user.fun=uf) {
		
		# subset just the relevant columns
		i.sub <- data.frame(
			id=i$id, 
			top=i$top, 
			bottom=i$bottom, 
			prop=i$value,
			groups=i[, groups]
			)
		
		## TODO: allow for seg_vect or seg_size	
		## currently only one or the other is supported
		# apply slotting according to grouping factor
		i.slotted <- ddply(i.sub, .(groups), .fun=soil.slot, seg_size=seg_size, strict=strict, user.fun=uf)
		
		return(i.slotted)
		})
		
	# convert tops and bottoms to integers
	d.slotted$top <- as.integer(d.slotted$top)
	d.slotted$bottom <- as.integer(d.slotted$bottom)
	
	# done
	return(d.slotted)
	}	






## this function will break when horizon boundaries do not make sense
## 
# TODO: we only need x.recon for 1cm aggregation, otherwise l.recon is used
# TODO: check weighted computations.... probably not quite correct
# TODO: optionally compute probability by dividing by number of profiles, not just profiles eith data to a given depth
# TODO: slice-wise probability does not work with categorical vectors, when slice size > 1
# TODO: re-factor how profile weights are used, consider using rq()
# TODO: return the number of profiles + number of unique horizons when using custom segmenting interval
# TODO: replace by() with equivilant plyr functions
soil.slot <- function(data, seg_size=NA, seg_vect=NA, return.raw=FALSE, use.wts=FALSE, strict=FALSE, user.fun=NULL)
	{
	
	## check for fatal errors	
	# currently this will only work with integer depths
	if(any( !as.integer(data$top[data$top != 0]) == data$top[data$top != 0] ) | any( !as.integer(data$bottom) == data$bottom))
		stop('this function can only accept integer horizon depths')
	
	# no NA allowed in top or bottom
	hz.test.top <- is.na(data$top)
	hz.test.bottom <- is.na(data$bottom)
	
	if(any(hz.test.top))
		{
		print(data[which(hz.test.top), ])
		stop('Error: NA in horizon top boundary')
		}
		
	if(any(hz.test.bottom))
		{
		print(data[which(hz.test.bottom), ])
		stop('Error: NA in horizon top boundary')
		}
	
	# can't pass in a bogus aggregate function
	if(!is.null(user.fun) & !is.function(user.fun)) 
		stop('Error: user.fun is not a function')
		
	
	# re-level id factor according to account for subsets
	data$id <- factor(data$id)
	
	# what is the datatype of 'prop'
	prop.class <- class(data$prop)
	
	# if we have a character, then convert to factor
	if(prop.class == 'character')
		{
		cat('converting to categorical variable to factor \n')
		data$prop <- factor(data$prop)
		}
	
	# test for use of categorical variable and >1 seg vect
	if(prop.class == 'factor' & (!missing(seg_size) | !missing(seg_vect)))
		stop('sorry, aggregation of categorical variables by segments sizes >1 is not yet supported')
	
	# get the max depth for the entire dataset
	max_d <- max(data$bottom)
	
	# unroll the dataset, a pedon at a time
	x.unrolled <- dlply(data, .(id), .fun=function(i, m=max_d) 
		{
		
		u <- try(unroll(top=i$top, bottom=i$bottom, prop=i$prop, max_depth=m, strict=strict))
		
		## TODO: could be better
		# check for a non-NULL attribute of 'class'
		# this will only happen when there was an error
		if( !is.null(attr(u, 'class')))
			{
			if(attr(u, 'class') == 'try-error')
				{
				print(i)
				stop('Error: bad horizon structure')
				}
			}
		else
			return(u)
		} )
	
	
	# note that these will be used later on, based on segmenting approach
	# reconstitute into a matrix with 1:n-depth interval rows, and n_pedons columns
	x.recon_original <- sapply(x.unrolled, '[')
	
	if(use.wts == TRUE)
		{
		# unroll a weight vector for each pedon
		x.unrolled.wts <- by(data, data$id, function(i, m=max_d) unroll(top=i$top, bottom=i$bottom, prop=i$wt, max_depth=m))
		
		# reconstitute weights:
		x.recon.wts_original <- sapply(x.unrolled.wts, '[')
		}
	
	# if we have a regular-interval segment size, re-group the data following the segmenting id
	# note that we are testing for the default value of seg_size and seg_vect
	# must be a better way to do this..
	if(!missing(seg_size) | !missing(seg_vect))
		{
		
		# give a warning about weights and SD
		cat('calculation of SD with a user-defined segment size is unrealiable\n')
		
		# use a user-defined segmenting vector, starting from 0
		if(!missing(seg_vect))
			{
			wind.idx <- rep(seg_vect[-1], diff(seg_vect))[1:max_d]
			}
			
		# using a fixed-interval segmenting vector
		else
			{
			# generate a vector of unique segment ids
			segment_label <- 1:((max_d/seg_size)+1)
			
			# generate combined segment id vector
			# truncating at the max depth
			wind.idx <- rep(segment_label, each=seg_size)[1:max_d]
			}
		
		
		## this was the old method, resulting in deflated SD, due to artificial inflation of 'n'
		# subset values and weights by id
		# note that we are lumping the subset values by id into a single row of a matrix
		
		
		###############################################################
		#### first idea: results inflated 'n' for SD calculation       ##
		###############################################################
		
		# generate a list, where each entry is a vector corresponding to the collection of 
		# values from all profiles, for those depths defined by each slice
		# we can't directly rbind this list together into a DF, because there are cases where the last
		# entry has a shorter length than all of the other entries
		# this is caused by a seg_size that does not divide evenly into our max depth (max_d)
		l.recon <- by(x.recon_original, wind.idx, unlist)

#		if(!missing(seg_size))
#			{
#			# figure out the proper number of elements that _should_ be in each entry
#			n.recon.items.per.row <- ncol(x.recon_original) * seg_size
#		
#			# get an index to the last entry in the list, as this one is likely to be shorter than the rest
#			l.last.idx <- length(l.recon)
#		
#			# figure out how much NA padding need, and then append it to the last entry
#			n.NA.padding <- n.recon.items.per.row-length(l.recon[[l.last.idx]])
#			l.recon[[l.last.idx]] <- c(l.recon[[l.last.idx]], rep(NA, times=n.NA.padding))
#		
#			# some user-feedback
#			if(n.NA.padding > 0)
#				cat(paste('requested segment interval (', seg_size, ') does not divide evenly into max depth (', max_d, ') ... padding with NA\n', sep=''))
#			}
#		
#		# convert list into dataframe, for row-wise calculation of summary stats
#		# this should not error when max_d is not evenly divisible by seg_size 
#		# x.recon <- do.call('rbind', l.recon)

		
		###############################################################
		#### second idea: results in equal-weighting between profiles ##
		###############################################################
		
		# compute hz-thickness wt-mean value within each user-defined slice
		# this will increase SD by not artifically inflating 'n'
		# x.recon <- apply(x.recon_original, 2, function(i, idx=wind.idx) tapply(i, idx, mean, na.rm=TRUE))
		
		# NaN can result from missing data, convert them into NA for simplicity sake
		# x.recon[is.nan(x.recon)] <- NA
		
		
		## TODO: fix this!
		## weighted calculations are broken!
		if(use.wts == TRUE)
			{
			# subset values by  id
			x.recon.wts <- try(do.call('rbind', by(x.recon.wts_original, wind.idx, unlist) ))
			}
			
		# use a user-defined segmenting vector, starting from 0
		if(!missing(seg_vect))
			{
			
			# get actual length of segmented data
			# note that this might be less than the maximum depth suggested by the segmenting vector
			len <- length(l.recon)
			len.seg_vect <- length(seg_vect)
			
			# the actual max_depth may be less than the requested segments
			# in that case we will need to truncate the horizon label vector
			if(len < len.seg_vect)
				{
				seg_vect_legal_idx <- which( (seg_vect - max_d) <= 0)
				sv_clean <- seg_vect[c(1,seg_vect_legal_idx+1)]
				}
				
			# the actual depth may be more than the max depth requested in the seg_vect
			else if(len > len.seg_vect)
				{
				sv_clean <- c(seg_vect[-len.seg_vect], max_d)
				}
			
			# normal circumstances
			# ??? what are they?
			else
				{
				sv_clean <- seg_vect
				}
			
			len.clean <- length(sv_clean)
			
			# generate segment tops and bottoms
			# this generates an extra row sometimes, check for it below
			df.top_bottom <- data.frame(top=c(0,sv_clean[-c(1, len.clean)]), bottom=c(sv_clean[-c(1, len.clean)], max_d))
			
			# check for lower horizon where top == bottom, and remove it
			bad_hz_list_TF <- with(df.top_bottom, top == bottom)
			
			if(TRUE %in% bad_hz_list_TF)
				{
				bad_hz_list_idx <- which(bad_hz_list_TF)
				print(paste('Removing horizon with 0 thickness (', bad_hz_list_idx, ')', sep='' ))
				df.top_bottom <- df.top_bottom[-bad_hz_list_idx, ]
				}
						
			# done with user-defined segment vector
			}
			
		# using a fixed-interval segmenting vector
		else
			{
			# get the length of our segmented data set
			# and generate a new sequence of depths
			len <- length(l.recon)
			l.seq <- 1:len
			dz <- l.seq * seg_size
			
			# generate segment tops and bottoms
			df.top_bottom <- data.frame(top=c(0,dz[-length(dz)]), bottom=c(dz[-len], max_d))
								
			# done with fixed-interval segmenting vector
			}
		
		## compute row-wise summary statistics
		# always compute a contributing fraction
		# this is the number of profile contributing the the slice-wise aggregate
		contributing_fraction <- sapply(l.recon, function(i) length(na.omit(i)) / length(i))
		
		## compute summary stats by segment
		# standard summary statistics
		## this mean is the horizon-thickness weighted mean across all profiles
		p.mean <- sapply(l.recon, mean, na.rm=TRUE)
		
		## this estimate of SD is too low when using a segment size > 1 cm
		p.sd <- sapply(l.recon, conditional.sd)
		
		## these are the horizon-thickness weighted quantiles
		q.probs <- c(0.05, 0.25, 0.5, 0.75, 0.95)
		p.quantiles <- data.frame(t(sapply(l.recon, quantile, probs=q.probs, na.rm=TRUE)))
		names(p.quantiles) <- paste('p.q', round(q.probs * 100), sep='')
		
		# user function
		if(!is.null(user.fun))
			{
			p.user <- try( sapply(l.recon, user.fun) )
			}
		
		
		} # done with segmenting
	
	# no segmenting
	else
		{
		x.recon <- x.recon_original
		
		if(use.wts == TRUE)
			x.recon.wts <- x.recon.wts_original
		
		df.top_bottom <- data.frame(top=0:(max_d-1), bottom=1:max_d)
		
		
		
		## compute row-wise summary statistics
		# always compute a contributing fraction
		# this is the number of profile contributing the the slice-wise aggregate
		contributing_fraction <- apply(x.recon, 1, function(i) length(na.omit(i)) / length(i))
	
	
		# the following are computed according to user-defined parameters
		if(prop.class %in% c('numeric','integer'))
			{
			# standard summary statistics
			## this mean is the horizon-thickness weighted mean across all profiles
			p.mean <- apply(x.recon, 1, mean, na.rm=TRUE)
		
			## this estimate of SD is too low when using a segment size > 1 cm
			p.sd <- apply(x.recon, 1, conditional.sd)
		
			## these are the horizon-thickness weighted quantiles
			q.probs <- c(0.05, 0.25, 0.5, 0.75, 0.95)
			p.quantiles <- data.frame(t(apply(x.recon, 1, quantile, probs=q.probs, na.rm=TRUE)))
			names(p.quantiles) <- paste('p.q', round(q.probs * 100), sep='')
		
			# user function
			if(!is.null(user.fun))
				{
				p.user <- try( apply(x.recon, 1, user.fun) )
				}
			}
		
		# todo: update this to use a different column
		if(prop.class == 'factor' | prop.class == 'character')
			{
			# the results of this operation are a list,
			# one element for each depth segment
		
			# get a vector of all possible categories
			# these are the factor codes...
			p.unique.classes <- as.vector(na.omit(unique(as.vector(x.recon))))
		
			# tabular frequences for complete set of possible categories
			# TODO: generalize to user-defined segmenting vectors
			p.table <- apply(x.recon, 1, function(i) { 
				table(factor(i, levels=p.unique.classes, labels=levels(data$prop)[p.unique.classes]), useNA='no')  
				} 
				)
		
			# convert into a dataframe
			p.freq <- as.data.frame(t(p.table))
		
			# convert into proportions
			# TODO: optionally divide by total number of profiles
			p.row.counts <- apply(p.freq, 1, sum)
			p.prop <- sweep(p.freq, 1, STATS=p.row.counts, FUN='/')
			}
			
		} # done with 1cm interval

	
	
	
	
	
		
	## NOTE: the calculation of the weighted SD is not quite right when using segments larger than 1 cm
	# no way to use weights with factor vectors yet...
	if(use.wts == TRUE)
		{
		## weighted mean calculation
		## reduces to standard mean, when weights are equal
		# compute the row-wise sum of weights vector
		wts_seg_sums <- apply(x.recon.wts, 1, sum, na.rm=TRUE) 
		
		# generate a row-wise fractional weight matrix,
		# same dimensions as reconstituted property matrix
		wt_matrix <- sweep(x.recon.wts, 1, STATS=wts_seg_sums, FUN='/')
		
		# scale each property by its associated weight
		x.recon.wted <- x.recon * wt_matrix
		
		# compute the mean by row, weights sum to 1, so there is no division step
		p.wtmean <- apply(x.recon.wted, 1, sum, na.rm=TRUE)
	
	
		## row-wise weighted sd calculations: only if there are 3 or more obs
		if(ncol(x.recon.wted) >= 3)
			{
			d1 <- apply(wt_matrix * x.recon^2, 1, sum, na.rm=TRUE)
			d2 <- apply(wt_matrix, 1, sum, na.rm=TRUE)
			d3 <- apply(x.recon * wt_matrix, 1, sum, na.rm=TRUE)^2
			
			n1 <- apply(wt_matrix, 1, sum, na.rm=TRUE)^2
			n2 <- apply(wt_matrix^2, 1, sum, na.rm=TRUE)
		
			# weighted variance
			var.wt <- (d1 * d2 - d3) / (n1 - n2)
			
			# weighted sd: id wt. variances less than 0 -- these were probably computed by too few observations
			var.wt[which(var.wt < 0)] <- NA
			p.wtsd <- sqrt(var.wt)
			}
		else # not enough obs to compute SD
			{
			p.wtsd <- NA
			}
			
		# re-make final dataframe, note that df.top_bottom is made based on segmenting/non-segmenting
		df.stats <- data.frame(p.mean, p.wtmean, p.sd, p.wtsd)
		}
		
	# no weighted stats needed
	else
		{
		if(prop.class %in% c('numeric','integer'))
			{
			if(!is.null(user.fun))
				{
				# TODO: might be good to check for a try-error
				df.stats <- data.frame(p.mean, p.sd, p.quantiles, p.user)
				}
			else
				df.stats <- data.frame(p.mean, p.sd, p.quantiles)
			}
		if(prop.class == 'factor')
			{
			df.stats <- data.frame(p.prop)
			}
		}
	
	
	## form into dataframe for returning to the user 
	# this is usually where we have problems, caused by bad horizon boundaries
	if(nrow(df.top_bottom) == nrow(df.stats))
		{
		x.slotted <- data.frame(df.top_bottom, contributing_fraction, df.stats)
		}
	# something is wrong
	else
		{
		print("ERROR!")
		print(data)
		}
	
	# covert tops / bottoms to integers
	x.slotted$top <- as.integer(x.slotted$top)
	x.slotted$bottom <- as.integer(x.slotted$bottom)
	
	# done
	return(x.slotted)
	}

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



# TODO: update this to generalize over several grouping variables
# function used to slot soils one 'group' at a time 
soil.slot.vec <- function(i, ...)
	{
	# when there is only a single soil being slotted, 
	# we need to fix the levels of the 'id' factor:
	i$id <- factor(i$id)
	
	# slot this data set with associated parameters
	s <- soil.slot(data=i, ...)
	
	return(s)
	}



## this function will cause errors when the levels of an id (factor) do not match those levels that actually have data!!
## 
## this function will also break when horizon boundaries do not make sense
## 
# means and confidence intervals should be calculated by population defined by seg_size and n pedons
# note that this requires a wt column now
# 
#
# TODO: check depth probability calculations
# 
# TODO: warnings generated in some cases... could it be when there is a single soil / group?
# 
soil.slot <- function(data, seg_size=NA, seg_vect=NA, return.raw=FALSE, use.wts=FALSE, compute.depth.prob=FALSE)
	{
	# what is the datatype of 'prop'
	prop.class <- class(data$prop)
	
	# print(paste('property is:', prop.class))
	
	# get the max depth for the entire dataset
	max_d <- max(data$bottom)
	
	# unroll the dataset, a pedon at a time
	x.unrolled <- by(data, data$id, function(i, m=max_d) unroll(top=i$top, bottom=i$bottom, prop=i$prop, max_depth=m))
	
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
	if(!missing(seg_size) | !missing(seg_vect))
		{
		
		# use a user-defined segmenting vector, starting from 0		
		if(!missing(seg_vect))
			{
			wind.idx <- rep(seg_vect[-1], diff(seg_vect))[1:max_d]
			}
		# using a fixed-interval segmenting vector
		else
			{
			# generate a vector of unique segment ids
			# adding one extra can sometimes cause warnings... not sure if it matters
			segment_label <- 1:((max_d/seg_size)+1)
			
			# generate combined segment id vector
			# truncating at the max depth
			wind.idx <- rep(segment_label, each=seg_size)[1:max_d]
			}
		
		
		
		## warnings are being generated here
		## it looks like values are being recycled, possibly the weights
		# subset values and weights by id
		# note that we are lumping the subset values by id into a single row of a matrix
		x.recon <- try(do.call('rbind', by(x.recon_original, wind.idx, unlist) ))
		
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
			len <- nrow(x.recon)
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
			
			# return a dataframe with all values, indexed by segment
			# useful for looking at the distribution of properties by segment
			# bwplot(factor(seg_interval) ~ p, data=s)
			if(return.raw == TRUE)
					{
					# generate the index as an ordered factor, with labels in the depth order
					# note that we have to use the sv_clean vector, minus the first element which will always be 0
					s.idx <- factor(rep(sv_clean[-1], times=ncol(x.recon)), ordered=TRUE, levels=sv_clean[-1])
					return( data.frame(seg_interval=s.idx, p=as.vector(x.recon)) )
					}
			
			}
			
		# using a fixed-interval segmenting vector	
		else
			{
			# get the length of our segmented data set
			# and generate a new sequence of depths
			len <- nrow(x.recon)
			l.seq <- 1:len
			dz <- l.seq * seg_size
			
			# generate segment tops and bottoms
			df.top_bottom <- data.frame(top=c(0,dz[-length(dz)]), bottom=c(dz[-len], max_d))
			
			
			# return a dataframe with all values, indexed by segment
			# useful for looking at the distribution of properties by segment
			# bwplot(factor(seg_interval) ~ p, data=s)
			if(return.raw == TRUE)
					{
					# generate the index as an ordered factor, with labels in the depth order
					s.idx <- factor(rep(dz, times=ncol(x.recon)), ordered=TRUE, levels=dz)
					return( data.frame(seg_interval=s.idx, p=as.vector(x.recon)) )
					}

			}
				
		} # segmenting
	
	# no segmenting
	else
		{
		x.recon <- x.recon_original
		
		if(use.wts == TRUE)
			x.recon.wts <- x.recon.wts_original
		
		df.top_bottom <- data.frame(top=0:(max_d-1), bottom=1:max_d)
		}

	
	# compute row-wise summary statistics
	if(prop.class %in% c('numeric','integer'))
		{
		# standard summary statistics
		p.mean <- apply(x.recon, 1, mean, na.rm=TRUE)
		p.sd <- apply(x.recon, 1, conditional.sd)
		
		# quantiles
		q.probs <- c(0.05, 0.25, 0.5, 0.75, 0.95)
		p.quantiles <- data.frame(t(apply(x.recon, 1, quantile, probs=q.probs, na.rm=TRUE)))
		names(p.quantiles) <- paste('p.q', round(q.probs * 100), sep='')
		}
	
	# todo: update this to use a different column
	if(prop.class == 'character')
		{
		# the results of this operation are a list,
		# one element for each depth segment
		
		# get a vector of all possible categories		
		p.unique.classes <- as.vector(na.omit(unique(as.vector(x.recon))))
		
		# tabular frequences for complete set of possible categories
		p.table <- apply(x.recon, 1, function(i) { table(factor(i, levels=p.unique.classes), useNA='no')  } )
		
		
		# convert into a dataframe
		p.freq <- as.data.frame(t(p.table))
		
		# convert into proportions
		p.row.counts <- apply(p.freq, 1, sum)
		p.prop <- sweep(p.freq, 1, STATS=p.row.counts, FUN='/')
		
		## TODO: finish this
		# remove proportions of 0
# 		p.prop[which(p.prop <= 0.000001), ] <- NA
		
		}
		
	if(compute.depth.prob == TRUE)
		{
		p.prop <- apply(x.recon, 1, function(i) sum(i, na.rm=TRUE) / length(i))
		}
		
	# no way to use weights with character vectors yet...
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
	# no weithed stats needed
	else
		{
		if(prop.class %in% c('numeric','integer'))
			{
			df.stats <- data.frame(p.mean, p.sd, p.quantiles)
			}
		if(prop.class == 'character')
			{
			df.stats <- data.frame(p.prop)
			}
		if(compute.depth.prob == TRUE)
			{
			df.stats <- data.frame(p.prop)
			}
		}
	
	
	## form into dataframe for returning to the user 
	# this is usually where we have problems, caused by bad horizon boundaries
	if(nrow(df.top_bottom) == nrow(df.stats))
		{
		x.slotted <- data.frame(df.top_bottom, df.stats)
		}
	# something is wrong
	else
		{
		print("ERROR!")
		print(data)
		}
		
	# done
	return(x.slotted)
	}


##############################################################
## profile classification functions ##
##############################################################

## consider using 'ff' package for file-based storage of VERY large objects. Probably just the dissimilarity matrix

## soil.slot() would be a better approach than using every nth depth slice

## careful -- we are getting the labels for the final dissimilarity matrix from the levels of the profile ID


# Seems to scale to 1000 profiles with 5 variables, could use optimization
# function requires at least two attributes
# hard coded reference to s$id
# set k to 0 for no depth weighting 
profile_compare <- function(s, vars, max_d, k, sample_interval=NA, replace_na=FALSE, add_soil_flag=FALSE, return_depth_distances=FALSE, strict_hz_eval=FALSE)
	{
	
	# currently this will only work with integer depths
	if(!is.integer(na.omit(s$top)) | !is.integer(na.omit(s$bottom)))
		stop('this function can only accept integer horizon depths')
	
	# check to make sure that there is an 'id' column
	if(is.null(s$id))
		stop("'s' must contain a column named 'id' ")
	
	# if the id column is not a factor, convert it to one:
	if(class(s$id) != 'factor')
		s$id <- factor(s$id)
	
	## this is still experimental
	# check for ability to use parallel computations:
	parallel <- checkMC()
		
	
	
	
	# identify the number of profiles
	n.profiles <- length(levels(s$id))
	
	# number of variables
	n.vars <- length(vars)
	
	# sequence describing depth slice indices	
	# use a sequence from 1 ... max depth
	depth_slice_seq <- 1:max_d
	
	# use decimated sampling if requested
	if(!is.na(sample_interval) & sample_interval != 1)
		depth_slice_seq <- depth_slice_seq[depth_slice_seq %% sample_interval == 1]
		
		
	# compute a weighting vector based on k	
	w <- 1 * exp(-k * depth_slice_seq)
	
	# TODO: if max_d < profile depth, s.unrolled is not truncated to max_d
	## unroll each named soil property, for each soil profile
	## the result is a list matricies with dimensions: depth, num_properties 
	# this approach requires a named list of soil properties
	cat(paste("Unrolling ", length(levels(s$id)), " Profiles\n", sep=""))
	s.unrolled <- dlply(s, .(id), .progress='text', .parallel=parallel, .fun=function(di, p=vars, d=max_d, strict=strict_hz_eval) 
		{
		
		# iterate over the set of properties, unrolling as we go
		# the result is a [z by p] matrix unrolled to max_d
		m <- try(sapply(p, function(p_i) unroll(di$top, di$bottom, prop=di[,p_i], max_depth=d, strict=strict) ))
		
		## TODO: could be better
		# check for a non-NULL attribute of 'class'
		# this will only happen when there was an error
		if( !is.null(attr(m, 'class')))
			{
			if(attr(m, 'class') == 'try-error')
				{
				stop(paste('Error: bad horizon structure in soil id', as.character(unique(di$id))))
				}
			}
		else
			return(m)
		}
	)

	
	## NOTE: this will not work when a user-defined interval is used for slicing!!
	## 
	## generate a matrix storing a flag describing soil vs. non-soil at each slice
	## note that this does not take into account missing horizon data within the profile
	if(add_soil_flag)
		{
		# get the depth of each profile
		## could be converted to 'vaggregate' in latest version of plyr
		s.slices_of_soil <- tapply(s$bottom, s$id, function(i) max(i, na.rm=TRUE) )
		
		# truncate to the max requested depth
		s.slices_of_soil <- ifelse(s.slices_of_soil <= max_d, s.slices_of_soil, max_d)
		s.slices_of_non_soil <- max_d - s.slices_of_soil
		
		s.slices_of_soil.length <- length(s.slices_of_soil)
		
		# init a matrix with dimensions: depth slices, number of profiles
		soil.matrix <- matrix(ncol=s.slices_of_soil.length, nrow=max_d)
		
		# fille with TRUE for 'soil' or FALSE for 'non-soil'
		for(s.i in 1:s.slices_of_soil.length)
			soil.matrix[, s.i] <- c(rep(TRUE, s.slices_of_soil[s.i]), rep(FALSE, s.slices_of_non_soil[s.i]))
		
		
		# debugging: plot a diagnostic image
		image(1:n.profiles, 1:max_d, t(soil.matrix), col=c('white','grey'), ylim=c(max_d, 1), xlab='ID', ylab='Depth', main='Soil/Non-Soil Matrix')
		}
		
	
	
	## NOTE: careful iterating over lists with a for() loop, and when there may be a NULL lurking
	# init a list to store distance matrices, one for each depth interval
	d <- vector('list', max(seq_along(depth_slice_seq)))
	
	# init a progress bar
	pb <- txtProgressBar(min=1, max=max(seq_along(depth_slice_seq)), style=3, width=40)
	cat("Computing Dissimilarity Matrices\n")
	
	# 'i' is not the depth slice, rather, the index
	for(i in seq_along(depth_slice_seq))
		{
		# for each z, generate distance matrix
		# note that we have to pass in variable 'i', as this is the 
		# current depth segment
		ps <- sapply(s.unrolled, function(dz, z_i=depth_slice_seq[i]) { dz[z_i,] })
		sp <- t(ps)
		
		# compute distance metric for this depth
		# distance metric has large effect on results
		# Gower's distance gives the best looking results, and automatically standardizes variables
		
		# compute the proportion of cases where NA occurs
		proportion_non_NA <- 1 - (length(which(is.na(sp))) / (n.profiles * n.vars))
		
		# use some criteria for deciding when to keep the dissimilarities for this slice
		if(proportion_non_NA >= 0.5)
			{
			## this is where we run into memory-size limitations
			## an ff object would help here... however it can not preserve all of the information 
			## that a list can... we would need to store these data as raw matrices
			d[[i]] <- daisy(sp, metric='gower')
			}
		
		# otherwise contribute no dissimilarity to the total
		else
			{
			print(paste(round(proportion_non_NA, 2), 'non-missing in slice', depth_slice_seq[i]))
			
			# generate an appropriately formatted dissimilarity matrix, full of NA
			m.ref <- lower.tri(matrix(ncol=n.profiles,nrow=n.profiles), diag=FALSE)
			m.ref[which(m.ref == FALSE)] <- NA
			
			## this is where we run into memory-size limitations
			# assign to this slice
			d[[i]] <- as.dist(m.ref)
			
			# clean-up
			rm(m.ref)
			}
			
		# cleanup: not sure if this helps... seems to
		gc()
			
		# update progress bar
		setTxtProgressBar(pb, i)
		
		
		# debugging information on memory consumption
		cat(paste(" [size of D:", round(object.size(d) / 1024^2, 1), "Mb] "))
		
		}
	# done creating list of slice-wise dissimilarties
	# finish progress bar	
	close(pb)
	
	# clean-up
	rm(s.unrolled) ; gc()
	
	
	# should NA in the dissimilarity matrix be replaced with max(D) ?
	if(replace_na)
		{
		# replace all NA with the MAX distance between any observations
		max.distance <- max(sapply(d, max, na.rm=TRUE))
		
		## note: this will not work with sample_interval set
		# should we use a more expensive approach, that uses the soil/non_soil flag?
		if(add_soil_flag)
			{
			# kind of messy: re-using an object like this
			for(i in 1:length(d))
				{
				d_i <- as.matrix(d[[i]])

				# set all pairs that are made between deep vs. shallow soil
				# to the maximum distance- by row and column
				cells.with.na.rows <- which(is.na(d_i[, which(soil.matrix[i, ])]))
				cells.with.na.cols <- which(is.na(d_i[which(soil.matrix[i, ]), ]))
				
				d_i[, which(soil.matrix[i, ])][cells.with.na.rows] <- max.distance
				d_i[which(soil.matrix[i, ]), ][cells.with.na.cols] <- max.distance
				
				# convert back to dist object
				d_i <- as.dist(d_i)
				
				# copy original attributes
				attributes(d_i) <- attributes(d[[i]])
				
				# save back to original position in list
				d[[i]] <- d_i
				}
			
			# remove the soil.matrix object to save some space
			rm(soil.matrix) ; gc()
			}
		# use a less expensive approach, where all NA are replaced by the max distance
		else
			{
			d <- lapply(d, function(d_i) 
				{
				cells.with.na <- which(is.na(d_i))
				d_i[cells.with.na] <- max.distance
				return(d_i)
				} )
			}
		}
	
	
	
	# perform depth-weighting 	
	for(i in seq_along(depth_slice_seq))
		d[[i]] <- d[[i]] * w[i]
	
	
	# optionally return the distances for each depth, after weighting
	if(return_depth_distances)
		return(d)
	
	
	# compute the total distance, for all dept intervals,
	# by pedon:
	# consider using mean diss, or something different that total
	cat("Computing Profile Total Dissimilarities\n")
	d.vect <- colSums(t(sapply(d, '[')), na.rm=TRUE)
	
	# remove list of dissimilarities to save RAM
	rm(d) ; gc()
	
	# now make into a combined distance matrix
	m.ref <- lower.tri(matrix(ncol=n.profiles, nrow=n.profiles), diag=FALSE)
	m.ref[which(m.ref == FALSE)] <- NA
	m.ref[which(m.ref)] <- d.vect
	
	# remove unformatted disimilarities
	rm(d.vect) ; gc()
	
	# coerce to 'dist' class
	D <- as.dist(m.ref)
	
	# update labels from our list of hz-dissimilarities
	attr(D, 'Labels') <- levels(s$id)
	
	# add distance metric
	attr(D, 'Distance Metric') <- 'Gower'
	
	# return the distance matrix, class = 'dist'
	return(D)	
	}
	
	
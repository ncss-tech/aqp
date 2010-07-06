
##############################################################
## profile classification functions ##
##############################################################

## consider using 'ff' package for file-based storage of VERY large objects. Probably just the dissimilarity matrix

## soil.slot() would be a better approach than using every nth depth slice


# Seems to scale to 1000 profiles with 5 variables, could use optimization
# TODO: convert soil_flag into a factor
# function requires at least two attributes
# hard coded reference to id
# seems to work with different total depths... need to check
# set k to 0 for no depth weighting 
profile_compare <- function(s, vars, max_d, k, sample_interval=NA, replace_na=FALSE, add_soil_flag=FALSE, return_depth_distances=FALSE)
	{
	
	# check to make sure that there is an 'id' column
	if(is.null(s$id))
		stop("'s' must contain a column named 'id' ")
	
	# if the id column is not a factor, convert it to one:
	if(class(s$id) != 'factor')
		s$id <- factor(s$id)
	
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
	
	
	# this approach requires a named list of soil properties
	cat(paste("Unrolling ", length(levels(s$id)), " Profiles\n", sep=""))
	s.unrolled <- dlply(s, .(id), .progress='text', .fun=function(di, p=vars, d=max_d) 
		{
		# init a temp list
		l <- list()
		# iterate over named properties:
		for(p_i in p)
			{
			# unroll each named property to matching component of our list
			# if the profiles are shallower than max_depth, padd with NA
			l[[p_i]] <- unroll(di$top, di$bottom, prop=di[,p_i], max_depth=d)
			}
			
		
		
		# add a soil flag as one of the attributes 
		# doesn't seem to help-- resulting groupings do not make sense
		if(add_soil_flag)
			{
			# generate a soil flag: 1=soil, 0=not soil
			max_soil_depth <- max(di$bottom)
			if(max_soil_depth >= d)
				remaining_non_soil <- 0
			else
				remaining_non_soil <- d - max_soil_depth
				
			l$soil_flag <- factor(c(rep(1, times=max_soil_depth), rep(0, times=remaining_non_soil )))
			}
			
		# convert list into z by p matrix
		m <- sapply(l, '[')
		}
	)
	
		
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
	# finish progress bar	
	close(pb)
	
	if(replace_na)
		{
		# replace all NA with the MAX distance between any observations
		max.distance <- max(sapply(d, max, na.rm=TRUE))
		d <- lapply(d, function(d_i) {d_i[which(is.na(d_i))] <- max.distance ; return(d_i)} )
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
	d.vect <- apply(t(sapply(d, '[')), 2, sum, na.rm=TRUE)
	
	# now make into a combined distance matrix
	m.ref <- lower.tri(matrix(ncol=n.profiles, nrow=n.profiles), diag=FALSE)
	m.ref[which(m.ref == FALSE)] <- NA
	m.ref[which(m.ref)] <- d.vect
	
	# coerce to 'dist' class
	D <- as.dist(m.ref)
	
	# update labels from our list of hz-dissimilarities
	attr(D, 'Labels') <- attr(d[[1]], 'Labels')
	
	# add distance metric
	attr(D, 'Distance Metric') <- 'Gower'
	
	# return the distance matrix, class = 'dist'
	return(D)	
	}
	
	
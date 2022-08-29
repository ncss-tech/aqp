
##############################################################
## profile classification functions ##
##############################################################


## lots TODO: https://github.com/ncss-tech/aqp/issues/7


# define function for summing a list of dissimilarity matrices
# that have had NA replaced with 0
.SumDistanceList <- function(x) Reduce("+", x)



## low-level function that the user will probably not ever use directly
# Seems to scale to 1000 profiles with 5 variables, could use optimization
# function requires at least two attributes
# hard coded reference to s$id
# set k to 0 for no depth weighting
#' Numerical Soil Profile Comparison
#'
#' Performs a numerical comparison of soil profiles using named properties,
#' based on a weighted, summed, depth-segment-aligned dissimilarity
#' calculation. If \code{s} is a \code{\link{SoilProfileCollection}},
#' site-level variables (2 or more) can also be used. The site-level and
#' horizon-level dissimilarity matrices are then re-scaled and averaged.
#'
#' Variability in soil depth can interfere significantly with the calculation
#' of between-profile dissimilarity-- what is the numerical ``distance'' (or
#' dissimilarity) between a slice of soil from profile A and the corresponding,
#' but missing, slice from a shallower profile B? Gower's distance metric would
#' yield a NULL distance, despite the fact that intuition suggests otherwise:
#' shallower soils should be more dissimilar from deeper soils. For example,
#' when a 25 cm deep profile is compared with a 50 cm deep profile, numerical
#' distances are only accumulated for the first 25 cm of soil (distances from
#' 26 - 50 cm are NULL). When summed, the total distance between these profiles
#' will generally be less than the distance between two profiles of equal
#' depth. Our algorithm has an option (setting replace_na=TRUE) to replace NULL
#' distances with the maximum distance between any pair of profiles for the
#' current depth slice. In this way, the numerical distance between a slice of
#' soil and a corresponding slice of non-soil reflects the fact that these two
#' materials should be treated very differently (i.e. maximum dissimilarity).
#'
#' This alternative calculation of dissimilarities between soil and non-soil
#' slices solves the problem of comparing shallow profiles with deeper
#' profiles. However, it can result in a new problem: distances calculated
#' between two shallow profiles will be erroneously inflated beyond the extent
#' of either profile's depth. Our algorithm has an additional option (setting
#' add_soil_flag=TRUE) that will preserve NULL distances between slices when
#' both slices represent non-soil material. With this option enabled, shallow
#' profiles will only accumulate mutual dissimilarity to the depth of the
#' deeper profile.
#'
#' Note that when the \code{add_soil_flag} option is enabled (default), slices
#' are classified as 'soil' down to the maximum depth to which at least one of
#' variables used in the dissimilarity calculation is not NA. This will cause
#' problems when profiles within a collection contain all NAs within the
#' columns used to determine dissimilarity. An approach for identifying and
#' removing these kind of profiles is presented in the examples section below.
#'
#' A notice is issued if there are any NA values within the matrix used for
#' distance calculations, as these values are optionally replaced by the max
#' dissimilarity.
#'
#' Our approach builds on the work of (Moore, 1972) and the previously
#' mentioned depth-slicing algorithm.
#'
#' @aliases pc pc.SPC profile_compare
#' profile_compare,SoilProfileCollection-method
#' profile_compare,data.frame-method
#' @docType methods
#' @param s a dataframe with at least 2 columns of soil properties, and an 'id'
#' column for each profile. horizon depths must be integers and
#' self-consistent, or a \code{SoilProfileCollection} object
#' @param vars A vector with named properties that will be used in the
#' comparison. These are typically column names describing horizon-level
#' attributes (2 or more), but can also contain site-level attributes (2 or
#' more) if \code{s} is a \code{\link{SoilProfileCollection}}.
#' @param max_d depth-slices up to this depth are considered in the comparison
#' @param k a depth-weighting coeficient, use '0' for no depth-weighting (see
#' examples below)
#' @param filter an index used to determine which horizons (rows) are included
#' in the analysis
#' @param sample_interval use every n-th depth slice instead of every depth
#' slice, useful for working with > 1000 profiles at a time
#' @param replace_na if TRUE, missing data are replaced by maximum
#' dissimilarity (TRUE)
#' @param add_soil_flag The algorithm will generate a 'soil'/'non-soil' matrix
#' for use when comparing soil profiles with large differences in depth (TRUE).
#' See details section below.
#' @param return_depth_distances return intermediate, depth-wise dissimilarity
#' results (FALSE)
#' @param strict_hz_eval should horizons be strictly checked for internal
#' self-consistency? (FALSE)
#' @param progress 'none' (default): argument passed to \code{ddply} and
#' related functions, see \code{\link{create_progress_bar}} for all possible
#' options; 'text' is usually fine.
#' @param plot.depth.matrix should a plot of the 'soil'/'non-soil' matrix be
#' returned (FALSE)
#' @param rescale.result should the result be rescaled by dividing by max(D)
#' (FALSE)
#' @param verbose extra debug output (FALSE)
#' @return A dissimilarity matrix object of class 'dissimilarity, dist',
#' optionally scaled by max(D).

#' @author Dylan E. Beaudette
#' @seealso \code{\link{slice}}, \code{\link{daisy}}
#' @references
#'  - D.E. Beaudette, P. Roudier, A.T. O'Geen, Algorithms for quantitative pedology: A toolkit for soil scientists, Computers & Geosciences, Volume 52, 2013, Pages 258-268, ISSN 0098-3004, \doi{10.1016/j.cageo.2012.10.020}.
#'  - Moore, A.; Russell, J. & Ward, W. Numerical analysis of soils: A comparison of three soil profile models with field classification. Journal of Soil Science, 1972, 23, 194-209.
#' @keywords methods manip
#' @examples
#'
#' \donttest{
#' ## 1. check out the influence depth-weight coef:
#' library(lattice)
#'
#' z <- rep(1:100,4)
#' k <- rep(c(0,0.1,0.05,0.01), each=100)
#' w <- 100*exp(-k*z)
#'
#' xyplot(z ~ w, groups=k, ylim=c(105,-5), xlim=c(-5,105), type='l',
#'        ylab='Depth', xlab='Weighting Factor', asp=1.5,
#'        auto.key=list(columns=4, lines=TRUE, points=FALSE, title="k", cex=0.8, size=3),
#'        panel=function(...) {
#'          panel.grid(h=-1,v=-1)
#'          panel.superpose(...)
#'        }
#' )
#'
#' # more soil properties
#' data(sp2)
#' depths(sp2) <- id ~ top + bottom
#'
#' d.1 <- profile_compare(sp2, vars=c('prop','field_ph','hue','value'),
#'                        max_d=100, k=0.01, plot.depth.matrix=TRUE)
#'
#' # add some missing data:
#' sp2$prop[1:2] <- NA
#' d.2 <- profile_compare(sp2, vars=c('prop','field_ph','hue','value'),
#'                        max_d=100, k=0.01, plot.depth.matrix=TRUE)
#'
#' # note small changes in D:
#' cor(d.1, d.2)
#'
#' ## 3. identify profiles within a collection that contain all NAs
#' set.seed(1010101)
#' s <- pbindlist(lapply(letters[1:10], random_profile, SPC=TRUE))
#'
#' # replace first profile's data with NA
#' na.required <- nrow(s[1, ])
#' s$p1[1:na.required] <- NA
#' s$p2[1:na.required] <- NA
#'
#' # attempt profile comparison: this won't work, throws an error
#' d <- profile_compare(s, vars=c('p1','p2'), max_d=100, k=0)
#'
#' # check for soils that are missing all clay / total RF data
#' f.check.NA <- function(i) length(which(is.na(i$p1) | is.na(i$p2))) / nrow(i) == 1
#' missing.too.much.data.idx <- which(profileApply(s, f.check.NA))
#'
#' # remove bad profiles and try again: works
#' s.no.na <- profile_compare(s[-missing.too.much.data.idx, ],
#'                            vars=c('p1','p2'),
#'                            max_d=100, k=0, plot.depth.matrix=TRUE)
#'
#' ## 4. better plotting of dendrograms with ape package:
#' if(require(ape) & require(cluster) & require(MASS)) {
#'
#'   data(sp2)
#'   depths(sp2) <- id ~ top + bottom
#'   site(sp2) <- ~ surface
#'
#'   d <- profile_compare(sp2, vars=c('prop','field_ph','hue','value'),
#'                          max_d=100, k=0)
#'
#'   h <- diana(d)
#'   p <- as.phylo(as.hclust(h))
#'   plot(p, show.tip.label=FALSE)
#'   tiplabels(sp2$surface, col=cutree(h, 3), bg=NA, cex=0.75)
#'
#'   ## 5. other uses of the dissimilarity matrix
#'   # Sammon Mapping: doesn't like '0' values in dissimilarity matrix
#'   d.sam <- sammon(d)
#'
#'   # simple plot
#'   dev.off() ; dev.new()
#'   plot(d.sam$points, type = "n", xlim=range(d.sam$points[,1] * 1.5))
#'   text(d.sam$points, labels=row.names(as.data.frame(d.sam$points)),
#'        cex=0.75, col=cutree(h, 3))
#'
#' }
#'
#'
#' ## 6. try out the 'sample_interval' argument
#' # compute using sucessively larger sampling intervals
#' data(sp3)
#' d <- profile_compare(sp3, vars=c('clay','cec','ph'),
#'                      max_d=100, k=0.01)
#' d.2 <- profile_compare(sp3, vars=c('clay','cec','ph'),
#'                        max_d=100, k=0.01, sample_interval=2)
#' d.10 <- profile_compare(sp3, vars=c('clay','cec','ph'),
#'                         max_d=100, k=0.01, sample_interval=10)
#' d.20 <- profile_compare(sp3, vars=c('clay','cec','ph'),
#'                         max_d=100, k=0.01, sample_interval=20)
#'
#' # check the results via hclust / dendrograms
#' oldpar <- par(mfcol=c(1,4), mar=c(2,1,2,2))
#' plot(as.dendrogram(hclust(d)), horiz=TRUE, main='Every Depth Slice')
#' plot(as.dendrogram(hclust(d.2)), horiz=TRUE, main='Every 2nd Depth Slice')
#' plot(as.dendrogram(hclust(d.10)), horiz=TRUE, main='Every 10th Depth Slice')
#' plot(as.dendrogram(hclust(d.20)), horiz=TRUE, main='Every 20th Depth Slice')
#' par(oldpar)
#'
#' }
#'
pc <- function(s, vars, max_d, k, filter=NULL, sample_interval=NA, replace_na=TRUE,
add_soil_flag=TRUE, return_depth_distances=FALSE, strict_hz_eval=FALSE, progress='none',
plot.depth.matrix=FALSE, rescale.result=FALSE, verbose=FALSE) {

	# currently this will only work with integer depths
	# test by attempting to cast to integers
	# if there is no difference, then we are fine
	top.test <- any( ! as.integer(na.omit(s$top)) == na.omit(s$top))
	bottom.test <- any( ! as.integer(na.omit(s$bottom)) == na.omit(s$bottom))
	if(top.test | bottom.test)
		stop('this function can only accept integer horizon depths', call.=FALSE)

	## TODO: this should be updated
	# check to make sure that there is an 'id' column
	if(is.null(s$id))
		stop("'s' must contain a column named 'id' ", call.=FALSE)

	## TODO: evaluate the practical implications of this
	# optionally filter the data
	if(!missing(filter)) {
		s <- s[filter, ]
	}

	# identify the number of profiles
  # n.profiles <- length(s)
	n.profiles <- length(unique(s$id))

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

	## TODO: convert to dice() (#7)
	## BUG: !!! this step re-orders via string-factor-string conversion step by split() (#7)
	## --> this is only a bug when profile IDs have been altered after SPC init
	## --> https://stackoverflow.com/questions/17611734/r-split-preserving-natural-order-of-factors
	##
	## unroll each named soil property, for each soil profile
	## the result is a list matrices with dimensions: depth, num_properties
	# this approach requires a named list of soil properties
	s.unrolled <- dlply(s, "id", .progress=progress, .fun=function(di, p=vars, d=max_d, strict=strict_hz_eval, .parallel=getOption('AQP_parallel', default=FALSE)) {

		# iterate over the set of properties, unrolling as we go
		# the result is a [z by p] matrix unrolled to max_d
		m <- try(sapply(p, function(p_i) unroll(di$top, di$bottom, prop=di[, p_i], max_depth=d, strict=strict) ))

		## TODO: could be better
		# check for a non-NULL attribute of 'class'
		# this will only happen when there was an error
		if( !is.null(attr(m, 'class'))) {
			if(attr(m, 'class') == 'try-error') {
				stop(paste('Error: bad horizon structure in soil id', as.character(unique(di$id))), call.=FALSE)
				}
			}
		else
			return(m)
		}
	)


	## generate a matrix storing a flag describing soil vs. non-soil at each slice
	## note that this will truncate a profile to the max depth of actual data
	## profiles missing data in all variables will cause function to stop here
	if(add_soil_flag){

		# keep temp subset of the data so that soil/non-soil matrix is
		# evaluated based on presence of real data in at least 1 variable
		s.sub <- na.omit(s[, c('id', 'top', 'bottom', vars)])

    ## BUG!!! tapply() re-orders the results based on sorting of s.sub$id (#7)
    ## ----> this will cause problems when input isn't sorted by ID
		# get the depth of each profile
		s.slices_of_soil <- tapply(s.sub$bottom, s.sub$id, max, na.rm=TRUE)

		# truncate to the max requested depth
		s.slices_of_soil <- ifelse(s.slices_of_soil <= max_d, s.slices_of_soil, max_d)
		s.slices_of_non_soil <- max_d - s.slices_of_soil

		s.slices_of_soil.length <- length(s.slices_of_soil)

		# init a matrix with dimensions: depth slices, number of profiles
		soil.matrix <- matrix(ncol=s.slices_of_soil.length, nrow=max_d)

		# fill with TRUE for 'soil' or FALSE for 'non-soil'
		for(s.i in 1:s.slices_of_soil.length) {
			soil.matrix[, s.i] <- c(rep(TRUE, s.slices_of_soil[s.i]), rep(FALSE, s.slices_of_non_soil[s.i]))
		}

		# plot a diagnostic image, may not be useful for n > 100 profiles
		if(plot.depth.matrix) {
			# define color scheme: if all TRUE, then we only need 1 color
			if(length(table(soil.matrix)) > 1)
				image.cols <- c(NA, 'grey')
			else
				image.cols <- c('grey')

			## TODO: use SPC methods, unique() will not return labels in the correct order
      # labs <- profile_id(s)
		  labs <- unique(s.sub$id)
		  image(x=1:n.profiles, y=1:max_d, z=t(soil.matrix), col=image.cols, ylim=c(max_d, 0), xlab='ID', ylab='Slice Number (usually eq. to depth)', main='Soil / Non-Soil Matrix', axes=FALSE)
		  box()
		  abline(v=seq(1, n.profiles)+0.5, lty=2)
		  axis(side=2, at=pretty(c(0, depth_slice_seq)), las=1)
		  axis(side=1, at=1:n.profiles, labels=labs, las=2, cex.axis=0.75)
		  }

		# cleanup
		rm(s.sub)
		}
	
	

	##
	## new version for computing slice-wise dissimilarities... fast!
	##
	message(paste('Computing dissimilarity matrices from', n.profiles, 'profiles'), appendLF=FALSE)

	## TEMP HACK to supress warnings generated by calling daisy with all NA input (#7)
	ow <- options('warn')
	options(warn=-1)

	d <- llply(depth_slice_seq, .parallel=getOption('AQP_parallel', default=FALSE), .progress=progress, .fun=function(i, su=s.unrolled) {
    
	  
	  ## 2021-03-03
	  ## this approach breaks when using a single variable for the distance calc
	  ## sp is a 1-row matrix which returns an empty distance matrix
	  ## solution: don't attempt to fix here, re-write the entire thing
	  
	  ## this could be a source of slowness, esp. the t()
	  ## TODO: new implementation will require drop=FALSE
	  ps <- sapply(su, function(dz, z_i=depth_slice_seq[i]) { dz[z_i,] })
	  sp <- t(ps)

		# compute distance metric for this depth
		# distance metric has large effect on results
		# Gower's distance gives the best looking results, and automatically standardizes variables

		## this is where we run into memory-size limitations
		## an ff object would help here... however it can not preserve all of the information
		## that a list can... we would need to store these data as raw matrices
    
	  ## TODO: sometimes we don't want to standardize, and use Euclidean distance
	  
	  ## TODO: don't call daisy on bogus input data, temp fix: disable warnings (#7)
	  ## if all of the input to daisy is NA, then we get warnings from min() and max()
	  ## this happens when we set a max depth that is beyond most profiles
	  d.i <- daisy(sp, metric='gower')
		return(d.i)
	  }
	)
  # reset warning options
	options(ow)

	## TODO: does this actually do anything?
	# clean-up
	rm(s.unrolled) ; gc()

	# print total size of D
	message(paste(" [", round(object.size(d) / 1024^2, 2), " Mb]", sep=''))

	# should NA in the dissimilarity matrix be replaced with max(D) ?
	if(replace_na) {
		# replace all NA with the MAX distance between any observations
		# note that down deep, there may not be enough data for any pair-wise comparisons
		# therefore, we should not attempt to calculate max() on a matrix of all NA
		max.distance.vect <- sapply(d, function(i) if(all(is.na(i))) NA else max(i, na.rm=TRUE))
		max.distance <- max(max.distance.vect, na.rm=TRUE)

		## note: this will not work with sample_interval set
		# should we use a more expensive approach, that uses the soil/non_soil flag?
		if(add_soil_flag) {
			# kind of messy: re-using an object like this
			for(i in 1:length(d)) {
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
		else {
			d <- lapply(d, function(d_i)
				{
				cells.with.na <- which(is.na(d_i))
				d_i[cells.with.na] <- max.distance
				return(d_i)
				} )
			}
		}



	# optionally return the distances for each depth
	# depth-weighting is performed, but NA is not converted to 0
	if(return_depth_distances) {
		# depth-weighting
		for(i in seq_along(depth_slice_seq))
			d[[i]] <- d[[i]] * w[i]
		return(d)
		}


	# final tidy-ing of the list of dissimilarity matrices
	for(i in seq_along(depth_slice_seq)) {
		# convert NA -> 0
		na.idx <- which(is.na(d[[i]]))
		if(length(na.idx) > 0)
			d[[i]][na.idx] <- 0

		# depth-weighting
		d[[i]] <- d[[i]] * w[i]
		}


	# reduce list of dissimilarity matrices by summation
	D <- .SumDistanceList(d)

	# add distance metric
	attr(D, 'Distance Metric') <- 'Gower'

	# remove previous warnings about NA
	attr(D, 'NA.message') <- NULL

	# normalize by dividing by max(D)
	# this is important when incorporating site data
	# causes problems for some functions like sammon
	if(rescale.result)
		D <- D/max(D, na.rm=TRUE)

	## DEBUG
	if(verbose) {
	  cat('depth-slice seq:\n')
	  print(depth_slice_seq)

	  cat('depth-weighting vector:\n')
	  print(round(w, 2))

	  cat(paste('max dissimilarity:', max.distance, '\n'))
	}


	# return the distance matrix, class = 'dissimilarity, dist'
	return(D)
	}


pc.SPC <- function(s, vars, rescale.result=FALSE, ...){

	if(!requireNamespace("scales", quietly = TRUE))
	  stop("package `scales` is required", call.=FALSE)

  # default behavior: do not normalize D

  ## 2016-02-22: check for missing data moved from pc() to here
  ## TODO: this makes an assumption on the column containing horizon designations
  ## 2016-08-16: this function ignores vars that don't existin in @horizons

  ## 2019-12-19: disabled until PC is re-factored and / or there is a better way to get hz name column
  # # iterate over profiles and compute percent missing data by variable
  # pct_data <- evalMissingData(s, vars, name = name)
  #
  # ## TODO: review this, and make optional via argument (#7)
  # # keep track of profiles missing any or all of their data
  # problem.profiles.idx <- which(pct_data < 1)
  # # bad.profiles.idx <- which(pct_data == 0)
  #
  # if(length(problem.profiles.idx) > 0) {
  #   # assign('problem.profiles', value=pct_missing[problem.profiles.idx,], envir=aqp.env)
  #   message('Missing data will bias results, check inputs.')
  # }


  ## 2016-02-22: disabled for now
#   if(length(bad.profiles.idx) > 0) {
#     # stop stop and let the user know
#     bad.profiles <- profile_id(s)[bad.profiles.idx]
#     stop(paste('no non-NA values associated with profiles:', paste(bad.profiles, collapse=', '), '\nConsider removing these profiles and re-running.'), call.=FALSE)
#   }

  ## 2020-06-19: DEB
  ## temporary fix for #7, related to profile ID ordering in site, horizon, and results from tapply()
  ## this is only a problem when using profile IDs that are numeric and not stable when alpha-sorted
  ##
  ## However, this will result in changes to sorting of profile_id(), @site, @horizon
  ##
  ## Also, this will break with merge of aqpdf branch were sorting by ID is not performed at init time
  ##
  ## this was a dump idea -> going to remove these comments after re-write
  # s <- rebuildSPC(s)


	# extract horizons
	s.hz <- horizons(s)

	# extract site
	s.site <- site(s)
	sn <- names(s.site)

	# check for any site data, remove and a save for later
	if(any(vars %in% sn)) {

		# extract site-level vars
		matching.idx <- na.omit(match(sn, vars))
		site.vars <- vars[matching.idx]

		# remove from hz-level vars
		vars <- vars[-matching.idx]

		## TODO: BUG!!! horizon data are rescaled via D/max(D) !!!
		## TODO: allow user to pass-in variable type information
		# compute dissimilarty on site-level data: only works with 2 or more variables
		# rescale to [0,1]

		message(paste('site-level variables included:', paste(site.vars, collapse=', ')))
		d.site <- daisy(s.site[, site.vars, drop=FALSE], metric='gower')
		
		# re-scale to [0,1]
		d.site <- .rescaleRange(d.site, x0 = 0, x1 = 1)

		# reset default behavior of hz-level D
		rescale.result=TRUE

		## TODO: there might be cases where we get an NA in d.site ... seems like it happens with boolean variables
		## ... but why ? read-up on daisy
		if(any(is.na(d.site))) {
		  warning('NA in site-level dissimilarity matrix, replacing with min dissimilarity', call.=FALSE)
		  d.site[which(is.na(d.site))] <- min(d.site, na.rm=TRUE)
		}

		## TODO: ordering of D_hz vs D_site ... assumptions safe?

	} else {
	  # setup a dummy D_site
	  d.site <- NULL
	}


	##
	## TODO: update this next part
	##
	# add old-style, hard-coded {id, top, bottom} column names
	s.hz$id <- s.hz[[idname(s)]]
	hzDepthCols <- horizonDepths(s)
	s.hz$top <- s.hz[[hzDepthCols[1]]]
	s.hz$bottom <- s.hz[[hzDepthCols[2]]]

	# invoke data.frame method
	res <- profile_compare(s.hz, vars=vars, rescale.result=rescale.result, ...)

	# if we have site-level data and a valid D_site
	# combine via weighted average: using weights of 1 for now
	if(inherits(d.site, 'dist')) {
		res <- 	(res + d.site) / 2
		# re-scale to [0,1]
		res <- .rescaleRange(res, x0 = 0, x1 = 1)
	}

	## fail-safe check on ordering of input profile IDs vs. labels
	## #7
	test.labels <- ! profile_id(s) == attributes(res)$Labels
	if(any(test.labels)) {
	  warning('SPC / distance matrix IDs out of order, soon to be fixed (#7)', call. = FALSE)
	}

	# result is a distance matrix
	return(res)
}

##############
## S4 stuff ##
##############

## NOTE: don't mess with this!
# setup generic function
# if (!isGeneric("profile_compare"))
setGeneric("profile_compare", function(s, ...) standardGeneric("profile_compare"))

# temp interface to SPC class objects

setMethod(f='profile_compare', signature='SoilProfileCollection', definition=pc.SPC)

# temp interface for dataframes
setMethod(f='profile_compare', signature='data.frame', definition=pc)

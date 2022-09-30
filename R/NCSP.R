

## TODO: this can be run in parallel


#' @title Internal wrapper for distance calculations by NCSP
#'
#' @param m `data.frame`, rows are individuals columns are characteristics, IDs should be saved in rownames
#' 
#' @param sm vector, a single row from the soil/non-soil matrix, must be equal to `nrow(m)`
#' 
#' @param w numeric vector of length `ncol(m)`, optionally specified for weighted distance calc
#' @param ... additional arguments to `cluster::daisy()`
#'
#' @return dissimilarity object (cluster package)
#' 
#' @keywords internal
#' @noRd
#'
#' @examples
.NCSP_distanceCalc <- function(m, sm, w = NULL) {
  
  # maximum distance used to replace soil + non-soil distances
  # set to 1 for metric = gower
  d.max <- 1
  
  # distance used to replace (NA) non-soil + non-soil distances
  d.notsoil <- 0
  
  # distance used to replace NA (NA in m, not related to non-soil evaluation)
  d.NA <- 0
  
  # sanity checking on w is performed outside of this function
  if(!is.null(w)) {
    # weighted distances
    d <- cluster::daisy(m, metric = 'gower', weights = w) 
  } else {
    # standard, un-weighted distances
    d <- cluster::daisy(m, metric = 'gower')
  }
  
  
  # save original attributes from daisy()
  a <- attributes(d)
  
  # convert to full matrix for manipulation by row/col index
  d <- as.matrix(d)
  
  # locate soil / non-soil pairs
  # TRUE = soil | FALSE = non-soil
  idx.notsoil <- which(!sm)
  
  # of soil / non-soil pairs exist, process accordingly
  if(length(idx.notsoil) > 0) {
    
    ## create logical matrix of soil + non-soil co-occurences
    ## --> set to MAX distance
    #
    # soil XOR non-soil -> TRUE
    # soil XOR soil -> FALSE
    # non-soil XOR non-soil -> FALSE
    sm.mat <- outer(sm, sm, FUN = xor)
    d[which(sm.mat)] <- d.max
    
    
    ## create logical matrix of non-soil + non-soil co-occurences
    ## --> set to d.notsoil
    #
    # !soil AND !non-soil -> FALSE
    # !soil AND !soil -> FALSE
    # !non-soil AND !non-soil -> FALSE
    sm.mat <- outer(!sm, !sm, FUN = '&')
    d[which(sm.mat)] <- d.notsoil
    
    ## any remaining NA are related to missing data in m
    ## --> set to distance given to missing data
    d[which(is.na(d))] <- d.NA
    
    # reset diagonal
    diag(d) <- 0
  }
  
  
  ## replace remaining NA (missing data in m) with min distance
  idx.NA <- which(is.na(d))
  d[idx.NA] <- d.NA
  
  
  # back to reduced dist form and reset attributes
  d <- as.dist(d)
  attributes(d) <- a
  
  return(d)
  
}





## name suggested by Jon Maynard
## ideas, commentary, updates c/o Maynard et al. 2020


#' @title Numerical Classification of Soil Profiles
#' @description Replaces `profile_compare()`.
#' 
#' Performs a numerical comparison of soil profiles using named properties,
#' based on a weighted, summed, depth-segment-aligned dissimilarity
#' calculation. The site-level and
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
#' Slices
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
#' 
#' @param x x
#' @param vars x
#' @param var.wt x
#' @param maxDepth x
#' @param k x
#' @param rescaleResult x
#' @param progress x
#' @param verbose x
#' @param returnDepthDistances x
#'
#' @noRd


## TODO:
# * finish testing / comparing
# * benchmarking 
# * integrate dist(site data)
# * weighted mean of D_hz + D_site, user-defined weights
# * expose dice() fm
# * parallel operation
# * progress bar for large SPCs


NCSP <- function(x, 
                  vars, 
                  var.wt = rep(1, times = length(vars)), 
                  maxDepth = max(x), 
                  k = 0, 
                  rescaleResult = FALSE, 
                  progress = TRUE,
                  verbose = TRUE, 
                  returnDepthDistances = FALSE
) {

  
  ## depreciated arguments
  
  # return_depth_distances
  # rescale.result
  # max_d
  # filter
  # sample_interval
  # replace_na
  # add_soil_flag
  # strict_hz_eval
  # plot.depth.matrix
  # verbose
  
  
  ## sanity checks
  
  # x must be a SPC
  if(! inherits(x, 'SoilProfileCollection')) {
    stop('`x` must be a SoilProfileCollection', call. = FALSE)
  }
  
  # vars
  if(! all(vars %in% names(x))) {
    stop('`vars` must specify horizon or site level attributes of `x`', call. = FALSE)
  }
  
  # maxDepth
  if(maxDepth > max(x) | maxDepth < 1) {
    stop('`maxDepth` should be > 0 and <= max(`x`)', call. = FALSE)
  }
  
  
  
  ## TODO: reconsider hard-coded top depth
  ## truncate at maxDepth
  x <- trunc(x, 0, maxDepth)
  
  
  
  ## variables used in NCSP algorithm
  
  # number of variables
  n.vars <- length(vars)
  
  # compute a weighting vector based on k
  # indexed to distance matrix list
  w <- 1 * exp(-k * 1:maxDepth)
  
  ## split horizon / site vars
  hn <- horizonNames(x)
  sn <- siteNames(x)
  h.vars <- intersect(hn, vars)
  s.vars <- intersect(sn, vars)
  
  
  ## dice according to depthSequence and vars
  # preserve SPC for access to all site data
  # will have to think about arguments to dice() a little more
  # it could be better to perform sanity checks on horizonation outside of this function
  
  .fm <- as.formula(
    sprintf('0:%s ~ %s',
            maxDepth - 1,
            paste0(h.vars, collapse = ' + '))
  )
  
  ## TODO: consider exposing depth logic subset options in arguments to NCSP
  ##       for now, entire profiles are subset
  
  ## TODO: expose LHS of formula to dice()
  ## NOTE: dice() LHS usually starts from 0, sliceSequence and soil.matrix are indexed from 1
  
  ## dice
  # pctMissing is used to develop soil/non-soil matrix
  s <- suppressMessages(dice(x, fm = .fm, SPC = TRUE, fill = TRUE, byhz = FALSE, pctMissing = TRUE, strict = TRUE))
  
  # number of profiles, accounting for subset via dice()
  n.profiles <- length(s)
  
  # keep track of removed profiles, due to hz logic errors
  .removed.profiles <- metadata(s)$removed.profiles
  if(length(nchar(.removed.profiles)) > 0) {
    warning('hz depth logic subset has removed some profiles')
  }
  
  ## slice sequence
  sliceSequence <- 1:max(s)
  
  ## add soil flag, previously optional now required
  # using same data structure / conventions as profile_compare()
  # logical matrix [sliceSequence, 1:n.profiles]
  #                       [slices, profiles]
  
  ## TODO: ensure that in-line NA are correctly handled
  ## TODO: in-line NA may require another argument to determine assumptions
  ## TODO: define soil / non-soil using pattern matching on horizon designation
  
  # use a vector of horizon (slice) indices to extract all pctMissing values
  # develop a matrix from these
  soil.matrix <- matrix(
    s[, sliceSequence][['.pctMissing']], 
    ncol = n.profiles, 
    nrow = length(sliceSequence), 
    byrow = FALSE
  )
  
  # "soil" has a pctMising very close to 0
  soil.matrix <- soil.matrix < 0.00001
  # keep track of profile IDs
  dimnames(soil.matrix)[[2]] <- profile_id(s)
  
  # ## previous version, quite slow
  # # derive soil / non-soil matrix 
  # # rows are links to depth slices defined in depthSequence vect
  # soil.matrix <- profileApply(s, simplify = FALSE, FUN = function(i) {
  #   
  #   # working only with horizon variables
  #   .h <- horizons(i)[, h.vars, drop = FALSE]
  #   
  #   ## TODO: use complete.cases, or depth to ALL missing data?
  #   
  #   # indices to slices not missing ALL hz vars
  #   .fulldata_idx <- which(
  #     apply(.h[, h.vars, drop = FALSE], MARGIN = 1, FUN = function(j) {
  #     all(!is.na(j))
  #   })
  #   )
  #   
  #   ## TODO: ensure that nrow(soil.matrix) is correctly defined
  #   
  #   # soil = TRUE / non-soil = FALSE
  #   # .res is initialized with all FALSE
  #   .res <- vector(mode = 'logical', length = length(sliceSequence))
  #   .res[.fulldata_idx] <- TRUE
  #   
  #   return(.res)
  #   
  # })
  # 
  # # flatten soil matrix
  # # rows are depth slices
  # # columns are individuals
  # soil.matrix <- do.call('cbind', soil.matrix)
  
  
  ## evaluate distances by slice
  ## accounting for soil/non-soil comparisons
  ## filling NA due to missing data
  .d <- list()
  .ids <- profile_id(s)
  
  ## TODO: basic progress reporting
  
  ## TODO: convert this to parallel evaluation, maybe furrr package
  message(paste('Computing dissimilarity matrices from', n.profiles, 'profiles'), appendLF = FALSE)
  for(i in sliceSequence) {
    
    # horizon data for slice i
    .s <- horizons(s[, i])
    # characteristics for slice i
    .s <- .s[, vars, drop = FALSE]
    row.names(.s) <- .ids
    
    .d[[i]] <- .NCSP_distanceCalc(m = .s, sm = soil.matrix[i, ], w = var.wt)
  }
  
  
  ## optionally return list of distance matrices
  if(returnDepthDistances) {
    # depth-weighting
    for(i in sliceSequence)
      .d[[i]] <- .d[[i]] * w[i]
    return(.d)
  }
  
  # print total size of D
  message(paste(" [", signif(object.size(.d) / 1024^2, 1), " Mb]", sep=''))
  
  ## flatten list of distance matrices
  .d <- Reduce('+', .d)
  
  
  ## optionally normalize by dividing by max(D)
  # this is important when incorporating site data
  # TODO: causes problems for some functions like MASS::sammon() ?
  if(rescaleResult) {
    .d <- .d / max(.d, na.rm = TRUE)
  }
    
  
  
  ## metadata
  # distance metric 
  attr(.d, 'Distance Metric') <- 'Gower'
  
  # removed profiles, if any
  attr(.d, 'removed.profiles') <- .removed.profiles

  # remove warnings about NA from cluster::daisy()
  attr(.d, 'NA.message') <- NULL
  
  # done
  return(.d)
  
  
  
  # # keep temp subset of the data so that soil/non-soil matrix is
  # # evaluated based on presence of real data in at least 1 variable
  # s.sub <- na.omit(s[, c('id', 'top', 'bottom', vars)])
  # 
  # ## BUG!!! tapply() re-orders the results based on sorting of s.sub$id (#7)
  # ## ----> this will cause problems when input isn't sorted by ID
  # # get the depth of each profile
  # s.slices_of_soil <- tapply(s.sub$bottom, s.sub$id, max, na.rm=TRUE)
  # 
  # # truncate to the max requested depth
  # s.slices_of_soil <- ifelse(s.slices_of_soil <= max_d, s.slices_of_soil, max_d)
  # s.slices_of_non_soil <- max_d - s.slices_of_soil
  # 
  # s.slices_of_soil.length <- length(s.slices_of_soil)
  # 
  # # init a matrix with dimensions: depth slices, number of profiles
  # soil.matrix <- matrix(ncol=s.slices_of_soil.length, nrow=max_d)
  # 
  # # fill with TRUE for 'soil' or FALSE for 'non-soil'
  # for(s.i in 1:s.slices_of_soil.length) {
  #   soil.matrix[, s.i] <- c(rep(TRUE, s.slices_of_soil[s.i]), rep(FALSE, s.slices_of_non_soil[s.i]))
  # }
  # 
  
  
  # ## TODO: convert to dice() (#7)
  # ## BUG: !!! this step re-orders via string-factor-string conversion step by split() (#7)
  # ## --> this is only a bug when profile IDs have been altered after SPC init
  # ## --> https://stackoverflow.com/questions/17611734/r-split-preserving-natural-order-of-factors
  # ##
  # ## unroll each named soil property, for each soil profile
  # ## the result is a list matrices with dimensions: depth, num_properties
  # # this approach requires a named list of soil properties
  # s.unrolled <- dlply(s, "id", .progress=progress, .fun=function(di, p=vars, d=max_d, strict=strict_hz_eval, .parallel=getOption('AQP_parallel', default=FALSE)) {
  #   
  #   # iterate over the set of properties, unrolling as we go
  #   # the result is a [z by p] matrix unrolled to max_d
  #   m <- try(sapply(p, function(p_i) unroll(di$top, di$bottom, prop=di[, p_i], max_depth=d, strict=strict) ))
  #   
  #   ## TODO: could be better
  #   # check for a non-NULL attribute of 'class'
  #   # this will only happen when there was an error
  #   if( !is.null(attr(m, 'class'))) {
  #     if(attr(m, 'class') == 'try-error') {
  #       stop(paste('Error: bad horizon structure in soil id', as.character(unique(di$id))), call.=FALSE)
  #     }
  #   }
  #   else
  #     return(m)
  # }
  # )
  
  
#   
#   
#   ## generate a matrix storing a flag describing soil vs. non-soil at each slice
#   ## note that this will truncate a profile to the max depth of actual data
#   ## profiles missing data in all variables will cause function to stop here
#   if(add_soil_flag){
#     
#     # keep temp subset of the data so that soil/non-soil matrix is
#     # evaluated based on presence of real data in at least 1 variable
#     s.sub <- na.omit(s[, c('id', 'top', 'bottom', vars)])
#     
#     ## BUG!!! tapply() re-orders the results based on sorting of s.sub$id (#7)
#     ## ----> this will cause problems when input isn't sorted by ID
#     # get the depth of each profile
#     s.slices_of_soil <- tapply(s.sub$bottom, s.sub$id, max, na.rm=TRUE)
#     
#     # truncate to the max requested depth
#     s.slices_of_soil <- ifelse(s.slices_of_soil <= max_d, s.slices_of_soil, max_d)
#     s.slices_of_non_soil <- max_d - s.slices_of_soil
#     
#     s.slices_of_soil.length <- length(s.slices_of_soil)
#     
#     # init a matrix with dimensions: depth slices, number of profiles
#     soil.matrix <- matrix(ncol=s.slices_of_soil.length, nrow=max_d)
#     
#     # fill with TRUE for 'soil' or FALSE for 'non-soil'
#     for(s.i in 1:s.slices_of_soil.length) {
#       soil.matrix[, s.i] <- c(rep(TRUE, s.slices_of_soil[s.i]), rep(FALSE, s.slices_of_non_soil[s.i]))
#     }
#     
# 
#     # cleanup
#     rm(s.sub)
#   }
#   
#   
#   ##
#   ## new version for computing slice-wise dissimilarities... fast!
#   ##
#   message(paste('Computing dissimilarity matrices from', n.profiles, 'profiles'), appendLF=FALSE)
#   
#   ## TEMP HACK to supress warnings generated by calling daisy with all NA input (#7)
#   ow <- options('warn')
#   options(warn=-1)
#   
#   d <- llply(depthSequence, .parallel=getOption('AQP_parallel', default=FALSE), .progress=progress, .fun=function(i, su=s.unrolled) {
#     
#     
#     ## 2021-03-03
#     ## this approach breaks when using a single variable for the distance calc
#     ## sp is a 1-row matrix which returns an empty distance matrix
#     ## solution: don't attempt to fix here, re-write the entire thing
#     
#     ## this could be a source of slowness, esp. the t()
#     ## TODO: new implementation will require drop=FALSE
#     ps <- sapply(su, function(dz, z_i=depthSequence[i]) { dz[z_i,] })
#     sp <- t(ps)
#     
#     # compute distance metric for this depth
#     # distance metric has large effect on results
#     # Gower's distance gives the best looking results, and automatically standardizes variables
#     
#     ## this is where we run into memory-size limitations
#     ## an ff object would help here... however it can not preserve all of the information
#     ## that a list can... we would need to store these data as raw matrices
#     
#     ## TODO: sometimes we don't want to standardize, and use Euclidean distance
#     
#     ## TODO: don't call daisy on bogus input data, temp fix: disable warnings (#7)
#     ## if all of the input to daisy is NA, then we get warnings from min() and max()
#     ## this happens when we set a max depth that is beyond most profiles
#     d.i <- daisy(sp, metric='gower')
#     return(d.i)
#   }
#   )
#   # reset warning options
#   options(ow)
#   
#   ## TODO: does this actually do anything?
#   # clean-up
#   rm(s.unrolled) ; gc()
#   
#   # print total size of D
#   message(paste(" [", round(object.size(d) / 1024^2, 2), " Mb]", sep=''))
#   
#   # should NA in the dissimilarity matrix be replaced with max(D) ?
#   if(replace_na) {
#     # replace all NA with the MAX distance between any observations
#     # note that down deep, there may not be enough data for any pair-wise comparisons
#     # therefore, we should not attempt to calculate max() on a matrix of all NA
#     max.distance.vect <- sapply(d, function(i) if(all(is.na(i))) NA else max(i, na.rm=TRUE))
#     max.distance <- max(max.distance.vect, na.rm=TRUE)
#     
#     ## note: this will not work with sample_interval set
#     # should we use a more expensive approach, that uses the soil/non_soil flag?
#     if(add_soil_flag) {
#       # kind of messy: re-using an object like this
#       for(i in 1:length(d)) {
#         d_i <- as.matrix(d[[i]])
#         
#         # set all pairs that are made between deep vs. shallow soil
#         # to the maximum distance- by row and column
#         cells.with.na.rows <- which(is.na(d_i[, which(soil.matrix[i, ])]))
#         cells.with.na.cols <- which(is.na(d_i[which(soil.matrix[i, ]), ]))
#         
#         d_i[, which(soil.matrix[i, ])][cells.with.na.rows] <- max.distance
#         d_i[which(soil.matrix[i, ]), ][cells.with.na.cols] <- max.distance
#         
#         # convert back to dist object
#         d_i <- as.dist(d_i)
#         
#         # copy original attributes
#         attributes(d_i) <- attributes(d[[i]])
#         
#         # save back to original position in list
#         d[[i]] <- d_i
#       }
#       
#       # remove the soil.matrix object to save some space
#       rm(soil.matrix) ; gc()
#     }
#     # use a less expensive approach, where all NA are replaced by the max distance
#     else {
#       d <- lapply(d, function(d_i)
#       {
#         cells.with.na <- which(is.na(d_i))
#         d_i[cells.with.na] <- max.distance
#         return(d_i)
#       } )
#     }
#   }
#   
#   
#   
#   # optionally return the distances for each depth
#   # depth-weighting is performed, but NA is not converted to 0
#   if(return_depth_distances) {
#     # depth-weighting
#     for(i in seq_along(depthSequence))
#       d[[i]] <- d[[i]] * w[i]
#     return(d)
#   }
#   
#   
#   # final tidy-ing of the list of dissimilarity matrices
#   for(i in seq_along(depthSequence)) {
#     # convert NA -> 0
#     na.idx <- which(is.na(d[[i]]))
#     if(length(na.idx) > 0)
#       d[[i]][na.idx] <- 0
#     
#     # depth-weighting
#     d[[i]] <- d[[i]] * w[i]
#   }
#   
#   
#   # reduce list of dissimilarity matrices by summation
#   D <- .SumDistanceList(d)
#   
#   # add distance metric
#   attr(D, 'Distance Metric') <- 'Gower'
#   
#   # remove previous warnings about NA
#   attr(D, 'NA.message') <- NULL
#   
#   # normalize by dividing by max(D)
#   # this is important when incorporating site data
#   # causes problems for some functions like sammon
#   if(rescale.result)
#     D <- D/max(D, na.rm=TRUE)
#   
#   ## DEBUG
#   if(verbose) {
#     cat('depth-slice seq:\n')
#     print(depthSequence)
#     
#     cat('depth-weighting vector:\n')
#     print(round(w, 2))
#     
#     cat(paste('max dissimilarity:', max.distance, '\n'))
#   }
#   
#   
#   # return the distance matrix, class = 'dissimilarity, dist'
#   return(D)
# }
# 
# 
# pc.SPC <- function(s, vars, rescale.result=FALSE, ...){
#   
#   if(!requireNamespace("scales", quietly = TRUE))
#     stop("package `scales` is required", call.=FALSE)
#   
#   # default behavior: do not normalize D
#   
#   ## 2016-02-22: check for missing data moved from pc() to here
#   ## TODO: this makes an assumption on the column containing horizon designations
#   ## 2016-08-16: this function ignores vars that don't existin in @horizons
#   
#   ## 2019-12-19: disabled until PC is re-factored and / or there is a better way to get hz name column
#   # # iterate over profiles and compute percent missing data by variable
#   # pct_data <- evalMissingData(s, vars, name = name)
#   #
#   # ## TODO: review this, and make optional via argument (#7)
#   # # keep track of profiles missing any or all of their data
#   # problem.profiles.idx <- which(pct_data < 1)
#   # # bad.profiles.idx <- which(pct_data == 0)
#   #
#   # if(length(problem.profiles.idx) > 0) {
#   #   # assign('problem.profiles', value=pct_missing[problem.profiles.idx,], envir=aqp.env)
#   #   message('Missing data will bias results, check inputs.')
#   # }
#   
#   
#   ## 2016-02-22: disabled for now
#   #   if(length(bad.profiles.idx) > 0) {
#   #     # stop stop and let the user know
#   #     bad.profiles <- profile_id(s)[bad.profiles.idx]
#   #     stop(paste('no non-NA values associated with profiles:', paste(bad.profiles, collapse=', '), '\nConsider removing these profiles and re-running.'), call.=FALSE)
#   #   }
#   
#   ## 2020-06-19: DEB
#   ## temporary fix for #7, related to profile ID ordering in site, horizon, and results from tapply()
#   ## this is only a problem when using profile IDs that are numeric and not stable when alpha-sorted
#   ##
#   ## However, this will result in changes to sorting of profile_id(), @site, @horizon
#   ##
#   ## Also, this will break with merge of aqpdf branch were sorting by ID is not performed at init time
#   ##
#   ## this was a dump idea -> going to remove these comments after re-write
#   # s <- rebuildSPC(s)
#   
#   
#   # extract horizons
#   s.hz <- horizons(s)
#   
#   # extract site
#   s.site <- site(s)
#   sn <- names(s.site)
#   
#   # check for any site data, remove and a save for later
#   if(any(vars %in% sn)) {
#     
#     # extract site-level vars
#     matching.idx <- na.omit(match(sn, vars))
#     site.vars <- vars[matching.idx]
#     
#     # remove from hz-level vars
#     vars <- vars[-matching.idx]
#     
#     ## TODO: BUG!!! horizon data are rescaled via D/max(D) !!!
#     ## TODO: allow user to pass-in variable type information
#     # compute dissimilarty on site-level data: only works with 2 or more variables
#     # rescale to [0,1]
#     
#     message(paste('site-level variables included:', paste(site.vars, collapse=', ')))
#     d.site <- daisy(s.site[, site.vars, drop=FALSE], metric='gower')
#     
#     # re-scale to [0,1]
#     d.site <- .rescaleRange(d.site, x0 = 0, x1 = 1)
#     
#     # reset default behavior of hz-level D
#     rescale.result=TRUE
#     
#     ## TODO: there might be cases where we get an NA in d.site ... seems like it happens with boolean variables
#     ## ... but why ? read-up on daisy
#     if(any(is.na(d.site))) {
#       warning('NA in site-level dissimilarity matrix, replacing with min dissimilarity', call.=FALSE)
#       d.site[which(is.na(d.site))] <- min(d.site, na.rm=TRUE)
#     }
#     
#     ## TODO: ordering of D_hz vs D_site ... assumptions safe?
#     
#   } else {
#     # setup a dummy D_site
#     d.site <- NULL
#   }
#   
#   
#   ##
#   ## TODO: update this next part
#   ##
#   # add old-style, hard-coded {id, top, bottom} column names
#   s.hz$id <- s.hz[[idname(s)]]
#   hzDepthCols <- horizonDepths(s)
#   s.hz$top <- s.hz[[hzDepthCols[1]]]
#   s.hz$bottom <- s.hz[[hzDepthCols[2]]]
#   
#   # invoke data.frame method
#   res <- profile_compare(s.hz, vars=vars, rescale.result=rescale.result, ...)
#   
#   # if we have site-level data and a valid D_site
#   # combine via weighted average: using weights of 1 for now
#   if(inherits(d.site, 'dist')) {
#     res <- 	(res + d.site) / 2
#     # re-scale to [0,1]
#     res <- .rescaleRange(res, x0 = 0, x1 = 1)
#   }
#   
#   ## fail-safe check on ordering of input profile IDs vs. labels
#   ## #7
#   test.labels <- ! profile_id(s) == attributes(res)$Labels
#   if(any(test.labels)) {
#     warning('SPC / distance matrix IDs out of order, soon to be fixed (#7)', call. = FALSE)
#   }
#   
#   # result is a distance matrix
#   return(res)
}


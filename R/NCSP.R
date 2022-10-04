

## TODO: this can be run in parallel

## TODO: consider allowing variable color specifications


## Note: sanity checking on w is performed outside of this function


#' @title Internal wrapper for distance calculations by NCSP
#'
#' @param m `data.frame`, rows are individuals columns are characteristics, IDs should be saved in rownames
#' 
#' @param sm vector, a single row from the soil/non-soil matrix, must be equal to `nrow(m)`
#' 
#' @param w numeric vector of length `ncol(m)`, optionally specified for weighted distance calc
#' 
#' @param isColor logical, `m` contains CIELAB color coordinates, CIE2000 color contrast metric is used, requires `farver` package
#'
#' @return dissimilarity object (cluster package)
#' 
#' @keywords internal
#' @noRd
#'
.NCSP_distanceCalc <- function(m, sm, w = NULL, isColor) {
  
  # maximum distance used to replace soil + non-soil distances
  # set to 1 for metric = gower
  d.max <- 1
  
  ## TODO: consider adding as an argument
  # CIE2000 will requires an alternative max distance
  # use something reasonable, as this function is applied slice-wise
  # ~ Munsell value 1 -> 8 : dE00 ~ 70
  if(isColor) {
    d.max <- 70
  }
  
  # distance used to replace (NA) non-soil + non-soil distances
  d.notsoil <- 0
  
  # distance used to replace NA (NA in m, not related to non-soil evaluation)
  d.NA <- 0
  
  # number of individuals
  n <- ncol(m)
  
  if(isColor) {
    # CIE2000 color contrast
    # weights are ignored
    
    ## TODO: verify result when NA are present in data matrix
    # result is the full form of a distance matrix
    d <- farver::compare_colour(
      from = m, 
      to = m, 
      from_space = 'lab', 
      to_space = 'lab',
      method = 'CIE2000', 
      white_from = 'D65'
    )
    
  } else {
    # Gower distances
    
    if(!is.null(w)) {
      # weighted distances
      d <- cluster::daisy(m, metric = 'gower', weights = w) 
    } else {
      # standard, un-weighted distances
      d <- cluster::daisy(m, metric = 'gower')
    }
    
    # convert to full matrix for manipulation by row/col index
    d <- as.matrix(d)
  }
  
  
  # locate soil / non-soil pairs
  # TRUE = soil | FALSE = non-soil
  idx.notsoil <- which(!sm)
  
  # of soil / non-soil pairs exist, process accordingly
  if(length(idx.notsoil) > 0) {
    
    ## create logical matrix of soil + non-soil co-occurrences
    ## --> set to MAX distance
    #
    # soil XOR non-soil -> TRUE
    # soil XOR soil -> FALSE
    # non-soil XOR non-soil -> FALSE
    sm.mat <- outer(sm, sm, FUN = xor)
    d[which(sm.mat)] <- d.max
    
    
    ## create logical matrix of non-soil + non-soil co-occurrences
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
  
  # back to reduced format as dist object 
  d <- as.dist(d)
  
  return(d)
}



#' @title Numerical Classification of Soil Profiles
#' @description Experimental replacement for `profile_compare()`.
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
#' @param x `SoilProfileColection` object, should be pre-filtered to remove profiles with horizon depth logic, see [`HzDepthLogicSubset`]
#'
#' @param vars character vector, names of horizon attributes to use in the classification
#' 
#' @param weights numeric vector, same length as `vars`: variable importance weights, need not sum to 1
#' 
#' @param maxDepth numeric, maximum depth of analysis
#' 
#' @param k numeric, weighting coefficient, see examples
#' 
#' @param isColor, logical: variables represent color, should be CIELAB coordinates (D65 illuminant), weights are ignored. Variables should be named `L`, `A`, `B` in specified in that order.
#' 
#' @param rescaleResult logical, distance matrix is rescaled based on max(D)
#' 
#' @param progress logical, report progress
#' 
#' @param verbose logical, extra output messages
#' 
#' @param returnDepthDistances logical, return a list of distances by depth slice
#'
#' @author Dylan E. Beaudette and Jon Maynard
#' @seealso [`slice`], [`daisy`]
#' @references
#'  - D.E. Beaudette, P. Roudier, A.T. O'Geen, Algorithms for quantitative pedology: A toolkit for soil scientists, Computers & Geosciences, Volume 52, 2013, Pages 258-268, ISSN 0098-3004, \doi{10.1016/j.cageo.2012.10.020}.
#'  - Moore, A.; Russell, J. & Ward, W. Numerical analysis of soils: A comparison of three soil profile models with field classification. Journal of Soil Science, 1972, 23, 194-209.
#' @keywords methods manip
#'

#' @export

## TODO:
# * finish testing / comparing
# * benchmarking 
# * integrate dist(site data)
# * weighted mean of D_hz + D_site, user-defined weights
# * expose dice() fm
# * parallel operation
# * progress bar for large SPCs
# * think about a formula interface for simple specification of depths + vars
# * add Maynard et all. reference
# * list-output, with diagnostics and other interesting information

## Ideas:
# * see L1_profiles.R for ideas / examples related to selecting logical "bottom depths"

## name suggested by Jon Maynard
## ideas, commentary, updates c/o Maynard et al. 2020


NCSP <- function(
    x, 
    vars, 
    weights = rep(1, times = length(vars)), 
    maxDepth = max(x), 
    k = 0, 
    isColor = FALSE,
    rescaleResult = FALSE, 
    progress = TRUE,
    verbose = TRUE, 
    returnDepthDistances = FALSE
) {
  
  
  ## depreciated arguments
  
  # filter
  # sample_interval
  # replace_na
  # add_soil_flag
  # strict_hz_eval
  # plot.depth.matrix
  
  
  ## sanity checks
  
  # x must be a SPC
  if(! inherits(x, 'SoilProfileCollection')) {
    stop('`x` must be a SoilProfileCollection', call. = FALSE)
  }
  
  # vars
  if(! all(vars %in% names(x))) {
    stop('`vars` must specify horizon or site level attributes of `x`', call. = FALSE)
  }
  
  ## TODO: consider a message and setting maxDepth <- max(x)
  # maxDepth
  if(maxDepth > max(x) | maxDepth < 1) {
    stop('`maxDepth` should be > 0 and <= max(`x`)', call. = FALSE)
  }
  
  # color comparisons require farver pkg for dE00
  if(isColor & !requireNamespace('farver', quietly = TRUE)) {
    stop('color comparison requires the `farver` package', call. =FALSE)
    
    # for now, color comparisons are based on CIELAB color coordinates
    # must be named "L", "A", "B" and in that order
    if(any(vars != c('L', 'A', 'B'))) {
      stop('CIELAB color coordinates must be specified as `L`, `A`, `B` in `vars`', call. =FALSE)  
    }
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
  # profile IDs
  .ids <- profile_id(s)
  
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
  dimnames(soil.matrix)[[2]] <- .ids
  
  # ## previous version, quite slow
  #
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
  
  ## TODO: basic progress reporting
  ## TODO: cachining identical slices
  ## TODO: convert this to parallel evaluation, maybe furrr package
  
  ## TODO: if !returnDepthDistances: do not retain full list of dist mat, accumulate in single variable
  
  message(paste('Computing dissimilarity matrices from', n.profiles, 'profiles'), appendLF = FALSE)
  for(i in sliceSequence) {
    
    # horizon data for slice i
    .s <- horizons(s[, i])
    
    # characteristics for slice i
    .s <- .s[, vars, drop = FALSE]
    
    # preserve IDs in distance matrix
    row.names(.s) <- .ids
    
    # compute distance, with rules related to soil/non-soil matrix
    .d[[i]] <- .NCSP_distanceCalc(m = .s, sm = soil.matrix[i, ], w = weights, isColor = isColor)
    
    # apply depth-weighting
    .d[[i]] <- .d[[i]] * w[i]
  }
  
  
  ## optionally return list of distance matrices
  if(returnDepthDistances) {
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
  if(isColor) {
    attr(.d, 'Distance Metric') <- 'CIE2000'
  } else {
    attr(.d, 'Distance Metric') <- 'Gower'
  }
  
  # removed profiles, if any
  attr(.d, 'removed.profiles') <- .removed.profiles

  # remove warnings about NA from cluster::daisy()
  attr(.d, 'NA.message') <- NULL
  
  # full set of profile IDs are stored in attr(.d, 'Labels')
  
  # done
  return(.d)
  
  

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

}


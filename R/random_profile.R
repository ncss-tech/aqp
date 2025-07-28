



#' @title Generate a `SoilProfileCollection` of random profiles
#' 
#' @description
#' This function provides a convenient abstraction of `lapply()`, [random_profile()], and [combine()] which are typically used together to create a `SoilProfileCollection` object with >1 soil profiles. `rp()` creates zero-padded integer IDs for logical sorting and indexing of profiles. For more complex IDs or additional flexibility, see [random_profile()]. See [random_profile()] for all possible arguments.
#' 
#'
#' @param size integer, number of requested profiles 
#' @param prefix prefix added to zero-padded, integer IDs
#' @param ... additional arguments to [random_profile()]
#'
#' @returns a `SoilProfileCollection` object
#' @export
#'
#' @examples
#' 
#' # generate a SoilProfileCollection object with 10 profiles
#' # using 0-padded, integer IDs for intuitive sorting
#' spc <- rp(10, method = 'LPP')
#' plotSPC(spc, color = 'p1')
#' 
#' # apply a prefix to the IDs
#' spc <- rp(10, prefix = 'A-', method = 'LPP')
#' plotSPC(spc, color = 'p1')
#' 
rp <- function(size, prefix = NULL, ...) {
  
  # format string for just enough 0-padding 
  .padwidth <- nchar(size) + 1
  
  # optionally add prefix create formatting string
  if(is.null(prefix)) {
    .fmt <- sprintf("%%0%sd", .padwidth)
  } else {
    .fmt <- sprintf("%s%%0%sd", prefix, .padwidth)
  }
  
  # generate IDs
  .s <- sprintf(.fmt, 1:size)
  
  # typical pattern of random_profile() then combine()
  .x <- lapply(.s, random_profile, SPC = TRUE, ...)
  .x <- combine(.x)
  
  return(.x)
}


# logistic power peak function
.lpp <- function(x, a, b, u, d, e) {
  # the exponential term
  f.exp <- exp((x + d * log(e) - u) / d)
  # first part
  f1 <- (b/u) * (1 + f.exp)^((-e - 1) / e)
  # second part
  f2 <- f.exp * (e + 1)^((e+1) / e)
  # combine pieces
  res <- a + f1 * f2
  return(res)
  }


#' @title Random Profile
#'
#' @description Generate a random soil profile according to set criteria, with correlated
#' depth trends.
#'
#' The random walk method produces profiles with considerable variation between
#' horizons and is based on values from the normal distribution seeded with
#' means and standard deviations drawn from the uniform distribution of \[0,
#' 10].
#'
#' The logistic power peak (LPP) function can be used to generate random soil
#' property depth functions that are sharply peaked. LPP parameters can be
#' hard-coded using the optional arguments: "lpp.a", "lpp.b", "lpp.u", "lpp.d",
#' "lpp.e". Amplitude of the peak is controlled by ("lpp.a + "lpp.b"), depth of
#' the peak by "lpp.u", and abruptness by "lpp.d" and "lpp.e". Further
#' description of the method is outlined in (Brenton et al, 2011). Simulated
#' horizon distinctness codes are based on the USDA-NCSS field description
#' methods.
#' Simulated distinctness codes are constrained according to horizon thickness,
#' i.e. a gradual boundary (+/- 5cm) will not be simulated for horizons that
#' are thinner than 3x this vertical distance
#'
#' @aliases random_profile .lpp
#' @param id a character or numeric id used for this profile
#' @param n vector of possible number of horizons, or the exact number of
#' horizons (see below)
#' @param min_thick minimum thickness criteria for a simulated horizon
#' @param max_thick maximum thickness criteria for a simulated horizon
#' @param n_prop number of simulated soil properties (columns in the returned
#' dataframe)
#' @param exact should the exact number of requested horizons be generated?
#' (defaults to FALSE)
#' @param method named method used to synthesize depth function ('random_walk'
#' or 'LPP'), see details
#' @param HzDistinctSim optionally simulate horizon boundary distinctness codes
#' @param SPC result is a `SoilProfileCollection` object, otherwise a `data.frame` object
#' @param \dots additional parameters passed-in to the LPP `.lpp`)
#' function
#' @return A `data.frame` or `SoilProfileCollection` object.
#' @note See examples for ideas on simulating several profiles at once.
#' @author Dylan E. Beaudette
#' @seealso [hzDistinctnessCodeToOffset()]
#' @references Myers, D. B.; Kitchen, N. R.; Sudduth, K. A.; Miles, R. J.;
#' Sadler, E. J. & Grunwald, S. Peak functions for modeling high resolution
#' soil profile data Geoderma, 2011, 166, 74-83.
#' @keywords manip
#' @export
#' @examples
#'
#'
#' # generate 10 random profiles, result is a list of SoilProfileCollection objects
#' d <- lapply(1:10, random_profile, SPC=TRUE)
#'
#' # combine
#' d <- combine(d)
#'
#' # plot
#' opar <- par(mar=c(0,0,3,2))
#' plotSPC(d, color='p1', name='name', cex.names=0.75)
#' par(opar)
#'
#' # simulate horizon boundary distinctness codes:
#' d <- lapply(1:10, random_profile, SPC=TRUE, HzDistinctSim=TRUE)
#' d <- combine(d)
#'
#' d$HzD <- hzDistinctnessCodeToOffset(d$HzDistinctCode)
#'
#' opar <- par(mar=c(0,0,3,2))
#' plotSPC(d, name='name', color='p1', hz.distinctness.offset='HzD')
#' par(opar)
#'
#'
#' # depth functions are generated using the LPP function
#' opar <- par(mfrow=c(2,1), mar=c(0,0,3,0))
#'
#' # generate data
#' d.1 <- lapply(1:10, random_profile, SPC=TRUE, n=c(6, 7, 8), n_prop=1, method='LPP')
#' d.1 <- combine(d.1)
#'
#' # plot
#' plotSPC(d.1, name='name', color='p1', col.label = 'LPP Defaults')
#'
#'
#' # do this again, this time set all of the LPP parameters
#' d.2 <- lapply(1:10, random_profile, SPC=TRUE, n=c(6, 7, 8), n_prop=1, method='LPP',
#'            lpp.a=5, lpp.b=10, lpp.d=5, lpp.e=5, lpp.u=25)
#' d.2 <- combine(d.2)
#'
#' # plot
#' plotSPC(d.2, name='name', color='p1', col.label = 'Custom LPP Parameters')
#'
#'
#' # reset plotting defaults
#' par(opar)
#'
#'
#'
#' # try plotting the LPP-derived simulated data
#' # aggregated over all profiles
#' a <- slab(d.2, fm= ~ p1)
#' a$mid <- with(a, (top + bottom) / 2)
#'
#' library(lattice)
#' (p1 <- xyplot(mid ~ p.q50, data=a,
#'               lower=a$p.q25, upper=a$p.q75, ylim=c(150,-5), alpha=0.5,
#'               panel=panel.depth_function, prepanel=prepanel.depth_function,
#'               cf=a$contributing_fraction, xlab='Simulated Data', ylab='Depth',
#'               main='LPP(a=5, b=10, d=5, e=5, u=25)',
#'               par.settings=list(superpose.line=list(col='black', lwd=2))
#' ))
#'
#' # optionally add original data as step-functions
#' if(require(latticeExtra)) {
#'   h <- horizons(d.2)
#'   p1 + as.layer(xyplot(top ~ p1, groups=id, data=h,
#'                        horizontal=TRUE, type='S',
#'                        par.settings=list(superpose.line=list(col='blue', lwd=1, lty=2))))
#' }
#'
#'
#'
random_profile <- function(id, n = c(3,4,5,6), min_thick = 5, max_thick = 30, n_prop = 5, exact = FALSE, method= 'random_walk', HzDistinctSim = FALSE, SPC = FALSE, ...) {

  # sanity check
  if(missing(id))
	  stop('must specify an id')

  if(max_thick < min_thick)
	  stop('illogical horizon thickness constraints')

  if(! method %in% c('random_walk', 'LPP'))
	stop('invalid method')

  # get extra arguments
  dots <- list(...)

  # if requested, give back the exact number of horizons
  if(length(n) == 1 & exact)
	  n_hz <- n

  # otherwise randomly choose from suggestions
  else
	  n_hz <- sample(n, 1)

  # generate hz top bnd
  tops <- integer(n_hz-1)
  for(i in 1:(n_hz-1))
	  tops[i] <- sample(min_thick:max_thick, 1)

  # add 0, then generate bottom bnd
  tops <- as.integer(c(0, tops))
  bottoms <- as.integer(c(tops[-1], sample(min_thick:max_thick, 1)))

  # combine into a df
  # always treat ID as a character: "solves" some cases of SPC corruption due to re-ordering of integers cast to character:
  # https://github.com/ncss-tech/aqp/issues/90
  d <- data.frame(id=as.character(id), top=cumsum(tops), bottom=cumsum(bottoms), name=paste('H',1:n_hz,sep=''), stringsAsFactors = FALSE)

  # generate several properties
  # with different means / sd
  for(i in 1:n_prop) {
	# init storage
	  p <- numeric(n_hz)

	if(method == 'random_walk') {
		p[1] <- rnorm(1, mean=runif(n=1, min=-10, max=10), sd=runif(n=1, min=1, max=10))
		for(j in 2:n_hz)
			p[j] <- p[j-1] + rnorm(1, mean=runif(n=1, min=-10, max=10), sd=runif(n=1, min=1, max=10))
	  }

	if(method == 'LPP') {
	  # generate synthetic values at horizon mid-points
	  mids <- with(d, (top + bottom)/2)

	  # generate LPP parameters from uniform dist if not given as arguments
	  if(is.null(dots[['lpp.a']]))
			lpp.a <- runif(n=1, min=5, max=25)
	  else
			lpp.a <- dots[['lpp.a']]

	  if(is.null(dots[['lpp.b']]))
			lpp.b <- runif(n=1, min=20, max=60)
	  else
			lpp.b <- dots[['lpp.b']]

	  if(is.null(dots[['lpp.u']]))
			lpp.u <- runif(n=1, min=10, max=90)
	  else
			lpp.u <- dots[['lpp.u']]

	  if(is.null(dots[['lpp.d']]))
			lpp.d <- runif(n=1, min=1, max=10)
	  else
			lpp.d <- dots[['lpp.d']]

	  if(is.null(dots[['lpp.e']]))
			lpp.e <- runif(n=1, min=5, max=20)
	  else
			lpp.e <- dots[['lpp.e']]

	  # generate vector of synthetic values based on LPP
	  p <- .lpp(mids, a=lpp.a, b=lpp.b, u=lpp.u, d=lpp.d, e=lpp.e)
	  }

	  # add generated depth profile to horizons
	  new_col <- paste('p',i, sep='')
	  d[, new_col] <- p
	  }

  # optionally add horizon distinctness codes:
  # these are based on USDA-NCSS codes and approximate vertical offsets
	# codes are constrained to the thickness of the horizon
  if(HzDistinctSim) {
  	# standard codes and offsets
  	codes <- c('A','C','G','D')
  	offsets <- hzDistinctnessCodeToOffset(codes)
  	# compute horizon thickness vector
  	thick <- with(d, bottom-top)

  	# create matrix of distinctness codes based on (1/3) horizon thickness
  	# 1 when possible, 0 when impossible
  	prob.matrix <- t(sapply(thick, function(i) (i/3) >= offsets))
  	prob.matrix[which(prob.matrix)] <- 1

  	d.codes <- vector(mode='character', length=n_hz)
  	for(i in 1:n_hz) {
  		d.codes[i] <- sample(codes, size=1, prob=prob.matrix[i, ])
  	}

  	d$HzDistinctCode <- d.codes
  }

  # note: 3-4x performance hit when calling from lapply(1:big.number, ...)
  # optionally return as SPC
  if(SPC) {
    depths(d) <- id ~ top + bottom
  }

  # all done
  return(d)
}



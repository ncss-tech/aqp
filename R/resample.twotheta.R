# resample intensities according to a new 2-theta interval
#' Resample an XRD Pattern
#'
#' Resample an XRD pattern along a user-defined twotheta resolution via local
#' spline interpolation.
#'
#' Sometimes XRD patterns are collected at different resolutions, or at a
#' resolution that is too great for full pattern matching. This function can be
#' used to resample patterns to a consistent twotheta resolution, or to
#' decimate massive patterns.
#'
#' @param twotheta a vector of twotheta value
#' @param x a vector of diffraction intensities corresponding with twotheta
#' values
#' @param tt.min new minimum twotheta value, defaults to current minimum
#' @param tt.max new maximum twotheta value, defaults to current maximum
#' @param new.res new twotheta resolution, defaults to 0.02
#' @return A dataframe with the following columns \item{twotheta}{new sequence
#' of twotheta values} \item{x}{resampled diffraction intensities}
#' @author Dylan E Beaudette
#' @seealso \code{\link{rruff.sample}}
#' @references http://casoilresource.lawr.ucdavis.edu/
#' @keywords manip
#' @examples
#'
#' data(rruff.sample)
#'
#' # resample single pattern
#' nontronite.resamp <- with(rruff.sample,
#' resample.twotheta(twotheta, nontronite, new.res=0.02) )
#'
#' # plot original vs. resampled pattern
#' plot(nontronite ~ twotheta, data=rruff.sample, type='l', col='grey')
#' lines(nontronite.resamp, col='blue')
#'
resample.twotheta <- function(twotheta, x, tt.min=min(twotheta), tt.max=max(twotheta), new.res=0.02)
	{

  .Deprecated(new = 'fps', package = 'powdR', msg = 'https://github.com/benmbutler/powdR')

	# fit a spline-function to the data
	sf <- splinefun(twotheta, x)

	# generate a new sequence of two-theta values
	# according to the requested resolution
	s <- seq(tt.min, tt.max, by=new.res)

	# interpolate onto new two-theta sequence
	x.new <- data.frame(twotheta=s, x=sf(s))

	return(x.new)
	}



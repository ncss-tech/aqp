\name{random_profile}
\alias{random_profile}
\alias{.lpp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Random Profile}
\description{
Generate a random soil profile according to set criteria, with correlated depth trends.
}
\usage{
random_profile(id, n = c(3, 4, 5, 6), min_thick = 5,
 max_thick = 30, n_prop = 5, exact = FALSE, method = 'random_walk', ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{id}{a character or numeric id used for this profile}
  \item{n}{vector of possible number of horizons, or the exact number of horizons (see below)}
  \item{min_thick}{minimum thickness criteria for a simulated horizon}
  \item{max_thick}{maximum thickness criteria for a simulated horizon}
  \item{n_prop}{number of simulated soil properties (columns in the returned dataframe)}
  \item{exact}{should the exact number of requested horizons be generated? (defaults to FALSE)}
  \item{method}{named method used to synthesize depth function ('random_walk' or 'LPP'), see details}
  \item{\dots}{additional parameters passed-in to the LPP (\code{.lpp}) function}
}

\value{
A dataframe with the simulated profile.
}
\details{The logistic power peak (LPP) function can be used to generate random soil property depth functions that are sharply peaked. LPP parameters can be hard-coded using the optional arguments: "lpp.a", "lpp.b", "lpp.u", "lpp.d", "lpp.e". Amplitude of the peak is controlled by ("lpp.a + "lpp.b"), depth of the peak by "lpp.u", and abruptness by "lpp.d" and "lpp.e". Further description of the method is outlined in (Brenton et al, 2011).
}
\references{
D. Brenton Myers, Newell R. Kitchen, Kenneth A. Sudduth, Randall J. Miles, E. John Sadler, Sabine Grunwald, Peak functions for modeling high resolution soil profile data, Geoderma, Volume 166, Issue 1, 30 October 2011, Pages 74-83, ISSN 0016-7061, 10.1016/j.geoderma.2011.07.014.
}
\author{Dylan E. Beaudette}
\note{See examples for ideas on simulating several profiles at once.}
\seealso{\code{\link{profile_compare}}}
\examples{
library(lattice)

# generate 10 random profiles with default settings:
d <- ldply(1:10, random_profile)

# add a fake color
d$soil_color <- 'white'

# promote to SoilProfileCollection and plot
depths(d) <- id ~ top + bottom
profile_plot(d)

# if the plotrix package is installed, 
# make a more interesting color based on the first property
# depth functions are generated using the LPP function
if(require(plotrix))
	{
	d <- ldply(1:10, random_profile, n=c(6, 7, 8), n_prop=1, method='LPP')
	d$soil_color <- color.scale(d$p1, c(0,1,1),c(1,1,0),0)
	depths(d) <- id ~ top + bottom
	profile_plot(d)
	}

# do this again, this time set all of the LPP parameters
if(require(plotrix))
  {
  d <- ldply(1:10, random_profile, n=c(6, 7, 8), n_prop=1, method='LPP', 
  lpp.a=5, lpp.b=5, lpp.d=5, lpp.e=5, lpp.u=25)
  d$soil_color <- color.scale(d$p1, c(0,1,1),c(1,1,0),0)
  depths(d) <- id ~ top + bottom
	profile_plot(d)
  }

#
# simple cluster analysis:
d <- ldply(1:10, random_profile)
p <- profile_compare(d, vars=c('p1','p2','p3'), max_d=50, k=0)
plot(hclust(p))


# stress-test profile comparison functions (not run)
# d <- ldply(1:1000, random_profile)
# 
# 100 profiles, 4 varibales:
# 66 seconds on 1.3 Ghz Intel Mac Mini 
# D matrix = 192.3 Mb
# 768 Mb required
# p <- profile_compare(d, vars=c('p1','p2','p3','p4','p5'), max_d=50, k=0)

# more efficient computation, at the expense of precision, with 
# p <- profile_compare(d, vars=c('p1','p2','p3','p4','p5'),
# max_d=50, k=0, sample_interval=10)



}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}

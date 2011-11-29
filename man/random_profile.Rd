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
Myers, D. B.; Kitchen, N. R.; Sudduth, K. A.; Miles, R. J.; Sadler, E. J. & Grunwald, S. Peak functions for modeling high resolution soil profile data Geoderma, 2011, 166, 74-83.
}
\author{Dylan E. Beaudette}
\note{See examples for ideas on simulating several profiles at once.}
\seealso{\code{\link{profile_compare}}}
\examples{
# generate 10 random profiles with default settings:
d <- ldply(1:10, random_profile)

# add a fake color
d$soil_color <- 'white'

# promote to SoilProfileCollection and plot
depths(d) <- id ~ top + bottom
plot(d)


# make a more interesting color based on the first property
# depth functions are generated using the LPP function
if(require(scales)) {
  opar <- par(mfrow=c(2,1))
  
  # setup color palette and mapping function
  cols <- rev(brewer_pal(pal='Spectral')(8))
  grad <- gradient_n_pal(cols)
  
  # generate data and color scales
  d <- ldply(1:10, random_profile, n=c(6, 7, 8), n_prop=1, method='LPP')
  d$soil_color <- cscale(d$p1, grad)
  
  # promote to SPC and plot
  depths(d) <- id ~ top + bottom
  plot(d)
  legend('bottom', legend=pretty(d$p1), pt.bg=cscale(pretty(d$p1), grad),
  pch=22, pt.cex=2.5, bty='n', horiz=TRUE)
  
  
  # do this again, this time set all of the LPP parameters
  d <- ldply(1:10, random_profile, n=c(6, 7, 8), n_prop=1, method='LPP', 
  lpp.a=5, lpp.b=10, lpp.d=5, lpp.e=5, lpp.u=25)
  d$soil_color <- cscale(d$p1, grad)
  depths(d) <- id ~ top + bottom
  plot(d)
  legend('bottom', legend=pretty(d$p1), pt.bg=cscale(pretty(d$p1), grad),
  pch=22, pt.cex=2.5, bty='n', horiz=TRUE)
  
  par(opar)
  }


# try plotting the LPP-derived simulated data
# aggregated over all profiles
a <- soil.slot.multiple(d, fm= ~ p1)
a$mid <- with(a, (top + bottom) / 2)

(p1 <- xyplot(mid ~ p.q50, data=a,
lower=a$p.q25, upper=a$p.q75, ylim=c(150,-5), alpha=0.5,
panel=panel.depth_function, prepanel=prepanel.depth_function,
cf=a$contributing_fraction, xlab='Simulated Data', ylab='Depth',
main='LPP(a=5, b=10, d=5, e=5, u=25)',
par.settings=list(superpose.line=list(col='black', lwd=2))
))

# optionally add original data as step-functions
if(require(latticeExtra)) {
  h <- horizons(d)
  h$mid <- with(h, (top + bottom) / 2)
  p1 + as.layer(xyplot(top ~ p1, groups=id, data=h,
  horizontal=TRUE, type='S',
  par.settings=list(superpose.line=list(col='blue', lwd=1, lty=2))))
  }


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

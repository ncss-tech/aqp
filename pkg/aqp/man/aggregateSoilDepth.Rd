\name{aggregateSoilDepth}
\alias{aggregateSoilDepth}

\title{Probabalistic Estimation of Soil Depth}
\description{Estimate the most-likely depth to contact within a collection of soil profiles.}
\usage{
aggregateSoilDepth(x, groups, crit.prob = 0.9, name = "hzname", p = "Cr|R|Cd", ...)
}

\arguments{
  \item{x}{a \code{SoilProfileCollection} object}
  \item{groups}{the name of a site-level attribute that defines groups of profiles within a collection}
  \item{crit.prob}{probability cuttoff used to determine where the most likely depth to contact will be, e.g. 0.9 translates to 90\% of profiles are shallower than this depth}
  \item{name}{horizon-level attribute where horizon designation is stored}
  \item{p}{a REGEX pattern that matches non-soil genetic horizons}
  \item{\dots}{additional arguments to \code{slab}}
}

\details{This function computes a probability-based estimate of soil depth by group. If no grouping variable exists, a dummy value can be used to compute a single estimate. The \code{crit.prob} argument sets the critical probability (e.g. 0.9) at which soil depth within a group of profiles is determined. For example, a \code{crit.prob} of 0.95 might result in an estimated soil depth (e.g. 120cm) where 95\% of the profiles (by group) had depths that were less than or equal to 120cm.}

\value{A \code{data.frame} is returned, with as many rows as there are unique group labels, as specified in \code{groups}.}

\author{D.E. Beaudette}

\seealso{
\code{\link{estimateSoilDepth},\link{slab}}
}

\examples{
data(sp1)
depths(sp1) <- id ~ top + bottom
site(sp1) <- ~ group

aggregateSoilDepth(sp1, 'group', crit.prob = 0.9)
}

\keyword{manip}


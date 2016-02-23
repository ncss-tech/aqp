\name{evalMissingData}
\alias{evalMissingData}

\title{Evaluate Missing Data}
\description{Evaluate missing data in a SoilProfileCollection object}
\usage{
evalMissingData(x, vars, name = "hzname", p = "Cr|R|Cd")
}

\arguments{
  \item{x}{a \code{SoilProfileCollection} object}
  \item{vars}{a chatacter vector naming horizon-level attributes in \code{x}}
  \item{name}{the name of a horizon-level attribute where horizon designations are stored}
  \item{p}{REGEX pattern used to match non-soil horizons}
}

\details{Data completeness is evaluated by profile, based on the thickness of horizons with complete horizon-level attribute values (specified in \code{vars}) divided by the total thickness. The default REGEX pattern, \code{p}, should catch most non-soil horizons which are excluded from the evaluation.}

\value{A vector values ranging from 0 to 1, representing the percentage of non-NA data (as specified in \code{vars}) for each profile.}

\author{D.E. Beaudette}


\examples{
# example data
data(sp2)
# init SPC object
depths(sp2) <- id ~ top + bottom
# compute data completeness
sp2$data.complete <- evalMissingData(sp2, vars = c('r', 'g', 'b'), name = 'name')
# rank
new.order <- order(sp2$data.complete)
# plot along data completeness ranking
plot(sp2, plot.order=new.order)
# add axis, note re-ordering of axis labels
axis(side=1, at=1:length(sp2), labels = round(sp2$data.complete[new.order], 2),
line=-2, cex.axis=0.75)
}

\keyword{manip}


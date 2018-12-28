\name{crit.clay.argillic}
\alias{crit.clay.argillic}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
crit.clay.argillic
}
\description{
Given a vector or matrix of "eluvial" horizon clay contents (\%), \code{crit.clay.argillic()} returns a vector or matrix of minimum clay contents (thresholds) that must be met for an argillic horizon clay increase. Result is based on the clay increase criteria in the definition of the argillic horizon  in 12th Edition Keys to Soil Taxonomy (Soil Survey Staff, 2014). It uses standard equations for clay contents less than 15 \%, between 15 and 40 \%, and greater than 40 \%.
}
\usage{
crit.clay.argillic(eluvial_clay_content)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{eluvial_clay_content}{
  A numeric vector or matrix containing clay contents of potential "eluvial" horizons. May contain \code{NA}.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A vector or matrix (input-dependent) containing minimum "illuvial" horizon clay contents (thresholds) to be met for argillic horizon clay increase.
}
\references{
Soil Survey Staff. 2014. Keys to Soil Taxonomy, 12th ed. USDA-Natural Resources Conservation Service, Washington, DC.
}
\author{
Andrew Gene Brown
}
\note{
This function is intended for identifying clay content threshold required for an argillic horizon. These thresholds may not apply depending on the specifics of your soil. E.g. if the upper part of argillic has been plowed (has Ap immediately over upper boundary) the clay increase requirement is waived (Soil Survey Staff, 2014).
}

\seealso{
\code{\link{getArgillicBounds}}, \code{\link{get.increase.matrix}}
}
\examples{
# crit.clay.argillic uses different equations for clay content
# less than 15 \%, between 15 and 40 \%, and >40 \%

crit.clay.argillic(eluvial_clay_content=c(5, 20, 45))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~argillic }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~clay }% __ONLY ONE__ keyword per line

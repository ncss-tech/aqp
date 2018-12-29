\name{getArgillicBounds}
\alias{getArgillicBounds}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
getArgillicBounds - estimate upper and lower boundary of argillic d
}
\description{
\code{getArgillicBounds()} returns the upper and lower boundary of the argillic horizon, assuming that the upper boundary is determined by where the clay increase depth is met. Uses \code{crit.clay.argillic()} as the threshold function for determining whether a clay increase occurs and \code{get.increase.matrix} to determine whether the increase is met within sufficient depth and in which horizon.

The lower boundary is either a lithic/paralithic/densic contact or the end of evidence of illuviation. The depth to contact is estimated using `bottom.pattern`. 

There also is an option `require_t` to omit the requirement for eluviation (t subscript in `hzdesgn`). 

Even if "t" subscript is not required, the lower gradational horizon regular expression `lower.grad.pattern` is used to modify the lower boundary upwards, if needed. 
}
\usage{
getArgillicBounds(p, hzdesgn = "hzname", attr = "clay", require_t = TRUE, bottom.pattern = "Cr|R|Cd", lower.grad.pattern = "^[2-9]*CB*[^rt]*$", as.list = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{p}{
a single-profile SoilProfileCllection
}
  \item{hzdesgn}{
the name of the column/attribute containing the horizon designation; default="hzname"
}
  \item{attr}{
the name of the column/attribute containing the clay content; default="clay"
}
  \item{require_t}{
require a "t" subscript for positive identification of upper and lower bound of argillic? default:
}
  \item{bottom.pattern}{
regular expression passed to \code{estimateSoilDepth()} to match the lower boundary of the soil. default is "Cr|R|Cd" which approximately matches paralithic, lithic and densic contacts.
}
  \item{lower.grad.pattern}{
this is a pattern for adjusting the bottom depth of the argillic horizon upwards from the bottom depth of the soil. The absence of illuviation is used as a final control on horizon pattern matching.
}
\item{as.list}{
return result as a 'list'?
}
\value{
eturns a numeric vector; first value is top depth, second value is bottom depth. if as.list is TRUE, returns a list with top depth named "ubound" and bottom depth named "lbound"
}
\author{
Andrew Gene Brown
}
\seealso{
\code{\link{get.increase.matrix}}, \code{\link{get.argillic.increase.depth}}, \code{crit.clay.argillic}
}
\examples{

}
\keyword{ manip }

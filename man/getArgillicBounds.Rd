\name{getArgillicBounds}
\alias{getArgillicBounds}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
getArgillicBounds
}
\description{
\code{getArgillicBounds()} estimates upper and lower boundary of argillic diagnostic subsurface horizon for a profile in a single-profile SoilProfileCollection object (`p`). The upper boundary is where the clay increase threshold is met. Uses \code{crit.clay.argillic()} as the threshold function for determining whether a clay increase occurs and \code{get.increase.matrix} to determine whether the increase is met, whether vertical distance of increase is sufficiently short, and in which horizon.

The lower boundary is  first approximated as the depth to a lithic/paralithic/densic contact, or some other horizon matchable by a custom regular expression pattern. Subsequently, that boundary is extended upwards to the end of "evidence of illuviation." 

The depth to contact is estimated using `bottom.pattern` "Cr|R|Cd" by default. It matches anything containing Cr, R or Cd.

The lower gradational horizon regular expression `lower.grad.pattern` default is '"^[2-9]*CB*[^rt]*$"'. It matches anything that starts with a lithologic discontinuity (or none) and a C master horizon designation. May contain B as second horizon designation in transitional horizon. May not contain 'r' or 't' subscript. 

There also is an option `require_t` to omit the requirement for evidence of eluviation in form of 't' subscript in `hzdesgn`. 

Even if "t" subscript is not required for positive identification, the presence of lower gradational C horizons lacking 't' will still be used to modify the lower boundary upward from a detected contact, if needed. If this behavior is not desired, just set `lower.grad.pattern` to something that will not match any horizons in your data.
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
}
\value{
Returns a numeric vector; first value is top depth, second value is bottom depth. If as.list is TRUE, returns a list with top depth named "ubound" and bottom depth named "lbound"
}
\author{
Andrew Gene Brown
}
\seealso{
\code{get.increase.matrix}, \code{get.argillic.increase.depth}, \code{crit.clay.argillic}
}
\examples{
data(sp1, package = 'aqp')
depths(sp1) <- id ~ top + bottom
site(sp1) <- ~ group

p <- sp1[1]
attr <- 'prop' # clay contents 
foo <- getArgillicBounds(p, hzdesgn='name', attr = attr)
foo
}
\keyword{ manip }

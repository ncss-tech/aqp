\name{test_hz_logic}
\alias{test_hz_logic}

\title{Test Horizon Logic}
\description{Simple tests for horizon logic, based on a simple data.frame of ordered horizons.}
\usage{
test_hz_logic(i, topcol, bottomcol, test.NA = TRUE, strict = FALSE)
}
\arguments{
  \item{i}{a data.frame associated with a single soil profile, ordered by depth}
  \item{topcol}{character, giving the name of the column in \code{i} that describes horizon top depth}
  \item{bottomcol}{character, giving the name of the column in \code{i} that describes horizon bottom depth}
  \item{test.NA}{logicial, should tests for missing (NA) horizon top OR bottom boundaries be performed?}
  \item{strict}{logical, should continuity tests be performed-- i.e. for non-contiguous horizon boundaries}
}
\details{This function is used as part of the validity-methods for SoilProfileCollection objects.}
\value{logical: TRUE --> pass, FALSE --> fail}
\references{}
\author{D.E. Beaudette}
\note{}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\link{depths<-}}
\examples{

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}


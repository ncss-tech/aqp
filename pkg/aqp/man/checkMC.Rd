\name{checkMC}
\alias{checkMC}
\title{Check for Multi-Core Resources}
\description{Internally used function for determining the availability of resources required for parallel computations via the plyr (version >= 1.2), doMC, and foreach packages.}
\usage{
checkMC(n = 2)
}
\arguments{
  \item{n}{number of cores to use, defaults to 2}
}
\details{The introduction of parallel computing to the AQP package is still experimental. Therefore, parallel computation for AQP functions must manually be enabled with \code{options(AQP_parallel=TRUE)}.}
\value{TRUE if required packages are installed, otherwise FALSE}
\references{
http://www.mail-archive.com/r-help@r-project.org/msg109348.html
}
\author{
Dylan Beaudette
}
\seealso{\code{\link{ddply}}}
\examples{
# manually enable parallel computations
options(AQP_parallel=TRUE)

# result will depend on the above option and your specific system configuration
checkMC(n=2)
}
\keyword{manip}

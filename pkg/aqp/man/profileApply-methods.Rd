\name{profileApply-methods}
\docType{methods}
\alias{profileApply}
\alias{profileApply,SoilProfileCollection-method}
\title{Apply a function to soil profiles within a SoilProfileCollection object.}
\description{Apply a function to soil profiles within a SoilProfileCollection object.}
\section{Methods}{
\describe{

\item{\code{signature(object = "SoilProfileCollection")}}{}
}}

\value{A vector of length \code{nrow(horizons(object))}.}

\examples{
data(sp1)
depths(sp1) <- id ~ top + bottom

# make a new, empty column within @horizons
sp1$d <- rep(NA, times=nrow(horizons(sp1)))

# rescale properties within each profile
sp1$d <- profileApply(sp1, FUN=function(x) round(scale(x$prop), 2))
plot(sp1, name='d')

# compute depth-wise differencing by profile
f <- function(x) { c(x$prop[1], diff(x$prop)) }
sp1$d <- profileApply(sp1, FUN=f)
plot(sp1, name='d')

}
\keyword{methods}
\keyword{manip}

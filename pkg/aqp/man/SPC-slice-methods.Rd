\name{slice-methods}
\docType{methods}
\alias{get.slice}
\alias{slice.fast}
\alias{slice}
\alias{slice,SoilProfileCollection-method}
\title{Slicing of SoilProfilecollection Objects}
\description{Slicing of SoilProfilecollection Objects}
\section{Methods}{
\describe{

\item{\code{signature(object = "SoilProfileCollection")}}{
%%  ~~describe this method here~~
}
}}

\references{http://casoilresource.lawr.ucdavis.edu/}
\author{Dylan E. Beaudette}

\seealso{\code{\link{slab}}}

\examples{
# simulate some data, IDs are 1:20
library(plyr)
d <- ldply(1:20, random_profile)

# init SoilProfilecollection object
depths(d) <- id ~ top + bottom
head(horizons(d))

# generate single slice at 10 cm
# output is a SoilProfilecollection object
s <- slice(d, 10 ~ name + p1 + p2 + p3)

# generate single slice at 10 cm, output data.frame
s <- slice(d, 10 ~ name + p1 + p2 + p3, just.the.data=TRUE)

# generate integer slices from 0 - 25 cm
s <- slice(d, 0:25 ~ name + p1 + p2 + p3)
plot(s)

# generate slices from 0 - 10 cm, for all variables
s <- slice(d, 0:10 ~ .)
print(s)

}
\keyword{methods}
\keyword{manip}

\name{segment}
\alias{segment}
\docType{methods}
\title{Segmenting of horizon data into depth intervals}
\description{This function segments horizon data from both SoilProfileCollections and data frames into depth intervals or slices.}


\usage{
segment(object = NULL, intervals = NULL, trim = TRUE, hzdepcols = NULL)
}

\arguments{
  \item{object}{either a SoilProfileCollection or data frame}
  
  \item{intervals}{a vector of integers over which to slice the horizon data (e.g. c(25, 100) or 25:100)}
  
  \item{trim}{a logical, to specify whether the horizon depths should be clipped to the depth intervals (e.g. 15 -> 25)}
  
  \item{hzdepcols}{a character vector of length 2 specifying the names of the horizon depths (e.g. c("hzdept", "hzdepb")), only necessary if object is a data frame}
}

\section{Details}{
This function will segment horizon data into arbitary depth intervals (e.g. c(25, 100) or 25:100). Compared to \code{slice}, segment is more generic, as it supports artibary intervals, and works with either SoilProfileCollections or data.frames. In addition \code{segment} is slightly faster than \code{slice}. Compared to \code{glom}, \code{segment} works on more than a single SoilProfileCollection.

\code{segment} is intended to make it less computationally intensive to calculate weighted averages within specified depth intervals, because horizon thicknesses can be used as weights. Therefore it is not necessary to slice horizon data into 1-cm increments, which increases \code{object.size} a hundred fold or more, prior to computing averages or quantiles in order to properly weight the contribution from horizons of varying thicknesses.
}

\value{Either a SoilProfileCollection or data frame with the original horizon data segmented by depth intervals.

\code{segment_id} a new column identifying the depth interval
}


\author{Stephen Roecker}

\seealso{\code{\link{slice}}, \code{\link{glom}}}

\examples{
library(aqp)

data(sp5)

# segment by upper 25-cm
test1 <- segment(sp5, intervals = c(0, 100))
nrow(test1)
print(object.size(test1), units = "Mb")

# segment by 1-cm increments
test2 <- segment(sp5, intervals = 0:100)
nrow(test2)
print(object.size(test2), units = "Mb")


# segment and aggregate
test3 <- segment(horizons(sp5), 
                 intervals = c(0, 5, 15, 30, 60, 100, 200), 
                 hzdepcols = c("top", "bottom")
                 )
test3$hzthk <- test3$bottom - test3$top
test3_agg <- by(test3, test3$segment_id, function(x) {
  data.frame(
    hzID = x$hzID[1],
    segment_id = x$segment_id[1],
    average = weighted.mean(x$clay, w = x$hzthk)
    )
  })
test3_agg <- do.call("rbind", test3_agg)
}
\keyword{methods}
\keyword{manip}

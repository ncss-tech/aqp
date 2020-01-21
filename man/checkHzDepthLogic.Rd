\name{checkHzDepthLogic}
\alias{checkHzDepthLogic}
\alias{test_hz_logic}

\title{Check a \code{SoilProfileCollection} object for errors in horizon depths.}
\description{This function inspects a \code{SoilProfileCollection} object, looking for 4 common errors in horizon depths: 1) bottom depths shallower than top depths, 2) equal top and bottom depths, 3) missing top or bottom depths (e.g. NA), and, 4) gaps or overlap between adjacent horizons.}

\usage{
checkHzDepthLogic(x)
}

\arguments{
  \item{x}{a \code{SoilProfileCollection} object}
}

\details{This function replaces \code{test_hz_logic}, now marked as deprecated.}

\value{A \code{data.frame} with as many rows as profiles in \code{x}.
	\describe{
		\item{id}{Profile IDs, named according to \code{idname(x)}}

		\item{depthLogic}{boolean, errors related to depth logic}

		\item{sameDepth}{boolean, errors related to same top/bottom depths}
    
    \item{missingDepth}{boolean, NA in top / bottom depths}
    
    \item{overlapOrGap}{boolean, gaps or overlap in adjacent horizons}
    
    \item{valid}{boolean, profile passes all tests}
		}
}


\author{D.E. Beaudette}

\note{There is currently no simple way to fix errors identified by this function. Stay tuned for a \code{fixHzDepthErrors()}.}




\examples{

## sample data
data(sp3)
depths(sp3) <- id ~ top + bottom
 
# these data should be clean
(res <- checkHzDepthLogic(sp3))

}

\keyword{ manip }


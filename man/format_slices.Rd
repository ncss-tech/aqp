\name{format_slices}
\alias{format_slices}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Format Depth Slices of a Soil Profile}
\description{
This function constructions a spatial data frame from soil profile data that has
already been 'sliced' into regular depth intervals.
}
\usage{
format_slices(data, crs, depths, variable = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{a dataframe containing at least: x and y coordinates, and some soil data}
  \item{crs}{a CRS object that is to be assigned to each depth slice SPDF}
  \item{depths}{a vector of depth slices to return}
  \item{variable}{the name of a variable in 'data' that is used to filter out points in 
  space when this variable is NA (optional)}
}
\details{
If the resulting SPDF are to be used for geostatistical interpolation or simulation, it is necessary to supply an argument for 'variable', such that points in space where that variable is NA will be removed.
}
\value{
  A list of SPDF slices are returned, element in the list for each reqested slice in 'depths'.}
  
\references{http://casoilresource.lawr.ucdavis.edu/}
\author{Dylan E Beaudette}
\note{ ~~further notes~~ 

}
\seealso{\code{\link{plot_slices}}}
\examples{

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}

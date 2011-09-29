\name{SoilProfileCollection}
\alias{SoilProfileCollection}
\alias{depths<-}
\alias{horizons}
\alias{horizons<-}
\alias{metadata}
\alias{metadata<-}
\alias{site}
\alias{site<-}
\alias{units}
\alias{units<-}
\alias{profile_id}
\alias{profile_id<-}
\alias{idname}
\alias{names.SoilProfileCollection}

\title{SoilProfileCollection Class}
\description{Basic class for storing soil profile collections, associated site data, and metadata.}

\usage{
SoilProfileCollection(idcol = "id", topcol = "top", bottomcol = "bottom", horizons, 
site = data.frame(), sp = SpatialPoints(matrix(c(1, 1), nrow = 1)), metadata = data.frame())
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{idcol}{the name of the column used to uniquely identify profiles}
  \item{topcol}{the name of the column with horizon top boundaries}
  \item{bottomcol}{the name of the column with horizon bottom boundaries}
  \item{horizons}{a dataframe with horizon data, 1 or more rows per profile}
  \item{site}{a data frame with site data, 1 row per profile}
  \item{sp}{a SpatialPoints class object, 1 row per profile}
  \item{metadata}{a 1-row dataframe with metadata such as depth units}
}
\details{pending}
\value{}
\references{}
\author{Pierre Roudier and Dylan E. Beaudette}
\note{}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\link{profile_plot}}
\examples{
data(sp1)
depths(sp1) <- id ~ top + bottom
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}


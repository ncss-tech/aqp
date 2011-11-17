\name{SoilProfileCollection-class}
\Rdversion{1.1}
\docType{class}
\alias{SoilProfileCollection-class}
\alias{SoilProfileCollection}
\alias{depths<-}
\alias{$,SoilProfileCollection-method}
\alias{$<-,SoilProfileCollection-method}
\alias{[,SoilProfileCollection,ANY,missing-method}
\alias{[[,SoilProfileCollection,ANY,missing-method}
\alias{[[<-,SoilProfileCollection,ANY,missing-method}
\alias{proj4string,SoilProfileCollection-method}
\alias{proj4string<-,SoilProfileCollection-method}
\alias{coordinates,SoilProfileCollection-method}
\alias{coordinates<-,SoilProfileCollection-method}
\alias{horizonDepths}
\alias{horizons}
\alias{horizons<-}
\alias{idname}
\alias{names,SoilProfileCollection-method}
\alias{length,SoilProfileCollection-method}
\alias{max,SoilProfileCollection-method}
\alias{metadata}
\alias{metadata<-}
\alias{min,SoilProfileCollection-method}
\alias{profile_id}
\alias{show,SoilProfileCollection-method}
\alias{site}
\alias{site<-}
\alias{soil.slot.multiple,SoilProfileCollection-method}
\alias{depth_units}
\alias{depth_units<-}
\alias{slice}
\alias{get.slice}
\alias{spatial_subset}

\title{SoilProfileCollection Class}
\description{Basic class for storing soil profile collections, associated site data, and metadata.}

\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("SoilProfileCollection", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{idcol}:}{Object of class \code{"character"} the name of the column used to uniquely identify profiles }
    \item{\code{depthcols}:}{Object of class \code{"character"} with the names of columns containing the horizon top and bottom boundaries}
    \item{\code{metadata}:}{Object of class \code{"data.frame"} a dataframe with horizon data, 1 or more rows per profile }
    \item{\code{horizons}:}{Object of class \code{"data.frame"} a data frame with site data, 1 row per profile }
    \item{\code{site}:}{Object of class \code{"data.frame"} a SpatialPoints class object, 1 row per profile }
    \item{\code{sp}:}{Object of class \code{"SpatialPoints"} a 1-row dataframe with metadata such as depth units }
  }
}
\section{Methods}{
  \describe{
    \item{$}{\code{signature(x = "SoilProfileCollection")}: ... }
    \item{$<-}{\code{signature(x = "SoilProfileCollection")}: ... }
    \item{[}{\code{signature(x = "SoilProfileCollection", i = "ANY", j = "missing")}: ... }
    \item{[[}{\code{signature(x = "SoilProfileCollection", i = "ANY", j = "missing")}: ... }
    \item{[[<-}{\code{signature(x = "SoilProfileCollection", i = "ANY", j = "missing")}: ... }
    \item{coordinates<-}{\code{signature(object = "SoilProfileCollection")}: ... }
    \item{horizonDepths}{\code{signature(object = "SoilProfileCollection")}: ... }
    \item{horizons}{\code{signature(object = "SoilProfileCollection")}: ... }
    \item{horizons<-}{\code{signature(object = "SoilProfileCollection")}: ... }
    \item{idname}{\code{signature(object = "SoilProfileCollection")}: ... }
    \item{names}{\code{signature(x = "SoilProfileCollection")}: ... }
    \item{length}{\code{signature(x = "SoilProfileCollection")}: ... }
    \item{max}{\code{signature(x = "SoilProfileCollection")}: ... }
    \item{metadata}{\code{signature(object = "SoilProfileCollection")}: ... }
    \item{metadata<-}{\code{signature(object = "SoilProfileCollection")}: ... }
    \item{min}{\code{signature(x = "SoilProfileCollection")}: ... }
    \item{profile_id}{\code{signature(object = "SoilProfileCollection")}: ... }
    \item{profile_plot}{\code{signature(object = "SoilProfileCollection")}: ... }
    \item{show}{\code{signature(object = "SoilProfileCollection")}: ... }
    \item{site}{\code{signature(object = "SoilProfileCollection")}: ... }
    \item{site<-}{\code{signature(object = "SoilProfileCollection")}: ... }
    \item{soil.slot.multiple}{\code{signature(data = "SoilProfileCollection")}: ... }
    \item{units}{\code{signature(object = "SoilProfileCollection")}: ... }
    \item{units<-}{\code{signature(object = "SoilProfileCollection")}: ... }
	 }
}
\references{
%%  ~~put references to the literature/web site here~~
}
\author{Pierre Roudier and Dylan E. Beaudette}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
data(sp1)
depths(sp1) <- id ~ top + bottom
}
\keyword{classes}

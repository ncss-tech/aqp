% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Class-SoilProfileCollection.R
\docType{methods}
\name{show}
\alias{show}
\alias{show,SoilProfileCollection-method}
\alias{as.character,SoilProfileCollection-method}
\title{SoilProfileCollection show method}
\usage{
\S4method{show}{SoilProfileCollection}(object)

\S4method{as.character}{SoilProfileCollection}(x, ...)
}
\arguments{
\item{object}{a SoilProfileCollection}

\item{x}{a SoilProfileCollection}

\item{...}{additional arguments (not used)}
}
\description{
Pretty output method for SoilProfileCollection objects. By default this method limits output to 10 columns and 6 rows from the site and horizon tables respectively.

There is an aqp environment option you can set to increase the number of columns shown by default: \code{options(.aqp.show.n.cols = 100)},

\code{as.character()}: Character Representation of SoilProfileCollection Object
}
\examples{

# load a SoilProfileCollection
data(sp5)

# use the show() method
show(sp5)

# which is same as this (in the console)
sp5

}
\keyword{internal}

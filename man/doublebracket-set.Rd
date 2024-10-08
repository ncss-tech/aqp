% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SoilProfileCollection-operators.R
\docType{methods}
\name{[[<-}
\alias{[[<-}
\alias{[[<-,SoilProfileCollection-method}
\alias{[[<-,SoilProfileCollection,ANY,ANY-method}
\title{Add or change column of horizon or site data in a SoilProfileCollection}
\usage{
\S4method{[[}{SoilProfileCollection}(x, i) <- value
}
\arguments{
\item{x}{a SoilProfileCollection}

\item{i}{an expression resolving to a single column name in site or horizon
table-}

\item{value}{New value to replace -- unit length or equal in length to number
of sites or horizons in the collection.}
}
\description{
Add or change the data from a column accessed by name. Column names other
than profile ID are not shared between site and horizons. The benefit of
using double bracket setter over \code{$} is that \code{name} can be
calculated, whereas with \code{$}, it must be known a priori and hard coded.

When using the double bracket setter the length of input and output matching
either the number of sites or number of horizons is used to determine which
slot new columns are assigned to.
}

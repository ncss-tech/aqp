\name{split-methods}
\docType{methods}
\alias{split}
\alias{split,SoilProfileCollection-method}

\title{Split a SoilProfileCollection object into a list of SoilProfileCollection objects.}

\description{This function splits a \code{SoilProfileCollection} into a \code{list} of \code{SoilProfileCollection} objects using a site-level attribute to define groups or profile ID (\code{idname(x)}).}

\section{Methods}{
  \describe{
    
    \item{\code{signature(x = "SoilProfileCollection")}}{
    }
  }}

\usage{
  split(x, f=NULL, drop=TRUE, ...)
}

\arguments{
  \item{x}{a \code{SoilProfileCollection} object}
  \item{f}{character vector naming a single site-level attribute that defines groups, values are automatically converted to factors if not already}
  \item{drop}{logical vector, drop unused levels of \code{f}?}
  \item{\dots}{ignored}
}


\details{Ommision of \code{f} will result in an "identity" split, according to \code{idname(x)}, e.g. a list as long as \code{length(x)}, with a single \code{SoilProfileCollection} per list element.}

\value{A list object.}

\author{D.E. Beaudette}


\keyword{methods}
\keyword{manip}

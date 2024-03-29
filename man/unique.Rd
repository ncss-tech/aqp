% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SoilProfileCollection-methods.R
\docType{methods}
\name{unique,SoilProfileCollection-method}
\alias{unique,SoilProfileCollection-method}
\alias{unique}
\title{Uniqueness within a \code{SoilProfileCollection} via MD5 Hash}
\usage{
\S4method{unique}{SoilProfileCollection}(x, vars, SPC = TRUE)
}
\arguments{
\item{x}{a \code{SoilProfileCollection}}

\item{vars}{Variables to consider in uniqueness.}

\item{SPC}{logical return a \code{SoilProfileCollection} when \code{TRUE}, otherwise vector of profile indices}
}
\value{
\code{SoilProfileCollection} when \code{SPC = TRUE}, otherwise a vector of integers
}
\description{
Unique profiles within a \code{SoilProfileCollection} using and MD5 hash of select horizon and / or site level attributes.
}
\examples{

  # an example soil profile
  x <- data.frame(
    id = 'A',
    name = c('A', 'E', 'Bhs', 'Bt1', 'Bt2', 'BC', 'C'),
    top = c(0, 10, 20, 30, 40, 50, 100),
    bottom = c(10, 20, 30, 40, 50, 100, 125),
    z = c(8, 5, 3, 7, 10, 2, 12)
  )
  
  # init SPC
  depths(x) <- id ~ top + bottom
  
  # horizon depth variability for simulation
  horizons(x)$.sd <- 2
  
  # duplicate several times
  x.dupes <- duplicate(x, times = 5)
  
  # simulate some new profiles based on example
  x.sim <- perturb(x, n = 5, thickness.attr = '.sd')
  
  # graphical check
  plotSPC(x.dupes, name.style = 'center-center')
  plotSPC(x.sim, name.style = 'center-center')
  
  # inspect unique results
  plotSPC(unique(x.dupes, vars = c('top', 'bottom')), name.style = 'center-center')
  
  # uniqueness is a function of variable selection
  plotSPC(unique(x.sim, vars = c('top', 'bottom')), name.style = 'center-center')
  plotSPC(unique(x.sim, vars = c('name')), name.style = 'center-center')
  


}

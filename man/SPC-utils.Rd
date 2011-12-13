\name{SPC-utils}
\docType{methods}
\alias{depths<-}
\alias{depths<-,data.frame-method}

\alias{horizons}
\alias{horizons<-}
\alias{horizons,SoilProfileCollection-method}
\alias{horizons<-,SoilProfileCollection-method}

\alias{site}
\alias{site<-}
\alias{site,SoilProfileCollection-method}
\alias{site<-,SoilProfileCollection-method}

\alias{depth_units}
\alias{depth_units<-}
\alias{depth_units<-,SoilProfileCollection-method}
\alias{depth_units,SoilProfileCollection-method}

\alias{metadata}
\alias{metadata<-}
\alias{metadata,SoilProfileCollection-method}
\alias{metadata<-,SoilProfileCollection-method}

\alias{horizonDepths}
\alias{horizonDepths,SoilProfileCollection-method}

\alias{idname}
\alias{idname,SoilProfileCollection-method}

\alias{profile_id}
\alias{profile_id,SoilProfileCollection-method}

\title{Getters, Setters, and Utility Methods for SoilProfileCollection Objects}
\description{Getters, Setters, and Utility Methods for SoilProfileCollection Objects}
\section{Methods}{
\describe{

\item{\code{signature(object = "SoilProfileCollection")}}{
%%  ~~describe this method here~~
}
}}


\examples{
data(sp1)

## init SoilProfileCollection objects from data.frame
depths(sp1) <- id ~ top + bottom

## depth units
(du <- depth_units(sp1))
depth_units(sp1) <- 'in'
depth_units(sp1) <- du

## get/set metadata on SoilProfileCollection objects
# this is a 1-row data.frame
m <- metadata(sp1)
m$sampler <- 'Dylan'
metadata(sp1) <- m

## extract horizon data from SoilProfileCollection objects as data.frame
h <- horizons(sp1)

# also replace horizon data in SoilProfileCollection objects
# original order and length must be preserved!
horizons(sp1) <- h

	
## getting site-level data
site(sp1)

## setting site-level data
# site-level data from horizon-level data
site(sp1) <- ~ group

# make some fake site data, and append from data.frame
# ordering must match order of profile IDs !!
site(sp1) <- data.frame(p=runif(n=length(sp1)))

# this won't clobber existing data
site(sp1) <- data.frame(p2=runif(n=length(sp1)), p3=runif(n=length(sp1)))
}
\keyword{methods}

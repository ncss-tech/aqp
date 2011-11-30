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

\title{ ~~ Methods for Function \code{horizons} in Package \pkg{aqp} ~~}
\description{
 ~~ Methods for function \code{horizons} in package \pkg{aqp} ~~
}
\section{Methods}{
\describe{

\item{\code{signature(object = "SoilProfileCollection")}}{
%%  ~~describe this method here~~
}
}}


\examples{
## setting site-level data
data(sp1)
depths(sp1) <- id ~ top + bottom
# site-level data from horizon-level data
site(sp1) <- ~ group

# make some fake site data, and append from data.frame
# ordering must match order of profile IDs !!
site(sp1) <- data.frame(p=runif(n=length(sp1)))
# this won't clobber existing data
site(sp1) <- data.frame(p2=runif(n=length(sp1)), p3=runif(n=length(sp1)))

## getting site-level data
site(sp1)
}
\keyword{methods}
